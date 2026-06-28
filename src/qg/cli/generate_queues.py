"""CLI entry point for queue generation."""

from __future__ import annotations

import sys
from pathlib import Path
from typing import Annotated, Literal

import cyclopts
from loguru import logger
from pydantic import ValidationError

from qg.config_models.loader import ConfigValidationError, QGConfiguration, qg_configuration
from qg.generator import QueueGenerator
from qg.params_models import QueueInput, read_queue_input


def _warn(message: str) -> None:
    """Emit a reproducibility warning to stderr (always visible, regardless of -v)."""
    print(f"qg: warning: {message}", file=sys.stderr)


def _current_qg_version() -> str | None:
    from importlib.metadata import PackageNotFoundError, version

    try:
        return version("qg")
    except PackageNotFoundError:
        return None


def _resolve_config(
    queue_input: QueueInput,
    input_json: Path,
    config_dir: Path,
    prefer: Literal["embedded", "external"],
) -> QGConfiguration:
    """Choose the configuration source, warning about version / config drift.

    With ``prefer='embedded'`` (the default), a params JSON that carries a
    ``resolved_config`` regenerates from that self-contained snapshot — no
    ``qg_configs/`` required and immune to later edits of it. Falls back to the
    external ``config_dir`` when no snapshot is present or ``--prefer external``.
    """
    embedded = queue_input.resolved_config
    use_embedded = prefer == "embedded" and embedded is not None

    if prefer == "embedded" and embedded is None:
        logger.info("No embedded resolved_config in {}; using external config dir {}.", input_json, config_dir)

    stamped, current = queue_input.qg_version, _current_qg_version()
    if embedded is not None and stamped and current and stamped != current:
        _warn(
            f"params.json was generated with qg {stamped} but qg {current} is running; "
            "the generation algorithm may differ between versions."
        )

    if use_embedded:
        # Best-effort drift check: if a live config dir is present, report when it has
        # diverged from the embedded snapshot. Never fatal — reproduction uses the snapshot.
        if config_dir.is_dir():
            try:
                live = qg_configuration(config_dir)
                if embedded.differs_from(live, queue_input.parameters):
                    _warn(
                        f"embedded resolved_config differs from {config_dir}; reproducing from the "
                        "embedded snapshot (pass --prefer external to use the live config instead)."
                    )
            except (ConfigValidationError, ValidationError, OSError) as exc:
                logger.debug("Drift check skipped (could not load {}): {}", config_dir, exc)
        return embedded.to_configuration()

    return qg_configuration(config_dir)


def cli_main() -> None:
    """CLI entry point for queue generation."""
    app = cyclopts.App(
        help="Generate queue files for mass spectrometry instruments.",
    )

    @app.default
    def main(
        input_json: Annotated[
            Path,
            cyclopts.Parameter(help="Input JSON file with parameters and samples"),
        ],
        *,
        output: Annotated[
            Path | None,
            cyclopts.Parameter(name=["--output", "-o"], help="Output file (default: stdout)"),
        ] = None,
        config_dir: Annotated[
            Path,
            cyclopts.Parameter(name=["--config-dir", "-c"], help="Config directory"),
        ] = Path("qg_configs"),
        prefer: Annotated[
            Literal["embedded", "external"],
            cyclopts.Parameter(
                name=["--prefer"],
                help="Config source when the params carry an embedded resolved_config (default: embedded).",
            ),
        ] = "embedded",
        verbose: Annotated[
            bool,
            cyclopts.Parameter(name=["--verbose", "-v"], help="Enable verbose logging"),
        ] = False,
    ) -> None:
        """Generate queue from input JSON (CSV or XML based on output_format)."""
        # Configure logging
        if not verbose:
            logger.disable("qg")

        queue_input = read_queue_input(input_json)
        config = _resolve_config(queue_input, input_json, config_dir, prefer)

        # Generate queue
        generator = QueueGenerator(config, queue_input)
        content = generator.write()

        # Output (respects output_format: CSV or XML)
        if output:
            output.write_text(content)
        else:
            print(content)

    app()


if __name__ == "__main__":
    cli_main()
