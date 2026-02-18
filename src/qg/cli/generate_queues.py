"""CLI entry point for queue generation."""

from __future__ import annotations

from pathlib import Path
from typing import Annotated

import cyclopts
from loguru import logger

from qg.config_models.loader import qg_configuration
from qg.generator import QueueGenerator
from qg.params_models import read_queue_input


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
        verbose: Annotated[
            bool,
            cyclopts.Parameter(name=["--verbose", "-v"], help="Enable verbose logging"),
        ] = False,
    ) -> None:
        """Generate queue from input JSON (CSV or XML based on output_format)."""
        # Configure logging
        if not verbose:
            logger.disable("qg")

        config = qg_configuration(config_dir)

        queue_input = read_queue_input(input_json)

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
