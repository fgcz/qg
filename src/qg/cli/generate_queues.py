"""CLI entry point for queue generation."""

from __future__ import annotations

import logging
from pathlib import Path
from typing import Annotated

import cyclopts

from qg.config_models.loader import qg_configuration
from qg.generator import QueueGenerator
from qg.params_models import PlateQueueInput, read_queue_input


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
            cyclopts.Parameter(name=["--output", "-o"], help="Output CSV file (default: stdout)"),
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
        """Generate queue CSV from input JSON."""
        # Configure logging
        if verbose:
            logging.basicConfig(
                level=logging.INFO,
                format="%(name)s: %(message)s",
            )

        config = qg_configuration(config_dir)

        queue_input = read_queue_input(input_json)

        # Generate queue
        layout_mode = "plate" if isinstance(queue_input, PlateQueueInput) else "vial"
        generator = QueueGenerator(config, queue_input, layout_mode)
        df = generator.generate()

        # Output
        if output:
            df.write_csv(output)
        else:
            print(df.write_csv())

    app()


if __name__ == "__main__":
    cli_main()
