"""CLI entry point for queue generation."""

from __future__ import annotations

import json
import logging
from pathlib import Path
from typing import Annotated

import cyclopts

from qg.builder import QueueGeneratorBuilder
from qg.config import load_all_configs
from qg.params_models import QueueInput


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

        # Load configs
        configs = load_all_configs(config_dir)

        # Load and parse input
        with open(input_json) as f:
            input_data = json.load(f)
        queue_input = QueueInput(**input_data)

        # Build generator and generate queue
        builder = QueueGeneratorBuilder(configs)
        generator = builder.build(queue_input.parameters)
        df = generator.generate(queue_input.samples)

        # Output
        if output:
            df.write_csv(output)
        else:
            print(df.write_csv())

    app()


if __name__ == "__main__":
    cli_main()
