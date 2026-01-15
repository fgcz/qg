"""CLI entry point for queue generation."""

from __future__ import annotations

import json
from pathlib import Path
from typing import Annotated

import cyclopts

from qg.config import load_all_configs
from qg.generator import QueueGenerator, QueueInput


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
    ) -> None:
        """Generate queue CSV from input JSON."""
        # Load configs (required for Pydantic validation)
        load_all_configs(config_dir)

        # Load and parse input
        with open(input_json) as f:
            input_data = json.load(f)
        queue_input = QueueInput(**input_data)

        # Generate queue
        generator = QueueGenerator(config_dir)
        rows = generator.generate(queue_input)
        csv_output = generator.to_csv(rows, queue_input.parameters.output_format)

        # Output
        if output:
            output.write_text(csv_output)
        else:
            print(csv_output)

    app()


if __name__ == "__main__":
    cli_main()
