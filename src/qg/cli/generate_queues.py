"""CLI entry point for queue generation."""

from __future__ import annotations

from pathlib import Path
from typing import Annotated

import cyclopts
from loguru import logger

from qg.artifacts import save_generation_artifact, save_positioning_artifacts
from qg.generator import QueueGenerator, format_table, write_queue
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
        positioned_input = queue_input.position_queue()
        config = positioned_input.resolved_config.to_configuration()
        generator = QueueGenerator(config, positioned_input)
        queue_rows = generator.build_rows()
        generated = format_table(
            queue_rows,
            generator.output_format,
            generator.plate_layout,
            generator.tech_area,
        )
        content = write_queue(generated, generator.output_format)

        # Output (respects output_format: CSV or XML)
        if output:
            output.write_text(content)
        else:
            print(content)

        stem = save_positioning_artifacts(queue_input, positioned_input)
        save_generation_artifact(positioned_input, queue_rows.to_table(), stem=stem)

    app()


if __name__ == "__main__":
    cli_main()
