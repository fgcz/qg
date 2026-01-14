"""CLI entry point for queue generation."""

from __future__ import annotations

import json
import sys
from pathlib import Path
from typing import Annotated

import cyclopts

from qg.generator import GenerationSummary, QueueGenerator, QueueInput


def cli_main() -> None:
    """CLI entry point for queue generation.

    Usage:
        uv run qg input.json
        uv run qg input.json -o output.csv
    """
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
        quiet: Annotated[
            bool,
            cyclopts.Parameter(name=["--quiet", "-q"], help="Suppress summary output"),
        ] = False,
    ) -> None:
        """Generate queue CSV from input JSON."""
        summary = GenerationSummary(
            input_file=str(input_json),
            output_file=str(output) if output else None,
            technology="",
            instrument="",
            sampler="",
            pattern="",
            container_id=0,
            input_samples=0,
            output_rows=0,
            qc_rows=0,
            sample_rows=0,
            success=False,
        )

        # Load input JSON
        try:
            with open(input_json) as f:
                input_data = json.load(f)
        except Exception as e:
            summary.error = f"Error loading input JSON: {e}"
            if not quiet:
                summary.print_summary()
            sys.exit(1)

        # Parse input with Pydantic
        try:
            queue_input = QueueInput(**input_data)
            params = queue_input.parameters
            summary.technology = params.technology
            summary.instrument = params.instrument
            summary.sampler = params.sampler
            summary.pattern = params.pattern
            summary.container_id = params.container_id
            summary.input_samples = len(queue_input.samples)
        except Exception as e:
            summary.error = f"Error parsing input: {e}"
            if not quiet:
                summary.print_summary()
            sys.exit(1)

        # Generate queue
        try:
            generator = QueueGenerator(config_dir)
            rows = generator.generate(queue_input)
            csv_output = generator.to_csv(rows, queue_input.parameters.software)
            summary.output_rows = len(rows)
            summary.qc_rows = sum(1 for r in rows if r.sample_type == "qc")
            summary.sample_rows = sum(1 for r in rows if r.sample_type != "qc")
            summary.success = True
        except Exception as e:
            summary.error = f"Error generating queue: {e}"
            if not quiet:
                summary.print_summary()
            sys.exit(1)

        # Output
        if output:
            output.write_text(csv_output)
        else:
            print(csv_output)

        if not quiet:
            summary.print_summary()

    app()
