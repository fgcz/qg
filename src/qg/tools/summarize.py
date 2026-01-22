"""Aggregate comparison results into test_results.csv database."""

from __future__ import annotations

import json
from pathlib import Path
from typing import Annotated

import cyclopts
import polars as pl

app = cyclopts.App(
    name="summarize",
    help="Aggregate comparison results into a summary CSV.",
)


def extract_instrument_sampler(config_path: str) -> tuple[str, str]:
    """Extract instrument and sampler from config JSON file."""
    config_file = Path(config_path)
    if not config_file.exists():
        return "", ""

    with open(config_file) as f:
        config = json.load(f)
    params = config.get("parameters", {})
    return params.get("instrument", ""), params.get("sampler", "")


def aggregate_results(input_files: list[Path]) -> pl.DataFrame:
    """Aggregate comparison result JSON files into a DataFrame."""
    rows = []
    for filepath in input_files:
        with open(filepath) as f:
            result = json.load(f)
        config_file = result.get("config_file", "")
        instrument, sampler = extract_instrument_sampler(config_file)
        rows.append(
            {
                "instrument": instrument,
                "sampler": sampler,
                "queue_orig": result.get("original_file", ""),
                "queue_generated": result.get("generated_file", ""),
                "qg_parameters": config_file,
                "comparison_result": result.get("comparison_result", 0),
            }
        )

    return pl.DataFrame(rows).sort(["instrument", "comparison_result"], descending=[False, True])


@app.default
def main(
    output: Annotated[
        Path,
        cyclopts.Parameter(help="Output CSV file path"),
    ],
    input_files: Annotated[
        list[Path],
        cyclopts.Parameter(help="Input JSON comparison result files"),
    ],
) -> None:
    """Aggregate comparison result JSONs into a summary CSV."""
    if not input_files:
        print("No input files provided")
        return

    df = aggregate_results(input_files)
    df.write_csv(output)
    print(f"Wrote {len(df)} results to {output}")


if __name__ == "__main__":
    app()
