"""Summarize comparison results into a CSV.

Snakemake script that aggregates all comparison JSON files.
"""

import json
from pathlib import Path

import polars as pl

# Snakemake provides: snakemake.input, snakemake.output


def summarize_comparisons(input_files: list[str], output_file: str) -> None:
    """Aggregate comparison results into summary CSV."""
    rows = []

    for filepath in input_files:
        try:
            with open(filepath) as f:
                result = json.load(f)

            metrics = result.get("metrics", {})
            row = {
                "name": Path(filepath).stem,
                "status": result.get("status", "unknown"),
                "generated_samples": metrics.get("generated_sample_count", 0),
                "original_samples": metrics.get("original_sample_count", 0),
                "samples_match": metrics.get("sample_ids_match", False),
                "generated_qc": metrics.get("generated_qc_count", 0),
                "original_qc": metrics.get("original_qc_count", 0),
                "generated_rows": metrics.get("generated_total_rows", 0),
                "original_rows": metrics.get("original_total_rows", 0),
                "errors": "; ".join(result.get("errors", [])),
                "warnings": "; ".join(result.get("warnings", [])),
            }
            rows.append(row)
        except Exception as e:
            rows.append({
                "name": Path(filepath).stem,
                "status": "error",
                "errors": str(e),
            })

    df = pl.DataFrame(rows)

    # Sort by status (errors first, then fails, then partial, then pass)
    status_order = {"error": 0, "fail": 1, "partial": 2, "pass": 3, "unknown": 4}
    df = df.with_columns(
        pl.col("status").replace(status_order).alias("_sort_order")
    ).sort("_sort_order", "name").drop("_sort_order")

    df.write_csv(output_file)

    # Print summary stats
    status_counts = df.group_by("status").len().sort("status")
    print("\n=== Summary ===")
    for row in status_counts.iter_rows(named=True):
        print(f"  {row['status']}: {row['len']}")
    print(f"  Total: {len(df)}")


if __name__ == "__main__":
    import sys

    if len(sys.argv) < 2:
        print("Usage: summarize.py <output_csv> <input_json1> [input_json2 ...]")
        sys.exit(1)

    output_file = sys.argv[1]
    input_files = sys.argv[2:]

    summarize_comparisons(input_files, output_file)
