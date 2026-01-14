"""Aggregate comparison results into test_results.csv database."""

import json
import sys
from pathlib import Path

import polars as pl


def main():
    if len(sys.argv) < 3:
        print("Usage: summarize_results.py <output.csv> <input1.json> [input2.json ...]")
        sys.exit(1)

    output_path = sys.argv[1]
    input_files = sys.argv[2:]

    rows = []
    for filepath in input_files:
        with open(filepath) as f:
            result = json.load(f)
        rows.append({
            "queue_orig": result.get("original_file", ""),
            "queue_generated": result.get("generated_file", ""),
            "qg_parameters": result.get("config_file", ""),
            "comparison_result": result.get("comparison_result", 0),
        })

    df = pl.DataFrame(rows).sort("comparison_result", descending=True)
    df.write_csv(output_path)
    print(f"Wrote {len(rows)} results to {output_path}")


if __name__ == "__main__":
    main()
