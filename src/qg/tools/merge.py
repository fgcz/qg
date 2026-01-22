"""Merge all instrument CSV files into a single file with source metadata.

Creates reference/merged.csv which is used for queue analysis.
"""

from __future__ import annotations

from pathlib import Path
from typing import Annotated

import cyclopts
import polars as pl

app = cyclopts.App(
    name="merge",
    help="Merge all instrument CSV files into a single file.",
)


def merge_all_csvs(base_path: Path) -> pl.DataFrame:
    """Merge all CSV files from all instruments into a single DataFrame.

    Args:
        base_path: Path to directory containing instrument subdirs (e.g., reference/csv/)
    """
    all_dfs = []

    # Map directory names to instrument IDs
    instrument_map = {
        "ascend": "ASCEND_1",
        "astral": "ASTRAL_1",
        "exploris2": "EXPLORIS_2",
        "lumos2": "LUMOS_2",
    }

    for instrument_dir, instrument_id in instrument_map.items():
        csv_dir = base_path / instrument_dir
        if not csv_dir.exists():
            print(f"Skipping {instrument_dir} - directory not found")
            continue

        for csv_file in csv_dir.glob("*.csv"):
            try:
                df = pl.read_csv(csv_file, infer_schema_length=0)
                if df.height == 0:
                    continue

                # Add metadata columns
                df = df.with_columns(
                    [
                        pl.lit(instrument_id).alias("instrument"),
                        pl.lit(csv_file.stem).alias("source_file"),
                    ]
                )
                all_dfs.append(df)
            except (pl.exceptions.ComputeError, OSError) as e:
                # Log and continue - don't let one bad file stop the merge
                print(f"Warning: skipping {csv_file}: {e}")

    if not all_dfs:
        return pl.DataFrame()

    # Concatenate all dataframes
    merged = pl.concat(all_dfs, how="diagonal")

    return merged


def classify_sample(filename: str) -> str:
    """Classify sample type based on filename."""
    if not filename:
        return "unknown"
    fn_lower = filename.lower()
    if "autoqc" in fn_lower:
        return "qc"
    elif "clean" in fn_lower:
        return "clean"
    elif "blank" in fn_lower:
        return "blank"
    else:
        return "sample"


@app.default
def main(
    input_dir: Annotated[
        Path,
        cyclopts.Parameter(help="Input directory containing instrument subdirs with CSVs"),
    ] = Path("reference/csv"),
    *,
    output: Annotated[
        Path,
        cyclopts.Parameter(name=["--output", "-o"], help="Output merged CSV file"),
    ] = Path("reference/merged.csv"),
) -> None:
    """Merge all CSVs from instrument subdirectories into a single file."""
    merged = merge_all_csvs(input_dir)

    if merged.height == 0:
        print("No data to merge")
        return

    # Add sample_type classification
    merged = merged.with_columns(
        [pl.col("filename").map_elements(classify_sample, return_dtype=pl.Utf8).alias("sample_type")]
    )

    # Save to CSV
    output.parent.mkdir(parents=True, exist_ok=True)
    merged.write_csv(output)

    print(f"Merged {merged.height} rows from {merged['source_file'].n_unique()} files")
    print(f"Instruments: {merged['instrument'].unique().to_list()}")
    print(f"Sample types: {merged.group_by('sample_type').len().sort('len', descending=True)}")
    print(f"Saved to: {output}")


if __name__ == "__main__":
    app()
