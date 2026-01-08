"""
Analyze queue CSV files from queue/csvfiles to classify and extract metadata.
"""

import polars as pl
from pathlib import Path
import re


def get_columns(csv_path: Path) -> tuple[str, ...]:
    """Read just the header to get column names."""
    with open(csv_path) as f:
        header = f.readline().strip()
    return tuple(header.split(","))


def classify_by_structure(csv_dir: Path) -> dict[tuple[str, ...], list[Path]]:
    """Group CSV files by their column structure."""
    structure_groups: dict[tuple[str, ...], list[Path]] = {}

    for csv_path in sorted(csv_dir.glob("*.csv")):
        try:
            cols = get_columns(csv_path)
            if cols not in structure_groups:
                structure_groups[cols] = []
            structure_groups[cols].append(csv_path)
        except Exception as e:
            print(f"Error reading {csv_path.name}: {e}")

    return structure_groups


def merge_csvs(csv_paths: list[Path]) -> pl.DataFrame:
    """Merge multiple CSVs with same structure into one DataFrame."""
    dfs = []
    for path in csv_paths:
        df = pl.read_csv(path)
        df = df.with_columns(pl.lit(path.name).alias("source_file"))
        dfs.append(df)
    return pl.concat(dfs)


def extract_order_id(filename: str) -> str | None:
    """Extract order ID from filename (e.g., 'o39090' or '40253')."""
    # Pattern like o39090 or O40499
    match = re.search(r'[oO](\d+)', filename)
    if match:
        return match.group(1)
    # Pattern like _40253_ in filename
    match = re.search(r'_(\d{5})_', filename)
    if match:
        return match.group(1)
    return None


def analyze_methods(df: pl.DataFrame) -> pl.DataFrame:
    """Extract unique methods and their components."""
    if "method" not in df.columns:
        return pl.DataFrame()

    methods = df.select("method").unique()
    return methods


def main():
    csv_dir = Path("/Users/wolski/projects/queue/csvfiles")

    print("=" * 60)
    print("STEP 1: Classify CSV files by column structure")
    print("=" * 60)

    structure_groups = classify_by_structure(csv_dir)

    for cols, files in structure_groups.items():
        print(f"\nColumns: {cols}")
        print(f"  File count: {len(files)}")
        print(f"  Example files: {[f.name for f in files[:3]]}")

    # Work with the main structure (most common)
    main_cols = max(structure_groups.keys(), key=lambda k: len(structure_groups[k]))
    main_files = structure_groups[main_cols]

    print("\n" + "=" * 60)
    print(f"STEP 2: Merge {len(main_files)} CSV files with main structure")
    print("=" * 60)

    merged_df = merge_csvs(main_files)
    print(f"Total rows: {len(merged_df)}")

    print("\n" + "=" * 60)
    print("STEP 3: Analyze unique methods")
    print("=" * 60)

    if "method" in merged_df.columns:
        unique_methods = merged_df.select("method").unique().sort("method")
        print(f"Unique methods: {len(unique_methods)}")
        for row in unique_methods.iter_rows():
            print(f"  {row[0]}")

    print("\n" + "=" * 60)
    print("STEP 4: Analyze output paths")
    print("=" * 60)

    if "output_path" in merged_df.columns:
        unique_paths = merged_df.select("output_path").unique().sort("output_path")
        print(f"Unique output paths: {len(unique_paths)}")
        for row in unique_paths.iter_rows():
            print(f"  {row[0]}")

    print("\n" + "=" * 60)
    print("STEP 5: Analyze vial/position patterns")
    print("=" * 60)

    if "vial" in merged_df.columns:
        # Get unique plate prefixes (Y:, B:, R:, G:, 1:, etc.)
        vials = merged_df.select("vial").unique()
        prefixes = set()
        for row in vials.iter_rows():
            if row[0] and ":" in str(row[0]):
                prefix = str(row[0]).split(":")[0]
                prefixes.add(prefix)
        print(f"Vial prefixes: {sorted(prefixes)}")

    print("\n" + "=" * 60)
    print("STEP 6: Files with order IDs")
    print("=" * 60)

    files_with_orders = []
    for f in main_files:
        order_id = extract_order_id(f.name)
        if order_id:
            files_with_orders.append((f.name, order_id))

    print(f"Files with order IDs: {len(files_with_orders)}")
    for fname, oid in sorted(set((f, o) for f, o in files_with_orders), key=lambda x: x[1]):
        print(f"  {fname} -> order {oid}")

    # Save merged data for inspection
    output_path = Path("/Users/wolski/projects/qg_dump/merged_queue_data.csv")
    merged_df.write_csv(output_path)
    print(f"\nMerged data saved to: {output_path}")


if __name__ == "__main__":
    main()
