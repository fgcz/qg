"""Compare generated queue CSV with original.

Snakemake script that compares:
- Sample counts
- Sample IDs present
- QC injection counts
- Position plate usage
"""

import json
import re
from pathlib import Path

import polars as pl

# Snakemake provides: snakemake.input, snakemake.output, snakemake.params

SAMPLE_ID_PATTERN = re.compile(r"_S(\d+)_")
QC_PATTERNS = re.compile(r"(autoQC|clean|blank)", re.IGNORECASE)


def extract_sample_ids(df: pl.DataFrame, filename_col: str) -> set[int]:
    """Extract sample IDs from filename column."""
    ids = set()
    for filename in df[filename_col].to_list():
        if filename and not QC_PATTERNS.search(str(filename)):
            match = SAMPLE_ID_PATTERN.search(str(filename))
            if match:
                ids.add(int(match.group(1)))
    return ids


def count_qc(df: pl.DataFrame, filename_col: str) -> int:
    """Count QC injections."""
    count = 0
    for filename in df[filename_col].to_list():
        if filename and QC_PATTERNS.search(str(filename)):
            count += 1
    return count


def get_plates_used(df: pl.DataFrame, position_col: str) -> set[str]:
    """Extract unique plates from positions (e.g., Y:A1 -> Y)."""
    plates = set()
    for pos in df[position_col].to_list():
        if pos and ":" in str(pos):
            plate = str(pos).split(":")[0]
            plates.add(plate)
    return plates


def compare_queues(generated_path: str, original_path: str, config_path: str) -> dict:
    """Compare generated and original queue CSVs."""
    result = {
        "generated_file": generated_path,
        "original_file": original_path,
        "config_file": config_path,
        "status": "unknown",
        "errors": [],
        "warnings": [],
        "metrics": {},
    }

    # Check files exist
    if not Path(generated_path).exists():
        result["status"] = "error"
        result["errors"].append("Generated file does not exist")
        return result

    if not Path(original_path).exists():
        result["status"] = "error"
        result["errors"].append(f"Original file does not exist: {original_path}")
        return result

    # Check generated file is not empty
    if Path(generated_path).stat().st_size == 0:
        result["status"] = "error"
        result["errors"].append("Generated file is empty (generation failed)")
        return result

    try:
        # Read generated CSV (queue_generator format)
        gen_df = pl.read_csv(generated_path)
        # Read original CSV (SLD format)
        orig_df = pl.read_csv(original_path)
    except Exception as e:
        result["status"] = "error"
        result["errors"].append(f"Failed to read CSVs: {e}")
        return result

    # Determine column names
    gen_filename_col = "File Name" if "File Name" in gen_df.columns else "filename"
    gen_position_col = "Position" if "Position" in gen_df.columns else "vial"
    orig_filename_col = "filename" if "filename" in orig_df.columns else "File Name"
    orig_position_col = "vial" if "vial" in orig_df.columns else "Position"

    # Extract metrics
    gen_sample_ids = extract_sample_ids(gen_df, gen_filename_col)
    orig_sample_ids = extract_sample_ids(orig_df, orig_filename_col)

    gen_qc_count = count_qc(gen_df, gen_filename_col)
    orig_qc_count = count_qc(orig_df, orig_filename_col)

    gen_plates = get_plates_used(gen_df, gen_position_col)
    orig_plates = get_plates_used(orig_df, orig_position_col)

    result["metrics"] = {
        "generated_total_rows": len(gen_df),
        "original_total_rows": len(orig_df),
        "generated_sample_count": len(gen_sample_ids),
        "original_sample_count": len(orig_sample_ids),
        "generated_qc_count": gen_qc_count,
        "original_qc_count": orig_qc_count,
        "generated_plates": sorted(gen_plates),
        "original_plates": sorted(orig_plates),
        "sample_ids_match": gen_sample_ids == orig_sample_ids,
        "missing_in_generated": sorted(orig_sample_ids - gen_sample_ids),
        "extra_in_generated": sorted(gen_sample_ids - orig_sample_ids),
    }

    # Determine status
    if gen_sample_ids == orig_sample_ids:
        result["status"] = "pass"
    elif gen_sample_ids.issubset(orig_sample_ids) or orig_sample_ids.issubset(gen_sample_ids):
        result["status"] = "partial"
        result["warnings"].append("Sample ID sets overlap but don't match exactly")
    else:
        result["status"] = "fail"
        result["errors"].append("Sample ID sets don't match")

    # Add warnings for QC differences
    if abs(gen_qc_count - orig_qc_count) > 2:
        result["warnings"].append(
            f"QC count differs significantly: {gen_qc_count} vs {orig_qc_count}"
        )

    return result


if __name__ == "__main__":
    import sys

    if len(sys.argv) != 5:
        print("Usage: compare_queues.py <generated> <original> <config> <output>")
        sys.exit(1)

    generated_path = sys.argv[1]
    original_path = sys.argv[2]
    config_path = sys.argv[3]
    output_path = sys.argv[4]

    result = compare_queues(generated_path, original_path, config_path)

    with open(output_path, "w") as f:
        json.dump(result, f, indent=2)
