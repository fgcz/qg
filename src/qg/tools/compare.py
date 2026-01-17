"""Compare generated queue CSV with original reference.

Compares:
- Sample counts
- Sample IDs present
- QC injection counts
- Position plate usage
"""

from __future__ import annotations

import json
import re
from pathlib import Path
from typing import Annotated

import cyclopts
import polars as pl

app = cyclopts.App(
    name="compare",
    help="Compare generated queue CSV with original reference.",
)

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


def compare_queues(generated_path: str | Path, original_path: str | Path, config_path: str | Path) -> dict:
    """Compare generated and original queue CSVs."""
    result = {
        "generated_file": str(generated_path),
        "original_file": str(original_path),
        "config_file": str(config_path),
        "status": "unknown",
        "errors": [],
        "warnings": [],
        "metrics": {},
    }

    generated_path = Path(generated_path)
    original_path = Path(original_path)

    # Check files exist
    if not generated_path.exists():
        result["status"] = "error"
        result["comparison_result"] = 0
        result["errors"].append("Generated file does not exist")
        return result

    if not original_path.exists():
        result["status"] = "error"
        result["comparison_result"] = 0
        result["errors"].append(f"Original file does not exist: {original_path}")
        return result

    # Check generated file is not empty
    if generated_path.stat().st_size == 0:
        result["status"] = "error"
        result["comparison_result"] = 0
        result["errors"].append("Generated file is empty (generation failed)")
        return result

    try:
        # Read generated CSV (queue_generator format)
        gen_df = pl.read_csv(generated_path)
        # Read original CSV (SLD format)
        orig_df = pl.read_csv(original_path)
    except Exception as e:
        result["status"] = "error"
        result["comparison_result"] = 0
        result["errors"].append(f"Failed to read CSVs: {e}")
        return result

    # Determine column names - support multiple naming conventions
    def find_column(df: pl.DataFrame, candidates: list[str]) -> str | None:
        for col in candidates:
            if col in df.columns:
                return col
        return None

    gen_filename_col = find_column(gen_df, ["File Name", "filename", "Xcalibur Filename", "Data File"])
    gen_position_col = find_column(gen_df, ["Position", "vial", "Source Vial"])
    orig_filename_col = find_column(orig_df, ["filename", "File Name", "Xcalibur Filename"])
    orig_position_col = find_column(orig_df, ["vial", "Position", "Source Vial"])

    if not gen_filename_col or not orig_filename_col:
        result["status"] = "error"
        result["comparison_result"] = 0
        result["errors"].append(f"Missing filename column. Generated: {gen_df.columns}, Original: {orig_df.columns}")
        return result

    # Extract metrics
    gen_sample_ids = extract_sample_ids(gen_df, gen_filename_col)
    orig_sample_ids = extract_sample_ids(orig_df, orig_filename_col)

    gen_qc_count = count_qc(gen_df, gen_filename_col)
    orig_qc_count = count_qc(orig_df, orig_filename_col)

    gen_plates = get_plates_used(gen_df, gen_position_col) if gen_position_col else set()
    orig_plates = get_plates_used(orig_df, orig_position_col) if orig_position_col else set()

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

    # Determine status and comparison_result score
    # 10 = perfect, 5 = samples+paths OK but QC mismatch, 0 = fail
    qc_mismatch = abs(gen_qc_count - orig_qc_count) > 2

    if gen_sample_ids == orig_sample_ids:
        result["status"] = "pass"
        if qc_mismatch:
            result["comparison_result"] = 5
            result["warnings"].append(
                f"QC count differs significantly: {gen_qc_count} vs {orig_qc_count}"
            )
        else:
            result["comparison_result"] = 10
    elif gen_sample_ids.issubset(orig_sample_ids) or orig_sample_ids.issubset(gen_sample_ids):
        result["status"] = "partial"
        result["comparison_result"] = 0
        result["warnings"].append("Sample ID sets overlap but don't match exactly")
    else:
        result["status"] = "fail"
        result["comparison_result"] = 0
        result["errors"].append("Sample ID sets don't match")

    return result


@app.default
def main(
    generated: Annotated[
        Path,
        cyclopts.Parameter(help="Path to generated queue CSV"),
    ],
    original: Annotated[
        Path,
        cyclopts.Parameter(help="Path to original reference CSV"),
    ],
    config: Annotated[
        Path,
        cyclopts.Parameter(help="Path to queue params JSON config"),
    ],
    output: Annotated[
        Path,
        cyclopts.Parameter(help="Output JSON file for comparison result"),
    ],
) -> None:
    """Compare generated queue CSV with original reference CSV."""
    result = compare_queues(generated, original, config)

    with open(output, "w") as f:
        json.dump(result, f, indent=2)


if __name__ == "__main__":
    app()
