#!/usr/bin/env python3
"""Parse Thermo Xcalibur .sld sequence list files and extract queue information."""

import json
import re
from pathlib import Path


def parse_sld_file_new_format(text: str) -> list[dict]:
    """Parse new format SLD files (with JSON blocks)."""
    json_pattern = r'\{"[$]type":"Thermo\.Xcalibur\.Common\.Models\.Experiment\.Capabilities\.SampleExtensionInfo.*?"DisplayRowNumber":(\d+),"DisplayLabel":\d+\}\}\}'
    json_matches = list(re.finditer(json_pattern, text))

    if not json_matches:
        return []

    samples = []
    for match in json_matches:
        json_str = match.group(0)
        try:
            data = json.loads(json_str)
            reinject_data = data.get("CapabilityData", {}).get("ReInject", {})
            raw_filename = reinject_data.get("OriginalRawFileNameWithoutExtension", "")
            row_number = reinject_data.get("DisplayRowNumber", 0)

            # Parse the run number from filename (e.g., "20250425_003_S930976_..." -> 3)
            run_match = re.match(r"\d+_(\d+)_", raw_filename)
            run_number = int(run_match.group(1)) if run_match else row_number

            samples.append({
                "row": row_number,
                "run": run_number,
                "filename": raw_filename,
            })
        except json.JSONDecodeError:
            continue

    # Extract more details by finding text before each JSON block
    for i, match in enumerate(json_matches):
        start = json_matches[i - 1].end() if i > 0 else 0
        end = match.start()
        chunk = text[start:end]

        # Extract method and output BEFORE removing nulls (they are separators)
        method_match = re.search(r"(C:\\Xcalibur\\methods\\[^\x00-\x1f@]+)", chunk)
        if method_match and i < len(samples):
            samples[i]["method"] = method_match.group(1)

        path_match = re.search(r"(D:\\Data2San\\[^\x00-\x1f@]+)", chunk)
        if path_match and i < len(samples):
            samples[i]["output_path"] = path_match.group(1)

        # Now remove nulls for vial extraction (vials may have nulls around them)
        chunk_clean = chunk.replace("\x00", "")
        # Match vial patterns: Y:A1 (vials), 1:F, 1:F8, 2:A1 (plates)
        vial_match = re.search(r"(\d+:[A-Z]\d*|[A-Z]:[A-Z]\d+)", chunk_clean)
        if vial_match and i < len(samples):
            samples[i]["vial"] = vial_match.group(1)

    return samples


def parse_sld_file_old_format(text: str) -> list[dict]:
    r"""Parse old format SLD files (without JSON blocks).

    Old format stores sample data as null-separated fields with patterns:
    - Method: C:\Xcalibur\methods\...
    - Filename: 20YYMMDD_... pattern between method and output path
    - Output path: D:\Data2San\...
    - Vial: patterns like 1:F, EvoSlot 2:1, B:A1
    """
    samples = []

    # Find all method paths with their positions
    method_pattern = r'(C:\\Xcalibur\\methods\\[^\x00-\x1f]+?)(?=[\x00-\x1f]|\s{2,}|$)'
    methods = [(m.start(), m.group(1).rstrip()) for m in re.finditer(method_pattern, text)]

    # Find all output paths with their positions
    output_pattern = r'(D:\\Data2San\\[^\x00-\x1f]+?)(?=[\x00-\x1f]|\s{2,}|$)'
    outputs = [(m.start(), m.group(1).rstrip()) for m in re.finditer(output_pattern, text)]

    # Find all vial positions - multiple patterns for different systems
    # Trap/Vial: 1:F, 1:A1, B:F9, Y:A1
    # Evosep: EvoSlot 2:1, EvoSlot 2:64
    vial_pattern = r'(?:EvoSlot\s+)?(\d+:[A-Z]?\d*|[A-Z]:[A-Z]?\d+)'
    vials = [(m.start(), m.group(0)) for m in re.finditer(vial_pattern, text)]

    # Find filenames - appear between method and output path
    # Patterns: 20YYMMDD_XXX_... (flexible middle part - can be numbers, order IDs like C3000, etc.)
    filename_pattern = r'(20\d{6}_[^\x00-\x1f]+?)(?=[\x00-\x1f]|D:\\|$)'
    filenames = [(m.start(), m.group(1).rstrip()) for m in re.finditer(filename_pattern, text)]

    # Match components by proximity - each sample should have method, filename, output in sequence
    for i, (method_pos, method) in enumerate(methods):
        sample = {
            "row": i + 1,
            "run": i + 1,
            "method": method,
        }

        # Find the closest filename after this method
        next_method_pos = methods[i + 1][0] if i + 1 < len(methods) else len(text)

        for fn_pos, filename in filenames:
            if method_pos < fn_pos < next_method_pos:
                sample["filename"] = filename
                # Extract run number from filename - try multiple patterns
                # Pattern 1: 20YYMMDD_NNN_... (run number as 3 digits)
                run_match = re.match(r"20\d{6}_(\d{3})_", filename)
                if run_match:
                    sample["run"] = int(run_match.group(1))
                else:
                    # Pattern 2: 20YYMMDD_CXXXX_NNN_... (order format)
                    run_match = re.match(r"20\d{6}_C\d+_(\d{3})", filename)
                    if run_match:
                        sample["run"] = int(run_match.group(1))
                break

        # Find the closest output path after this method
        for out_pos, output in outputs:
            if method_pos < out_pos < next_method_pos:
                sample["output_path"] = output
                break

        # Find vial - can be before method (trap systems) or after output (Evosep)
        prev_method_pos = methods[i - 1][0] if i > 0 else 0
        closest_vial = None
        closest_dist = float('inf')

        # First try: vial before method (trap/vial systems)
        for vial_pos, vial in vials:
            if prev_method_pos < vial_pos < method_pos:
                dist = method_pos - vial_pos
                if dist < closest_dist:
                    closest_dist = dist
                    closest_vial = vial

        # Second try: vial after output path (Evosep systems)
        if not closest_vial and "output_path" in sample:
            out_pos = None
            for op, _ in outputs:
                if method_pos < op < next_method_pos:
                    out_pos = op
                    break
            if out_pos:
                for vial_pos, vial in vials:
                    if out_pos < vial_pos < next_method_pos:
                        closest_vial = vial
                        break

        if closest_vial:
            sample["vial"] = closest_vial

        samples.append(sample)

    return samples


def parse_sld_file(filepath: str) -> list[dict]:
    """Parse a Thermo .sld file and return a list of sample entries."""
    with open(filepath, "rb") as f:
        raw_data = f.read()

    # Decode from UTF-16LE, ignoring errors for binary portions
    try:
        text = raw_data.decode("utf-16-le", errors="replace")
    except Exception:
        text = raw_data.decode("latin-1", errors="replace")

    # Try new format first (with JSON blocks)
    samples = parse_sld_file_new_format(text)

    # Fall back to old format if no JSON found
    if not samples:
        samples = parse_sld_file_old_format(text)

    # Sort by row number
    samples.sort(key=lambda x: x.get("row", 0))

    return samples


def print_queue_table(samples: list[dict], filename: str):
    """Print samples as a formatted table."""
    print(f"\n{'=' * 80}")
    print(f"Queue from: {filename}")
    print(f"{'=' * 80}")
    print(f"{'Row':<5} {'Run':<5} {'Vial':<8} {'Filename':<50}")
    print("-" * 80)

    for s in samples:
        print(
            f"{s.get('row', ''):<5} "
            f"{s.get('run', ''):<5} "
            f"{s.get('vial', 'N/A'):<8} "
            f"{s.get('filename', ''):<50}"
        )

    print(f"\nTotal samples: {len(samples)}")


def export_csv(samples: list[dict], output_path: str):
    """Export samples to CSV."""
    import csv

    with open(output_path, "w", newline="") as f:
        writer = csv.DictWriter(
            f, fieldnames=["row", "run", "vial", "filename", "method", "output_path"]
        )
        writer.writeheader()
        writer.writerows(samples)


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="Parse Thermo Xcalibur .sld files")
    parser.add_argument("sld_file", help="Input .sld file")
    parser.add_argument("--csv", dest="csv_output", help="Output CSV file")
    parser.add_argument("-v", "--verbose", action="store_true", help="Print table to stdout")
    args = parser.parse_args()

    samples = parse_sld_file(args.sld_file)

    if args.verbose:
        print_queue_table(samples, Path(args.sld_file).name)

    if args.csv_output:
        export_csv(samples, args.csv_output)
        if args.verbose:
            print(f"Exported to: {args.csv_output}")
