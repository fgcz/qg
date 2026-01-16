"""Parse Thermo Xcalibur .sld sequence list files and extract queue information."""

from __future__ import annotations

import csv
import json
import re
import sys
from pathlib import Path
from typing import Annotated

import cyclopts
from loguru import logger

# Regex patterns for field extraction
# Matches JSON blocks in new format SLD files
JSON_BLOCK_PATTERN = re.compile(
    r'\{"[$]type":"Thermo\.Xcalibur\.Common\.Models\.Experiment\.Capabilities\.'
    r'SampleExtensionInfo.*?"DisplayRowNumber":(\d+),"DisplayLabel":\d+\}\}\}'
)

# Matches run number in filename (e.g., "20250425_003_S930976_..." -> "003")
RUN_NUMBER_PATTERN = re.compile(r"20\d{6}_(\d{3})_")

# Matches run number in order format (e.g., "20250425_C3000_003_..." -> "003")
RUN_NUMBER_ORDER_PATTERN = re.compile(r"20\d{6}_C\d+_(\d{3})")

# Field extraction patterns (method paths, output paths, etc.)
# The (?=...) lookahead stops at control chars, multiple spaces, or end
FIELD_TERMINATOR = r"(?=[\x00-\x1f]|\s{2,}|$)"
METHOD_PATH_PATTERN = re.compile(rf"(C:\\Xcalibur\\methods\\[^\x00-\x1f]+?){FIELD_TERMINATOR}")
OUTPUT_PATH_PATTERN = re.compile(rf"(D:\\Data2San\\[^\x00-\x1f]+?){FIELD_TERMINATOR}")
FILENAME_PATTERN = re.compile(rf"(20\d{{6}}_[^\x00-\x1f]+?){FIELD_TERMINATOR.replace('|$', '|D:\\\\|$')}")

# Vial patterns: Y:A1 (vials), 1:F, 1:F8, 2:A1 (plates), EvoSlot 2:1
VIAL_PATTERN = re.compile(r"(?:EvoSlot\s+)?(\d+:[A-Z]?\d*|[A-Z]:[A-Z]?\d+)")


def _find_all_with_positions(pattern: re.Pattern, text: str) -> list[tuple[int, str]]:
    """Find all pattern matches and return (position, matched_text) tuples."""
    return [(m.start(), m.group(1).rstrip()) for m in pattern.finditer(text)]


def _extract_run_number(filename: str, default: int) -> int:
    """Extract run number from filename using known patterns."""
    match = RUN_NUMBER_PATTERN.match(filename)
    if match:
        return int(match.group(1))

    match = RUN_NUMBER_ORDER_PATTERN.match(filename)
    if match:
        return int(match.group(1))

    return default


def _find_in_range(
    items: list[tuple[int, str]], start: int, end: int
) -> str | None:
    """Find first item with position in the given range."""
    for pos, value in items:
        if start < pos < end:
            return value
    return None


def _find_closest_before(
    items: list[tuple[int, str]], target_pos: int, min_pos: int
) -> str | None:
    """Find the item closest to target_pos that falls between min_pos and target_pos."""
    closest_value = None
    closest_dist = float("inf")

    for pos, value in items:
        if min_pos < pos < target_pos:
            dist = target_pos - pos
            if dist < closest_dist:
                closest_dist = dist
                closest_value = value

    return closest_value


def check_run_numbering(samples: list[dict], source_file: str = "") -> list[str]:
    """Check for run numbering issues. Returns list of warning messages.

    Checks performed:
    - Run numbers should start at 1
    - Run numbers should be sequential (no gaps)
    """
    if not samples:
        return []

    issues = []
    runs = sorted(s.get("run", 0) for s in samples)
    min_run = runs[0]
    max_run = runs[-1]

    # Check if runs start at 1
    if min_run != 1:
        msg = f"Run numbers start at {min_run} instead of 1 (possible file manipulation)"
        issues.append(msg)
        logger.warning(f"{source_file}: {msg}" if source_file else msg)

    # Check for gaps in sequence
    expected = set(range(min_run, max_run + 1))
    actual = set(runs)
    missing = expected - actual
    if missing:
        msg = f"Missing run numbers: {sorted(missing)}"
        issues.append(msg)
        logger.warning(f"{source_file}: {msg}" if source_file else msg)

    # Check for duplicates
    if len(runs) != len(actual):
        duplicates = [r for r in actual if runs.count(r) > 1]
        msg = f"Duplicate run numbers: {duplicates}"
        issues.append(msg)
        logger.warning(f"{source_file}: {msg}" if source_file else msg)

    return issues


def _update_filename_run_number(filename: str, new_run: int) -> str:
    """Update the run number embedded in a filename.

    Handles patterns like:
    - 20250425_003_S930976_... → 20250425_001_S930976_...
    - 20250425_C3000_003_... → 20250425_C3000_001_...
    """
    new_run_str = f"{new_run:03d}"

    # Try standard pattern first: 20YYMMDD_NNN_...
    match = RUN_NUMBER_PATTERN.match(filename)
    if match:
        # Replace just the run number portion (group 1)
        return filename[:match.start(1)] + new_run_str + filename[match.end(1):]

    # Try order pattern: 20YYMMDD_CXXXX_NNN_...
    match = RUN_NUMBER_ORDER_PATTERN.match(filename)
    if match:
        return filename[:match.start(1)] + new_run_str + filename[match.end(1):]

    return filename


def sanitize_samples(samples: list[dict]) -> list[dict]:
    """Renumber runs sequentially starting at 1, updating filenames too.

    Returns a new list with sanitized samples (does not modify input).
    """
    if not samples:
        return []

    # Sort by current run number to maintain order
    sorted_samples = sorted(samples, key=lambda s: (s.get("run", 0), s.get("row", 0)))

    sanitized = []
    for new_run, sample in enumerate(sorted_samples, start=1):
        new_sample = sample.copy()
        old_run = sample.get("run", 0)
        new_sample["run"] = new_run

        # Update filename if present and run number changed
        if "filename" in sample and old_run != new_run:
            new_sample["filename"] = _update_filename_run_number(sample["filename"], new_run)
            logger.info(f"Renumbered run {old_run} → {new_run}: {sample['filename']} → {new_sample['filename']}")

        sanitized.append(new_sample)

    return sanitized


def parse_sld_file_new_format(text: str) -> list[dict]:
    """Parse new format SLD files (with JSON blocks)."""
    json_matches = list(JSON_BLOCK_PATTERN.finditer(text))
    if not json_matches:
        return []

    samples = []
    for match in json_matches:
        try:
            data = json.loads(match.group(0))
            reinject_data = data.get("CapabilityData", {}).get("ReInject", {})
            raw_filename = reinject_data.get("OriginalRawFileNameWithoutExtension", "")
            row_number = reinject_data.get("DisplayRowNumber", 0)

            samples.append({
                "row": row_number,
                "run": _extract_run_number(raw_filename, row_number),
                "filename": raw_filename,
            })
        except json.JSONDecodeError:
            continue

    # Extract more details by finding text before each JSON block
    for i, match in enumerate(json_matches):
        start = json_matches[i - 1].end() if i > 0 else 0
        chunk = text[start:match.start()]

        # Extract method and output BEFORE removing nulls (they are separators)
        method_match = re.search(r"(C:\\Xcalibur\\methods\\[^\x00-\x1f@]+)", chunk)
        if method_match:
            samples[i]["method"] = method_match.group(1)

        path_match = re.search(r"(D:\\Data2San\\[^\x00-\x1f@]+)", chunk)
        if path_match:
            samples[i]["output_path"] = path_match.group(1)

        # Remove nulls for vial extraction (vials may have nulls around them)
        chunk_clean = chunk.replace("\x00", "")
        vial_match = VIAL_PATTERN.search(chunk_clean)
        if vial_match:
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
    # Find all field positions
    methods = _find_all_with_positions(METHOD_PATH_PATTERN, text)
    outputs = _find_all_with_positions(OUTPUT_PATH_PATTERN, text)
    filenames = _find_all_with_positions(FILENAME_PATTERN, text)
    vials = [(m.start(), m.group(0)) for m in VIAL_PATTERN.finditer(text)]

    samples = []
    for i, (method_pos, method) in enumerate(methods):
        sample: dict = {
            "row": i + 1,
            "run": i + 1,
            "method": method,
        }

        # Define search boundaries
        prev_method_pos = methods[i - 1][0] if i > 0 else 0
        next_method_pos = methods[i + 1][0] if i + 1 < len(methods) else len(text)

        # Find filename between this method and the next
        filename = _find_in_range(filenames, method_pos, next_method_pos)
        if filename:
            sample["filename"] = filename
            sample["run"] = _extract_run_number(filename, sample["run"])

        # Find output path between this method and the next
        output = _find_in_range(outputs, method_pos, next_method_pos)
        if output:
            sample["output_path"] = output

        # Find vial - first try before method (trap/vial systems)
        vial = _find_closest_before(vials, method_pos, prev_method_pos)

        # If not found, try after output path (Evosep systems)
        if not vial and output:
            output_pos = next(
                (pos for pos, _ in outputs if method_pos < pos < next_method_pos),
                None,
            )
            if output_pos:
                vial = _find_in_range(vials, output_pos, next_method_pos)

        if vial:
            sample["vial"] = vial

        samples.append(sample)

    return samples


def parse_sld_file(filepath: str | Path) -> list[dict]:
    """Parse a Thermo .sld file and return a list of sample entries."""
    with open(filepath, "rb") as f:
        raw_data = f.read()

    # Decode from UTF-16LE, ignoring errors for binary portions
    try:
        text = raw_data.decode("utf-16-le", errors="replace")
    except Exception:
        text = raw_data.decode("latin-1", errors="replace")

    # Try new format first (with JSON blocks), fall back to old format
    samples = parse_sld_file_new_format(text)
    if not samples:
        samples = parse_sld_file_old_format(text)

    samples.sort(key=lambda x: x.get("row", 0))
    return samples


def print_queue_table(samples: list[dict], filename: str) -> None:
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


def export_csv(samples: list[dict], output_path: str | Path) -> None:
    """Export samples to CSV."""
    fieldnames = ["row", "run", "vial", "filename", "method", "output_path"]
    with open(output_path, "w", newline="") as f:
        writer = csv.DictWriter(f, fieldnames=fieldnames)
        writer.writeheader()
        writer.writerows(samples)


app = cyclopts.App(
    name="sld-to-csv",
    help="Parse Thermo Xcalibur .sld files and extract queue information.",
)


@app.default
def main(
    sld_file: Annotated[
        Path,
        cyclopts.Parameter(help="Input .sld file"),
    ],
    *,
    csv_output: Annotated[
        Path | None,
        cyclopts.Parameter(name=["--csv"], help="Output CSV file"),
    ] = None,
    verbose: Annotated[
        bool,
        cyclopts.Parameter(name=["-v", "--verbose"], help="Print table to stdout"),
    ] = False,
    check: Annotated[
        bool,
        cyclopts.Parameter(help="Check for run numbering issues (logs warnings)"),
    ] = False,
    sanitize: Annotated[
        bool,
        cyclopts.Parameter(help="Renumber runs sequentially starting at 1 (also updates filenames)"),
    ] = False,
    log_file: Annotated[
        Path,
        cyclopts.Parameter(help="Log file path"),
    ] = Path("sld_parser.log"),
) -> None:
    """Parse a Thermo .sld file and optionally export to CSV."""
    # Configure loguru to log to file
    logger.remove()  # Remove default stderr handler
    logger.add(log_file, format="{time:YYYY-MM-DD HH:mm:ss} | {level} | {message}")
    if verbose:
        logger.add(sys.stderr, format="{level} | {message}")

    samples = parse_sld_file(sld_file)
    source_name = Path(sld_file).name

    # Run sanity check if requested
    if check:
        issues = check_run_numbering(samples, source_name)
        if issues and verbose:
            print(f"\nWarnings for {source_name}:")
            for issue in issues:
                print(f"  - {issue}")

    # Sanitize if requested
    if sanitize:
        samples = sanitize_samples(samples)
        if verbose:
            print(f"\nSanitized: runs renumbered 1-{len(samples)}")

    if verbose:
        print_queue_table(samples, source_name)

    if csv_output:
        export_csv(samples, csv_output)
        if verbose:
            print(f"Exported to: {csv_output}")


if __name__ == "__main__":
    app()
