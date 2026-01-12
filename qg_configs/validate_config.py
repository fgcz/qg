#!/usr/bin/env python3
"""Configuration validator for queue generation system.

Validates TOML configuration files for schema compliance, cross-file
references, and business rules.

Usage:
    uv run python qg_configs/validate_config.py
"""

from __future__ import annotations

import csv
import sys
import tomllib
from pathlib import Path
from typing import Any


class Colors:
    """ANSI color codes for terminal output."""

    RED = "\033[91m"
    GREEN = "\033[92m"
    YELLOW = "\033[93m"
    BLUE = "\033[94m"
    BOLD = "\033[1m"
    RESET = "\033[0m"


VALID_TECHNOLOGIES = {"proteomics", "metabolomics", "lipidomics"}


def flatten_toml(data: dict, prefix: str = "") -> dict[str, Any]:
    """Flatten nested TOML dict into dotted keys."""
    result = {}
    for key, value in data.items():
        full_key = f"{prefix}.{key}" if prefix else key
        if isinstance(value, dict) and not _is_leaf_table(value):
            result.update(flatten_toml(value, full_key))
        else:
            result[full_key] = value
    return result


def _is_leaf_table(value: dict) -> bool:
    """Check if a dict is a leaf table (has non-dict values or is Evosep position)."""
    if not value:
        return True
    # Evosep position tables have tray/position_start/position_end
    if "tray" in value or "position_start" in value:
        return True
    # Check if any value is not a dict (leaf table)
    return any(not isinstance(v, dict) for v in value.values())


class ConfigValidator:
    """Validates queue generation configuration files."""

    def __init__(self, config_dir: Path) -> None:
        self.config_dir = config_dir
        self.errors: list[str] = []
        self.warnings: list[str] = []

        # Raw nested config data (for sampler sub-table access)
        self.samplers_raw: dict[str, Any] = {}

        # Flattened config data (for easy lookup with dotted keys)
        self.samplers: dict[str, Any] = {}
        self.samples: dict[str, Any] = {}
        self.patterns: dict[str, Any] = {}
        self.qc_layouts: dict[str, Any] = {}
        self.output_formats: dict[str, Any] = {}
        self.combinations: list[tuple[str, str, str]] = []  # (instrument, sampler, output_format)

        # CSV-based data
        self.instruments: list[dict[str, str]] = []  # rows from instruments.csv
        self.instrument_patterns: list[dict[str, str]] = []  # rows from instrument_patterns.csv

    def _error(self, msg: str) -> None:
        self.errors.append(msg)

    def _warn(self, msg: str) -> None:
        self.warnings.append(msg)

    def load_all(self) -> bool:
        """Load all configuration files. Returns True if all loaded successfully."""
        success = True

        # Load TOML files
        toml_files = [
            ("sampler.toml", "samplers"),
            ("samples.toml", "samples"),
            ("queue_patterns.toml", "patterns"),
            ("qc_layouts.toml", "qc_layouts"),
            ("output_formats.toml", "output_formats"),
        ]

        for filename, attr in toml_files:
            filepath = self.config_dir / filename
            if not filepath.exists():
                self._error(f"Missing file: {filename}")
                success = False
                continue

            try:
                with open(filepath, "rb") as f:
                    raw_data = tomllib.load(f)
                    # Keep raw data for samplers (need nested structure)
                    if attr == "samplers":
                        self.samplers_raw = raw_data
                    # Flatten for validation
                    setattr(self, attr, flatten_toml(raw_data))
            except tomllib.TOMLDecodeError as e:
                self._error(f"TOML syntax error in {filename}: {e}")
                success = False

        # Load combinations.csv
        csv_path = self.config_dir / "combinations.csv"
        if not csv_path.exists():
            self._error("Missing file: combinations.csv")
            success = False
        else:
            try:
                with open(csv_path, newline="") as f:
                    reader = csv.DictReader(f)
                    for row in reader:
                        if "instrument" in row and "sampler" in row and "output_format" in row:
                            self.combinations.append(
                                (row["instrument"], row["sampler"], row["output_format"])
                            )
                        else:
                            self._error("combinations.csv missing required columns (instrument, sampler, output_format)")
                            success = False
                            break
            except csv.Error as e:
                self._error(f"CSV error in combinations.csv: {e}")
                success = False

        # Load instruments.csv
        instr_path = self.config_dir / "instruments.csv"
        if not instr_path.exists():
            self._error("Missing file: instruments.csv")
            success = False
        else:
            try:
                with open(instr_path, newline="") as f:
                    reader = csv.DictReader(f)
                    required_cols = {"technology", "instrument", "methods_file"}
                    if reader.fieldnames and required_cols <= set(reader.fieldnames):
                        self.instruments = list(reader)
                    else:
                        self._error("instruments.csv missing required columns (technology, instrument, methods_file)")
                        success = False
            except csv.Error as e:
                self._error(f"CSV error in instruments.csv: {e}")
                success = False

        # Load instrument_patterns.csv
        patterns_path = self.config_dir / "instrument_patterns.csv"
        if not patterns_path.exists():
            self._error("Missing file: instrument_patterns.csv")
            success = False
        else:
            try:
                with open(patterns_path, newline="") as f:
                    reader = csv.DictReader(f)
                    required_cols = {"technology", "instrument", "pattern", "is_default"}
                    if reader.fieldnames and required_cols <= set(reader.fieldnames):
                        self.instrument_patterns = list(reader)
                    else:
                        self._error("instrument_patterns.csv missing required columns (technology, instrument, pattern, is_default)")
                        success = False
            except csv.Error as e:
                self._error(f"CSV error in instrument_patterns.csv: {e}")
                success = False

        return success

    def _count_items(self, data: dict, prefix_depth: int = 1) -> int:
        """Count items at specified nesting depth."""
        count = 0
        for key in data:
            parts = key.split(".")
            if len(parts) >= prefix_depth:
                count += 1
        return count

    def validate_schema(self) -> None:
        """Validate schema and data types."""
        self._validate_samples_schema()
        self._validate_patterns_schema()
        self._validate_instruments_schema()
        self._validate_samplers_schema()
        self._validate_output_formats_schema()
        self._validate_qc_layouts_schema()

    def _validate_samples_schema(self) -> None:
        """Validate samples.toml schema."""
        for key, value in self.samples.items():
            parts = key.split(".")
            if len(parts) < 2:
                continue  # Skip technology-only keys

            tech = parts[0]
            if tech not in VALID_TECHNOLOGIES:
                self._error(f"samples.toml: Unknown technology '{tech}' in [{key}]")

            if not isinstance(value, dict):
                continue

            # Check inj_vol type
            if "inj_vol" in value and not isinstance(value["inj_vol"], (int, float)):
                self._error(f"samples.toml: [{key}].inj_vol must be a number")

    def _validate_patterns_schema(self) -> None:
        """Validate queue_patterns.toml schema."""
        for key, value in self.patterns.items():
            parts = key.split(".")
            if len(parts) < 2:
                continue

            tech = parts[0]
            if tech not in VALID_TECHNOLOGIES:
                self._error(f"queue_patterns.toml: Unknown technology '{tech}' in [{key}]")

            if not isinstance(value, dict):
                continue

            # Required fields
            if "run_QC_after_n_samples" not in value:
                self._error(f"queue_patterns.toml: [{key}] missing run_QC_after_n_samples")
            elif not isinstance(value["run_QC_after_n_samples"], int):
                self._error(f"queue_patterns.toml: [{key}].run_QC_after_n_samples must be integer")

            # Array fields
            for arr_field in ["start", "middle", "end"]:
                if arr_field in value and not isinstance(value[arr_field], list):
                    self._error(f"queue_patterns.toml: [{key}].{arr_field} must be array")

    def _validate_instruments_schema(self) -> None:
        """Validate instruments.csv schema."""
        seen_instruments = set()
        for row in self.instruments:
            tech = row.get("technology", "")
            instr = row.get("instrument", "")
            key = f"{tech}.{instr}"

            if tech not in VALID_TECHNOLOGIES:
                self._error(f"instruments.csv: Unknown technology '{tech}' for {instr}")

            if key in seen_instruments:
                self._error(f"instruments.csv: Duplicate entry {key}")
            seen_instruments.add(key)

            if not row.get("methods_file"):
                self._error(f"instruments.csv: {key} missing methods_file")

        # Validate instrument_patterns.csv
        seen_defaults = {}  # (tech, instr) -> count of defaults
        for row in self.instrument_patterns:
            tech = row.get("technology", "")
            instr = row.get("instrument", "")
            pattern = row.get("pattern", "")
            is_default = row.get("is_default", "").lower() == "true"

            key = f"{tech}.{instr}"
            if key not in seen_instruments:
                self._error(f"instrument_patterns.csv: Unknown instrument {key}")

            if tech not in VALID_TECHNOLOGIES:
                self._error(f"instrument_patterns.csv: Unknown technology '{tech}'")

            # Track defaults
            if is_default:
                seen_defaults[key] = seen_defaults.get(key, 0) + 1

        # Check each instrument has exactly one default
        for key in seen_instruments:
            count = seen_defaults.get(key, 0)
            if count == 0:
                self._warn(f"instrument_patterns.csv: {key} has no default pattern")
            elif count > 1:
                self._error(f"instrument_patterns.csv: {key} has {count} default patterns (should be 1)")

    def _validate_samplers_schema(self) -> None:
        """Validate sampler.toml schema."""
        # Samplers are now hardware-only, output_format is in combinations.csv
        for sampler_name, sampler_config in self.samplers_raw.items():
            if not isinstance(sampler_config, dict):
                continue
            # Check required fields for each sampler type
            if "description" not in sampler_config:
                self._warn(f"sampler.toml: [{sampler_name}] missing description")

    def _validate_output_formats_schema(self) -> None:
        """Validate output_formats.toml schema."""
        for key, value in self.output_formats.items():
            if not isinstance(value, dict):
                continue

            if "columns" not in value:
                self._error(f"output_formats.toml: [{key}] missing columns")

    def _validate_qc_layouts_schema(self) -> None:
        """Validate qc_layouts.toml schema."""
        for key in self.qc_layouts:
            parts = key.split(".")
            if len(parts) < 2:
                continue

            tech = parts[0]
            if tech not in VALID_TECHNOLOGIES:
                self._error(f"qc_layouts.toml: Unknown technology '{tech}' in [{key}]")

    def validate_references(self) -> None:
        """Validate cross-file references."""
        self._validate_combinations_references()
        self._validate_pattern_references()
        self._validate_qc_layout_references()
        self._validate_methods_files()

    def _validate_combinations_references(self) -> None:
        """Validate instrument/sampler/output_format references in combinations.csv."""
        # Get all instrument names (without technology prefix)
        instrument_names = {row["instrument"] for row in self.instruments}

        # Get all sampler names (top-level keys)
        sampler_names = set(self.samplers_raw.keys())

        # Get all output format names
        output_format_names = set(self.output_formats.keys())

        seen = set()
        for instrument, sampler, output_format in self.combinations:
            # Check for duplicates
            pair = (instrument, sampler)
            if pair in seen:
                self._warn(f"combinations.csv: Duplicate entry ({instrument}, {sampler})")
            seen.add(pair)

            # Validate instrument exists
            if instrument not in instrument_names:
                self._error(f"combinations.csv: Unknown instrument '{instrument}'")

            # Validate sampler exists (handle nested .vial/.plate)
            sampler_base = sampler.split(".")[0]
            if sampler_base not in sampler_names:
                self._error(f"combinations.csv: Unknown sampler '{sampler_base}'")

            # If nested, check sub-table exists
            if "." in sampler:
                container = sampler.split(".")[1]
                if sampler_base in self.samplers_raw:
                    sampler_config = self.samplers_raw[sampler_base]
                    if isinstance(sampler_config, dict) and container not in sampler_config:
                        self._error(f"combinations.csv: Missing sub-table [{sampler_base}.{container}]")

            # Validate output_format exists
            if output_format not in output_format_names:
                self._error(f"combinations.csv: Unknown output_format '{output_format}'")

    def _validate_pattern_references(self) -> None:
        """Validate sample references in queue patterns."""
        # Build sample lookup by technology
        samples_by_tech: dict[str, set[str]] = {t: set() for t in VALID_TECHNOLOGIES}
        for key in self.samples:
            parts = key.split(".")
            if len(parts) >= 2:
                tech, sample_id = parts[0], parts[1]
                if tech in samples_by_tech:
                    samples_by_tech[tech].add(sample_id)

        # Check pattern references
        for pattern_key, pattern in self.patterns.items():
            parts = pattern_key.split(".")
            if len(parts) < 2 or not isinstance(pattern, dict):
                continue

            tech = parts[0]
            if tech not in samples_by_tech:
                continue

            available = samples_by_tech[tech]

            for arr_field in ["start", "middle", "end", "middle_extended"]:
                if arr_field not in pattern:
                    continue
                for sample_id in pattern[arr_field]:
                    if sample_id not in available:
                        self._error(
                            f"queue_patterns.toml: [{pattern_key}].{arr_field} "
                            f"references unknown sample '{sample_id}'"
                        )

        # Check instrument pattern references from instrument_patterns.csv
        for row in self.instrument_patterns:
            tech = row.get("technology", "")
            pattern = row.get("pattern", "")
            instr = row.get("instrument", "")
            pattern_ref = f"{tech}.{pattern}"

            if pattern_ref not in self.patterns:
                self._error(
                    f"instrument_patterns.csv: {tech}.{instr} "
                    f"references unknown pattern '{pattern_ref}'"
                )

    def _validate_qc_layout_references(self) -> None:
        """Validate QC layout references to samples."""
        # Build sample lookup by technology
        samples_by_tech: dict[str, set[str]] = {t: set() for t in VALID_TECHNOLOGIES}
        for key in self.samples:
            parts = key.split(".")
            if len(parts) >= 2:
                tech, sample_id = parts[0], parts[1]
                if tech in samples_by_tech:
                    samples_by_tech[tech].add(sample_id)

        # Check QC layout sample references
        for layout_key, layout in self.qc_layouts.items():
            parts = layout_key.split(".")
            if len(parts) < 2:
                continue

            tech = parts[0]
            if tech not in samples_by_tech:
                continue

            available = samples_by_tech[tech]

            # Handle both string positions (grid) and dict positions (Evosep)
            if isinstance(layout, str):
                # Grid sampler: layout_key is tech.sampler.sample_id, layout is position string
                # Extract sample_id from the key
                if len(parts) >= 3:
                    sample_id = parts[-1]
                    if sample_id not in available:
                        self._error(
                            f"qc_layouts.toml: [{layout_key}] "
                            f"references unknown sample '{sample_id}'"
                        )
            elif isinstance(layout, dict):
                # Could be Evosep position dict or a table of sample positions
                if "tray" in layout or "position_start" in layout:
                    # This is an Evosep position - extract sample from key
                    if len(parts) >= 3:
                        sample_id = parts[-1]
                        if sample_id not in available:
                            self._error(
                                f"qc_layouts.toml: [{layout_key}] "
                                f"references unknown sample '{sample_id}'"
                            )
                else:
                    # Table of sample_id -> position mappings
                    for sample_id, position in layout.items():
                        if sample_id not in available:
                            self._error(
                                f"qc_layouts.toml: [{layout_key}] "
                                f"references unknown sample '{sample_id}'"
                            )

    def _validate_methods_files(self) -> None:
        """Validate methods file paths exist and content is valid."""
        for row in self.instruments:
            tech = row.get("technology", "")
            instr = row.get("instrument", "")
            methods_file = row.get("methods_file", "")
            instr_key = f"{tech}.{instr}"

            if not methods_file:
                continue

            methods_path = self.config_dir / methods_file
            if not methods_path.exists():
                self._warn(
                    f"instruments.csv: {instr_key}.methods_file "
                    f"'{methods_file}' does not exist"
                )
                continue

            # Validate methods file content
            self._validate_methods_file_content(methods_path, instr_key, tech)

    def _validate_methods_file_content(
        self, methods_path: Path, instr_key: str, tech: str | None
    ) -> None:
        """Validate content of a methods CSV file."""
        # Build sample lookup for this technology
        valid_samples = {"default"}  # "default" is always valid
        if tech:
            for key in self.samples:
                parts = key.split(".")
                if len(parts) >= 2 and parts[0] == tech:
                    valid_samples.add(parts[1])

        try:
            with open(methods_path, newline="") as f:
                reader = csv.DictReader(f)

                # Check required columns
                if reader.fieldnames is None:
                    self._error(f"{methods_path.name}: Empty or invalid CSV")
                    return

                required_cols = {"sample_type", "method_name", "method_path"}
                missing = required_cols - set(reader.fieldnames)
                if missing:
                    self._error(
                        f"{methods_path.name}: Missing columns {missing}"
                    )
                    return

                # Validate each row
                for row_num, row in enumerate(reader, start=2):
                    sample_type = row.get("sample_type", "").strip()
                    if sample_type and sample_type not in valid_samples:
                        self._error(
                            f"{methods_path.name}:{row_num}: Unknown sample_type "
                            f"'{sample_type}' (valid: {sorted(valid_samples)})"
                        )

        except csv.Error as e:
            self._error(f"{methods_path.name}: CSV error: {e}")

    def validate_business_rules(self) -> None:
        """Validate business logic rules."""
        self._validate_default_samples()
        self._validate_positive_values()
        self._validate_evosep_ranges()

    def _validate_default_samples(self) -> None:
        """Ensure each technology has a default sample."""
        for tech in VALID_TECHNOLOGIES:
            default_key = f"{tech}.default"
            if default_key not in self.samples:
                self._error(f"samples.toml: Missing [{default_key}] entry")

    def _validate_positive_values(self) -> None:
        """Validate positive numeric values."""
        # Check inj_vol
        for key, sample in self.samples.items():
            if isinstance(sample, dict) and "inj_vol" in sample:
                if sample["inj_vol"] <= 0:
                    self._error(f"samples.toml: [{key}].inj_vol must be positive")

        # Check run_QC_after_n_samples
        for key, pattern in self.patterns.items():
            if isinstance(pattern, dict) and "run_QC_after_n_samples" in pattern:
                if pattern["run_QC_after_n_samples"] <= 0:
                    self._error(f"queue_patterns.toml: [{key}].run_QC_after_n_samples must be positive")

    def _validate_evosep_ranges(self) -> None:
        """Validate Evosep position ranges are valid (1-96)."""
        for layout_key, layout in self.qc_layouts.items():
            if not isinstance(layout, dict):
                continue

            # Check if this is an Evosep layout (inline tables with tray/position)
            for sample_id, position in layout.items():
                if isinstance(position, dict) and "position_start" in position:
                    # Evosep format
                    start = position.get("position_start", 0)
                    end = position.get("position_end", 0)

                    if start < 1 or start > 96:
                        self._error(
                            f"qc_layouts.toml: [{layout_key}].{sample_id}.position_start "
                            f"must be 1-96, got {start}"
                        )
                    if end < 1 or end > 96:
                        self._error(
                            f"qc_layouts.toml: [{layout_key}].{sample_id}.position_end "
                            f"must be 1-96, got {end}"
                        )
                    if start > end:
                        self._error(
                            f"qc_layouts.toml: [{layout_key}].{sample_id} "
                            f"position_start ({start}) > position_end ({end})"
                        )

    def run(self) -> bool:
        """Run all validations. Returns True if no errors."""
        print(f"{Colors.BOLD}Queue Configuration Validator{Colors.RESET}")
        print(f"Config directory: {self.config_dir}\n")

        # Load files
        print(f"{Colors.BLUE}Loading configuration files...{Colors.RESET}")
        if not self.load_all():
            self._print_results()
            return False

        # Count actual items (at technology.item level)
        sample_count = sum(1 for k in self.samples if len(k.split(".")) >= 2)
        pattern_count = sum(1 for k in self.patterns if len(k.split(".")) >= 2)
        layout_count = sum(1 for k in self.qc_layouts if len(k.split(".")) >= 2)

        print(f"  Loaded {len(self.samplers_raw)} samplers")
        print(f"  Loaded {sample_count} samples")
        print(f"  Loaded {pattern_count} patterns")
        print(f"  Loaded {layout_count} QC layouts")
        print(f"  Loaded {len(self.instruments)} instruments")
        print(f"  Loaded {len(self.instrument_patterns)} instrument-pattern mappings")
        print(f"  Loaded {len(self.output_formats)} output formats")
        print(f"  Loaded {len(self.combinations)} combinations\n")

        # Run validations
        print(f"{Colors.BLUE}Validating schema...{Colors.RESET}")
        self.validate_schema()

        print(f"{Colors.BLUE}Validating references...{Colors.RESET}")
        self.validate_references()

        print(f"{Colors.BLUE}Validating business rules...{Colors.RESET}")
        self.validate_business_rules()

        self._print_results()
        return len(self.errors) == 0

    def _print_results(self) -> None:
        """Print validation results."""
        print()

        if self.errors:
            print(f"{Colors.RED}{Colors.BOLD}ERRORS ({len(self.errors)}):{Colors.RESET}")
            for error in self.errors:
                print(f"  {Colors.RED}[ERROR]{Colors.RESET} {error}")
            print()

        if self.warnings:
            print(f"{Colors.YELLOW}{Colors.BOLD}WARNINGS ({len(self.warnings)}):{Colors.RESET}")
            for warning in self.warnings:
                print(f"  {Colors.YELLOW}[WARN]{Colors.RESET} {warning}")
            print()

        if not self.errors and not self.warnings:
            print(f"{Colors.GREEN}{Colors.BOLD}All validations passed!{Colors.RESET}")
        elif not self.errors:
            print(f"{Colors.GREEN}No errors found.{Colors.RESET}")
        else:
            print(f"{Colors.RED}Validation failed with {len(self.errors)} error(s).{Colors.RESET}")


def main() -> int:
    """Main entry point."""
    # Determine config directory
    script_dir = Path(__file__).parent
    if script_dir.name == "qg_configs":
        config_dir = script_dir
    else:
        config_dir = script_dir / "qg_configs"

    if not config_dir.exists():
        print(f"Error: Config directory not found: {config_dir}", file=sys.stderr)
        return 1

    validator = ConfigValidator(config_dir)
    success = validator.run()
    return 0 if success else 1


if __name__ == "__main__":
    sys.exit(main())
