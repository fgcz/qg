"""Configuration loading functions for queue generation."""

import tomllib
from dataclasses import dataclass
from pathlib import Path
from typing import Any

import polars as pl

from qg.models import (
    Combination,
    CombinationsConfig,
    EvosepPosition,
    Instrument,
    InstrumentPattern,
    InstrumentPatternsConfig,
    InstrumentsConfig,
    OutputFormat,
    OutputFormatsConfig,
    QCLayoutsConfig,
    QCPosition,
    QueuePattern,
    QueuePatternsConfig,
    Sample,
    SamplesConfig,
    SamplersConfig,
)


def load_samples(path: Path | str) -> SamplesConfig:
    """Load and validate samples from CSV file.

    Args:
        path: Path to samples.csv

    Returns:
        Validated SamplesConfig

    Raises:
        ValidationError: If validation fails
    """
    df = pl.read_csv(path)
    samples = [Sample(**row) for row in df.iter_rows(named=True)]
    return SamplesConfig(samples=samples)


def load_instruments(path: Path | str) -> InstrumentsConfig:
    """Load and validate instruments from CSV file."""
    df = pl.read_csv(path)
    instruments = [Instrument(**row) for row in df.iter_rows(named=True)]
    return InstrumentsConfig(instruments=instruments)


def load_instrument_patterns(path: Path | str) -> InstrumentPatternsConfig:
    """Load and validate instrument patterns from CSV file."""
    df = pl.read_csv(path)
    patterns = [InstrumentPattern(**row) for row in df.iter_rows(named=True)]
    return InstrumentPatternsConfig(patterns=patterns)


def load_combinations(path: Path | str) -> CombinationsConfig:
    """Load and validate combinations from CSV file."""
    df = pl.read_csv(path)
    combinations = [Combination(**row) for row in df.iter_rows(named=True)]
    return CombinationsConfig(combinations=combinations)


def load_samplers(path: Path | str) -> SamplersConfig:
    """Load and validate samplers from TOML file."""
    with open(path, "rb") as f:
        data = tomllib.load(f)
    return SamplersConfig(**data)


def load_queue_patterns(path: Path | str) -> QueuePatternsConfig:
    """Load and validate queue patterns from TOML file.

    TOML structure: [technology.pattern_name] -> converts to patterns dict
    """
    with open(path, "rb") as f:
        raw_data = tomllib.load(f)

    # Convert raw TOML structure to patterns dict
    patterns: dict[str, dict[str, QueuePattern]] = {}
    for tech, tech_patterns in raw_data.items():
        patterns[tech] = {
            name: QueuePattern(**pattern_data) for name, pattern_data in tech_patterns.items()
        }

    return QueuePatternsConfig(patterns=patterns)


def load_qc_layouts(path: Path | str) -> QCLayoutsConfig:
    """Load and validate QC layouts from TOML file.

    Handles both grid positions (strings) and Evosep positions (dicts).
    TOML structure is nested: [proteomics.Vanquish.vial] -> flattened to "Vanquish.vial" key.
    """
    with open(path, "rb") as f:
        raw_data = tomllib.load(f)

    def flatten_sampler_keys(
        tech_data: dict[str, Any], prefix: str = ""
    ) -> dict[str, dict[str, QCPosition]]:
        """Flatten nested sampler dicts to dot-notation keys.

        [proteomics.Vanquish.vial] becomes {"Vanquish.vial": {...}}
        [proteomics.Evosep] becomes {"Evosep": {...}}
        """
        result: dict[str, dict[str, QCPosition]] = {}
        for key, value in tech_data.items():
            full_key = f"{prefix}.{key}" if prefix else key

            # Check if this is a QC positions dict or a nested sampler dict
            # QC positions have sample_id -> position (str or {tray, pos_start, pos_end})
            # Nested sampler has container_type keys like "vial", "plate"
            if isinstance(value, dict):
                # Check if all values are positions (str or Evosep dict)
                is_positions = all(
                    isinstance(v, str) or (isinstance(v, dict) and "tray" in v)
                    for v in value.values()
                )
                if is_positions:
                    # This is a QC positions dict - convert Evosep dicts
                    positions: dict[str, QCPosition] = {}
                    for sample_id, pos in value.items():
                        if isinstance(pos, dict):
                            positions[sample_id] = EvosepPosition(**pos)
                        else:
                            positions[sample_id] = pos
                    result[full_key] = positions
                else:
                    # This is a nested sampler dict - recurse
                    result.update(flatten_sampler_keys(value, full_key))
        return result

    # Process each technology
    layouts: dict[str, dict[str, dict[str, QCPosition]]] = {}
    for tech, tech_data in raw_data.items():
        layouts[tech] = flatten_sampler_keys(tech_data)

    return QCLayoutsConfig(layouts=layouts)


def load_output_formats(path: Path | str) -> OutputFormatsConfig:
    """Load and validate output formats from TOML file."""
    with open(path, "rb") as f:
        raw_data = tomllib.load(f)

    # Convert to formats dict
    formats = {name: OutputFormat(**format_data) for name, format_data in raw_data.items()}

    return OutputFormatsConfig(formats=formats)


@dataclass(slots=True)
class ConfigBundle:
    """Consolidated container for all configuration files."""

    samples: SamplesConfig
    instruments: InstrumentsConfig
    instrument_patterns: InstrumentPatternsConfig
    combinations: CombinationsConfig
    samplers: SamplersConfig
    queue_patterns: QueuePatternsConfig
    qc_layouts: QCLayoutsConfig
    output_formats: OutputFormatsConfig
    instruments_df: pl.DataFrame  # Raw DataFrame for path templates


# =============================================================================
# Validation Helpers
# =============================================================================


def _validate_samples(config_dir: Path) -> tuple[SamplesConfig | None, list[tuple[str, Exception]]]:
    """Validate samples.csv and print results."""
    errors: list[tuple[str, Exception]] = []
    print("Validating samples.csv...")
    try:
        samples = load_samples(config_dir / "samples.csv")
        print(f"  OK: {len(samples.samples)} samples")
        technologies = sorted({s.technology for s in samples.samples})
        for tech in technologies:
            count = len(samples.get_by_technology(tech))
            print(f"    - {tech}: {count} samples")
        return samples, errors
    except Exception as e:
        print(f"  FAILED: {e}")
        errors.append(("samples.csv", e))
        return None, errors


def _validate_instruments(config_dir: Path) -> tuple[InstrumentsConfig | None, list[tuple[str, Exception]]]:
    """Validate instruments.csv and print results."""
    errors: list[tuple[str, Exception]] = []
    print("\nValidating instruments.csv...")
    try:
        instruments = load_instruments(config_dir / "instruments.csv")
        print(f"  OK: {len(instruments.instruments)} instruments")
        technologies = sorted({i.technology for i in instruments.instruments})
        for tech in technologies:
            count = len(instruments.get_by_technology(tech))
            print(f"    - {tech}: {count} instruments")
        return instruments, errors
    except Exception as e:
        print(f"  FAILED: {e}")
        errors.append(("instruments.csv", e))
        return None, errors


def _validate_instrument_patterns(
    config_dir: Path,
) -> tuple[InstrumentPatternsConfig | None, list[tuple[str, Exception]]]:
    """Validate instrument_patterns.csv and print results."""
    errors: list[tuple[str, Exception]] = []
    print("\nValidating instrument_patterns.csv...")
    try:
        patterns = load_instrument_patterns(config_dir / "instrument_patterns.csv")
        print(f"  OK: {len(patterns.patterns)} instrument-pattern mappings")
        return patterns, errors
    except Exception as e:
        print(f"  FAILED: {e}")
        errors.append(("instrument_patterns.csv", e))
        return None, errors


def _validate_combinations(
    config_dir: Path,
) -> tuple[CombinationsConfig | None, list[tuple[str, Exception]]]:
    """Validate combinations.csv and print results."""
    errors: list[tuple[str, Exception]] = []
    print("\nValidating combinations.csv...")
    try:
        combos = load_combinations(config_dir / "combinations.csv")
        print(f"  OK: {len(combos.combinations)} valid combinations")
        return combos, errors
    except Exception as e:
        print(f"  FAILED: {e}")
        errors.append(("combinations.csv", e))
        return None, errors


def _validate_samplers(config_dir: Path) -> tuple[SamplersConfig | None, list[tuple[str, Exception]]]:
    """Validate sampler.toml and print results."""
    errors: list[tuple[str, Exception]] = []
    print("\nValidating sampler.toml...")
    try:
        samplers = load_samplers(config_dir / "sampler.toml")
        print(f"  OK: {len(samplers.get_sampler_names())} samplers")
        for name in samplers.get_sampler_names():
            sampler = getattr(samplers, name)
            if hasattr(sampler, "plates"):
                print(f"    - {name}: grid sampler ({len(sampler.plates)} plates)")
            else:
                print(f"    - {name}: tray sampler ({len(sampler.slots)} slots)")
        return samplers, errors
    except Exception as e:
        print(f"  FAILED: {e}")
        errors.append(("sampler.toml", e))
        return None, errors


def _validate_queue_patterns(
    config_dir: Path,
) -> tuple[QueuePatternsConfig | None, list[tuple[str, Exception]]]:
    """Validate queue_patterns.toml and print results."""
    errors: list[tuple[str, Exception]] = []
    print("\nValidating queue_patterns.toml...")
    try:
        queue_patterns = load_queue_patterns(config_dir / "queue_patterns.toml")
        for tech in queue_patterns.get_technologies():
            tech_patterns = queue_patterns.get_patterns_for_technology(tech)
            print(f"  OK: {tech}: {len(tech_patterns)} patterns")
        return queue_patterns, errors
    except Exception as e:
        print(f"  FAILED: {e}")
        errors.append(("queue_patterns.toml", e))
        return None, errors


def _validate_qc_layouts(config_dir: Path) -> tuple[QCLayoutsConfig | None, list[tuple[str, Exception]]]:
    """Validate qc_layouts.toml and print results."""
    errors: list[tuple[str, Exception]] = []
    print("\nValidating qc_layouts.toml...")
    try:
        qc_layouts = load_qc_layouts(config_dir / "qc_layouts.toml")
        for tech in qc_layouts.get_technologies():
            sampler_count = len(qc_layouts.get_samplers_for_technology(tech))
            print(f"  OK: {tech}: {sampler_count} sampler layouts")
        return qc_layouts, errors
    except Exception as e:
        print(f"  FAILED: {e}")
        errors.append(("qc_layouts.toml", e))
        return None, errors


def _validate_output_formats(
    config_dir: Path,
) -> tuple[OutputFormatsConfig | None, list[tuple[str, Exception]]]:
    """Validate output_formats.toml and print results."""
    errors: list[tuple[str, Exception]] = []
    print("\nValidating output_formats.toml...")
    try:
        output_formats = load_output_formats(config_dir / "output_formats.toml")
        format_names = output_formats.get_format_names()
        print(f"  OK: {len(format_names)} output formats ({', '.join(format_names)})")
        return output_formats, errors
    except Exception as e:
        print(f"  FAILED: {e}")
        errors.append(("output_formats.toml", e))
        return None, errors


def _cross_validate_instrument_refs(
    instruments: InstrumentsConfig | None,
    patterns: InstrumentPatternsConfig | None,
    combos: CombinationsConfig | None,
) -> None:
    """Cross-validate instrument references in patterns and combinations."""
    if instruments and patterns:
        print("\nCross-validating instrument_patterns against instruments...")
        valid_instruments = {(i.technology, i.instrument) for i in instruments.instruments}
        pattern_instruments = {(p.technology, p.instrument) for p in patterns.patterns}
        unknown = pattern_instruments - valid_instruments
        if unknown:
            print(f"  WARNING: instrument_patterns reference unknown instruments: {unknown}")
        else:
            print("  OK: All instrument_patterns reference valid instruments")

    if instruments and combos:
        print("\nCross-validating combinations against instruments...")
        valid_instr_names = {i.instrument for i in instruments.instruments}
        combo_instruments = {c.instrument for c in combos.combinations}
        unknown = combo_instruments - valid_instr_names
        if unknown:
            print(f"  WARNING: combinations reference unknown instruments: {unknown}")
        else:
            print("  OK: All combinations reference valid instruments")


def _cross_validate_sample_refs(
    samples: SamplesConfig | None,
    queue_patterns: QueuePatternsConfig | None,
    qc_layouts: QCLayoutsConfig | None,
) -> None:
    """Cross-validate sample references in patterns and layouts."""
    if samples and queue_patterns:
        print("\nCross-validating queue_patterns against samples...")
        all_ok = True
        for tech in queue_patterns.get_technologies():
            pattern_refs = queue_patterns.get_all_sample_refs(tech)
            valid_sample_ids = {s.sample_id for s in samples.get_by_technology(tech)}
            unknown = pattern_refs - valid_sample_ids
            if unknown:
                print(f"  WARNING: {tech} patterns reference unknown samples: {unknown}")
                all_ok = False
        if all_ok:
            print("  OK: All queue_patterns reference valid samples")

    if samples and qc_layouts:
        print("\nCross-validating qc_layouts against samples...")
        all_ok = True
        for tech in qc_layouts.get_technologies():
            valid_sample_ids = {s.sample_id for s in samples.get_by_technology(tech)}
            for sampler_key in qc_layouts.get_samplers_for_technology(tech):
                layout = qc_layouts.get_layout(tech, sampler_key)
                if layout:
                    layout_samples = set(layout.keys())
                    unknown = layout_samples - valid_sample_ids
                    if unknown:
                        print(f"  WARNING: {tech}.{sampler_key} references unknown samples: {unknown}")
                        all_ok = False
        if all_ok:
            print("  OK: All qc_layouts reference valid samples")


# =============================================================================
# Main Validation Entry Point
# =============================================================================


def validate_all_configs(config_dir: Path | str) -> bool:
    """Validate all configuration files and print results.

    Args:
        config_dir: Path to configuration directory

    Returns:
        True if all validations pass, False otherwise
    """
    config_dir = Path(config_dir)
    errors: list[tuple[str, Exception]] = []

    # Validate CSV configs
    samples, errs = _validate_samples(config_dir)
    errors.extend(errs)

    instruments, errs = _validate_instruments(config_dir)
    errors.extend(errs)

    patterns, errs = _validate_instrument_patterns(config_dir)
    errors.extend(errs)

    combos, errs = _validate_combinations(config_dir)
    errors.extend(errs)

    # Cross-validate CSV configs
    _cross_validate_instrument_refs(instruments, patterns, combos)

    # Validate TOML configs
    _, errs = _validate_samplers(config_dir)
    errors.extend(errs)

    queue_patterns, errs = _validate_queue_patterns(config_dir)
    errors.extend(errs)

    qc_layouts, errs = _validate_qc_layouts(config_dir)
    errors.extend(errs)

    _, errs = _validate_output_formats(config_dir)
    errors.extend(errs)

    # Cross-validate TOML configs against samples
    _cross_validate_sample_refs(samples, queue_patterns, qc_layouts)

    # Summary
    if errors:
        print(f"\n{len(errors)} validation(s) FAILED:")
        for name, err in errors:
            print(f"  - {name}: {err}")
        return False

    print("\nAll validations passed!")
    return True


def load_all_configs(config_dir: Path | str) -> ConfigBundle:
    """Load all configuration files from a directory.

    Args:
        config_dir: Path to configuration directory (e.g., qg_configs/)

    Returns:
        ConfigBundle with all validated configurations
    """
    config_dir = Path(config_dir)

    return ConfigBundle(
        samples=load_samples(config_dir / "samples.csv"),
        instruments=load_instruments(config_dir / "instruments.csv"),
        instrument_patterns=load_instrument_patterns(config_dir / "instrument_patterns.csv"),
        combinations=load_combinations(config_dir / "combinations.csv"),
        samplers=load_samplers(config_dir / "sampler.toml"),
        queue_patterns=load_queue_patterns(config_dir / "queue_patterns.toml"),
        qc_layouts=load_qc_layouts(config_dir / "qc_layouts.toml"),
        output_formats=load_output_formats(config_dir / "output_formats.toml"),
        instruments_df=pl.read_csv(config_dir / "instruments.csv"),
    )
