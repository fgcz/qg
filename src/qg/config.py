"""Configuration loading functions for queue generation."""

from dataclasses import dataclass
from pathlib import Path

import polars as pl

from qg.models import (
    Sample,
    SamplesConfig,
    Instrument,
    InstrumentsConfig,
    InstrumentPattern,
    InstrumentPatternsConfig,
    Combination,
    CombinationsConfig,
    SamplersConfig,
    QueuePatternsConfig,
    QCLayoutsConfig,
    EvosepPosition,
    QCPosition,
    OutputFormatsConfig,
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
    import tomllib

    with open(path, "rb") as f:
        data = tomllib.load(f)
    return SamplersConfig(**data)


def load_queue_patterns(path: Path | str) -> QueuePatternsConfig:
    """Load and validate queue patterns from TOML file."""
    import tomllib

    with open(path, "rb") as f:
        data = tomllib.load(f)
    return QueuePatternsConfig(**data)


def load_qc_layouts(path: Path | str) -> QCLayoutsConfig:
    """Load and validate QC layouts from TOML file.

    Handles both grid positions (strings) and Evosep positions (dicts).
    TOML structure is nested: [proteomics.Vanquish.vial] -> flattened to "Vanquish.vial" key.
    """
    import tomllib

    with open(path, "rb") as f:
        raw_data = tomllib.load(f)

    def flatten_sampler_keys(
        tech_data: dict, prefix: str = ""
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
    processed: dict[str, dict[str, dict[str, QCPosition]]] = {}
    for tech, tech_data in raw_data.items():
        processed[tech] = flatten_sampler_keys(tech_data)

    return QCLayoutsConfig(**processed)


def load_output_formats(path: Path | str) -> OutputFormatsConfig:
    """Load and validate output formats from TOML file."""
    import tomllib

    with open(path, "rb") as f:
        data = tomllib.load(f)
    return OutputFormatsConfig(**data)


@dataclass
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


def validate_all_configs(config_dir: Path | str) -> bool:
    """Validate all configuration files and print results.

    Args:
        config_dir: Path to configuration directory

    Returns:
        True if all validations pass, False otherwise
    """
    import sys

    config_dir = Path(config_dir)
    errors = []

    print("Validating samples.csv...")
    try:
        samples = load_samples(config_dir / "samples.csv")
        print(f"  OK: {len(samples.samples)} samples")
        for tech in ("proteomics", "metabolomics", "lipidomics"):
            count = len(samples.get_by_technology(tech))
            print(f"    - {tech}: {count} samples")
    except Exception as e:
        print(f"  FAILED: {e}")
        errors.append(("samples.csv", e))
        samples = None

    print("\nValidating instruments.csv...")
    try:
        instruments = load_instruments(config_dir / "instruments.csv")
        print(f"  OK: {len(instruments.instruments)} instruments")
        for tech in ("proteomics", "metabolomics", "lipidomics"):
            count = len(instruments.get_by_technology(tech))
            print(f"    - {tech}: {count} instruments")
    except Exception as e:
        print(f"  FAILED: {e}")
        errors.append(("instruments.csv", e))
        instruments = None

    print("\nValidating instrument_patterns.csv...")
    try:
        patterns = load_instrument_patterns(config_dir / "instrument_patterns.csv")
        print(f"  OK: {len(patterns.patterns)} instrument-pattern mappings")
    except Exception as e:
        print(f"  FAILED: {e}")
        errors.append(("instrument_patterns.csv", e))
        patterns = None

    print("\nValidating combinations.csv...")
    try:
        combos = load_combinations(config_dir / "combinations.csv")
        print(f"  OK: {len(combos.combinations)} valid combinations")
    except Exception as e:
        print(f"  FAILED: {e}")
        errors.append(("combinations.csv", e))
        combos = None

    # Cross-validation: instrument_patterns vs instruments
    if instruments and patterns:
        print("\nCross-validating instrument_patterns against instruments...")
        valid_instruments = {(i.technology, i.instrument) for i in instruments.instruments}
        pattern_instruments = {(p.technology, p.instrument) for p in patterns.patterns}
        unknown = pattern_instruments - valid_instruments
        if unknown:
            print(f"  WARNING: instrument_patterns reference unknown instruments: {unknown}")
        else:
            print("  OK: All instrument_patterns reference valid instruments")

    # Cross-validation: combinations vs instruments
    if instruments and combos:
        print("\nCross-validating combinations against instruments...")
        valid_instr_names = {i.instrument for i in instruments.instruments}
        combo_instruments = {c.instrument for c in combos.combinations}
        unknown = combo_instruments - valid_instr_names
        if unknown:
            print(f"  WARNING: combinations reference unknown instruments: {unknown}")
        else:
            print("  OK: All combinations reference valid instruments")

    # TOML configs
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
    except Exception as e:
        print(f"  FAILED: {e}")
        errors.append(("sampler.toml", e))
        samplers = None

    print("\nValidating queue_patterns.toml...")
    try:
        queue_patterns = load_queue_patterns(config_dir / "queue_patterns.toml")
        for tech in ("proteomics", "metabolomics", "lipidomics"):
            tech_patterns = getattr(queue_patterns, tech)
            print(f"  OK: {tech}: {len(tech_patterns)} patterns")
    except Exception as e:
        print(f"  FAILED: {e}")
        errors.append(("queue_patterns.toml", e))
        queue_patterns = None

    print("\nValidating qc_layouts.toml...")
    try:
        qc_layouts = load_qc_layouts(config_dir / "qc_layouts.toml")
        for tech in ("proteomics", "metabolomics", "lipidomics"):
            tech_layouts = getattr(qc_layouts, tech)
            print(f"  OK: {tech}: {len(tech_layouts)} sampler layouts")
    except Exception as e:
        print(f"  FAILED: {e}")
        errors.append(("qc_layouts.toml", e))
        qc_layouts = None

    print("\nValidating output_formats.toml...")
    try:
        output_formats = load_output_formats(config_dir / "output_formats.toml")
        formats = [f for f in ["xcalibur", "chronos", "hystar"] if output_formats.get_format(f)]
        print(f"  OK: {len(formats)} output formats ({', '.join(formats)})")
    except Exception as e:
        print(f"  FAILED: {e}")
        errors.append(("output_formats.toml", e))
        output_formats = None

    # Cross-validation: queue_patterns vs samples
    if samples and queue_patterns:
        print("\nCross-validating queue_patterns against samples...")
        all_ok = True
        for tech in ("proteomics", "metabolomics", "lipidomics"):
            pattern_refs = queue_patterns.get_all_sample_refs(tech)
            valid_sample_ids = {s.sample_id for s in samples.get_by_technology(tech)}
            unknown = pattern_refs - valid_sample_ids
            if unknown:
                print(f"  WARNING: {tech} patterns reference unknown samples: {unknown}")
                all_ok = False
        if all_ok:
            print("  OK: All queue_patterns reference valid samples")

    # Cross-validation: qc_layouts vs samples
    if samples and qc_layouts:
        print("\nCross-validating qc_layouts against samples...")
        all_ok = True
        for tech in ("proteomics", "metabolomics", "lipidomics"):
            tech_layouts = getattr(qc_layouts, tech)
            valid_sample_ids = {s.sample_id for s in samples.get_by_technology(tech)}
            for sampler_key, positions in tech_layouts.items():
                layout_samples = set(positions.keys())
                unknown = layout_samples - valid_sample_ids
                if unknown:
                    print(f"  WARNING: {tech}.{sampler_key} references unknown samples: {unknown}")
                    all_ok = False
        if all_ok:
            print("  OK: All qc_layouts reference valid samples")

    # Summary
    if errors:
        print(f"\n{len(errors)} validation(s) FAILED:")
        for name, err in errors:
            print(f"  - {name}: {err}")
        return False
    else:
        print("\nAll validations passed!")
        return True


def validate_cli() -> None:
    """CLI entry point for config validation."""
    import sys

    config_dir = Path("qg_configs")
    success = validate_all_configs(config_dir)
    sys.exit(0 if success else 1)


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
