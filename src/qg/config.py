"""Configuration loading functions for queue generation."""

import tomllib
from dataclasses import dataclass
from functools import lru_cache
from pathlib import Path

import polars as pl
from loguru import logger

from qg.config_models import (
    Combination,
    CombinationsConfig,
    EvosepPosition,
    Instrument,
    InstrumentPattern,
    InstrumentPatternsConfig,
    InstrumentsConfig,
    Method,
    MethodsConfig,
    OutputFormat,
    OutputFormatsConfig,
    QCLayoutsConfig,
    QCPosition,
    QueuePattern,
    QueuePatternsConfig,
    Sample,
    SamplersConfig,
    SamplesConfig,
)
from qg.positions import QCLayoutPattern

# =============================================================================
# Module-level path cache (set by qg_config)
# =============================================================================

_core_dir: Path | None = None




def _load_samples(path: Path | str) -> SamplesConfig:
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


def _load_instruments(path: Path | str) -> InstrumentsConfig:
    """Load and validate instruments from CSV file."""
    df = pl.read_csv(path)
    instruments = [Instrument(**row) for row in df.iter_rows(named=True)]
    return InstrumentsConfig(instruments=instruments)


def _load_instrument_patterns(path: Path | str) -> InstrumentPatternsConfig:
    """Load and validate instrument patterns from CSV file."""
    df = pl.read_csv(path)
    patterns = [InstrumentPattern(**row) for row in df.iter_rows(named=True)]
    return InstrumentPatternsConfig(patterns=patterns)


def _load_combinations(path: Path | str) -> CombinationsConfig:
    """Load and validate combinations from CSV file."""
    df = pl.read_csv(path)
    combinations = [Combination(**row) for row in df.iter_rows(named=True)]
    return CombinationsConfig(combinations=combinations)


def _load_samplers(path: Path | str) -> SamplersConfig:
    """Load and validate samplers from TOML file."""
    with open(path, "rb") as f:
        data = tomllib.load(f)
    return SamplersConfig(**data)


def _load_queue_patterns(path: Path | str) -> QueuePatternsConfig:
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


def _load_qc_layouts(config_dir: Path | str) -> QCLayoutsConfig:
    """Load QC layouts from CSV files.

    Reads from two CSV files:
      - qc_layouts_grid.csv: Grid sampler positions (plate, row, col)
      - qc_layouts_evosep.csv: Evosep position ranges (tray, position_start, position_end)

    Args:
        config_dir: Path to config directory containing the CSV files

    Returns:
        QCLayoutsConfig with nested dict structure: {tech: {sampler: {sample_id: position}}}
    """
    config_dir = Path(config_dir)
    layouts: dict[str, dict[str, dict[str, QCPosition]]] = {}

    # Load grid positions (Vanquish, MClass48)
    grid_path = config_dir / "qc_layouts_grid.csv"
    if grid_path.exists():
        df = pl.read_csv(grid_path, comment_prefix="#")
        for row in df.iter_rows(named=True):
            tech = row["technology"]
            sampler = row["sampler"]
            sample_id = row["sample_id"]
            position: QCPosition = {
                "plate": row["plate"],
                "row": row["row"],
                "col": row["col"],
            }

            if tech not in layouts:
                layouts[tech] = {}
            if sampler not in layouts[tech]:
                layouts[tech][sampler] = {}
            layouts[tech][sampler][sample_id] = position

    # Load Evosep positions (consumable tip ranges)
    evosep_path = config_dir / "qc_layouts_evosep.csv"
    if evosep_path.exists():
        df = pl.read_csv(evosep_path, comment_prefix="#")
        for row in df.iter_rows(named=True):
            tech = row["technology"]
            sampler = row["sampler"]
            sample_id = row["sample_id"]
            position = EvosepPosition(
                tray=row["tray"],
                position_start=row["position_start"],
                position_end=row["position_end"],
            )

            if tech not in layouts:
                layouts[tech] = {}
            if sampler not in layouts[tech]:
                layouts[tech][sampler] = {}
            layouts[tech][sampler][sample_id] = position

    return QCLayoutsConfig(layouts=layouts)


def _load_output_formats(path: Path | str) -> OutputFormatsConfig:
    """Load and validate output formats from TOML file."""
    with open(path, "rb") as f:
        raw_data = tomllib.load(f)

    # Convert to formats dict
    formats = {name: OutputFormat(**format_data) for name, format_data in raw_data.items()}

    return OutputFormatsConfig(formats=formats)


def _load_methods(methods_dir: Path, instruments: InstrumentsConfig) -> MethodsConfig:
    """Load all methods CSVs based on instruments config.

    Discovers methods files from instruments.methods_file paths.
    """
    methods: dict[str, dict[str, list[Method]]] = {}

    for instr in instruments.instruments:
        tech = instr.technology
        methods_file = methods_dir / instr.methods_file.removeprefix("methods/")

        if not methods_file.exists():
            logger.warning(f"Methods file not found: {methods_file}")
            continue

        df = pl.read_csv(methods_file)
        instr_methods = [Method(**row) for row in df.iter_rows(named=True)]

        if tech not in methods:
            methods[tech] = {}
        methods[tech][instr.instrument] = instr_methods

    return MethodsConfig(methods=methods)


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
    methods: MethodsConfig


# =============================================================================
# Cross-Validation Helpers
# =============================================================================


def _cross_validate_instrument_refs(
    instruments: InstrumentsConfig,
    instrument_patterns: InstrumentPatternsConfig,
    combinations: CombinationsConfig,
) -> list[str]:
    """Cross-validate instrument references in patterns and combinations."""
    warnings: list[str] = []

    logger.info("Cross-validating instrument_patterns against instruments...")
    valid_instruments = {(i.technology, i.instrument) for i in instruments.instruments}
    pattern_instruments = {(p.technology, p.instrument) for p in instrument_patterns.patterns}
    unknown = pattern_instruments - valid_instruments
    if unknown:
        msg = f"instrument_patterns reference unknown instruments: {unknown}"
        warnings.append(msg)
        logger.warning(msg)
    else:
        logger.info("  OK: All instrument_patterns reference valid instruments")

    logger.info("Cross-validating combinations against instruments...")
    valid_instr_names = {i.instrument for i in instruments.instruments}
    combo_instruments = {c.instrument for c in combinations.combinations}
    unknown = combo_instruments - valid_instr_names
    if unknown:
        msg = f"combinations reference unknown instruments: {unknown}"
        warnings.append(msg)
        logger.warning(msg)
    else:
        logger.info("  OK: All combinations reference valid instruments")

    return warnings


def _cross_validate_sampler_refs(
    samplers: SamplersConfig,
    combinations: CombinationsConfig,
) -> list[str]:
    """Cross-validate sampler references in combinations."""
    warnings: list[str] = []

    logger.info("Cross-validating combinations against samplers...")
    valid_samplers = samplers.get_valid_sampler_keys()
    combo_samplers = {c.sampler for c in combinations.combinations}
    unknown = combo_samplers - valid_samplers
    if unknown:
        msg = f"combinations reference unknown samplers: {unknown}"
        warnings.append(msg)
        logger.warning(msg)
    else:
        logger.info("  OK: All combinations reference valid samplers")

    return warnings


def _cross_validate_sample_refs(
    samples: SamplesConfig,
    queue_patterns: QueuePatternsConfig,
    qc_layouts: QCLayoutsConfig,
) -> list[str]:
    """Cross-validate sample references in patterns and layouts."""
    warnings: list[str] = []

    logger.info("Cross-validating queue_patterns against samples...")
    all_ok = True
    for tech in queue_patterns.get_technologies():
        pattern_refs = queue_patterns.get_all_sample_refs(tech)
        valid_sample_ids = {s.sample_id for s in samples.get_by_technology(tech)}
        unknown = pattern_refs - valid_sample_ids
        if unknown:
            msg = f"{tech} patterns reference unknown samples: {unknown}"
            warnings.append(msg)
            logger.warning(msg)
            all_ok = False
    if all_ok:
        logger.info("  OK: All queue_patterns reference valid samples")

    logger.info("Cross-validating qc_layouts against samples...")
    all_ok = True
    for tech in qc_layouts.get_technologies():
        valid_sample_ids = {s.sample_id for s in samples.get_by_technology(tech)}
        for sampler_key in qc_layouts.get_samplers_for_technology(tech):
            layout = qc_layouts.get_layout(tech, sampler_key)
            if layout:
                layout_samples = set(layout.keys())
                unknown = layout_samples - valid_sample_ids
                if unknown:
                    msg = f"{tech}.{sampler_key} references unknown samples: {unknown}"
                    warnings.append(msg)
                    logger.warning(msg)
                    all_ok = False
    if all_ok:
        logger.info("  OK: All qc_layouts reference valid samples")

    return warnings


def _cross_validate_qc_layout_patterns(
    queue_patterns: QueuePatternsConfig,
    qc_layouts: QCLayoutsConfig,
) -> list[str]:
    """Cross-validate that patterns and QC layouts are compatible.

    Validates all (technology, pattern, sampler) combinations using
    QCLayoutPattern.create() to check:
    - Coverage: All QC IDs in pattern have positions in qc_layout
    - Uniqueness: No two QC samples map to the same position
    """
    warnings: list[str] = []
    logger.info("Cross-validating queue_patterns against qc_layouts...")
    all_ok = True

    for tech in queue_patterns.get_technologies():
        tech_patterns = queue_patterns.get_patterns_for_technology(tech)
        samplers = qc_layouts.get_samplers_for_technology(tech)

        for pattern_name, pattern in tech_patterns.items():
            for sampler_key in samplers:
                qc_layout = qc_layouts.get_layout(tech, sampler_key)
                if not qc_layout:
                    continue
                try:
                    QCLayoutPattern.create(pattern, qc_layout)
                except ValueError as e:
                    msg = f"{tech}.{pattern_name} + {sampler_key}: {e}"
                    warnings.append(msg)
                    logger.warning(msg)
                    all_ok = False

    if all_ok:
        logger.info("  OK: All pattern/layout combinations are valid")

    return warnings


def _print_config_summary(bundle: ConfigBundle) -> None:
    """Print summary of loaded configs."""
    logger.info("Validating samples.csv...")
    logger.info(f"  OK: {len(bundle.samples.samples)} samples")
    for tech in sorted({s.technology for s in bundle.samples.samples}):
        count = len(bundle.samples.get_by_technology(tech))
        logger.info(f"    - {tech}: {count} samples")

    logger.info("Validating instruments.csv...")
    logger.info(f"  OK: {len(bundle.instruments.instruments)} instruments")
    for tech in sorted({i.technology for i in bundle.instruments.instruments}):
        count = len(bundle.instruments.get_by_technology(tech))
        logger.info(f"    - {tech}: {count} instruments")

    logger.info("Validating instrument_patterns.csv...")
    logger.info(f"  OK: {len(bundle.instrument_patterns.patterns)} instrument-pattern mappings")

    logger.info("Validating combinations.csv...")
    logger.info(f"  OK: {len(bundle.combinations.combinations)} valid combinations")

    logger.info("Validating sampler.toml...")
    logger.info(f"  OK: {len(bundle.samplers.get_sampler_names())} samplers")
    for name in bundle.samplers.get_sampler_names():
        sampler = getattr(bundle.samplers, name)
        if hasattr(sampler, "plates"):
            logger.info(f"    - {name}: grid sampler ({len(sampler.plates)} plates)")
        else:
            logger.info(f"    - {name}: tray sampler ({len(sampler.slots)} slots)")

    logger.info("Validating queue_patterns.toml...")
    for tech in bundle.queue_patterns.get_technologies():
        tech_patterns = bundle.queue_patterns.get_patterns_for_technology(tech)
        logger.info(f"  OK: {tech}: {len(tech_patterns)} patterns")

    logger.info("Validating qc_layouts CSVs...")
    for tech in bundle.qc_layouts.get_technologies():
        sampler_count = len(bundle.qc_layouts.get_samplers_for_technology(tech))
        logger.info(f"  OK: {tech}: {sampler_count} sampler layouts")

    logger.info("Validating output_formats.toml...")
    format_names = bundle.output_formats.get_format_names()
    logger.info(f"  OK: {len(format_names)} output formats ({', '.join(format_names)})")


# =============================================================================
# Main Validation Entry Point
# =============================================================================


def _validate_all_configs(bundle: ConfigBundle) -> bool:
    """Validate ConfigBundle and print cross-validation results.

    Args:
        bundle: Pre-loaded ConfigBundle from qg_config()

    Returns:
        True if all validations pass, False otherwise
    """
    warnings: list[str] = []

    # Print summary of loaded configs
    _print_config_summary(bundle)

    # Cross-validate references between configs
    warnings.extend(
        _cross_validate_instrument_refs(
            bundle.instruments, bundle.instrument_patterns, bundle.combinations
        )
    )
    warnings.extend(
        _cross_validate_sampler_refs(bundle.samplers, bundle.combinations)
    )
    warnings.extend(
        _cross_validate_sample_refs(bundle.samples, bundle.queue_patterns, bundle.qc_layouts)
    )
    warnings.extend(_cross_validate_qc_layout_patterns(bundle.queue_patterns, bundle.qc_layouts))

    # Summary
    if warnings:
        logger.error(f"{len(warnings)} cross-validation warning(s):")
        for msg in warnings:
            logger.error(f"  - {msg}")
        return False

    logger.info("All validations passed!")
    return True

@lru_cache(maxsize=1)
def qg_config(config_dir: Path | None = None) -> ConfigBundle:
    """Load all configuration files from a directory.

    Args:
        config_dir: Path to configuration directory (e.g., qg_configs/)
                   Contains core/ and ui/ subdirectories.

    Returns:
        ConfigBundle with all validated configurations
    """

    if config_dir is None:
        config_dir = Path(__file__).parent.parent.parent / "qg_configs"
    config_dir = Path(config_dir)
    core_dir = config_dir / "core"
    ui_dir = config_dir / "ui"

    # Load and validate all configs
    instruments = _load_instruments(core_dir / "instruments.csv")
    config_bundle = ConfigBundle(
        samples=_load_samples(core_dir / "samples.csv"),
        instruments=instruments,
        instrument_patterns=_load_instrument_patterns(ui_dir / "instrument_patterns.csv"),
        combinations=_load_combinations(ui_dir / "combinations.csv"),
        samplers=_load_samplers(core_dir / "sampler.toml"),
        queue_patterns=_load_queue_patterns(core_dir / "queue_patterns.toml"),
        qc_layouts=_load_qc_layouts(core_dir),
        output_formats=_load_output_formats(core_dir / "output_formats.toml"),
        methods=_load_methods(core_dir / "methods", instruments),
    )
    if not _validate_all_configs(config_bundle):
        raise ValueError("Config validation failed")
    return config_bundle


