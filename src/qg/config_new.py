"""Configuration loading for queue generation (new config structure).

This module provides the QGConfiguration class that loads configs from qg_configs_new/.
"""

from __future__ import annotations

import tomllib
from dataclasses import dataclass
from functools import lru_cache
from pathlib import Path

import polars as pl
from loguru import logger

from qg.config_model_formatting_new import (
    InstrumentsConfig,
    OutputFormatsConfig,
    SamplesConfig,
)
from qg.config_model_position_new import (
    PlateLayoutsConfig,
    QCLayoutsEvosepConfig,
    QCLayoutsGridConfig,
    SamplerPlateLayoutsConfig,
    SamplersConfig,
)
from qg.config_model_structure_new import QueuePatternsConfig
from qg.config_model_ui_new import InstrumentConfigsConfig
from qg.config_models_methods_new import MethodsConfig

# =============================================================================
# Exception Class
# =============================================================================


class ConfigValidationError(Exception):
    """Raised when config validation fails.

    Attributes:
        errors: List of validation error messages
    """

    def __init__(self, errors: list[str]):
        self.errors = errors
        message = f"{len(errors)} validation error(s):\n" + "\n".join(f"  - {e}" for e in errors)
        super().__init__(message)


# =============================================================================
# Private CSV Loaders
# =============================================================================


def _load_samples(path: Path) -> SamplesConfig:
    """Load samples from CSV file."""
    df = pl.read_csv(path)
    return SamplesConfig.from_table(df)


def _load_instruments(path: Path) -> InstrumentsConfig:
    """Load instruments from CSV file."""
    df = pl.read_csv(path)
    return InstrumentsConfig.from_table(df)


def _load_sampler_plate_layouts(path: Path) -> SamplerPlateLayoutsConfig:
    """Load sampler to plate_layout mappings from CSV file."""
    df = pl.read_csv(path)
    return SamplerPlateLayoutsConfig.from_table(df)


def _load_qc_layouts_grid(path: Path) -> QCLayoutsGridConfig:
    """Load QC layouts for grid samplers from CSV file."""
    df = pl.read_csv(path, comment_prefix="#")
    return QCLayoutsGridConfig.from_table(df)


def _load_qc_layouts_evosep(path: Path) -> QCLayoutsEvosepConfig:
    """Load QC layouts for Evosep from CSV file."""
    df = pl.read_csv(path, comment_prefix="#")
    return QCLayoutsEvosepConfig.from_table(df)


def _load_instrument_configs(path: Path) -> InstrumentConfigsConfig:
    """Load instrument configurations from CSV file."""
    df = pl.read_csv(path)
    return InstrumentConfigsConfig.from_table(df)


# =============================================================================
# Private TOML Loaders
# =============================================================================


def _load_output_formats(path: Path) -> OutputFormatsConfig:
    """Load output formats from TOML file."""
    with open(path, "rb") as f:
        data = tomllib.load(f)
    return OutputFormatsConfig.from_dict(data)


def _load_plate_layouts(path: Path) -> PlateLayoutsConfig:
    """Load plate layouts from TOML file."""
    with open(path, "rb") as f:
        data = tomllib.load(f)
    return PlateLayoutsConfig.from_dict(data)


def _load_samplers(path: Path) -> SamplersConfig:
    """Load samplers from TOML file."""
    with open(path, "rb") as f:
        data = tomllib.load(f)
    return SamplersConfig.from_dict(data)


def _load_queue_patterns(path: Path) -> QueuePatternsConfig:
    """Load queue patterns from TOML file."""
    with open(path, "rb") as f:
        data = tomllib.load(f)
    return QueuePatternsConfig.from_dict(data)


# =============================================================================
# Private Methods Loader (Multiple CSVs)
# =============================================================================


def _load_methods(methods_dir: Path, instruments: InstrumentsConfig) -> MethodsConfig:
    """Load all methods CSVs based on instruments config.

    Discovers methods files from instruments.methods_file paths and loads them
    into a MethodsConfig keyed by (tech_area, instrument).

    Args:
        methods_dir: Base directory for methods files (e.g., core/methods/)
        instruments: InstrumentsConfig to determine which methods files to load

    Returns:
        MethodsConfig with all methods loaded
    """
    tables: dict[tuple[str, str], pl.DataFrame] = {}

    for instr in instruments.instruments:
        # methods_file is like "methods/proteomics/ASTRAL_1_methods.csv"
        # Remove the "methods/" prefix to get relative path from methods_dir
        relative_path = instr.methods_file.removeprefix("methods/")
        methods_file = methods_dir / relative_path

        if not methods_file.exists():
            logger.warning(f"Methods file not found: {methods_file}")
            continue

        df = pl.read_csv(methods_file)
        tables[(instr.tech_area, instr.instrument)] = df

    return MethodsConfig.from_tables(tables)


# =============================================================================
# Cross-Validation Functions
# =============================================================================


def _validate_pattern_sample_refs(
    samples: SamplesConfig,
    queue_patterns: QueuePatternsConfig,
) -> list[str]:
    """Validate that queue patterns only reference defined samples.

    Checks that all sample_id values in pattern blocks (start, middle, end,
    separation, middle_extended) exist in samples.csv for the same tech_area.

    Returns:
        List of validation error messages
    """
    errors: list[str] = []
    logger.info("Validating queue_patterns sample references...")

    all_ok = True
    for tech_area in queue_patterns.get_technologies():
        valid_sample_ids = {s.sample_id for s in samples.get_by_tech_area(tech_area)}
        pattern_sample_ids = queue_patterns.get_all_sample_ids(tech_area)

        unknown = pattern_sample_ids - valid_sample_ids
        if unknown:
            msg = f"{tech_area} patterns reference unknown samples: {unknown}"
            errors.append(msg)
            logger.warning(msg)
            all_ok = False

    if all_ok:
        logger.info("  OK: All queue_patterns reference valid samples")

    return errors


def _validate_layout_sample_refs(
    samples: SamplesConfig,
    qc_layouts_grid: QCLayoutsGridConfig,
    qc_layouts_evosep: QCLayoutsEvosepConfig,
) -> list[str]:
    """Validate that QC layouts only reference defined samples.

    Checks that all sample_id values in qc_layouts_grid.csv and qc_layouts_evosep.csv
    exist in samples.csv for the same tech_area.

    Returns:
        List of validation error messages
    """
    errors: list[str] = []
    logger.info("Validating QC layout sample references...")

    all_ok = True

    # Validate grid layout samples
    grid_tech_samples: dict[str, set[str]] = {}
    for qc_sample in qc_layouts_grid.samples:
        tech = qc_sample.tech_area
        if tech not in grid_tech_samples:
            grid_tech_samples[tech] = set()
        grid_tech_samples[tech].add(qc_sample.sample_id)

    for tech, sample_ids in grid_tech_samples.items():
        valid_sample_ids = {s.sample_id for s in samples.get_by_tech_area(tech)}
        unknown = sample_ids - valid_sample_ids
        if unknown:
            msg = f"qc_layouts_grid.csv: {tech} references unknown samples: {unknown}"
            errors.append(msg)
            logger.warning(msg)
            all_ok = False

    # Validate evosep layout samples
    evosep_tech_samples: dict[str, set[str]] = {}
    for qc_sample in qc_layouts_evosep.samples:
        tech = qc_sample.tech_area
        if tech not in evosep_tech_samples:
            evosep_tech_samples[tech] = set()
        evosep_tech_samples[tech].add(qc_sample.sample_id)

    for tech, sample_ids in evosep_tech_samples.items():
        valid_sample_ids = {s.sample_id for s in samples.get_by_tech_area(tech)}
        unknown = sample_ids - valid_sample_ids
        if unknown:
            msg = f"qc_layouts_evosep.csv: {tech} references unknown samples: {unknown}"
            errors.append(msg)
            logger.warning(msg)
            all_ok = False

    if all_ok:
        logger.info("  OK: All QC layouts reference valid samples")

    return errors


def _validate_configs(
    *,
    samples: SamplesConfig,
    queue_patterns: QueuePatternsConfig,
    qc_layouts_grid: QCLayoutsGridConfig,
    qc_layouts_evosep: QCLayoutsEvosepConfig,
) -> list[str]:
    """Validate cross-references between configs.

    Returns:
        List of validation error messages (empty if all pass)
    """
    errors: list[str] = []

    errors.extend(_validate_pattern_sample_refs(samples, queue_patterns))
    errors.extend(_validate_layout_sample_refs(samples, qc_layouts_grid, qc_layouts_evosep))

    if errors:
        logger.error(f"{len(errors)} cross-validation error(s):")
        for msg in errors:
            logger.error(f"  - {msg}")
    else:
        logger.info("All validations passed!")

    return errors


# =============================================================================
# QGConfiguration Dataclass
# =============================================================================


@dataclass(slots=True)
class QGConfiguration:
    """Consolidated container for all configuration files.

    Use QGConfiguration.create() to construct validated instances.

    Attributes:
        # formatting/
        samples: Sample definitions (QC samples and defaults)
        instruments: Instrument definitions with methods file paths
        output_formats: Output format definitions (xcalibur, chronos, hystar)

        # position/
        plate_layouts: Plate layout definitions (grid geometries)
        samplers: Sampler definitions (trays, position function)
        sampler_plate_layouts: Sampler to plate_layout mappings
        qc_layouts_grid: QC sample positions for grid samplers
        qc_layouts_evosep: QC sample positions for Evosep

        # structure/
        queue_patterns: Queue pattern definitions (QC injection sequences)

        # methods/
        methods: Methods by tech_area and instrument

        # ui/
        instrument_configs: UI instrument configurations
    """

    # formatting/
    samples: SamplesConfig
    instruments: InstrumentsConfig
    output_formats: OutputFormatsConfig

    # position/
    plate_layouts: PlateLayoutsConfig
    samplers: SamplersConfig
    sampler_plate_layouts: SamplerPlateLayoutsConfig
    qc_layouts_grid: QCLayoutsGridConfig
    qc_layouts_evosep: QCLayoutsEvosepConfig

    # structure/
    queue_patterns: QueuePatternsConfig

    # methods/
    methods: MethodsConfig

    # ui/
    instrument_configs: InstrumentConfigsConfig

    @staticmethod
    def create(
        *,
        samples: SamplesConfig,
        instruments: InstrumentsConfig,
        output_formats: OutputFormatsConfig,
        plate_layouts: PlateLayoutsConfig,
        samplers: SamplersConfig,
        sampler_plate_layouts: SamplerPlateLayoutsConfig,
        qc_layouts_grid: QCLayoutsGridConfig,
        qc_layouts_evosep: QCLayoutsEvosepConfig,
        queue_patterns: QueuePatternsConfig,
        methods: MethodsConfig,
        instrument_configs: InstrumentConfigsConfig,
    ) -> QGConfiguration:
        """Create a validated QGConfiguration.

        Validates all cross-references between configs before constructing.
        Raises ConfigValidationError if validation fails.
        """
        errors = _validate_configs(
            samples=samples,
            queue_patterns=queue_patterns,
            qc_layouts_grid=qc_layouts_grid,
            qc_layouts_evosep=qc_layouts_evosep,
        )
        if errors:
            raise ConfigValidationError(errors)

        return QGConfiguration(
            samples=samples,
            instruments=instruments,
            output_formats=output_formats,
            plate_layouts=plate_layouts,
            samplers=samplers,
            sampler_plate_layouts=sampler_plate_layouts,
            qc_layouts_grid=qc_layouts_grid,
            qc_layouts_evosep=qc_layouts_evosep,
            queue_patterns=queue_patterns,
            methods=methods,
            instrument_configs=instrument_configs,
        )


# =============================================================================
# Public Loader Function
# =============================================================================


@lru_cache(maxsize=1)
def qg_configuration(config_dir: Path | None = None) -> QGConfiguration:
    """Load all configuration files from a directory.

    Args:
        config_dir: Path to configuration directory (e.g., qg_configs_new/).
                   Contains core/ and ui/ subdirectories.
                   Defaults to qg_configs_new/ relative to this module.

    Returns:
        QGConfiguration with all validated configurations

    Raises:
        ConfigValidationError: If cross-validation fails
        FileNotFoundError: If required config files are missing
        pydantic.ValidationError: If config data doesn't match schema
    """
    if config_dir is None:
        config_dir = Path(__file__).parent.parent.parent / "qg_configs_new"
    config_dir = Path(config_dir)

    core_dir = config_dir / "core"
    ui_dir = config_dir / "ui"

    # formatting/
    samples = _load_samples(core_dir / "formatting" / "samples.csv")
    instruments = _load_instruments(core_dir / "formatting" / "instruments.csv")
    output_formats = _load_output_formats(core_dir / "formatting" / "output_formats.toml")

    # position/
    plate_layouts = _load_plate_layouts(core_dir / "position" / "plate_layouts.toml")
    samplers = _load_samplers(core_dir / "position" / "sampler.toml")
    sampler_plate_layouts = _load_sampler_plate_layouts(core_dir / "position" / "sampler_plate_layouts.csv")
    qc_layouts_grid = _load_qc_layouts_grid(core_dir / "position" / "qc_layouts_grid.csv")
    qc_layouts_evosep = _load_qc_layouts_evosep(core_dir / "position" / "qc_layouts_evosep.csv")

    # structure/
    queue_patterns = _load_queue_patterns(core_dir / "structure" / "queue_patterns.toml")

    # methods/
    methods = _load_methods(core_dir / "methods", instruments)

    # ui/
    instrument_configs = _load_instrument_configs(ui_dir / "instrument_config.csv")

    # Validate and create configuration
    return QGConfiguration.create(
        samples=samples,
        instruments=instruments,
        output_formats=output_formats,
        plate_layouts=plate_layouts,
        samplers=samplers,
        sampler_plate_layouts=sampler_plate_layouts,
        qc_layouts_grid=qc_layouts_grid,
        qc_layouts_evosep=qc_layouts_evosep,
        queue_patterns=queue_patterns,
        methods=methods,
        instrument_configs=instrument_configs,
    )
