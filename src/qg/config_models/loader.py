"""Configuration loading for queue generation (new config structure).

This module provides the QGConfiguration class that loads configs from qg_configs/.
"""

from __future__ import annotations

from dataclasses import dataclass
from functools import lru_cache
from pathlib import Path

import polars as pl
import tomli_w
from loguru import logger

from .formatting import (
    InstrumentsConfig,
    OutputFormatsConfig,
    SamplesConfig,
)
from .methods import MethodsConfig
from .positions import (
    PlateLayoutsConfig,
    QCLayoutsEvosepConfig,
    QCLayoutsGridConfig,
    SamplerPlateLayoutsConfig,
    SamplersConfig,
)
from .structure import QueuePatternsConfig
from .ui import InstrumentConfigsConfig

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

    def to_overview_table(self) -> pl.DataFrame:
        """Return denormalized table of all valid config combinations.

        Joins: instrument_configs → sampler_plate_layouts → queue_patterns (all patterns)

        Each (instrument, sampler, plate_layout) combination appears multiple times -
        once per available pattern for that tech_area.

        Returns:
            DataFrame with columns: tech_area, instrument, sampler, plate_layout,
            queue_type, output_format, default_pattern, pattern_name, qc_layout_name
        """
        # Start with instrument_configs
        df = self.instrument_configs.to_table()

        # Join sampler_plate_layouts on sampler
        spl = self.sampler_plate_layouts.to_table()
        df = df.join(spl, on="sampler", how="left")

        # Build patterns table: (tech_area, pattern_name, qc_layout_name)
        patterns_rows = [
            {
                "tech_area": tech,
                "pattern_name": name,
                "qc_layout_name": pattern.qc_layout_name,
            }
            for tech, patterns in self.queue_patterns.patterns.items()
            for name, pattern in patterns.items()
        ]
        patterns_df = pl.DataFrame(patterns_rows)

        # Cross join: each instrument config gets ALL patterns for its tech_area
        df = df.join(patterns_df, on="tech_area", how="left")

        # Select final columns
        return df.select(
            [
                "tech_area",
                "instrument",
                "sampler",
                "plate_layout",
                "queue_type",
                "output_format",
                "default_pattern",
                "pattern_name",
                "qc_layout_name",
            ]
        )

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

    def write_all(self, config_dir: Path) -> list[str]:
        """Write all configs to disk after validation.

        Args:
            config_dir: Directory to write configs to.

        Returns:
            List of written file paths (relative to config_dir).

        Raises:
            ConfigValidationError: If cross-validation fails.
        """
        errors = _validate_configs(
            samples=self.samples,
            queue_patterns=self.queue_patterns,
            qc_layouts_grid=self.qc_layouts_grid,
            qc_layouts_evosep=self.qc_layouts_evosep,
        )
        if errors:
            raise ConfigValidationError(errors)

        written: list[str] = []

        # CSV configs (use config_path ClassVar as single source of truth)
        csv_configs = [
            self.instruments,
            self.samples,
            self.sampler_plate_layouts,
            self.qc_layouts_grid,
            self.qc_layouts_evosep,
            self.instrument_configs,
        ]
        for cfg in csv_configs:
            path = config_dir / cfg.config_path
            path.parent.mkdir(parents=True, exist_ok=True)
            cfg.to_table().write_csv(path)
            written.append(str(cfg.config_path))

        # TOML configs (use config_path ClassVar as single source of truth)
        toml_configs = [
            self.output_formats,
            self.plate_layouts,
            self.samplers,
            self.queue_patterns,
        ]
        for cfg in toml_configs:
            path = config_dir / cfg.config_path
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_bytes(tomli_w.dumps(cfg.to_dict()).encode())
            written.append(str(cfg.config_path))

        # Methods (multiple CSV files, use config_folder ClassVar)
        methods_base = MethodsConfig.config_folder
        for (tech_area, instrument), df in self.methods.to_tables().items():
            instr = self.instruments.get_instrument(tech_area, instrument)
            if instr is None:
                continue
            # methods_file is like "methods/proteomics/ASTRAL_1_methods.csv"
            # Remove the "methods/" prefix to get relative path from methods_base
            relative_path = instr.methods_file.removeprefix("methods/")
            path = config_dir / methods_base / relative_path
            path.parent.mkdir(parents=True, exist_ok=True)
            df.write_csv(path)
            written.append(str(methods_base / relative_path))

        logger.info("Wrote {} config files to {}", len(written), config_dir)
        return written


# =============================================================================
# Public Loader Function
# =============================================================================


@lru_cache(maxsize=1)
def qg_configuration(config_dir: Path | None = None) -> QGConfiguration:
    """Load all configuration files from a directory.

    Args:
        config_dir: Path to configuration directory (e.g., qg_configs/).
                   Contains core/ and ui/ subdirectories.
                   Defaults to qg_configs/ relative to this module.

    Returns:
        QGConfiguration with all validated configurations

    Raises:
        ConfigValidationError: If cross-validation fails
        FileNotFoundError: If required config files are missing
        pydantic.ValidationError: If config data doesn't match schema
    """
    if config_dir is None:
        # Navigate from loader.py up to project root: src/qg/config_models_new/ -> qg_configs/
        config_dir = Path(__file__).parent.parent.parent.parent / "qg_configs"
    config_dir = Path(config_dir)

    # Load configs using ClassVar paths as single source of truth
    # formatting/
    samples = SamplesConfig.load(config_dir)
    instruments = InstrumentsConfig.load(config_dir)
    output_formats = OutputFormatsConfig.load(config_dir)

    # position/
    plate_layouts = PlateLayoutsConfig.load(config_dir)
    samplers = SamplersConfig.load(config_dir)
    sampler_plate_layouts = SamplerPlateLayoutsConfig.load(config_dir)
    qc_layouts_grid = QCLayoutsGridConfig.load(config_dir)
    qc_layouts_evosep = QCLayoutsEvosepConfig.load(config_dir)

    # structure/
    queue_patterns = QueuePatternsConfig.load(config_dir)

    # methods/ (requires instruments for dynamic loading)
    methods = MethodsConfig.load(config_dir, instruments)

    # ui/
    instrument_configs = InstrumentConfigsConfig.load(config_dir)

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
