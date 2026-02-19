"""Configuration loading for queue generation (new config structure).

This module provides the QGConfiguration class that loads configs from qg_configs/.
"""

from __future__ import annotations

import re
from dataclasses import dataclass
from functools import lru_cache
from importlib.resources import files
from pathlib import Path

import polars as pl
import tomli_w
from loguru import logger

from .formatting import (
    InstrumentsConfig,
    OutputFormatsConfig,
)
from .methods import MethodsConfig
from .positions import (
    PlateLayoutsConfig,
    QCLayoutsTipConfig,
    QCLayoutsWellConfig,
    QCSampleTip,
    QCSampleWell,
    Sampler,
    SamplerPlateLayoutsConfig,
    SamplersConfig,
)
from .structure import QueuePatternsConfig, SamplesConfig
from .ui import InstrumentConfigsConfig

# =============================================================================
# Helper Functions
# =============================================================================


def _compact_toml(toml_str: str) -> str:
    """Collapse tomli_w's multi-line arrays back to inline format."""
    pattern = r"(\w+)\s*=\s*\[\s*\n((?:\s+[^\]]+,?\s*\n)+)\s*\]"

    def replace_array(match: re.Match[str]) -> str:
        key = match.group(1)
        items_block = match.group(2)
        items = [item.strip().rstrip(",") for item in items_block.strip().split("\n") if item.strip()]
        return f"{key} = [{', '.join(items)}]"

    return re.sub(pattern, replace_array, toml_str)


def read_header_comments(text: str) -> str:
    """Extract leading comment and blank lines from TOML text.

    Reads lines until the first non-comment, non-blank line is found.
    Trailing blank lines are trimmed, and a blank-line separator is appended.

    Returns:
        Header block ending with ``\\n\\n``, or empty string if no comments.
    """
    lines: list[str] = []
    for line in text.splitlines():
        stripped = line.strip()
        if stripped.startswith("#") or stripped == "":
            lines.append(line)
        else:
            break
    # Trim trailing blank lines
    while lines and lines[-1].strip() == "":
        lines.pop()
    return "\n".join(lines) + "\n\n" if lines else ""


def _default_config_dir() -> Path:
    """Get default config directory using package resources.

    Uses importlib.resources to locate the qg package, then navigates to qg_configs/.
    Falls back to __file__ navigation if the directory doesn't exist (e.g., editable install).

    Returns:
        Path to qg_configs/ directory
    """
    try:
        # files("qg") returns the qg package directory (e.g., src/qg/ or site-packages/qg/)
        pkg_path = Path(str(files("qg")))
        # qg_configs is at project root: 2 levels up from src/qg/
        config_dir = (pkg_path.parent.parent / "qg_configs").resolve()
        if config_dir.is_dir():
            return config_dir
    except (TypeError, ModuleNotFoundError):
        pass

    # Fallback: navigate from __file__ (for editable installs or when resources fail)
    return (Path(__file__).parent.parent.parent.parent / "qg_configs").resolve()


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
    qc_layouts_well: QCLayoutsWellConfig,
    qc_layouts_tip: QCLayoutsTipConfig,
) -> list[str]:
    """Validate that QC layouts only reference defined samples.

    Checks that all sample_id values in qc_layouts_well.csv and qc_layouts_tip.csv
    exist in samples.csv for the same tech_area.

    Returns:
        List of validation error messages
    """
    errors: list[str] = []
    logger.info("Validating QC layout sample references...")

    all_ok = True

    # Validate well-plate layout samples
    well_tech_samples: dict[str, set[str]] = {}
    for qc_sample in qc_layouts_well.samples:
        tech = qc_sample.tech_area
        if tech not in well_tech_samples:
            well_tech_samples[tech] = set()
        well_tech_samples[tech].add(qc_sample.sample_id)

    for tech, sample_ids in well_tech_samples.items():
        valid_sample_ids = {s.sample_id for s in samples.get_by_tech_area(tech)}
        unknown = sample_ids - valid_sample_ids
        if unknown:
            msg = f"qc_layouts_well.csv: {tech} references unknown samples: {unknown}"
            errors.append(msg)
            logger.warning(msg)
            all_ok = False

    # Validate tip-plate layout samples
    tip_tech_samples: dict[str, set[str]] = {}
    for qc_sample in qc_layouts_tip.samples:
        tech = qc_sample.tech_area
        if tech not in tip_tech_samples:
            tip_tech_samples[tech] = set()
        tip_tech_samples[tech].add(qc_sample.sample_id)

    for tech, sample_ids in tip_tech_samples.items():
        valid_sample_ids = {s.sample_id for s in samples.get_by_tech_area(tech)}
        unknown = sample_ids - valid_sample_ids
        if unknown:
            msg = f"qc_layouts_tip.csv: {tech} references unknown samples: {unknown}"
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
    qc_layouts_well: QCLayoutsWellConfig,
    qc_layouts_tip: QCLayoutsTipConfig,
) -> list[str]:
    """Validate cross-references between configs.

    Returns:
        List of validation error messages (empty if all pass)
    """
    errors: list[str] = []

    errors.extend(_validate_pattern_sample_refs(samples, queue_patterns))
    errors.extend(_validate_layout_sample_refs(samples, qc_layouts_well, qc_layouts_tip))

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
        instruments: Instrument definitions with methods file paths
        output_formats: Output format definitions (xcalibur, chronos, hystar)

        # position/
        plate_layouts: Plate layout definitions (grid geometries)
        samplers: Sampler definitions (trays, position function)
        sampler_plate_layouts: Sampler to plate_layout mappings
        qc_layouts_well: QC sample positions for well-plate samplers
        qc_layouts_tip: QC sample positions for tip-plate samplers

        # structure/
        samples: Sample definitions (QC samples and defaults)
        queue_patterns: Queue pattern definitions (QC injection sequences)

        # methods/
        methods: Methods by tech_area and instrument

        # ui/
        instrument_configs: UI instrument configurations
    """

    # formatting/
    instruments: InstrumentsConfig
    output_formats: OutputFormatsConfig

    # position/
    plate_layouts: PlateLayoutsConfig
    samplers: SamplersConfig
    sampler_plate_layouts: SamplerPlateLayoutsConfig
    qc_layouts_well: QCLayoutsWellConfig
    qc_layouts_tip: QCLayoutsTipConfig

    # structure/
    samples: SamplesConfig
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

    def get_qc_samples(
        self,
        tech_area: str,
        qc_layout_name: str,
        plate_layout_name: str,
        sampler: Sampler,
    ) -> list[QCSampleWell] | list[QCSampleTip]:
        """Get QC samples from appropriate layout based on sampler type.

        Centralizes the well vs tip branching logic.

        Args:
            tech_area: Technology area (e.g., "Proteomics")
            qc_layout_name: QC layout name (e.g., "standard")
            plate_layout_name: Plate layout name (e.g., "Vanquish_54")
            sampler: Sampler config object (uses sampler.is_tip to select layout)

        Returns:
            List of QC samples (QCSampleWell or QCSampleTip)
        """
        if sampler.is_tip:
            return self.qc_layouts_tip.get_samples(tech_area, qc_layout_name, plate_layout_name)
        return self.qc_layouts_well.get_samples(tech_area, qc_layout_name, plate_layout_name)

    @staticmethod
    def create(
        *,
        samples: SamplesConfig,
        instruments: InstrumentsConfig,
        output_formats: OutputFormatsConfig,
        plate_layouts: PlateLayoutsConfig,
        samplers: SamplersConfig,
        sampler_plate_layouts: SamplerPlateLayoutsConfig,
        qc_layouts_well: QCLayoutsWellConfig,
        qc_layouts_tip: QCLayoutsTipConfig,
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
            qc_layouts_well=qc_layouts_well,
            qc_layouts_tip=qc_layouts_tip,
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
            qc_layouts_well=qc_layouts_well,
            qc_layouts_tip=qc_layouts_tip,
            queue_patterns=queue_patterns,
            methods=methods,
            instrument_configs=instrument_configs,
        )

    def serialize_all(self) -> dict[str, str]:
        """Serialize all configs to strings without writing to disk.

        Returns:
            Dict mapping repo-relative paths to file contents as strings.

        Raises:
            ConfigValidationError: If cross-validation fails.
        """
        errors = _validate_configs(
            samples=self.samples,
            queue_patterns=self.queue_patterns,
            qc_layouts_well=self.qc_layouts_well,
            qc_layouts_tip=self.qc_layouts_tip,
        )
        if errors:
            raise ConfigValidationError(errors)

        contents: dict[str, str] = {}

        # CSV configs (use config_path ClassVar as single source of truth)
        csv_configs = [
            self.instruments,
            self.samples,
            self.sampler_plate_layouts,
            self.qc_layouts_well,
            self.qc_layouts_tip,
            self.instrument_configs,
        ]
        for cfg in csv_configs:
            contents[str(cfg.config_path)] = cfg.to_table().write_csv()

        # TOML configs (use config_path ClassVar as single source of truth)
        toml_configs = [
            self.output_formats,
            self.plate_layouts,
            self.samplers,
            self.queue_patterns,
        ]
        for cfg in toml_configs:
            header = cfg.header_comments
            body = _compact_toml(tomli_w.dumps(cfg.to_dict()))
            contents[str(cfg.config_path)] = header + body

        # Methods (multiple CSV files, use config_folder ClassVar)
        methods_base = MethodsConfig.config_folder
        for (tech_area, instrument), df in self.methods.to_tables().items():
            instr = self.instruments.get_instrument(tech_area, instrument)
            relative_path = instr.methods_file.removeprefix("methods/")
            contents[str(methods_base / relative_path)] = df.write_csv()

        return contents

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
            qc_layouts_well=self.qc_layouts_well,
            qc_layouts_tip=self.qc_layouts_tip,
        )
        if errors:
            raise ConfigValidationError(errors)

        written: list[str] = []

        # CSV configs (use config_path ClassVar as single source of truth)
        csv_configs = [
            self.instruments,
            self.samples,
            self.sampler_plate_layouts,
            self.qc_layouts_well,
            self.qc_layouts_tip,
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
            header = cfg.header_comments
            body = _compact_toml(tomli_w.dumps(cfg.to_dict()))
            path.write_text(header + body)
            written.append(str(cfg.config_path))

        # Methods (multiple CSV files, use config_folder ClassVar)
        methods_base = MethodsConfig.config_folder
        for (tech_area, instrument), df in self.methods.to_tables().items():
            instr = self.instruments.get_instrument(tech_area, instrument)
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
        config_dir = _default_config_dir()
    config_dir = Path(config_dir)

    # Load configs using ClassVar paths as single source of truth
    # formatting/
    instruments = InstrumentsConfig.load(config_dir)
    output_formats = OutputFormatsConfig.load(config_dir)

    # position/
    plate_layouts = PlateLayoutsConfig.load(config_dir)
    samplers = SamplersConfig.load(config_dir)
    sampler_plate_layouts = SamplerPlateLayoutsConfig.load(config_dir)
    qc_layouts_well = QCLayoutsWellConfig.load(config_dir)
    qc_layouts_tip = QCLayoutsTipConfig.load(config_dir)

    # structure/
    samples = SamplesConfig.load(config_dir)
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
        qc_layouts_well=qc_layouts_well,
        qc_layouts_tip=qc_layouts_tip,
        queue_patterns=queue_patterns,
        methods=methods,
        instrument_configs=instrument_configs,
    )
