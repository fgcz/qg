"""Configuration loading functions for queue generation."""

from __future__ import annotations

import tomllib
from dataclasses import dataclass
from functools import lru_cache
from pathlib import Path
from typing import TYPE_CHECKING

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
    QCLayoutPattern,
    QCLayoutsConfig,
    QCPosition,
    QueuePattern,
    QueuePatternsConfig,
    Sample,
    SamplersConfig,
    SamplesConfig,
)

if TYPE_CHECKING:
    from qg.params_models import QueueParameters

# =============================================================================
# Module-level path cache (set by qg_config)
# =============================================================================

_core_dir: Path | None = None


# =============================================================================
# DataFrame/String Loaders (for ConfigStore validation)
# =============================================================================


def _samples_from_df(df: pl.DataFrame) -> SamplesConfig:
    """Create SamplesConfig from DataFrame."""
    samples = [Sample(**row) for row in df.iter_rows(named=True)]
    return SamplesConfig(samples=samples)


def _instruments_from_df(df: pl.DataFrame) -> InstrumentsConfig:
    """Create InstrumentsConfig from DataFrame."""
    instruments = [Instrument(**row) for row in df.iter_rows(named=True)]
    return InstrumentsConfig(instruments=instruments)


def _instrument_patterns_from_df(df: pl.DataFrame) -> InstrumentPatternsConfig:
    """Create InstrumentPatternsConfig from DataFrame."""
    patterns = [InstrumentPattern(**row) for row in df.iter_rows(named=True)]
    return InstrumentPatternsConfig(patterns=patterns)


def _combinations_from_df(df: pl.DataFrame) -> CombinationsConfig:
    """Create CombinationsConfig from DataFrame."""
    combinations = [Combination(**row) for row in df.iter_rows(named=True)]
    return CombinationsConfig(combinations=combinations)


def _samplers_from_toml_str(content: str) -> SamplersConfig:
    """Create SamplersConfig from TOML string."""
    data = tomllib.loads(content)
    return SamplersConfig(**data)


def _queue_patterns_from_toml_str(content: str) -> QueuePatternsConfig:
    """Create QueuePatternsConfig from TOML string."""
    raw_data = tomllib.loads(content)
    patterns: dict[str, dict[str, QueuePattern]] = {}
    for tech, tech_patterns in raw_data.items():
        patterns[tech] = {name: QueuePattern(**pattern_data) for name, pattern_data in tech_patterns.items()}
    return QueuePatternsConfig(patterns=patterns)


def _output_formats_from_toml_str(content: str) -> OutputFormatsConfig:
    """Create OutputFormatsConfig from TOML string."""
    raw_data = tomllib.loads(content)
    formats = {name: OutputFormat(**format_data) for name, format_data in raw_data.items()}
    return OutputFormatsConfig(formats=formats)


def _qc_layouts_from_dfs(grid_df: pl.DataFrame | None, evosep_df: pl.DataFrame | None) -> QCLayoutsConfig:
    """Create QCLayoutsConfig from grid and evosep DataFrames."""
    layouts: dict[str, dict[str, dict[str, QCPosition]]] = {}

    if grid_df is not None:
        for row in grid_df.iter_rows(named=True):
            tech = row["tech_area"]
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

    if evosep_df is not None:
        for row in evosep_df.iter_rows(named=True):
            tech = row["tech_area"]
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


def _methods_from_dfs(methods_dfs: dict[Path, pl.DataFrame], instruments: InstrumentsConfig) -> MethodsConfig:
    """Create MethodsConfig from dict of {path: DataFrame}.

    Args:
        methods_dfs: Dict mapping absolute paths to their DataFrames
        instruments: InstrumentsConfig to determine which methods files to use
    """
    methods: dict[str, dict[str, list[Method]]] = {}

    for instr in instruments.instruments:
        tech = instr.tech_area
        # Find matching DataFrame by checking if path ends with methods_file (case-insensitive)
        methods_file_suffix = instr.methods_file.removeprefix("methods/").lower()
        matching_df = None
        for path, df in methods_dfs.items():
            if path.as_posix().lower().endswith(methods_file_suffix):
                matching_df = df
                break

        if matching_df is None:
            logger.warning(f"Methods DataFrame not found for: {instr.methods_file}")
            continue

        instr_methods = [Method(**row) for row in matching_df.iter_rows(named=True)]

        if tech not in methods:
            methods[tech] = {}
        methods[tech][instr.instrument] = instr_methods

    return MethodsConfig(methods=methods)


# =============================================================================
# File-based Loaders (used by qg_config)
# =============================================================================


def _load_samples(path: Path | str) -> SamplesConfig:
    """Load and validate samples from CSV file."""
    df = pl.read_csv(path)
    return _samples_from_df(df)


def _load_instruments(path: Path | str) -> InstrumentsConfig:
    """Load and validate instruments from CSV file."""
    df = pl.read_csv(path)
    return _instruments_from_df(df)


def _load_instrument_patterns(path: Path | str) -> InstrumentPatternsConfig:
    """Load and validate instrument patterns from CSV file."""
    df = pl.read_csv(path)
    return _instrument_patterns_from_df(df)


def _load_combinations(path: Path | str) -> CombinationsConfig:
    """Load and validate combinations from CSV file."""
    df = pl.read_csv(path)
    return _combinations_from_df(df)


def _load_samplers(path: Path | str) -> SamplersConfig:
    """Load and validate samplers from TOML file."""
    with open(path, "rb") as f:
        content = f.read().decode("utf-8")
    return _samplers_from_toml_str(content)


def _load_queue_patterns(path: Path | str) -> QueuePatternsConfig:
    """Load and validate queue patterns from TOML file."""
    with open(path, "rb") as f:
        content = f.read().decode("utf-8")
    return _queue_patterns_from_toml_str(content)


def _load_qc_layouts(config_dir: Path | str) -> QCLayoutsConfig:
    """Load QC layouts from CSV files.

    Reads from two CSV files:
      - qc_layouts_grid.csv: Grid sampler positions (plate, row, col)
      - qc_layouts_evosep.csv: Evosep position ranges (tray, position_start, position_end)
    """
    config_dir = Path(config_dir)

    grid_df = None
    grid_path = config_dir / "qc_layouts_grid.csv"
    if grid_path.exists():
        grid_df = pl.read_csv(grid_path, comment_prefix="#")

    evosep_df = None
    evosep_path = config_dir / "qc_layouts_evosep.csv"
    if evosep_path.exists():
        evosep_df = pl.read_csv(evosep_path, comment_prefix="#")

    return _qc_layouts_from_dfs(grid_df, evosep_df)


def _load_output_formats(path: Path | str) -> OutputFormatsConfig:
    """Load and validate output formats from TOML file."""
    with open(path, "rb") as f:
        content = f.read().decode("utf-8")
    return _output_formats_from_toml_str(content)


def _load_methods(methods_dir: Path, instruments: InstrumentsConfig) -> MethodsConfig:
    """Load all methods CSVs based on instruments config.

    Discovers methods files from instruments.methods_file paths.
    """
    methods: dict[str, dict[str, list[Method]]] = {}

    for instr in instruments.instruments:
        tech = instr.tech_area
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
    """Consolidated container for all configuration files.

    Use ConfigBundle.create() to construct validated instances.
    """

    samples: SamplesConfig
    instruments: InstrumentsConfig
    instrument_patterns: InstrumentPatternsConfig
    combinations: CombinationsConfig
    samplers: SamplersConfig
    queue_patterns: QueuePatternsConfig
    qc_layouts: QCLayoutsConfig
    output_formats: OutputFormatsConfig
    methods: MethodsConfig

    @staticmethod
    def create(
        samples: SamplesConfig,
        instruments: InstrumentsConfig,
        instrument_patterns: InstrumentPatternsConfig,
        combinations: CombinationsConfig,
        samplers: SamplersConfig,
        queue_patterns: QueuePatternsConfig,
        qc_layouts: QCLayoutsConfig,
        output_formats: OutputFormatsConfig,
        methods: MethodsConfig,
    ) -> ConfigBundle:
        """Create a validated ConfigBundle.

        Validates all cross-references between configs before constructing the bundle.
        Raises ConfigValidationError if validation fails.
        """
        errors = _validate_configs(
            samples=samples,
            instruments=instruments,
            instrument_patterns=instrument_patterns,
            combinations=combinations,
            samplers=samplers,
            queue_patterns=queue_patterns,
            qc_layouts=qc_layouts,
        )
        if errors:
            raise ConfigValidationError(errors)

        return ConfigBundle(
            samples=samples,
            instruments=instruments,
            instrument_patterns=instrument_patterns,
            combinations=combinations,
            samplers=samplers,
            queue_patterns=queue_patterns,
            qc_layouts=qc_layouts,
            output_formats=output_formats,
            methods=methods,
        )

    def get_valid_samplers(self, tech_area: str | None = None) -> pl.DataFrame:
        """Get valid (tech_area, sampler) combinations from QC layouts.

        Args:
            tech_area: Filter to specific tech_area, or None for all

        Returns:
            DataFrame with columns: tech_area, sampler
        """
        rows = []
        for tech in self.qc_layouts.get_technologies():
            if tech_area is not None and tech != tech_area:
                continue
            for sampler in self.qc_layouts.get_samplers_for_tech_area(tech):
                rows.append({"tech_area": tech, "sampler": sampler})
        return pl.DataFrame(rows)

    def get_valid_instruments_samplers(self) -> pl.DataFrame:
        """Get valid (tech_area, instrument, sampler, output_format) combinations.

        Intersects instruments.csv (tech_area, instrument) with combinations.csv
        (instrument, sampler, output_format) and filters by QC layouts availability.

        Returns:
            DataFrame with columns: tech_area, instrument, sampler, output_format
        """
        # Build instruments DataFrame (tech_area, instrument)
        instruments_df = pl.DataFrame(
            [{"tech_area": i.tech_area, "instrument": i.instrument} for i in self.instruments.instruments]
        )

        # Build combinations DataFrame (instrument, sampler, output_format)
        combinations_df = self.combinations.to_table()

        # Get valid samplers from QC layouts (tech_area, sampler)
        valid_samplers_df = self.get_valid_samplers()

        # Join: instruments + combinations on instrument
        result = instruments_df.join(combinations_df, on="instrument", how="inner")

        # Filter by valid samplers (tech_area, sampler must exist in QC layouts)
        result = result.join(valid_samplers_df, on=["tech_area", "sampler"], how="inner")

        return result.select(["tech_area", "instrument", "sampler", "output_format"]).sort(
            ["tech_area", "instrument", "sampler"]
        )

    def get_qc_layout_pattern(self, params: QueueParameters) -> QCLayoutPattern:
        """Create a QCLayoutPattern for the given queue parameters.

        Resolves the pattern and QC layout, validates them, and returns a QCLayoutPattern.

        Args:
            params: Queue parameters with tech_area, sampler (e.g., "Vanquish.vial"), queue_pattern.

        Returns:
            Validated QCLayoutPattern instance.
        """
        pattern = self.queue_patterns.get_pattern(params.tech_area, params.queue_pattern)
        qc_layout = self.qc_layouts.get_layout(params.tech_area, params.sampler)
        return QCLayoutPattern.create(pattern, qc_layout)


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
    valid_instruments = {(i.tech_area, i.instrument) for i in instruments.instruments}
    pattern_instruments = {(p.tech_area, p.instrument) for p in instrument_patterns.patterns}
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
        valid_sample_ids = {s.sample_id for s in samples.get_by_tech_area(tech)}
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
        valid_sample_ids = {s.sample_id for s in samples.get_by_tech_area(tech)}
        for sampler_key in qc_layouts.get_samplers_for_tech_area(tech):
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

    Validates all (tech_area, pattern, sampler) combinations using
    QCLayoutPattern.create() to check:
    - Coverage: All QC IDs in pattern have positions in qc_layout
    - Uniqueness: No two QC samples map to the same position
    """
    warnings: list[str] = []
    logger.info("Cross-validating queue_patterns against qc_layouts...")
    all_ok = True

    for tech in queue_patterns.get_technologies():
        tech_patterns = queue_patterns.get_patterns_for_tech_area(tech)
        samplers = qc_layouts.get_samplers_for_tech_area(tech)

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


# =============================================================================
# Main Validation Entry Point
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


def _validate_configs(
    *,
    samples: SamplesConfig,
    instruments: InstrumentsConfig,
    instrument_patterns: InstrumentPatternsConfig,
    combinations: CombinationsConfig,
    samplers: SamplersConfig,
    queue_patterns: QueuePatternsConfig,
    qc_layouts: QCLayoutsConfig,
) -> list[str]:
    """Validate config components and return any validation errors.

    Cross-validates references between configs before they are bundled.

    Returns:
        List of validation error messages (empty if all pass)
    """
    errors: list[str] = []

    # Cross-validate references between configs
    errors.extend(_cross_validate_instrument_refs(instruments, instrument_patterns, combinations))
    errors.extend(_cross_validate_sampler_refs(samplers, combinations))
    errors.extend(_cross_validate_sample_refs(samples, queue_patterns, qc_layouts))
    errors.extend(_cross_validate_qc_layout_patterns(queue_patterns, qc_layouts))

    # Summary
    if errors:
        logger.error(f"{len(errors)} cross-validation error(s):")
        for msg in errors:
            logger.error(f"  - {msg}")
    else:
        logger.info("All validations passed!")

    return errors


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

    # Load all configs
    instruments = _load_instruments(core_dir / "instruments.csv")
    samples = _load_samples(core_dir / "samples.csv")
    instrument_patterns = _load_instrument_patterns(ui_dir / "instrument_patterns.csv")
    combinations = _load_combinations(ui_dir / "combinations.csv")
    samplers = _load_samplers(core_dir / "sampler.toml")
    queue_patterns = _load_queue_patterns(core_dir / "queue_patterns.toml")
    qc_layouts = _load_qc_layouts(core_dir)
    output_formats = _load_output_formats(core_dir / "output_formats.toml")
    methods = _load_methods(core_dir / "methods", instruments)

    # Validate and create bundle
    return ConfigBundle.create(
        samples=samples,
        instruments=instruments,
        instrument_patterns=instrument_patterns,
        combinations=combinations,
        samplers=samplers,
        queue_patterns=queue_patterns,
        qc_layouts=qc_layouts,
        output_formats=output_formats,
        methods=methods,
    )
