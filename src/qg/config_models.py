"""Pydantic models for queue generation configuration."""

from __future__ import annotations

import polars as pl
from pydantic import BaseModel, Field, field_validator, model_validator

from qg.config_models_samplers import SamplersConfig

__all__ = [
    # Re-exported from config_models_samplers
    "SamplersConfig",
    # Defined here
    "Sample",
    "SamplesConfig",
    "Instrument",
    "InstrumentsConfig",
    "InstrumentPattern",
    "InstrumentPatternsConfig",
    "Combination",
    "CombinationsConfig",
    "QueuePattern",
    "QueuePatternsConfig",
    "EvosepPosition",
    "QCPosition",
    "QCLayoutsConfig",
    "QCLayoutPattern",
    "OutputFormat",
    "OutputFormatsConfig",
    "Method",
    "MethodsConfig",
]


# =============================================================================
# Constants and Dynamic Derivation
# =============================================================================

# Valid placeholders in file_name_template (code-defined: what the generator can fill)
VALID_PLACEHOLDERS = {
    "date",
    "run",
    "container",
    "sample_id",
    "sample_name",
    "polarity",
}

# =============================================================================
# Sample Model
# =============================================================================


class Sample(BaseModel):
    """A QC or default sample definition."""

    tech_area: str = Field(..., min_length=1, description="tech_area identifier")
    sample_id: str = Field(..., min_length=1, description="Unique sample ID within tech_area")
    sample_name: str = Field(default="", description="Display name (empty for 'default')")
    description: str = Field(default="", description="Human-readable description")
    inj_vol: float = Field(..., gt=0, description="Injection volume in uL")
    file_name_template: str = Field(..., min_length=1, description="Template for output file names")

    @field_validator("sample_name", "description", mode="before")
    @classmethod
    def empty_str_if_none(cls, v):
        """Convert None to empty string (CSV empty cells)."""
        return v if v is not None else ""

    @field_validator("file_name_template")
    @classmethod
    def validate_template_placeholders(cls, v: str) -> str:
        """Check that all placeholders in the template are valid."""
        import re

        placeholders = re.findall(r"\{(\w+)\}", v)
        invalid = set(placeholders) - VALID_PLACEHOLDERS
        if invalid:
            raise ValueError(f"Invalid placeholders: {invalid}. Valid: {VALID_PLACEHOLDERS}")
        return v

    @model_validator(mode="after")
    def validate_default_has_sample_id(self) -> Sample:
        """Default samples should have {sample_id} in template."""
        if self.sample_id == "default":
            if "{sample_id}" not in self.file_name_template:
                raise ValueError("'default' sample should have {sample_id} in file_name_template")
        return self


class SamplesConfig(BaseModel):
    """Collection of all samples."""

    samples: list[Sample]

    @model_validator(mode="after")
    def validate_unique_keys(self) -> SamplesConfig:
        """Check that (tech_area, sample_id) pairs are unique."""
        keys = [(s.tech_area, s.sample_id) for s in self.samples]
        if len(keys) != len(set(keys)):
            duplicates = [k for k in keys if keys.count(k) > 1]
            raise ValueError(f"Duplicate (tech_area, sample_id) pairs: {set(duplicates)}")
        return self

    @model_validator(mode="after")
    def validate_each_tech_has_default(self) -> SamplesConfig:
        """Check that each tech_area has a 'default' sample."""
        techs_with_default = {s.tech_area for s in self.samples if s.sample_id == "default"}
        all_techs = {s.tech_area for s in self.samples}
        missing = all_techs - techs_with_default
        if missing:
            raise ValueError(f"Technologies missing 'default' sample: {missing}")
        return self

    def get_by_tech_area(self, tech: str) -> list[Sample]:
        """Get all samples for a tech_area."""
        return [s for s in self.samples if s.tech_area == tech]

    def get_sample(self, tech: str, sample_id: str) -> Sample | None:
        """Get a specific sample by tech_area and sample_id."""
        for s in self.samples:
            if s.tech_area == tech and s.sample_id == sample_id:
                return s
        return None


# =============================================================================
# Instrument Model
# =============================================================================


class Instrument(BaseModel):
    """An instrument definition."""

    tech_area: str = Field(..., min_length=1, description="tech_area identifier")
    instrument: str = Field(..., min_length=1, description="Instrument identifier")
    methods_file: str = Field(..., min_length=1, description="Path to methods CSV file")
    path_template: str = Field(default="", description="Data path template with {container}, {user}, {date}")

    @field_validator("methods_file")
    @classmethod
    def validate_methods_file_path(cls, v: str) -> str:
        """Check that methods_file follows expected pattern."""
        if not v.startswith("methods/"):
            raise ValueError(f"methods_file should start with 'methods/': {v}")
        if not v.endswith("_methods.csv"):
            raise ValueError(f"methods_file should end with '_methods.csv': {v}")
        return v


class InstrumentsConfig(BaseModel):
    """Collection of all instruments."""

    instruments: list[Instrument]

    @model_validator(mode="after")
    def validate_unique_keys(self) -> InstrumentsConfig:
        """Check that (tech_area, instrument) pairs are unique."""
        keys = [(i.tech_area, i.instrument) for i in self.instruments]
        if len(keys) != len(set(keys)):
            duplicates = [k for k in keys if keys.count(k) > 1]
            raise ValueError(f"Duplicate (tech_area, instrument) pairs: {set(duplicates)}")
        return self

    def get_by_tech_area(self, tech: str) -> list[Instrument]:
        """Get all instruments for a tech_area."""
        return [i for i in self.instruments if i.tech_area == tech]

    def get_instrument(self, tech: str, instrument: str) -> Instrument | None:
        """Get a specific instrument by tech_area and name."""
        for i in self.instruments:
            if i.tech_area == tech and i.instrument == instrument:
                return i
        return None

    def to_table(self) -> pl.DataFrame:
        """Convert instruments to a polars DataFrame."""
        return pl.DataFrame([i.model_dump() for i in self.instruments])


# =============================================================================
# Instrument Pattern Model
# =============================================================================


class InstrumentPattern(BaseModel):
    """Mapping of instrument to available queue patterns."""

    tech_area: str = Field(..., min_length=1, description="tech_area identifier")
    instrument: str = Field(..., min_length=1)
    queue_pattern: str = Field(..., min_length=1, description="Pattern name (e.g., standard, frequent)")
    is_default: bool = Field(..., description="Whether this is the default pattern for the instrument")


class InstrumentPatternsConfig(BaseModel):
    """Collection of all instrument-pattern mappings."""

    patterns: list[InstrumentPattern]

    @model_validator(mode="after")
    def validate_unique_keys(self) -> InstrumentPatternsConfig:
        """Check that (tech_area, instrument, queue_pattern) triples are unique."""
        keys = [(p.tech_area, p.instrument, p.queue_pattern) for p in self.patterns]
        if len(keys) != len(set(keys)):
            duplicates = [k for k in keys if keys.count(k) > 1]
            raise ValueError(f"Duplicate (tech_area, instrument, queue_pattern) triples: {set(duplicates)}")
        return self

    @model_validator(mode="after")
    def validate_one_default_per_instrument(self) -> InstrumentPatternsConfig:
        """Each (tech_area, instrument) should have exactly one default pattern."""
        from collections import defaultdict

        defaults_count: dict[tuple[str, str], int] = defaultdict(int)
        for p in self.patterns:
            if p.is_default:
                defaults_count[(p.tech_area, p.instrument)] += 1

        # Check for instruments with multiple defaults
        multi_defaults = {k: v for k, v in defaults_count.items() if v > 1}
        if multi_defaults:
            raise ValueError(f"Instruments with multiple default patterns: {multi_defaults}")

        # Check for instruments with no defaults
        all_instruments = {(p.tech_area, p.instrument) for p in self.patterns}
        no_defaults = all_instruments - set(defaults_count.keys())
        if no_defaults:
            raise ValueError(f"Instruments with no default pattern: {no_defaults}")

        return self

    def get_patterns_for_instrument(self, tech: str, instrument: str) -> list[InstrumentPattern]:
        """Get all patterns for a specific instrument."""
        return [p for p in self.patterns if p.tech_area == tech and p.instrument == instrument]

    def get_default_pattern(self, tech: str, instrument: str) -> InstrumentPattern | None:
        """Get the default pattern for an instrument."""
        for p in self.patterns:
            if p.tech_area == tech and p.instrument == instrument and p.is_default:
                return p
        return None

    def to_table(self) -> pl.DataFrame:
        """Convert instrument patterns to a polars DataFrame."""
        return pl.DataFrame([p.model_dump() for p in self.patterns])


# =============================================================================
# Combination Model
# =============================================================================


class Combination(BaseModel):
    """A valid instrument + sampler + output_format combination."""

    instrument: str = Field(..., min_length=1)
    sampler: str = Field(..., min_length=1, description="Sampler.container key")
    output_format: str = Field(..., min_length=1, description="Output format identifier (software)")


class CombinationsConfig(BaseModel):
    """Collection of all valid combinations."""

    combinations: list[Combination]

    @model_validator(mode="after")
    def validate_unique_keys(self) -> CombinationsConfig:
        """Check that (instrument, sampler) pairs are unique."""
        keys = [(c.instrument, c.sampler) for c in self.combinations]
        if len(keys) != len(set(keys)):
            duplicates = [k for k in keys if keys.count(k) > 1]
            raise ValueError(f"Duplicate (instrument, sampler) pairs: {set(duplicates)}")
        return self

    def get_samplers_for_instrument(self, instrument: str) -> list[str]:
        """Get all valid samplers for an instrument."""
        return [c.sampler for c in self.combinations if c.instrument == instrument]

    def get_combination(self, instrument: str, sampler: str) -> Combination | None:
        """Get the combination for a specific instrument+sampler."""
        for c in self.combinations:
            if c.instrument == instrument and c.sampler == sampler:
                return c
        return None

    def to_table(self) -> pl.DataFrame:
        """Convert combinations to a polars DataFrame."""
        return pl.DataFrame([c.model_dump() for c in self.combinations])


# =============================================================================
# Queue Pattern Models (TOML)
# =============================================================================


class QueuePattern(BaseModel):
    """A queue pattern defining QC injection sequences."""

    description: str
    run_QC_after_n_samples: int = Field(..., gt=0)
    start: list[str] = Field(default_factory=list)
    middle: list[str] = Field(default_factory=list)
    end: list[str] = Field(default_factory=list)
    # Extended middle for metabolomics/lipidomics dilution series
    middle_extended: list[str] | None = None
    middle_extended_frequency_multiplier: int | None = None
    # Separation block between project groups (falls back to middle if None)
    separation: list[str] | None = None

    @property
    def effective_separation(self) -> list[str]:
        """Get separation block, falling back to middle if not defined."""
        return self.separation if self.separation is not None else self.middle


class QueuePatternsConfig(BaseModel):
    """All queue patterns by tech_area.

    Structure: tech_area -> pattern_name -> QueuePattern
    """

    patterns: dict[str, dict[str, QueuePattern]] = Field(default_factory=dict)

    def get_technologies(self) -> list[str]:
        """Get list of all technologies with patterns defined."""
        return list(self.patterns.keys())

    def get_pattern(self, tech: str, pattern_name: str) -> QueuePattern | None:
        """Get a specific pattern by tech_area and name."""
        tech_patterns = self.patterns.get(tech, {})
        return tech_patterns.get(pattern_name)

    def get_patterns_for_tech_area(self, tech: str) -> dict[str, QueuePattern]:
        """Get all patterns for a tech_area."""
        return self.patterns.get(tech, {})

    def get_all_sample_refs(self, tech: str) -> set[str]:
        """Get all sample IDs referenced by patterns for a tech_area."""
        refs: set[str] = set()
        tech_patterns = self.patterns.get(tech, {})
        for pattern in tech_patterns.values():
            refs.update(pattern.start)
            refs.update(pattern.middle)
            refs.update(pattern.end)
            if pattern.middle_extended:
                refs.update(pattern.middle_extended)
            if pattern.separation:
                refs.update(pattern.separation)
        return refs


# =============================================================================
# QC Layout Models (TOML)
# =============================================================================


class EvosepPosition(BaseModel):
    """Evosep QC position range - consumable tips need sequential positions."""

    tray: int = Field(..., gt=0)
    position_start: int = Field(..., gt=0)
    position_end: int = Field(..., gt=0)

    @model_validator(mode="after")
    def validate_range(self) -> EvosepPosition:
        """End must be >= start."""
        if self.position_end < self.position_start:
            raise ValueError(f"position_end ({self.position_end}) < position_start ({self.position_start})")
        return self


# QC positions:
#   Grid samplers: {"plate": str, "row": str, "col": int}
#   Evosep samplers: EvosepPosition (range of consumable tips)
QCPosition = dict[str, str | int] | EvosepPosition


class QCLayoutsConfig(BaseModel):
    """All QC layouts by tech_area and sampler.

    Structure: tech_area -> sampler_key -> sample_id -> QCPosition
    sampler_key examples: "Vanquish.vial", "Vanquish.plate", "MClass48", "Evosep"
    """

    layouts: dict[str, dict[str, dict[str, QCPosition]]] = Field(default_factory=dict)

    def get_technologies(self) -> list[str]:
        """Get list of all technologies with layouts defined."""
        return list(self.layouts.keys())

    def get_layout(self, tech: str, sampler_key: str) -> dict[str, QCPosition] | None:
        """Get QC layout for tech_area and sampler.

        Tries exact key first (e.g., 'Vanquish.vial'), falls back to parent (e.g., 'Vanquish').
        """
        tech_layouts = self.layouts.get(tech, {})
        # Try exact match first
        if sampler_key in tech_layouts:
            return tech_layouts[sampler_key]
        # Try parent sampler (e.g., "Vanquish.vial" -> "Vanquish")
        parent = sampler_key.split(".")[0]
        return tech_layouts.get(parent)

    def get_samplers_for_tech_area(self, tech: str) -> list[str]:
        """Get all sampler keys for a tech_area."""
        return list(self.layouts.get(tech, {}).keys())


# =============================================================================
# QC Layout Pattern (Validated)
# =============================================================================


def _collect_pattern_qc_ids(pattern: QueuePattern) -> set[str]:
    """Collect all unique QC sample IDs from a pattern."""
    qc_ids: set[str] = set()
    qc_ids.update(pattern.start)
    qc_ids.update(pattern.middle)
    qc_ids.update(pattern.end)
    if pattern.separation:
        qc_ids.update(pattern.separation)
    if pattern.middle_extended:
        qc_ids.update(pattern.middle_extended)
    return qc_ids


def _position_to_key(pos: QCPosition) -> tuple:
    """Convert position to a hashable key for uniqueness check."""
    if isinstance(pos, EvosepPosition):
        # For Evosep ranges, use tray + start as key
        return ("tray", pos.tray, pos.position_start)
    # Sort items for consistent key regardless of dict order
    return tuple(sorted(pos.items()))


def _validate_unique_positions(positions: dict[str, QCPosition]) -> None:
    """Validate that all positions are unique.

    Raises:
        ValueError: If two different QC samples share the same position
    """
    seen: dict[tuple, str] = {}  # position_key -> qc_id
    for qc_id, pos in positions.items():
        key = _position_to_key(pos)
        if key in seen:
            raise ValueError(
                f"Position conflict: '{qc_id}' and '{seen[key]}' "
                f"both map to position {pos}"
            )
        seen[key] = qc_id


class QCLayoutPattern:
    """Validated QC layout for a specific queue pattern.

    Ensures that QC samples used in the pattern have unique positions.
    For Evosep, tracks position counters to allocate sequential positions.
    """

    def __init__(
        self,
        positions: dict[str, QCPosition],
        evosep_counters: dict[str, int] | None = None,
    ):
        self.positions = positions
        self._evosep_counters = evosep_counters or {}

    @classmethod
    def create(
        cls,
        pattern: QueuePattern,
        qc_layout: dict[str, QCPosition],
    ) -> QCLayoutPattern:
        """Create and validate QC layout for a pattern.

        Args:
            pattern: Queue pattern with QC sample IDs
            qc_layout: Raw QC positions from config

        Returns:
            Validated QCLayoutPattern

        Raises:
            ValueError: If a QC sample is missing from qc_layout or positions conflict
        """
        # 1. Collect unique QC IDs from pattern
        qc_ids = _collect_pattern_qc_ids(pattern)

        # 2. Map to positions
        positions: dict[str, QCPosition] = {}
        evosep_counters: dict[str, int] = {}

        for qc_id in qc_ids:
            if qc_id not in qc_layout:
                raise ValueError(f"QC sample '{qc_id}' not in qc_layout")
            pos = qc_layout[qc_id]
            if isinstance(pos, EvosepPosition):
                positions[qc_id] = pos
                evosep_counters[qc_id] = pos.position_start
            else:
                positions[qc_id] = dict(pos)  # Copy grid dict

        # 3. Validate uniqueness
        _validate_unique_positions(positions)

        return cls(positions=positions, evosep_counters=evosep_counters)

    def get_position(self, qc_id: str) -> dict[str, str | int]:
        """Get position dict for a QC sample ID.

        Returns unified format: {"tray": ..., "grid_position": ...}
        For Evosep, returns the next available position and increments counter.
        """
        pos = self.positions.get(qc_id)
        if pos is None:
            return {}

        if isinstance(pos, EvosepPosition):
            # Get next position in range
            current = self._evosep_counters.get(qc_id, pos.position_start)
            if current > pos.position_end:
                raise ValueError(
                    f"Evosep position range exhausted for '{qc_id}': "
                    f"needed position {current}, range is {pos.position_start}-{pos.position_end}"
                )
            # Increment counter for next call
            self._evosep_counters[qc_id] = current + 1
            return {"tray": pos.tray, "grid_position": current}
        else:
            # Grid position from qc_layouts_grid.csv: {plate, row, col}
            # Convert to unified format: {tray, grid_position}
            plate = pos.get("plate", "")
            row = pos.get("row", "")
            col = pos.get("col", "")
            grid_position = f"{row}{col}"
            return {"tray": plate, "grid_position": grid_position}


# =============================================================================
# Output Format Models (TOML)
# =============================================================================


class OutputFormat(BaseModel):
    """Output format definition."""

    description: str
    file_extension: str
    position_format: str # How to format {tray, grid_position} into position string
    columns: dict[str, str]  # output_column_name -> internal_field_name


class OutputFormatsConfig(BaseModel):
    """All output format definitions.

    Structure: format_name (software) -> OutputFormat
    """

    formats: dict[str, OutputFormat] = Field(default_factory=dict)

    def get_format_names(self) -> list[str]:
        """Get list of all defined output format names."""
        return list(self.formats.keys())

    def get_format(self, format_id: str) -> OutputFormat | None:
        """Get an output format by ID."""
        return self.formats.get(format_id)


# =============================================================================
# Method Models (CSV)
# =============================================================================


class Method(BaseModel):
    """A method definition from methods CSV."""

    sample_type: str = Field(..., min_length=1, description="Sample type (default, QC03dda, etc.)")
    polarity: str = Field(..., description="Polarity (pos, neg, or empty for proteomics)")
    method_name: str = Field(..., min_length=1, description="Method name")
    method_path: str = Field(..., min_length=1, description="Full path to method file")

    @field_validator("polarity", mode="before")
    @classmethod
    def empty_str_if_none(cls, v):
        """Convert None to empty string (CSV empty cells)."""
        return v if v is not None else ""


class MethodsConfig(BaseModel):
    """All methods by tech_area and instrument.

    Structure: tech_area -> instrument -> list[Method]
    """

    methods: dict[str, dict[str, list[Method]]] = Field(default_factory=dict)

    def get_methods(self, tech_area: str, instrument: str) -> list[Method]:
        """Get methods for a tech_area/instrument combination."""
        tech_methods = self.methods.get(tech_area, {})
        return tech_methods.get(instrument, [])

    def to_table(self, tech_area: str, instrument: str) -> pl.DataFrame:
        """Get methods as a polars DataFrame."""
        methods = self.get_methods(tech_area, instrument)
        if not methods:
            return pl.DataFrame()
        return pl.DataFrame([m.model_dump() for m in methods])

    def get_method_path(
        self,
        tech_area: str,
        instrument: str,
        sample_type: str,
        polarity: str,
        method_name: str = "",
    ) -> str:
        """Get method path for given parameters.

        Args:
            tech_area: Technology area (proteomics, metabolomics, lipidomics)
            instrument: Instrument name
            sample_type: Sample type (default, QC03dda, etc.)
            polarity: Polarity (pos, neg, or empty for proteomics)
            method_name: Optional specific method name to match

        Returns:
            Method path, or empty string if not found.
            Falls back to 'default' sample_type if specific type not found.
        """
        methods = self.get_methods(tech_area, instrument)
        if not methods:
            return ""

        # Find matching method
        for m in methods:
            if m.sample_type == sample_type and m.polarity == polarity:
                if not method_name or m.method_name == method_name:
                    return m.method_path

        # Fallback to "default" sample_type
        if sample_type != "default":
            return self.get_method_path(tech_area, instrument, "default", polarity, method_name)

        return ""
