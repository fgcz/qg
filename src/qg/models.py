"""Pydantic models for queue generation configuration."""

from typing import Literal

from pydantic import BaseModel, Field, field_validator, model_validator


# =============================================================================
# Constants
# =============================================================================

# Technologies requiring polarity expansion (business logic)
POLARITY_TECHNOLOGIES: set[str] = {"metabolomics", "lipidomics"}


def requires_polarity(technology: str) -> bool:
    """Check if a technology requires polarity expansion."""
    return technology in POLARITY_TECHNOLOGIES


# Valid placeholders in file_name_template
VALID_PLACEHOLDERS = {
    "date",
    "run",
    "container",
    "sample_id",
    "sample_name",
    "polarity",
}

# Valid sampler.container values
VALID_SAMPLERS = {
    "Vanquish.vial",
    "Vanquish.plate",
    "MClass48.vial",
    "MClass48.plate",
    "Evosep.vial",
    "Evosep.plate",
}


# =============================================================================
# Sample Model
# =============================================================================


class Sample(BaseModel):
    """A QC or default sample definition."""

    technology: str = Field(..., min_length=1, description="Technology identifier")
    sample_id: str = Field(..., min_length=1, description="Unique sample ID within technology")
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
    def validate_polarity_for_technology(self) -> "Sample":
        """Technologies requiring polarity should have {polarity} in template."""
        if requires_polarity(self.technology):
            if "{polarity}" not in self.file_name_template:
                raise ValueError(
                    f"{self.technology} samples should have {{polarity}} in file_name_template"
                )
        return self

    @model_validator(mode="after")
    def validate_default_has_sample_id(self) -> "Sample":
        """Default samples should have {sample_id} in template."""
        if self.sample_id == "default":
            if "{sample_id}" not in self.file_name_template:
                raise ValueError("'default' sample should have {sample_id} in file_name_template")
        return self


class SamplesConfig(BaseModel):
    """Collection of all samples."""

    samples: list[Sample]

    @model_validator(mode="after")
    def validate_unique_keys(self) -> "SamplesConfig":
        """Check that (technology, sample_id) pairs are unique."""
        keys = [(s.technology, s.sample_id) for s in self.samples]
        if len(keys) != len(set(keys)):
            duplicates = [k for k in keys if keys.count(k) > 1]
            raise ValueError(f"Duplicate (technology, sample_id) pairs: {set(duplicates)}")
        return self

    @model_validator(mode="after")
    def validate_each_tech_has_default(self) -> "SamplesConfig":
        """Check that each technology has a 'default' sample."""
        techs_with_default = {s.technology for s in self.samples if s.sample_id == "default"}
        all_techs = {s.technology for s in self.samples}
        missing = all_techs - techs_with_default
        if missing:
            raise ValueError(f"Technologies missing 'default' sample: {missing}")
        return self

    def get_by_technology(self, tech: str) -> list[Sample]:
        """Get all samples for a technology."""
        return [s for s in self.samples if s.technology == tech]

    def get_sample(self, tech: str, sample_id: str) -> Sample | None:
        """Get a specific sample by technology and sample_id."""
        for s in self.samples:
            if s.technology == tech and s.sample_id == sample_id:
                return s
        return None


# =============================================================================
# Instrument Model
# =============================================================================


class Instrument(BaseModel):
    """An instrument definition."""

    technology: str = Field(..., min_length=1, description="Technology identifier")
    instrument: str = Field(..., min_length=1, description="Instrument identifier")
    methods_file: str = Field(..., min_length=1, description="Path to methods CSV file")

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
    def validate_unique_keys(self) -> "InstrumentsConfig":
        """Check that (technology, instrument) pairs are unique."""
        keys = [(i.technology, i.instrument) for i in self.instruments]
        if len(keys) != len(set(keys)):
            duplicates = [k for k in keys if keys.count(k) > 1]
            raise ValueError(f"Duplicate (technology, instrument) pairs: {set(duplicates)}")
        return self

    def get_by_technology(self, tech: str) -> list[Instrument]:
        """Get all instruments for a technology."""
        return [i for i in self.instruments if i.technology == tech]

    def get_instrument(self, tech: str, instrument: str) -> Instrument | None:
        """Get a specific instrument by technology and name."""
        for i in self.instruments:
            if i.technology == tech and i.instrument == instrument:
                return i
        return None


# =============================================================================
# Instrument Pattern Model
# =============================================================================


class InstrumentPattern(BaseModel):
    """Mapping of instrument to available queue patterns."""

    technology: str = Field(..., min_length=1, description="Technology identifier")
    instrument: str = Field(..., min_length=1)
    pattern: str = Field(..., min_length=1, description="Pattern name (e.g., standard, frequent)")
    is_default: bool = Field(..., description="Whether this is the default pattern for the instrument")


class InstrumentPatternsConfig(BaseModel):
    """Collection of all instrument-pattern mappings."""

    patterns: list[InstrumentPattern]

    @model_validator(mode="after")
    def validate_unique_keys(self) -> "InstrumentPatternsConfig":
        """Check that (technology, instrument, pattern) triples are unique."""
        keys = [(p.technology, p.instrument, p.pattern) for p in self.patterns]
        if len(keys) != len(set(keys)):
            duplicates = [k for k in keys if keys.count(k) > 1]
            raise ValueError(f"Duplicate (technology, instrument, pattern) triples: {set(duplicates)}")
        return self

    @model_validator(mode="after")
    def validate_one_default_per_instrument(self) -> "InstrumentPatternsConfig":
        """Each (technology, instrument) should have exactly one default pattern."""
        from collections import defaultdict

        defaults_count: dict[tuple[str, str], int] = defaultdict(int)
        for p in self.patterns:
            if p.is_default:
                defaults_count[(p.technology, p.instrument)] += 1

        # Check for instruments with multiple defaults
        multi_defaults = {k: v for k, v in defaults_count.items() if v > 1}
        if multi_defaults:
            raise ValueError(f"Instruments with multiple default patterns: {multi_defaults}")

        # Check for instruments with no defaults
        all_instruments = {(p.technology, p.instrument) for p in self.patterns}
        no_defaults = all_instruments - set(defaults_count.keys())
        if no_defaults:
            raise ValueError(f"Instruments with no default pattern: {no_defaults}")

        return self

    def get_patterns_for_instrument(self, tech: str, instrument: str) -> list[InstrumentPattern]:
        """Get all patterns for a specific instrument."""
        return [p for p in self.patterns if p.technology == tech and p.instrument == instrument]

    def get_default_pattern(self, tech: str, instrument: str) -> InstrumentPattern | None:
        """Get the default pattern for an instrument."""
        for p in self.patterns:
            if p.technology == tech and p.instrument == instrument and p.is_default:
                return p
        return None


# =============================================================================
# Combination Model
# =============================================================================


class Combination(BaseModel):
    """A valid instrument + sampler + output_format combination."""

    instrument: str = Field(..., min_length=1)
    sampler: str = Field(..., min_length=1, description="Sampler.container key")
    output_format: str = Field(..., min_length=1, description="Output format identifier (software)")

    @field_validator("sampler")
    @classmethod
    def validate_sampler_format(cls, v: str) -> str:
        """Check that sampler is a valid Sampler.container key."""
        if v not in VALID_SAMPLERS:
            raise ValueError(f"Invalid sampler: {v}. Valid: {VALID_SAMPLERS}")
        return v


class CombinationsConfig(BaseModel):
    """Collection of all valid combinations."""

    combinations: list[Combination]

    @model_validator(mode="after")
    def validate_unique_keys(self) -> "CombinationsConfig":
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


# =============================================================================
# Sampler Models (TOML)
# =============================================================================


class GridContainer(BaseModel):
    """Container config for grid-based samplers (Vanquish, MClass48)."""

    container_type: Literal["Vial", "Plate"]
    position_source: Literal["generated", "input"]
    fill_order: Literal["row_major"] | None = None
    position_format: str | None = None
    # These may be inherited from parent or defined here
    sample_rows: list[str] | None = None
    qc_row: str | None = None
    cols: list[int] | None = None
    samples_per_plate: int | None = None

    @model_validator(mode="after")
    def validate_generated_has_fill_order(self) -> "GridContainer":
        """Generated positions need fill_order."""
        if self.position_source == "generated" and self.fill_order is None:
            raise ValueError("position_source='generated' requires fill_order")
        return self


class EvosepContainer(BaseModel):
    """Container config for Evosep sampler."""

    container_type: Literal["Vial", "Plate"]
    position_source: Literal["generated", "input"]
    fill_order: Literal["sequential"] | None = None


class GridSampler(BaseModel):
    """Grid-based sampler (Vanquish, MClass48)."""

    description: str
    plates: list[str]
    qc_plate: str
    # Grid layout (may be at parent or child level)
    sample_rows: list[str] | None = None
    qc_row: str | None = None
    cols: list[int] | None = None
    samples_per_plate: int | None = None
    # Container configs
    vial: GridContainer | None = None
    plate: GridContainer | None = None

    @model_validator(mode="after")
    def validate_qc_plate_in_plates(self) -> "GridSampler":
        """QC plate must be one of the available plates."""
        if self.qc_plate not in self.plates:
            raise ValueError(f"qc_plate '{self.qc_plate}' not in plates {self.plates}")
        return self


class EvosepSampler(BaseModel):
    """Evosep tray-based sampler."""

    description: str
    position_type: Literal["tray_position"] = "tray_position"
    slots: list[int]
    positions_per_slot: int = Field(..., gt=0)
    # Container configs
    vial: EvosepContainer | None = None
    plate: EvosepContainer | None = None


class SamplersConfig(BaseModel):
    """All sampler configurations."""

    Vanquish: GridSampler | None = None
    MClass48: GridSampler | None = None
    Evosep: EvosepSampler | None = None

    def get_sampler_names(self) -> list[str]:
        """Get names of all defined samplers."""
        names = []
        if self.Vanquish:
            names.append("Vanquish")
        if self.MClass48:
            names.append("MClass48")
        if self.Evosep:
            names.append("Evosep")
        return names


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


class QueuePatternsConfig(BaseModel):
    """All queue patterns by technology.

    Structure: technology -> pattern_name -> QueuePattern
    """

    patterns: dict[str, dict[str, QueuePattern]] = Field(default_factory=dict)

    def get_technologies(self) -> list[str]:
        """Get list of all technologies with patterns defined."""
        return list(self.patterns.keys())

    def get_pattern(self, tech: str, pattern_name: str) -> QueuePattern | None:
        """Get a specific pattern by technology and name."""
        tech_patterns = self.patterns.get(tech, {})
        return tech_patterns.get(pattern_name)

    def get_patterns_for_technology(self, tech: str) -> dict[str, QueuePattern]:
        """Get all patterns for a technology."""
        return self.patterns.get(tech, {})

    def get_all_sample_refs(self, tech: str) -> set[str]:
        """Get all sample IDs referenced by patterns for a technology."""
        refs: set[str] = set()
        tech_patterns = self.patterns.get(tech, {})
        for pattern in tech_patterns.values():
            refs.update(pattern.start)
            refs.update(pattern.middle)
            refs.update(pattern.end)
            if pattern.middle_extended:
                refs.update(pattern.middle_extended)
        return refs


# =============================================================================
# QC Layout Models (TOML)
# =============================================================================


class EvosepPosition(BaseModel):
    """Evosep QC position range."""

    tray: int = Field(..., gt=0)
    position_start: int = Field(..., gt=0)
    position_end: int = Field(..., gt=0)

    @model_validator(mode="after")
    def validate_range(self) -> "EvosepPosition":
        """End must be >= start."""
        if self.position_end < self.position_start:
            raise ValueError(f"position_end ({self.position_end}) < position_start ({self.position_start})")
        return self


# QC positions: either a string (grid) or EvosepPosition dict
QCPosition = str | EvosepPosition


class QCLayoutsConfig(BaseModel):
    """All QC layouts by technology and sampler.

    Structure: technology -> sampler_key -> sample_id -> QCPosition
    sampler_key examples: "Vanquish.vial", "Vanquish.plate", "MClass48", "Evosep"
    """

    layouts: dict[str, dict[str, dict[str, QCPosition]]] = Field(default_factory=dict)

    def get_technologies(self) -> list[str]:
        """Get list of all technologies with layouts defined."""
        return list(self.layouts.keys())

    def get_layout(self, tech: str, sampler_key: str) -> dict[str, QCPosition] | None:
        """Get QC layout for technology and sampler.

        Tries exact key first (e.g., 'Vanquish.vial'), falls back to parent (e.g., 'Vanquish').
        """
        tech_layouts = self.layouts.get(tech, {})
        # Try exact match first
        if sampler_key in tech_layouts:
            return tech_layouts[sampler_key]
        # Try parent sampler (e.g., "Vanquish.vial" -> "Vanquish")
        parent = sampler_key.split(".")[0]
        return tech_layouts.get(parent)

    def get_samplers_for_technology(self, tech: str) -> list[str]:
        """Get all sampler keys for a technology."""
        return list(self.layouts.get(tech, {}).keys())


# =============================================================================
# Output Format Models (TOML)
# =============================================================================


class OutputFormat(BaseModel):
    """Output format definition."""

    description: str
    file_extension: str
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
