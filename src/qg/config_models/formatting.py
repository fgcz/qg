# =============================================================================
# Instrument Model
# =============================================================================


import tomllib
from pathlib import Path
from typing import ClassVar, Self

import polars as pl
from pydantic import BaseModel, Field, field_validator, model_validator


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

    config_path: ClassVar[Path] = Path("core/formatting/instruments.csv")

    instruments: list[Instrument]

    @model_validator(mode="after")
    def validate_unique_keys(self) -> Self:
        """Check that (tech_area, instrument) pairs are unique."""
        keys = [(i.tech_area, i.instrument) for i in self.instruments]
        if len(keys) != len(set(keys)):
            duplicates = [k for k in keys if keys.count(k) > 1]
            raise ValueError(f"Duplicate (tech_area, instrument) pairs: {set(duplicates)}")
        return self

    def get_instrument(self, tech: str, instrument: str) -> Instrument | None:
        """Get a specific instrument by tech_area and name."""
        for i in self.instruments:
            if i.tech_area == tech and i.instrument == instrument:
                return i
        return None

    def to_table(self) -> pl.DataFrame:
        """Convert instruments to a polars DataFrame."""
        return pl.DataFrame([i.model_dump() for i in self.instruments])

    @classmethod
    def from_table(cls, df: pl.DataFrame) -> Self:
        """Create InstrumentsConfig from a DataFrame.

        Args:
            df: DataFrame with columns: tech_area, instrument, methods_file, path_template

        Returns:
            InstrumentsConfig with all instruments loaded
        """
        instruments = [Instrument(**row) for row in df.to_dicts()]
        return cls(instruments=instruments)

    @classmethod
    def load(cls, config_dir: Path) -> Self:
        """Load instruments from config directory.

        Args:
            config_dir: Root config directory (e.g., qg_configs/)

        Returns:
            InstrumentsConfig with all instruments loaded
        """
        path = config_dir / cls.config_path
        df = pl.read_csv(path)
        return cls.from_table(df)


# =============================================================================
# Output Format Models (TOML)
# =============================================================================


class OutputFormat(BaseModel):
    """Output format definition."""

    description: str
    file_extension: str
    position_format: str  # How to format {tray, grid_position} into position string
    columns: dict[str, str]  # output_column_name -> internal_field_name


class OutputFormatsConfig(BaseModel):
    """All output format definitions.

    Structure: format_name (software) -> OutputFormat
    """

    config_path: ClassVar[Path] = Path("core/formatting/output_formats.toml")

    formats: dict[str, OutputFormat] = Field(default_factory=dict)

    def get_format_names(self) -> list[str]:
        """Get list of all defined output format names."""
        return list(self.formats.keys())

    def get_format(self, format_id: str) -> OutputFormat | None:
        """Get an output format by ID."""
        return self.formats.get(format_id)

    def to_dict(self) -> dict:
        """Convert to dict for TOML serialization."""
        return {name: fmt.model_dump() for name, fmt in self.formats.items()}

    @classmethod
    def from_dict(cls, data: dict) -> Self:
        """Create OutputFormatsConfig from parsed TOML data.

        Args:
            data: Dict from tomllib.load() with format definitions

        Returns:
            OutputFormatsConfig with all formats loaded
        """
        formats: dict[str, OutputFormat] = {}
        for format_name, format_data in data.items():
            formats[format_name] = OutputFormat(
                description=format_data["description"],
                file_extension=format_data["file_extension"],
                position_format=format_data["position_format"],
                columns=format_data.get("columns", {}),
            )
        return cls(formats=formats)

    @classmethod
    def load(cls, config_dir: Path) -> Self:
        """Load output formats from config directory.

        Args:
            config_dir: Root config directory (e.g., qg_configs/)

        Returns:
            OutputFormatsConfig with all formats loaded
        """
        path = config_dir / cls.config_path
        with open(path, "rb") as f:
            data = tomllib.load(f)
        return cls.from_dict(data)


# =============================================================================
# Sample Model
# =============================================================================

# Valid placeholders in file_name_template
VALID_PLACEHOLDERS = {
    "date",
    "run",
    "container",
    "sample_id",
    "sample_name",
    "polarity",
}


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
    def validate_default_has_sample_id(self) -> Self:
        """Default samples should have {sample_id} in template."""
        if self.sample_id == "default":
            if "{sample_id}" not in self.file_name_template:
                raise ValueError("'default' sample should have {sample_id} in file_name_template")
        return self


class SamplesConfig(BaseModel):
    """Collection of all samples."""

    config_path: ClassVar[Path] = Path("core/formatting/samples.csv")

    samples: list[Sample]

    @model_validator(mode="after")
    def validate_unique_keys(self) -> Self:
        """Check that (tech_area, sample_id) pairs are unique."""
        keys = [(s.tech_area, s.sample_id) for s in self.samples]
        if len(keys) != len(set(keys)):
            duplicates = [k for k in keys if keys.count(k) > 1]
            raise ValueError(f"Duplicate (tech_area, sample_id) pairs: {set(duplicates)}")
        return self

    @model_validator(mode="after")
    def validate_each_tech_has_default(self) -> Self:
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

    def to_table(self) -> pl.DataFrame:
        """Convert samples to a polars DataFrame."""
        return pl.DataFrame([s.model_dump() for s in self.samples])

    @classmethod
    def from_table(cls, df: pl.DataFrame) -> Self:
        """Create SamplesConfig from a DataFrame.

        Args:
            df: DataFrame with columns: tech_area, sample_id, sample_name, description, inj_vol, file_name_template

        Returns:
            SamplesConfig with all samples loaded
        """
        samples = [Sample(**row) for row in df.to_dicts()]
        return cls(samples=samples)

    @classmethod
    def load(cls, config_dir: Path) -> Self:
        """Load samples from config directory.

        Args:
            config_dir: Root config directory (e.g., qg_configs/)

        Returns:
            SamplesConfig with all samples loaded
        """
        path = config_dir / cls.config_path
        df = pl.read_csv(path)
        return cls.from_table(df)
