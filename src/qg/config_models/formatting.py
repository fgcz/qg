# =============================================================================
# Instrument Model
# =============================================================================


import tomllib
from pathlib import Path
from typing import ClassVar, Self

import polars as pl
from pydantic import BaseModel, Field, field_validator, model_validator

from qg.utils import GridPositionConversion


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

    def get_instrument(self, tech: str, instrument: str) -> Instrument:
        """Get a specific instrument by tech_area and name."""
        for i in self.instruments:
            if i.tech_area == tech and i.instrument == instrument:
                return i
        raise KeyError(f"Instrument '{instrument}' not found for tech_area '{tech}'")

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
    writer: str = "csv"  # Writer function name (csv, xcalibur_csv, hystar_xml)
    position_format: str  # How to format {tray, grid_position} into position string
    grid_position_format: str = "{row}{col}"  # How to format {row, col, grid_position} for display
    grid_position_conversion: GridPositionConversion = GridPositionConversion.IDENTITY
    tray_format: str | None = None  # Format string for tray values, e.g. "EvoSlot {tray}"
    columns: dict[str, str]  # output_column_name -> internal_field_name or "literal:VALUE"
    # Per-technology overrides (merge into `columns` at write time). Defined in TOML as
    # `[<format>.columns.<TechArea>]` sub-tables.
    columns_by_tech: dict[str, dict[str, str]] = Field(default_factory=dict)

    def columns_for(self, tech_area: str | None) -> dict[str, str]:
        """Resolve the column map for a given tech_area by merging the base over its override."""
        if tech_area and tech_area in self.columns_by_tech:
            return {**self.columns, **self.columns_by_tech[tech_area]}
        return self.columns


class OutputFormatsConfig(BaseModel):
    """All output format definitions.

    Structure: format_name (software) -> OutputFormat
    """

    config_path: ClassVar[Path] = Path("core/formatting/output_formats.toml")

    formats: dict[str, OutputFormat] = Field(default_factory=dict)
    header_comments: str = ""

    def get_format_names(self) -> list[str]:
        """Get list of all defined output format names."""
        return list(self.formats.keys())

    def get_format(self, format_id: str) -> OutputFormat:
        """Get an output format by ID."""
        if format_id not in self.formats:
            raise KeyError(f"Output format '{format_id}' not found. Available: {list(self.formats.keys())}")
        return self.formats[format_id]

    def to_dict(self) -> dict:
        """Convert to dict for TOML serialization."""
        return {name: fmt.model_dump(exclude_none=True) for name, fmt in self.formats.items()}

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
            # `[<format>.columns]` is a mix of string entries (base columns) and
            # nested `[<format>.columns.<TechArea>]` sub-tables (per-tech overrides).
            raw_columns = format_data.get("columns", {})
            base_columns = {k: v for k, v in raw_columns.items() if isinstance(v, str)}
            columns_by_tech = {k: v for k, v in raw_columns.items() if isinstance(v, dict)}
            formats[format_name] = OutputFormat(
                description=format_data["description"],
                file_extension=format_data["file_extension"],
                writer=format_data.get("writer", "csv"),
                position_format=format_data["position_format"],
                grid_position_format=format_data.get("grid_position_format", "{row}{col}"),
                grid_position_conversion=format_data.get("grid_position_conversion", "identity"),
                tray_format=format_data.get("tray_format"),
                columns=base_columns,
                columns_by_tech=columns_by_tech,
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
        from .loader import read_header_comments

        path = config_dir / cls.config_path
        raw_text = path.read_text()
        with open(path, "rb") as f:
            data = tomllib.load(f)
        result = cls.from_dict(data)
        result.header_comments = read_header_comments(raw_text)
        return result
