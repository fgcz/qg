# =============================================================================
# UI Config Models (Layer 1: Pydantic - Load from files)
# =============================================================================

import tomllib
from pathlib import Path
from typing import ClassVar, Self

import polars as pl
from pydantic import BaseModel, Field, field_validator

# =============================================================================
# InstrumentConfig - from ui/instrument_config.csv
# =============================================================================


class InstrumentConfig(BaseModel):
    """Instrument configuration with sampler and defaults."""

    tech_area: str = Field(..., description="Technology area")
    instrument: str = Field(..., description="Instrument name")
    sampler: str = Field(..., description="Sampler name")
    output_format: str = Field(..., description="Output format (xcalibur, chronos, hystar)")
    default_pattern: str = Field(..., description="Default queue pattern name")


class InstrumentConfigsConfig(BaseModel):
    """All instrument configurations."""

    config_path: ClassVar[Path] = Path("ui/instrument_config.csv")

    configs: list[InstrumentConfig]

    def get_config(
        self,
        instrument: str,
        sampler: str,
    ) -> InstrumentConfig | None:
        """Get specific config by instrument and sampler."""
        for c in self.configs:
            if c.instrument == instrument and c.sampler == sampler:
                return c
        return None

    def get_instruments(self, tech_area: str | None = None) -> list[str]:
        """Get unique instrument names, optionally filtered by tech_area."""
        if tech_area:
            return list({c.instrument for c in self.configs if c.tech_area == tech_area})
        return list({c.instrument for c in self.configs})

    def get_samplers_for_instrument(self, instrument: str) -> list[str]:
        """Get available samplers for an instrument."""
        return [c.sampler for c in self.configs if c.instrument == instrument]

    def to_table(self) -> pl.DataFrame:
        """Convert to polars DataFrame."""
        return pl.DataFrame([c.model_dump() for c in self.configs])

    @classmethod
    def from_table(cls, df: pl.DataFrame) -> Self:
        """Create InstrumentConfigsConfig from a DataFrame."""
        configs = [InstrumentConfig(**row) for row in df.to_dicts()]
        return cls(configs=configs)

    @classmethod
    def load(cls, config_dir: Path) -> Self:
        """Load instrument configs from config directory.

        Args:
            config_dir: Root config directory (e.g., qg_configs/)

        Returns:
            InstrumentConfigsConfig with all configs loaded
        """
        path = config_dir / cls.config_path
        df = pl.read_csv(path)
        return cls.from_table(df)


# =============================================================================
# TechAreaDefaults - from ui/tech_area_defaults.toml
# =============================================================================


class TechAreaDefault(BaseModel):
    """Per-tech-area UI defaults for the queue app."""

    tech_area: str = Field(..., description="Technology area")
    default_user: str = Field(
        default="",
        description="Default value for the User field; empty ⇒ use logged-in user's login",
    )
    default_polarities: list[str] = Field(
        default_factory=list,
        description="Polarities pre-checked in the polarity group",
    )
    bfabric_areas: list[str] = Field(
        default_factory=list,
        description="B-Fabric 'Area' values whose orders belong to this tech area "
        "(used to filter the order browser); empty ⇒ no filtering",
    )
    sample_name_suffixes: list[str] = Field(
        default_factory=list,
        description="Prep-type labels offered in the sample-name suffix dropdown "
        "(e.g. enriched/total/lip for Proteomics); empty ⇒ only the 'none' no-op",
    )

    @field_validator("default_user", mode="before")
    @classmethod
    def _coerce_user(cls, v: object) -> str:
        return "" if v is None else str(v)


class TechAreaDefaultsConfig(BaseModel):
    """All per-tech-area UI defaults."""

    config_path: ClassVar[Path] = Path("ui/tech_area_defaults.toml")

    defaults: list[TechAreaDefault]
    header_comments: str = ""

    def get(self, tech_area: str) -> TechAreaDefault | None:
        for d in self.defaults:
            if d.tech_area == tech_area:
                return d
        return None

    def get_default_user(self, tech_area: str) -> str:
        d = self.get(tech_area)
        return d.default_user if d else ""

    def get_default_polarities(self, tech_area: str) -> list[str]:
        d = self.get(tech_area)
        return list(d.default_polarities) if d else []

    def get_bfabric_areas(self, tech_area: str) -> list[str]:
        d = self.get(tech_area)
        return list(d.bfabric_areas) if d else []

    def get_sample_name_suffixes(self, tech_area: str) -> list[str]:
        d = self.get(tech_area)
        return list(d.sample_name_suffixes) if d else []

    def to_dict(self) -> dict:
        """Convert to dict for TOML serialization (one table per tech_area)."""
        return {
            d.tech_area: {
                "default_user": d.default_user,
                "default_polarities": list(d.default_polarities),
                "bfabric_areas": list(d.bfabric_areas),
                "sample_name_suffixes": list(d.sample_name_suffixes),
            }
            for d in self.defaults
        }

    @classmethod
    def from_dict(cls, data: dict) -> Self:
        """Create TechAreaDefaultsConfig from parsed TOML data."""
        defaults = [TechAreaDefault(tech_area=name, **entry) for name, entry in data.items()]
        return cls(defaults=defaults)

    @classmethod
    def load(cls, config_dir: Path) -> Self:
        from .loader import read_header_comments

        path = config_dir / cls.config_path
        raw_text = path.read_text()
        with open(path, "rb") as f:
            data = tomllib.load(f)
        result = cls.from_dict(data)
        result.header_comments = read_header_comments(raw_text)
        return result
