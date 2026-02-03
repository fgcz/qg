# =============================================================================
# UI Config Models (Layer 1: Pydantic - Load from files)
# =============================================================================

from pathlib import Path
from typing import ClassVar, Self

import polars as pl
from pydantic import BaseModel, Field

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

    def get_by_instrument(self, instrument: str) -> list[InstrumentConfig]:
        """Get all configs for an instrument (may have multiple samplers)."""
        return [c for c in self.configs if c.instrument == instrument]

    def get_by_tech_area(self, tech_area: str) -> list[InstrumentConfig]:
        """Get all configs for a technology area."""
        return [c for c in self.configs if c.tech_area == tech_area]

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
