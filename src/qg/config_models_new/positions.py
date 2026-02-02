# =============================================================================
# Position Config Models (Layer 1: Pydantic - Load from files)
# =============================================================================
#
# Minimal validation, just load data from config files.
# Domain logic is in positions_new.py (Layer 2).

import tomllib
from pathlib import Path
from typing import ClassVar, Self

import polars as pl
from pydantic import BaseModel, Field

# =============================================================================
# PlateLayout - from plate_layouts.toml
# =============================================================================


class PlateLayout(BaseModel):
    """A plate layout definition (grid geometry)."""

    name: str = Field(..., description="Layout name (e.g., Vanquish_54)")
    rows: list[str] | list[int] = Field(..., description="Row identifiers")
    cols: list[int] = Field(..., description="Column identifiers")

    @property
    def capacity(self) -> int:
        """Total positions in this layout."""
        return len(self.rows) * len(self.cols)


class PlateLayoutsConfig(BaseModel):
    """All plate layouts."""

    config_path: ClassVar[Path] = Path("core/position/plate_layouts.toml")

    layouts: dict[str, PlateLayout] = Field(default_factory=dict)

    def get_layout(self, name: str) -> PlateLayout | None:
        """Get a specific plate layout by name."""
        return self.layouts.get(name)

    def get_layout_names(self) -> list[str]:
        """Get all layout names."""
        return list(self.layouts.keys())

    def to_dict(self) -> dict:
        """Convert to dict for TOML serialization."""
        return {name: {"rows": layout.rows, "cols": layout.cols} for name, layout in self.layouts.items()}

    @classmethod
    def from_dict(cls, data: dict) -> Self:
        """Create PlateLayoutsConfig from parsed TOML data.

        Args:
            data: Dict from tomllib.load() with layout definitions
        """
        layouts = {name: PlateLayout(name=name, **layout_data) for name, layout_data in data.items()}
        return cls(layouts=layouts)

    @classmethod
    def load(cls, config_dir: Path) -> Self:
        """Load plate layouts from config directory.

        Args:
            config_dir: Root config directory (e.g., qg_configs_new/)

        Returns:
            PlateLayoutsConfig with all layouts loaded
        """
        path = config_dir / cls.config_path
        with open(path, "rb") as f:
            data = tomllib.load(f)
        return cls.from_dict(data)


# =============================================================================
# Sampler - from sampler.toml
# =============================================================================


class Sampler(BaseModel):
    """A sampler definition."""

    name: str = Field(..., description="Sampler name (e.g., Vanquish)")
    description: str = Field(default="", description="Human-readable description")
    trays: list[str] | list[int] = Field(..., description="Available tray identifiers")
    position_fun: str = Field(..., description="Position function name (string_concat or int_add)")


class SamplersConfig(BaseModel):
    """All sampler definitions."""

    config_path: ClassVar[Path] = Path("core/position/sampler.toml")

    samplers: dict[str, Sampler] = Field(default_factory=dict)

    def get_sampler(self, name: str) -> Sampler | None:
        """Get a specific sampler by name."""
        return self.samplers.get(name)

    def get_sampler_names(self) -> list[str]:
        """Get all sampler names."""
        return list(self.samplers.keys())

    def to_dict(self) -> dict:
        """Convert to dict for TOML serialization."""
        return {
            name: {"description": s.description, "trays": s.trays, "position_fun": s.position_fun}
            for name, s in self.samplers.items()
        }

    @classmethod
    def from_dict(cls, data: dict) -> Self:
        """Create SamplersConfig from parsed TOML data.

        Args:
            data: Dict from tomllib.load() with sampler definitions
        """
        samplers = {name: Sampler(name=name, **sampler_data) for name, sampler_data in data.items()}
        return cls(samplers=samplers)

    @classmethod
    def load(cls, config_dir: Path) -> Self:
        """Load samplers from config directory.

        Args:
            config_dir: Root config directory (e.g., qg_configs_new/)

        Returns:
            SamplersConfig with all samplers loaded
        """
        path = config_dir / cls.config_path
        with open(path, "rb") as f:
            data = tomllib.load(f)
        return cls.from_dict(data)


# =============================================================================
# SamplerPlateLayout - from sampler_plate_layouts.csv
# =============================================================================


class SamplerPlateLayout(BaseModel):
    """A sampler to plate_layout mapping with queue_type."""

    sampler: str = Field(..., description="Sampler name")
    plate_layout: str = Field(..., description="Plate layout name")
    queue_type: str = Field(..., description="Queue type (Vial or Plate)")


class SamplerPlateLayoutsConfig(BaseModel):
    """All sampler to plate_layout mappings."""

    config_path: ClassVar[Path] = Path("core/position/sampler_plate_layouts.csv")

    mappings: list[SamplerPlateLayout]

    def get_plate_layouts_for_sampler(self, sampler: str, queue_type: str | None = None) -> list[str]:
        """Get plate layouts available for a sampler, optionally filtered by queue_type."""
        layouts = []
        for m in self.mappings:
            if m.sampler == sampler:
                if queue_type is None or m.queue_type == queue_type:
                    layouts.append(m.plate_layout)
        return layouts

    def get_queue_types_for_sampler(self, sampler: str) -> list[str]:
        """Get available queue types for a sampler."""
        return list({m.queue_type for m in self.mappings if m.sampler == sampler})

    def to_table(self) -> pl.DataFrame:
        """Convert to polars DataFrame."""
        return pl.DataFrame([m.model_dump() for m in self.mappings])

    @classmethod
    def from_table(cls, df: pl.DataFrame) -> Self:
        """Create SamplerPlateLayoutsConfig from a DataFrame."""
        mappings = [SamplerPlateLayout(**row) for row in df.to_dicts()]
        return cls(mappings=mappings)

    @classmethod
    def load(cls, config_dir: Path) -> Self:
        """Load sampler plate layouts from config directory.

        Args:
            config_dir: Root config directory (e.g., qg_configs_new/)

        Returns:
            SamplerPlateLayoutsConfig with all mappings loaded
        """
        path = config_dir / cls.config_path
        df = pl.read_csv(path)
        return cls.from_table(df)


# =============================================================================
# QCSampleGrid - from qc_layouts_grid.csv
# =============================================================================


class QCSampleGrid(BaseModel):
    """A QC sample position for grid samplers (fixed well positions)."""

    tech_area: str = Field(..., description="Technology area")
    qc_layout_name: str = Field(..., description="QC layout name")
    plate_layout: str = Field(..., description="Plate layout name")
    sample_id: str = Field(..., description="QC sample identifier")
    plate: str = Field(..., description="Plate/tray identifier")
    row: str = Field(..., description="Row identifier")
    col: int = Field(..., description="Column identifier")


class QCLayoutsGridConfig(BaseModel):
    """All QC sample positions for grid samplers."""

    config_path: ClassVar[Path] = Path("core/position/qc_layouts_grid.csv")

    samples: list[QCSampleGrid]

    def get_samples(
        self,
        tech_area: str,
        qc_layout_name: str,
        plate_layout: str,
    ) -> list[QCSampleGrid]:
        """Get QC samples for a specific (tech_area, qc_layout_name, plate_layout)."""
        return [
            s
            for s in self.samples
            if s.tech_area == tech_area and s.qc_layout_name == qc_layout_name and s.plate_layout == plate_layout
        ]

    def to_table(self) -> pl.DataFrame:
        """Convert to polars DataFrame."""
        return pl.DataFrame([s.model_dump() for s in self.samples])

    @classmethod
    def from_table(cls, df: pl.DataFrame) -> Self:
        """Create QCLayoutsGridConfig from a DataFrame."""
        samples = [QCSampleGrid(**row) for row in df.to_dicts()]
        return cls(samples=samples)

    @classmethod
    def load(cls, config_dir: Path) -> Self:
        """Load QC layouts for grid samplers from config directory.

        Args:
            config_dir: Root config directory (e.g., qg_configs_new/)

        Returns:
            QCLayoutsGridConfig with all samples loaded
        """
        path = config_dir / cls.config_path
        df = pl.read_csv(path, comment_prefix="#")
        return cls.from_table(df)


# =============================================================================
# QCSampleEvosep - from qc_layouts_evosep.csv
# =============================================================================


class QCSampleEvosep(BaseModel):
    """A QC sample position range for Evosep (consumable tips)."""

    tech_area: str = Field(..., description="Technology area")
    qc_layout_name: str = Field(..., description="QC layout name")
    plate_layout: str = Field(..., description="Plate layout name")
    sample_id: str = Field(..., description="QC sample identifier")
    tray: int = Field(..., description="Tray number")
    position_start: int = Field(..., ge=1, description="Start position (inclusive)")
    position_end: int = Field(..., ge=1, description="End position (inclusive)")


class QCLayoutsEvosepConfig(BaseModel):
    """All QC sample position ranges for Evosep."""

    config_path: ClassVar[Path] = Path("core/position/qc_layouts_evosep.csv")

    samples: list[QCSampleEvosep]

    def get_samples(
        self,
        tech_area: str,
        qc_layout_name: str,
        plate_layout: str,
    ) -> list[QCSampleEvosep]:
        """Get QC samples for a specific (tech_area, qc_layout_name, plate_layout)."""
        return [
            s
            for s in self.samples
            if s.tech_area == tech_area and s.qc_layout_name == qc_layout_name and s.plate_layout == plate_layout
        ]

    def to_table(self) -> pl.DataFrame:
        """Convert to polars DataFrame."""
        return pl.DataFrame([s.model_dump() for s in self.samples])

    @classmethod
    def from_table(cls, df: pl.DataFrame) -> Self:
        """Create QCLayoutsEvosepConfig from a DataFrame."""
        samples = [QCSampleEvosep(**row) for row in df.to_dicts()]
        return cls(samples=samples)

    @classmethod
    def load(cls, config_dir: Path) -> Self:
        """Load QC layouts for Evosep from config directory.

        Args:
            config_dir: Root config directory (e.g., qg_configs_new/)

        Returns:
            QCLayoutsEvosepConfig with all samples loaded
        """
        path = config_dir / cls.config_path
        df = pl.read_csv(path, comment_prefix="#")
        return cls.from_table(df)
