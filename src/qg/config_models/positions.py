# =============================================================================
# Position Config Models (Layer 1: Pydantic - Load from files)
# =============================================================================
#
# Minimal validation, just load data from config files.
# Domain logic is in positions.py (Layer 2).

import tomllib
from pathlib import Path
from typing import ClassVar, Literal, Protocol, Self

import polars as pl
from pydantic import BaseModel, Field

from qg.utils import GridPositionConversion, PositionFunction

# =============================================================================
# PlateLayout - from plate_layouts.toml
# =============================================================================


class PlateLayout(BaseModel):
    """A plate layout definition (grid geometry)."""

    name: str = Field(..., description="Layout name (e.g., Vanquish_54)")
    rows: list[str] = Field(..., description="Row identifiers (alpha: A, B, ...)")
    cols: list[int] = Field(..., description="Column identifiers")

    @property
    def capacity(self) -> int:
        """Total positions in this layout."""
        return len(self.rows) * len(self.cols)

    def alpha_to_flat(self, pos: str) -> int:
        """Convert alpha position to flat 1-based index (row-major).

        A1 → 1, A12 → 12, B1 → 13, H12 → 96.
        """
        row_letter = pos[0].upper()
        col = int(pos[1:])
        row_idx = ord(row_letter) - ord("A")
        return row_idx * len(self.cols) + col

    def flat_to_row_col(self, n: int) -> tuple[str, int]:
        """Convert flat 1-based index to (row_letter, col) tuple (row-major).

        1 → ("A", 1), 13 → ("B", 1), 96 → ("H", 12).
        """
        row_idx = (n - 1) // len(self.cols)
        col = (n - 1) % len(self.cols) + 1
        return (chr(ord("A") + row_idx), col)

    def split_alpha(self, pos: str) -> tuple[str, int]:
        """Parse alpha grid position into (row, col) components.

        'D8' → ('D', 8), 'A1' → ('A', 1).
        """
        return (pos[0].upper(), int(pos[1:]))


class PlateLayoutsConfig(BaseModel):
    """All plate layouts."""

    config_path: ClassVar[Path] = Path("core/position/plate_layouts.toml")

    layouts: dict[str, PlateLayout] = Field(default_factory=dict)
    header_comments: str = ""

    def get_layout(self, name: str) -> PlateLayout:
        """Get a specific plate layout by name."""
        if name not in self.layouts:
            raise KeyError(f"Plate layout '{name}' not found. Available: {list(self.layouts.keys())}")
        return self.layouts[name]

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
            config_dir: Root config directory (e.g., qg_configs/)

        Returns:
            PlateLayoutsConfig with all layouts loaded
        """
        from .loader import read_header_comments

        path = config_dir / cls.config_path
        raw_text = path.read_text()
        with open(path, "rb") as f:
            data = tomllib.load(f)
        result = cls.from_dict(data)
        result.header_comments = read_header_comments(raw_text)
        return result


# =============================================================================
# Sampler - from sampler.toml
# =============================================================================


class Sampler(BaseModel):
    """A sampler definition."""

    name: str = Field(..., description="Sampler name (e.g., Vanquish)")
    description: str = Field(default="", description="Human-readable description")
    sampler_type: Literal["well", "tip"] = Field(
        ..., description="Sampler type: 'well' (reusable vials) or 'tip' (consumable tips)"
    )
    trays: list[str] | list[int] = Field(..., description="Available tray identifiers")
    position_fun: PositionFunction = Field(..., description="Position function name")

    @property
    def is_tip(self) -> bool:
        """True if this sampler uses consumable tips (e.g., Evosep)."""
        return self.sampler_type == "tip"


class SamplersConfig(BaseModel):
    """All sampler definitions."""

    config_path: ClassVar[Path] = Path("core/position/sampler.toml")

    samplers: dict[str, Sampler] = Field(default_factory=dict)
    header_comments: str = ""

    def get_sampler(self, name: str) -> Sampler:
        """Get a specific sampler by name."""
        if name not in self.samplers:
            raise KeyError(f"Sampler '{name}' not found. Available: {list(self.samplers.keys())}")
        return self.samplers[name]

    def get_sampler_names(self) -> list[str]:
        """Get all sampler names."""
        return list(self.samplers.keys())

    def to_dict(self) -> dict:
        """Convert to dict for TOML serialization."""
        return {
            name: {
                "description": s.description,
                "sampler_type": s.sampler_type,
                "trays": s.trays,
                "position_fun": s.position_fun,
            }
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
            config_dir: Root config directory (e.g., qg_configs/)

        Returns:
            SamplersConfig with all samplers loaded
        """
        from .loader import read_header_comments

        path = config_dir / cls.config_path
        raw_text = path.read_text()
        with open(path, "rb") as f:
            data = tomllib.load(f)
        result = cls.from_dict(data)
        result.header_comments = read_header_comments(raw_text)
        return result


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
            config_dir: Root config directory (e.g., qg_configs/)

        Returns:
            SamplerPlateLayoutsConfig with all mappings loaded
        """
        path = config_dir / cls.config_path
        df = pl.read_csv(path)
        return cls.from_table(df)


# =============================================================================
# QCSampleWell - from qc_layouts_well.csv
# =============================================================================


class QCSampleWell(BaseModel):
    """A QC sample position for well-plate samplers (fixed well positions)."""

    tech_area: str = Field(..., description="Technology area")
    qc_layout_name: str = Field(..., description="QC layout name")
    plate_layout: str = Field(..., description="Plate layout name")
    sample_id: str = Field(..., description="QC sample identifier")
    tray: str = Field(..., description="Tray identifier")
    row: str = Field(..., description="Row identifier")
    col: int = Field(..., description="Column identifier")


class QCLayoutsWellConfig(BaseModel):
    """All QC sample positions for well-plate samplers."""

    config_path: ClassVar[Path] = Path("core/position/qc_layouts_well.csv")

    samples: list[QCSampleWell]

    def get_samples(
        self,
        tech_area: str,
        qc_layout_name: str,
        plate_layout: str,
    ) -> list[QCSampleWell]:
        """Get QC samples for a specific (tech_area, qc_layout_name, plate_layout)."""
        return [
            s
            for s in self.samples
            if s.tech_area == tech_area and s.qc_layout_name == qc_layout_name and s.plate_layout == plate_layout
        ]

    def get_layout_names(self, tech_area: str, plate_layout: str) -> list[str]:
        """Get distinct qc_layout_name values for (tech_area, plate_layout)."""
        return sorted(
            {s.qc_layout_name for s in self.samples if s.tech_area == tech_area and s.plate_layout == plate_layout}
        )

    def get_sample_ids(self, tech_area: str, qc_layout_name: str, plate_layout: str) -> set[str]:
        """Get sample_ids available in a specific layout."""
        return {s.sample_id for s in self.get_samples(tech_area, qc_layout_name, plate_layout)}

    def to_table(self) -> pl.DataFrame:
        """Convert to polars DataFrame."""
        return pl.DataFrame([s.model_dump() for s in self.samples])

    @classmethod
    def from_table(cls, df: pl.DataFrame) -> Self:
        """Create QCLayoutsWellConfig from a DataFrame."""
        samples = [QCSampleWell(**row) for row in df.to_dicts()]
        return cls(samples=samples)

    @classmethod
    def load(cls, config_dir: Path) -> Self:
        """Load QC layouts for well-plate samplers from config directory.

        Args:
            config_dir: Root config directory (e.g., qg_configs/)

        Returns:
            QCLayoutsWellConfig with all samples loaded
        """
        path = config_dir / cls.config_path
        df = pl.read_csv(path, comment_prefix="#")
        return cls.from_table(df)


# =============================================================================
# QCSampleTip - from qc_layouts_tip.csv
# =============================================================================


class QCSampleTip(BaseModel):
    """A QC sample position range for tip-plate samplers (consumable tips)."""

    tech_area: str = Field(..., description="Technology area")
    qc_layout_name: str = Field(..., description="QC layout name")
    plate_layout: str = Field(..., description="Plate layout name")
    sample_id: str = Field(..., description="QC sample identifier")
    tray: int = Field(..., description="Tray number")
    position_start: str = Field(..., description="Start position in alpha format (e.g., A1)")
    position_end: str = Field(..., description="End position in alpha format (e.g., D12)")


class QCLayoutsTipConfig(BaseModel):
    """All QC sample position ranges for tip-plate samplers."""

    config_path: ClassVar[Path] = Path("core/position/qc_layouts_tip.csv")

    samples: list[QCSampleTip]

    def get_samples(
        self,
        tech_area: str,
        qc_layout_name: str,
        plate_layout: str,
    ) -> list[QCSampleTip]:
        """Get QC samples for a specific (tech_area, qc_layout_name, plate_layout)."""
        return [
            s
            for s in self.samples
            if s.tech_area == tech_area and s.qc_layout_name == qc_layout_name and s.plate_layout == plate_layout
        ]

    def get_layout_names(self, tech_area: str, plate_layout: str) -> list[str]:
        """Get distinct qc_layout_name values for (tech_area, plate_layout)."""
        return sorted(
            {s.qc_layout_name for s in self.samples if s.tech_area == tech_area and s.plate_layout == plate_layout}
        )

    def get_sample_ids(self, tech_area: str, qc_layout_name: str, plate_layout: str) -> set[str]:
        """Get sample_ids available in a specific layout."""
        return {s.sample_id for s in self.get_samples(tech_area, qc_layout_name, plate_layout)}

    def to_table(self) -> pl.DataFrame:
        """Convert to polars DataFrame."""
        return pl.DataFrame([s.model_dump() for s in self.samples])

    @classmethod
    def from_table(cls, df: pl.DataFrame) -> Self:
        """Create QCLayoutsTipConfig from a DataFrame."""
        samples = [QCSampleTip(**row) for row in df.to_dicts()]
        return cls(samples=samples)

    @classmethod
    def load(cls, config_dir: Path) -> Self:
        """Load QC layouts for tip-plate samplers from config directory.

        Args:
            config_dir: Root config directory (e.g., qg_configs/)

        Returns:
            QCLayoutsTipConfig with all samples loaded
        """
        path = config_dir / cls.config_path
        df = pl.read_csv(path, comment_prefix="#")
        return cls.from_table(df)


# =============================================================================
# GridPositionConverter - dispatch grid_position conversion by enum
# =============================================================================


class GridPositionConverter(Protocol):
    def convert_grid_position(self, pos: str) -> str: ...


class _IdentityConverter:
    def convert_grid_position(self, pos: str) -> str:
        return pos


class _AlphaToFlatConverter:
    def __init__(self, plate_layout: PlateLayout) -> None:
        self._plate_layout = plate_layout

    def convert_grid_position(self, pos: str) -> str:
        return str(self._plate_layout.alpha_to_flat(pos))


def get_grid_position_converter(conversion: GridPositionConversion, plate_layout: PlateLayout) -> GridPositionConverter:
    match conversion:
        case GridPositionConversion.IDENTITY:
            return _IdentityConverter()
        case GridPositionConversion.ALPHA_TO_FLAT:
            return _AlphaToFlatConverter(plate_layout)
