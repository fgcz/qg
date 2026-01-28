"""Pydantic models for sampler configuration (TOML).

9 explicit models mapping directly to TOML sections:
- 6 container configs (VanquishVial, VanquishPlate, MClass48Vial, MClass48Plate, EvosepVial, EvosepPlate)
- 3 parent configs (Vanquish, MClass48, Evosep)
- 1 root config (SamplersConfig)
"""

from typing import Literal

from pydantic import BaseModel, Field

# =============================================================================
# Vanquish Container Configs
# =============================================================================


class VanquishVialConfig(BaseModel):
    """Vanquish vial container - generates positions row-major."""

    container_type: Literal["Vial"] = "Vial"
    position_source: Literal["generated"] = "generated"
    fill_order: Literal["row_major"] = "row_major"
    grid_position_format: str  # "{row}{col}" -> "A1"
    sample_rows: list[str]  # ["A", "B", "C", "D", "E"]
    cols: list[int]  # [1, 2, ..., 9]


class VanquishPlateConfig(BaseModel):
    """Vanquish plate container - positions from input."""

    container_type: Literal["Plate"] = "Plate"
    position_source: Literal["input"] = "input"
    grid_position_format: str  # "{row}{col}" - for QC samples from CSV
    sample_rows: list[str]  # ["A", "B", "C", "D", "E", "F", "G"]
    cols: list[int]  # [1, 2, ..., 12]


# =============================================================================
# MClass48 Container Configs
# =============================================================================


class MClass48VialConfig(BaseModel):
    """MClass48 vial container - generates positions row-major."""

    container_type: Literal["Vial"] = "Vial"
    position_source: Literal["generated"] = "generated"
    fill_order: Literal["row_major"] = "row_major"
    grid_position_format: str  # "{row}{col}" -> "A1"


class MClass48PlateConfig(BaseModel):
    """MClass48 plate container - positions from input."""

    container_type: Literal["Plate"] = "Plate"
    position_source: Literal["input"] = "input"
    grid_position_format: str  # "{row}{col}" - for QC samples from CSV


# =============================================================================
# Evosep Container Configs
# =============================================================================


class EvosepVialConfig(BaseModel):
    """Evosep vial container - generates positions sequentially."""

    container_type: Literal["Vial"] = "Vial"
    position_source: Literal["generated"] = "generated"
    fill_order: Literal["sequential"] = "sequential"


class EvosepPlateConfig(BaseModel):
    """Evosep plate container - positions from input."""

    container_type: Literal["Plate"] = "Plate"
    position_source: Literal["input"] = "input"
    grid_position_conversion: str


# =============================================================================
# Parent Sampler Configs
# =============================================================================


class VanquishConfig(BaseModel):
    """Vanquish sampler configuration."""

    description: str
    plates: list[str]  # ["Y", "R", "B", "G"]
    vial: VanquishVialConfig
    plate: VanquishPlateConfig


class MClass48Config(BaseModel):
    """MClass48 sampler configuration."""

    description: str
    plates: list[str]  # ["1", "2"]
    sample_rows: list[str]  # shared by vial/plate: ["A", "B", "C", "D", "E"]
    cols: list[int]  # shared by vial/plate: [1, 2, ..., 8]
    vial: MClass48VialConfig
    plate: MClass48PlateConfig


class EvosepConfig(BaseModel):
    """Evosep sampler configuration."""

    description: str
    position_type: Literal["tray_position"] = "tray_position"
    slots: list[int] = Field(..., min_length=1)  # [1, 2, 3, 4]
    positions_per_slot: int = Field(..., gt=0)  # 96
    vial: EvosepVialConfig
    plate: EvosepPlateConfig


# =============================================================================
# Root Config
# =============================================================================


class SamplersConfig(BaseModel):
    """All sampler configurations - root of sampler.toml."""

    Vanquish: VanquishConfig
    MClass48: MClass48Config
    Evosep: EvosepConfig

    def get_sampler_names(self) -> list[str]:
        """Get names of all defined samplers."""
        return ["Vanquish", "MClass48", "Evosep"]
