"""Pydantic models for sampler configuration (TOML)."""

from typing import Literal

from pydantic import BaseModel, Field, model_validator


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

    def get_sampler_config(self, sampler_name: str) -> dict:
        """Get merged sampler config dict for use with position generators.

        Args:
            sampler_name: Sampler name like "Vanquish.vial" or "MClass48.plate"

        Returns:
            Merged dict with base sampler + container config
        """
        parts = sampler_name.split(".")
        sampler_base = parts[0]
        container = parts[1] if len(parts) > 1 else "vial"

        sampler = getattr(self, sampler_base, None)
        if sampler is None:
            return {}

        # Get base config (non-container fields)
        base_dict = sampler.model_dump(exclude={"vial", "plate"}, exclude_none=True)

        # Get container config
        container_obj = getattr(sampler, container, None)
        if container_obj:
            container_dict = container_obj.model_dump(exclude_none=True)
            base_dict.update(container_dict)

        return base_dict
