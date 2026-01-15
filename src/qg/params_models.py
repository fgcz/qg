"""Pydantic models for queue parameters (runtime input)."""

from typing import Literal

from pydantic import BaseModel, ConfigDict, Field, model_validator

from qg.config_models import requires_polarity


class InputSample(BaseModel):
    """A sample from B-Fabric input JSON."""

    model_config = ConfigDict(populate_by_name=True)

    sample_name: str = Field(..., alias="Sample Name")
    sample_id: int = Field(..., alias="Sample ID")
    tube_id: str | None = Field(default=None, alias="Tube ID")
    position: str | None = Field(default=None, alias="Position")
    grid_position: str | None = Field(default=None, alias="GridPosition")


class QueueParameters(BaseModel):
    """Queue generation parameters from input JSON."""

    container_id: int
    technology: str = Field(..., min_length=1, description="Technology identifier")
    instrument: str
    sampler: str  # e.g., "Vanquish.vial"
    output_format: str = Field(..., min_length=1, description="Output format identifier")
    queue_pattern: str  # e.g., "standard"
    polarity: list[Literal["pos", "neg"]] = Field(default_factory=list)
    date: str  # YYYYMMDD
    user: str = ""  # Username for output path (e.g., "cpanse")
    method: str = ""  # Method name for user samples (e.g., "DIA_60min")
    randomization: bool = False
    inj_vol_override: float | None = None

    @model_validator(mode="after")
    def set_default_polarity(self) -> "QueueParameters":
        """Set default polarity for technologies requiring it."""
        if not self.polarity and requires_polarity(self.technology):
            self.polarity = ["pos", "neg"]
        return self


class QueueInput(BaseModel):
    """Complete input for queue generation."""

    parameters: QueueParameters
    samples: list[InputSample]
