"""Row schemas for sample DataFrames, independent of data source."""

from pydantic import BaseModel


class VialSampleRow(BaseModel):
    """Schema for a vial sample row. Matches QueueBuilder vial-mode input."""

    sample_name: str
    sample_id: int
    tube_id: str | None = None
    container_id: int
    grouping_var: str | None = None


class PlateSampleRow(BaseModel):
    """Schema for a plate sample row. Matches QueueBuilder plate-mode input."""

    sample_name: str
    sample_id: int
    container_id: int
    grid_position: str
    plate_id: int
    tray: str | int | None = None
    grouping_var: str | None = None
