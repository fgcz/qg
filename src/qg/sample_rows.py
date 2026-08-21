"""Typed row and table schemas for samples, independent of data source."""

from __future__ import annotations

from collections.abc import Iterable
from dataclasses import dataclass

import polars as pl
from pydantic import BaseModel


class VialSampleRow(BaseModel):
    """Schema for a vial sample row. Matches QueueBuilder vial-mode input."""

    sample_name: str
    sample_id: int
    tube_id: str | None = None
    container_id: int
    grouping_var: str | None = None
    sample_type: str | None = None


class PlateSampleRow(BaseModel):
    """Schema for a plate sample row. Matches QueueBuilder plate-mode input."""

    sample_name: str
    sample_id: int
    container_id: int
    grid_position: str
    plate_id: int
    tray: str | int | None = None
    grouping_var: str | None = None
    sample_type: str | None = None


_VIAL_SCHEMA: dict[str, type[pl.DataType]] = {
    "sample_name": pl.String,
    "sample_id": pl.Int64,
    "tube_id": pl.String,
    "container_id": pl.Int64,
    "grouping_var": pl.String,
    "sample_type": pl.String,
}

_PLATE_SCHEMA: dict[str, type[pl.DataType]] = {
    "sample_name": pl.String,
    "sample_id": pl.Int64,
    "container_id": pl.Int64,
    "grid_position": pl.String,
    "plate_id": pl.Int64,
    "tray": pl.String,
    "grouping_var": pl.String,
    "sample_type": pl.String,
}


@dataclass(frozen=True, slots=True)
class VialSampleTable:
    """Vial-mode sample table with a stable schema."""

    table: pl.DataFrame

    @classmethod
    def from_rows(cls, rows: Iterable[VialSampleRow]) -> VialSampleTable:
        """Build a table whose optional all-null fields retain their real types."""
        records = [row.model_dump() for row in rows]
        return cls(pl.DataFrame(records, schema=_VIAL_SCHEMA))

    @classmethod
    def concat(cls, tables: Iterable[VialSampleTable]) -> VialSampleTable:
        """Concatenate vial tables and retain the declared schema."""
        frames = [item.table for item in tables if not item.table.is_empty()]
        if not frames:
            return cls.from_rows([])
        table = pl.concat(frames, how="vertical").sort(["container_id", "sample_id"])
        return cls(table)


@dataclass(frozen=True, slots=True)
class PlateSampleTable:
    """Plate-mode sample table with a stable schema."""

    table: pl.DataFrame

    @classmethod
    def from_rows(cls, rows: Iterable[PlateSampleRow]) -> PlateSampleTable:
        """Build a table whose optional all-null fields retain their real types."""
        records = [row.model_dump() for row in rows]
        return cls(pl.DataFrame(records, schema=_PLATE_SCHEMA))

    @classmethod
    def concat(cls, tables: Iterable[PlateSampleTable]) -> PlateSampleTable:
        """Concatenate plate tables and retain the declared schema."""
        frames = [item.table for item in tables if not item.table.is_empty()]
        if not frames:
            return cls.from_rows([])
        table = pl.concat(frames, how="vertical").sort(["container_id", "sample_id"])
        return cls(table)
