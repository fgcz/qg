"""Pydantic models for queue parameters - new design with separate vial/plate types."""

from __future__ import annotations

import json
from pathlib import Path
from typing import TYPE_CHECKING, Literal, Self

from pydantic import AliasChoices, BaseModel, Field

if TYPE_CHECKING:
    from qg.config_models_new.loader import QGConfiguration


class VialSample(BaseModel):
    """A sample from a vial."""

    sample_name: str
    sample_id: int
    grouping_var: str | None = None
    tube_id: str | None = None
    container_id: int  # FK to ContainerBatch


class PlateCell(BaseModel):
    """A sample placed in a plate well."""

    sample: VialSample
    position: int
    grid_position: str | int
    plate_id: int | None  # FK to Plate


class ContainerBatch(BaseModel):
    """A group of samples from a single project/container."""

    container_id: int
    container_name: str | None = None


class Plate(BaseModel):
    """Plate metadata."""

    plate_id: int
    tray: str | int | None = None
    nr_samples: int


class VialQueue(BaseModel):
    """Queue structure for vial-based samples."""

    batches: dict[int, ContainerBatch] = Field(default_factory=dict)
    samples: list[VialSample] = Field(
        default_factory=list,
        validation_alias=AliasChoices("samples", "cells"),
    )


class PlateQueue(BaseModel):
    """Queue structure for plate-based samples."""

    batches: dict[int, ContainerBatch] = Field(default_factory=dict)
    plates: dict[int, Plate] = Field(default_factory=dict)
    cells: list[PlateCell] = Field(default_factory=list)


class QueueParameters(BaseModel):
    """Queue generation parameters."""

    tech_area: str
    instrument: str
    sampler: str
    output_format: str
    queue_pattern: str
    polarity: list[Literal["pos", "neg"]] = Field(default_factory=list)
    date: str  # YYYYMMDD
    user: str = ""
    # Method per polarity: {"pos": "DIA_60min", "neg": "DIA_60min"}
    method: dict[str, str] = Field(default_factory=dict)
    randomization: Literal["no", "random", "blocked"] = "no"
    inj_vol_override: float | None = None
    qc_frequency_override: int | None = None

    @classmethod
    def create(
        cls,
        configs: QGConfiguration,
        *,
        tech_area: str,
        instrument: str,
        sampler: str,
        output_format: str,
        queue_pattern: str,
        polarity: list[Literal["pos", "neg"]],
        date: str,
        user: str = "",
        method: dict[str, str] | None = None,
        randomization: Literal["no", "random", "blocked"] = "no",
        inj_vol_override: float | None = None,
        qc_frequency_override: int | None = None,
    ) -> Self:
        """Create validated QueueParameters."""
        if not configs.queue_patterns.get_pattern(tech_area, queue_pattern):
            raise ValueError(f"Pattern '{queue_pattern}' not found for {tech_area}")
        if not configs.output_formats.get_format(output_format):
            raise ValueError(f"Output format '{output_format}' not found")
        if not configs.instruments.get_instrument(tech_area, instrument):
            raise ValueError(f"Instrument '{instrument}' not found for {tech_area}")
        if not configs.samples.get_sample(tech_area, "default"):
            raise ValueError(f"No 'default' sample definition for {tech_area}")

        return cls(
            tech_area=tech_area,
            instrument=instrument,
            sampler=sampler,
            output_format=output_format,
            queue_pattern=queue_pattern,
            polarity=polarity,
            date=date,
            user=user,
            method=method or {},
            randomization=randomization,
            inj_vol_override=inj_vol_override,
            qc_frequency_override=qc_frequency_override,
        )


class VialQueueInput(BaseModel):
    """Input for vial-based queue generation."""

    parameters: QueueParameters
    queue: VialQueue


class PlateQueueInput(BaseModel):
    """Input for plate-based queue generation."""

    parameters: QueueParameters
    queue: PlateQueue


QueueInput = VialQueueInput | PlateQueueInput


def write_queue_input(queue_input: QueueInput, output_path: str | Path) -> Path:
    """Write queue input to JSON file."""
    output_path = Path(output_path)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text(queue_input.model_dump_json(indent=2))
    return output_path


def read_queue_input(input_path: str | Path) -> QueueInput:
    """Read queue input from JSON file."""
    input_path = Path(input_path)
    data = json.loads(input_path.read_text())
    if "plates" in data.get("queue", {}):
        return PlateQueueInput.model_validate(data)
    return VialQueueInput.model_validate(data)
