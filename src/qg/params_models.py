"""Pydantic models for queue parameters - new design with separate vial/plate types."""

from __future__ import annotations

import json
from pathlib import Path
from typing import TYPE_CHECKING, Literal, Self

from pydantic import AliasChoices, BaseModel, Field

if TYPE_CHECKING:
    from qg.config_models.loader import QGConfiguration


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
    grid_position: str
    plate_id: int | None  # FK to Plate
    row: str = ""
    col: int = 0


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
    queue_type: Literal["Vial", "Plate"]
    plate_layout: str
    qc_layout_name: str
    polarity: list[Literal["pos", "neg"]] = Field(default_factory=list)
    date: str  # YYYYMMDD
    user: str = ""
    # Method per polarity: {"pos": "DIA_60min", "neg": "DIA_60min"}
    method: dict[str, str] = Field(default_factory=dict)
    randomization: Literal["no", "random", "blocked"] = "no"
    inj_vol_override: float | None = None
    qc_frequency_override: int | None = None
    # If True, samples from different containers are placed on separate trays (vial mode only)
    one_container_per_tray: bool = False
    # Alpha grid position on first tray to start assigning from (e.g., "B3")
    start_position: str = "A1"
    # Tray to start assigning from (e.g., "R" for Vanquish, 2 for Evosep)
    # Empty string means use first tray of the sampler (resolved downstream)
    start_tray: str | int = ""
    # B-Fabric instance base URL (e.g. "https://fgcz-bfabric.uzh.ch/bfabric").
    # Stamped on saved params.json as queue-level provenance; per-container URIs
    # are derivable from this + ContainerBatch.container_id.
    bfabric_instance: str | None = None
    # Per-level concentration assignments for `standard`-type samples in the
    # selected QC layout (e.g. {1: "100ngml", 2: "50ngml", ...}). Empty when the
    # selected layout has no standard samples.
    # JSON keys are emitted as strings; pydantic re-coerces back to int on load.
    level_concentrations: dict[int, str] = Field(default_factory=dict)

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
        queue_type: Literal["Vial", "Plate"],
        plate_layout: str,
        qc_layout_name: str,
        polarity: list[Literal["pos", "neg"]],
        date: str,
        user: str = "",
        method: dict[str, str] | None = None,
        randomization: Literal["no", "random", "blocked"] = "no",
        inj_vol_override: float | None = None,
        qc_frequency_override: int | None = None,
        one_container_per_tray: bool = False,
        start_position: str = "A1",
        start_tray: str | int = "",
        bfabric_instance: str | None = None,
        level_concentrations: dict[int, str] | None = None,
    ) -> Self:
        """Create validated QueueParameters.

        Raises:
            KeyError: If any config reference (pattern, format, instrument, sampler, sample) is invalid.
            ValueError: If plate_layout is not valid for the sampler/queue_type combination.
        """
        from qg.config_models.structure import SamplesConfig

        # These raise KeyError with descriptive message if not found
        configs.queue_patterns.get_pattern(tech_area, queue_pattern)
        configs.output_formats.get_format(output_format)
        configs.instruments.get_instrument(tech_area, instrument)
        configs.samplers.get_sampler(sampler)
        configs.samples.get_sample(tech_area, SamplesConfig.DEFAULT_SAMPLE_ID)

        # Validate plate_layout exists for (sampler, queue_type)
        valid_layouts = configs.sampler_plate_layouts.get_plate_layouts_for_sampler(sampler, queue_type)
        if plate_layout not in valid_layouts:
            raise ValueError(f"Plate layout '{plate_layout}' not valid for {sampler}/{queue_type}")

        return cls(
            tech_area=tech_area,
            instrument=instrument,
            sampler=sampler,
            output_format=output_format,
            queue_pattern=queue_pattern,
            queue_type=queue_type,
            plate_layout=plate_layout,
            qc_layout_name=qc_layout_name,
            polarity=polarity,
            date=date,
            user=user,
            method=method or {},
            randomization=randomization,
            inj_vol_override=inj_vol_override,
            qc_frequency_override=qc_frequency_override,
            one_container_per_tray=one_container_per_tray,
            start_position=start_position,
            start_tray=start_tray,
            bfabric_instance=bfabric_instance,
            level_concentrations=level_concentrations or {},
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
