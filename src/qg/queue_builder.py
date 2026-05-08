"""Builder for QueueInput (vial or plate)."""

from __future__ import annotations

from typing import TYPE_CHECKING, Self

import polars as pl

from qg.params_models import (
    ContainerBatch,
    Plate,
    PlateCell,
    PlateQueue,
    PlateQueueInput,
    QueueInput,
    QueueParameters,
    VialQueue,
    VialQueueInput,
    VialSample,
)
from qg.utils import LayoutMode

if TYPE_CHECKING:
    from qg.config_models.loader import QGConfiguration


class QueueBuilder:
    """Fluent builder for QueueInput. Single-use."""

    def __init__(self, config: QGConfiguration) -> None:
        self.config = config
        self._parameters: QueueParameters | None = None
        self._layout_mode: LayoutMode | None = None
        self._batches: dict[int, ContainerBatch] = {}
        self._plates: dict[int, Plate] = {}
        self._vial_samples: list[VialSample] = []
        self._plate_cells: list[PlateCell] = []
        self._bfabric_base_url: str | None = None
        self._built = False

    def with_parameters(self, parameters: QueueParameters) -> Self:
        """Set queue parameters. Layout mode derived from parameters.queue_type."""
        if self._built:
            raise RuntimeError("Builder already used.")
        self._parameters = parameters
        self._layout_mode = LayoutMode.PLATE if parameters.queue_type == "Plate" else LayoutMode.VIAL
        return self

    def with_bfabric_instance(self, base_url: str) -> Self:
        """Provide the B-Fabric base URL; stamped on QueueParameters.bfabric_instance at build time."""
        if self._built:
            raise RuntimeError("Builder already used.")
        self._bfabric_base_url = base_url
        return self

    def add_samples_from_dataframe(self, df: pl.DataFrame) -> Self:
        """Add samples from DataFrame."""
        if self._built:
            raise RuntimeError("Builder already used.")
        if self._layout_mode is None:
            raise RuntimeError("Call with_parameters() first.")
        if "container_id" not in df.columns:
            raise ValueError("DataFrame must have 'container_id' column")

        if self._layout_mode == LayoutMode.PLATE:
            self._add_plate_samples(df)
        else:
            self._add_vial_samples(df)
        return self

    def _add_vial_samples(self, df: pl.DataFrame) -> None:
        for row in df.to_dicts():
            container_id = row.pop("container_id")
            if container_id not in self._batches:
                self._batches[container_id] = ContainerBatch(container_id=container_id)
            self._vial_samples.append(VialSample(container_id=container_id, **row))

    def _add_plate_samples(self, df: pl.DataFrame) -> None:
        required = {"plate_id", "grid_position", "tray"}
        missing = required - set(df.columns)
        if missing:
            raise ValueError(f"Plate mode requires columns: {missing}")

        for row in df.to_dicts():
            container_id = row.pop("container_id")
            plate_id = row.pop("plate_id")
            grid_position = row.pop("grid_position")
            tray = row.pop("tray")
            position = row.pop("position", 0)

            if container_id not in self._batches:
                self._batches[container_id] = ContainerBatch(container_id=container_id)

            if plate_id not in self._plates:
                self._plates[plate_id] = Plate(plate_id=plate_id, tray=tray, nr_samples=0)

            sample = VialSample(container_id=container_id, **row)
            cell = PlateCell(sample=sample, position=position, grid_position=grid_position, plate_id=plate_id)
            self._plate_cells.append(cell)

        # Update sample counts
        for plate_id in self._plates:
            self._plates[plate_id] = Plate(
                plate_id=plate_id,
                tray=self._plates[plate_id].tray,
                nr_samples=sum(1 for c in self._plate_cells if c.plate_id == plate_id),
            )

    def build(self) -> QueueInput:
        """Create QueueInput."""
        if self._built:
            raise RuntimeError("Builder already used.")
        if self._parameters is None or self._layout_mode is None:
            raise ValueError("Parameters not set.")

        self._built = True

        parameters = self._parameters
        if self._bfabric_base_url is not None:
            parameters = parameters.model_copy(update={"bfabric_instance": self._bfabric_base_url})

        if self._layout_mode == LayoutMode.PLATE:
            return PlateQueueInput(
                parameters=parameters,
                queue=PlateQueue(batches=self._batches, plates=self._plates, cells=self._plate_cells),
            )
        return VialQueueInput(
            parameters=parameters,
            queue=VialQueue(batches=self._batches, samples=self._vial_samples),
        )
