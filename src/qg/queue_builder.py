"""Builder for QueueInput (vial or plate)."""

from __future__ import annotations

from typing import TYPE_CHECKING, Literal, Self

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

if TYPE_CHECKING:
    from qg.config import ConfigBundle


class QueueBuilder:
    """Fluent builder for QueueInput. Single-use."""

    def __init__(self, configs: ConfigBundle) -> None:
        self.configs = configs
        self._parameters: QueueParameters | None = None
        self._layout_mode: Literal["vial", "plate"] | None = None
        self._batches: dict[int, ContainerBatch] = {}
        self._plates: dict[int, Plate] = {}
        self._vial_samples: list[VialSample] = []
        self._plate_cells: list[PlateCell] = []
        self._built = False

    def with_parameters(self, parameters: QueueParameters, layout_mode: Literal["vial", "plate"]) -> Self:
        """Set queue parameters and layout mode."""
        if self._built:
            raise RuntimeError("Builder already used.")
        self._parameters = parameters
        self._layout_mode = layout_mode
        return self

    def add_samples_from_dataframe(self, df: pl.DataFrame) -> Self:
        """Add samples from DataFrame."""
        if self._built:
            raise RuntimeError("Builder already used.")
        if self._layout_mode is None:
            raise RuntimeError("Call with_parameters() first.")
        if "container_id" not in df.columns:
            raise ValueError("DataFrame must have 'container_id' column")

        if self._layout_mode == "plate":
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

        samples = self._plate_cells if self._layout_mode == "plate" else self._vial_samples
        if not samples:
            raise ValueError("No samples added.")

        self._validate_randomization(samples)
        self._built = True

        if self._layout_mode == "plate":
            return PlateQueueInput(
                parameters=self._parameters,
                queue=PlateQueue(batches=self._batches, plates=self._plates, cells=self._plate_cells),
            )
        return VialQueueInput(
            parameters=self._parameters,
            queue=VialQueue(batches=self._batches, samples=self._vial_samples),
        )

    def _validate_randomization(self, samples: list) -> None:
        if self._parameters and self._parameters.randomization == "blocked":
            if self._layout_mode == "plate":
                has_grouping = any(c.sample.grouping_var is not None for c in samples)
            else:
                has_grouping = any(s.grouping_var is not None for s in samples)
            if not has_grouping:
                raise ValueError("randomization='blocked' requires grouping_var.")
