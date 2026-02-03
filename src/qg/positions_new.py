# =============================================================================
# Position Generation (Layer 2: Domain Logic - Pure Python)
# =============================================================================
#
# Public API:
#   - SamplerStrategyV2: Main strategy class for position assignment
#

from __future__ import annotations

from collections.abc import Callable
from itertools import product
from typing import Any

from qg.config_models_new.loader import QGConfiguration
from qg.config_models_new.positions import QCSampleEvosep, QCSampleGrid
from qg.params_models import (
    Plate,
    PlateCell,
    PlateQueue,
    VialQueue,
)

PositionDict = dict[str, Any]  # {"tray": plate, "grid_position": grid_position}


class _PositionFunctions:
    """Registry and generator for position functions."""

    @staticmethod
    def string_concat(row: str | int, col: int) -> str:
        return f"{row}{col}"

    @staticmethod
    def int_add(row: int, col: int) -> int:
        return row + col

    _registry: dict[str, Callable[[str | int, int], str | int]] = {
        "string_concat": string_concat.__func__,
        "int_add": int_add.__func__,
    }

    @classmethod
    def get(cls, name: str) -> Callable[[str | int, int], str | int]:
        return cls._registry[name]

    @staticmethod
    def generate_all(
        trays: list[str] | list[int],
        rows: list[str] | list[int],
        cols: list[int],
        position_fun: Callable[[str | int, int], str | int],
    ) -> list[tuple[str | int, str | int]]:
        return [(tray, position_fun(row, col)) for tray, row, col in product(trays, rows, cols)]


class _QCLayoutGrid:
    """Stateless QC layout for grid samplers with fixed well positions."""

    def __init__(
        self,
        samples: list[QCSampleGrid],
        position_fun: Callable[[str | int, int], str | int],
    ) -> None:
        self._positions: dict[str, tuple[str, str | int]] = {
            s.sample_id: (s.plate, position_fun(s.row, s.col)) for s in samples
        }

    def get_position(self, sample_id: str) -> tuple[str, str | int]:
        return self._positions[sample_id]

    def reserved_positions(self) -> set[tuple[str, str | int]]:
        return set(self._positions.values())


class _QCLayoutEvosep:
    """Stateful QC layout for Evosep with consumable tip positions."""

    def __init__(self, samples: list[QCSampleEvosep]) -> None:
        self._samples: dict[str, QCSampleEvosep] = {s.sample_id: s for s in samples}
        self._counters: dict[str, int] = {s.sample_id: 0 for s in samples}

    def get_position(self, sample_id: str) -> tuple[int, int]:
        s = self._samples[sample_id]
        idx = self._counters[sample_id]
        pos = s.position_start + idx

        if pos > s.position_end:
            raise ValueError(
                f"Position range exhausted for {sample_id!r}: "
                f"used {idx} of {s.position_end - s.position_start + 1} positions"
            )

        self._counters[sample_id] = idx + 1
        return (s.tray, pos)

    def reserved_positions(self) -> set[tuple[int, int]]:
        return {(s.tray, pos) for s in self._samples.values() for pos in range(s.position_start, s.position_end + 1)}

    def remaining(self, sample_id: str) -> int:
        s = self._samples[sample_id]
        used = self._counters[sample_id]
        total = s.position_end - s.position_start + 1
        return total - used

    def reset(self) -> None:
        self._counters = dict.fromkeys(self._counters, 0)


class _VialAssigner:
    """Assigns positions to VialQueue samples -> PlateQueue."""

    def __init__(
        self,
        all_positions: list[tuple[str | int, str | int]],
        qc_layout: _QCLayoutGrid | _QCLayoutEvosep,
    ) -> None:
        reserved = qc_layout.reserved_positions()
        self._available = [p for p in all_positions if p not in reserved]

    def assign(self, queue: VialQueue) -> PlateQueue:
        n = len(queue.samples)
        if n > len(self._available):
            raise ValueError(f"Not enough positions (need {n}, have {len(self._available)})")

        assignments = list(zip(queue.samples, self._available[:n], strict=True))
        by_tray = self._group_by_tray(assignments)
        return self._build_plate_queue(queue.batches, by_tray)

    def _group_by_tray(
        self,
        assignments: list[tuple[Any, tuple[str | int, str | int]]],
    ) -> dict[str | int, list[tuple[Any, str | int, int]]]:
        by_tray: dict[str | int, list[tuple[Any, str | int, int]]] = {}
        for i, (sample, (tray, grid_pos)) in enumerate(assignments):
            by_tray.setdefault(tray, []).append((sample, grid_pos, i + 1))
        return by_tray

    def _build_plate_queue(
        self,
        batches: Any,
        by_tray: dict[str | int, list[tuple[Any, str | int, int]]],
    ) -> PlateQueue:
        plates: dict[int, Plate] = {}
        cells: list[PlateCell] = []
        for tray, items in by_tray.items():
            plate_id = hash(tray) & 0xFFFFFFFF
            plates[plate_id] = Plate(plate_id=plate_id, tray=tray, nr_samples=len(items))
            cells.extend(
                PlateCell(sample=sample, position=pos, grid_position=grid_pos, plate_id=plate_id)
                for sample, grid_pos, pos in items
            )
        return PlateQueue(batches=batches, plates=plates, cells=cells)


class _PlateValidator:
    """Validates PlateQueue positions against QC reservations."""

    def __init__(
        self,
        available_trays: list[str] | list[int],
        qc_layout: _QCLayoutGrid | _QCLayoutEvosep,
    ) -> None:
        self._trays = available_trays
        self._reserved = qc_layout.reserved_positions()

    def validate(self, queue: PlateQueue) -> PlateQueue:
        queue = self._assign_trays_if_missing(queue)
        self._check_collisions(queue)
        return queue

    def _assign_trays_if_missing(self, queue: PlateQueue) -> PlateQueue:
        plates_needing_tray = [p for p in queue.plates.values() if p.tray is None]
        if not plates_needing_tray:
            return queue

        if len(plates_needing_tray) > len(self._trays):
            raise ValueError(f"Not enough tray positions ({len(self._trays)}) for {len(plates_needing_tray)} plates")

        new_plates = dict(queue.plates)
        for plate, tray in zip(plates_needing_tray, self._trays, strict=False):
            new_plates[plate.plate_id] = Plate(
                plate_id=plate.plate_id,
                tray=tray,
                nr_samples=plate.nr_samples,
            )

        return PlateQueue(batches=queue.batches, plates=new_plates, cells=queue.cells)

    def _check_collisions(self, queue: PlateQueue) -> None:
        for cell in queue.cells:
            plate = queue.plates.get(cell.plate_id)
            tray = plate.tray if plate else None
            if (tray, cell.grid_position) in self._reserved:
                raise ValueError(
                    f"Sample '{cell.sample.sample_name}' at {tray}:{cell.grid_position} conflicts with QC position"
                )


class SamplerStrategyV2:
    """Strategy for assigning positions to samples.

    Public API for position assignment. Delegates to:
    - _VialAssigner: VialQueue -> PlateQueue (assigns positions)
    - _PlateValidator: PlateQueue -> PlateQueue (validates against QC)
    """

    def __init__(
        self,
        sampler_name: str,
        layout_mode: str,
        config: QGConfiguration,
        tech_area: str,
        qc_layout_name: str = "standard",
        plate_layout_name: str | None = None,
    ) -> None:
        self._layout_mode = layout_mode

        # Resolve sampler
        sampler = config.samplers.get_sampler(sampler_name)
        if not sampler:
            raise ValueError(f"Unknown sampler: {sampler_name}")

        # Resolve plate_layout - use explicit if provided, else auto-resolve
        queue_type = "Vial" if layout_mode == "vial" else "Plate"
        if plate_layout_name:
            resolved_layout_name = plate_layout_name
        else:
            plate_layout_names = config.sampler_plate_layouts.get_plate_layouts_for_sampler(sampler_name, queue_type)
            if not plate_layout_names:
                raise ValueError(f"No {queue_type} layout for {sampler_name}")
            resolved_layout_name = plate_layout_names[0]
        plate_layout = config.plate_layouts.get_layout(resolved_layout_name)
        if not plate_layout:
            raise ValueError(f"Unknown plate layout: {resolved_layout_name}")

        # Resolve QC samples based on sampler type
        if sampler_name == "Evosep":
            qc_samples: list[QCSampleGrid] | list[QCSampleEvosep] = config.qc_layouts_evosep.get_samples(
                tech_area, qc_layout_name, plate_layout.name
            )
        else:
            qc_samples = config.qc_layouts_grid.get_samples(tech_area, qc_layout_name, plate_layout.name)

        position_fun = _PositionFunctions.get(sampler.position_fun)

        # Create QC layout based on sample type
        is_grid = qc_samples and hasattr(qc_samples[0], "plate")
        if is_grid:
            self._qc_layout: _QCLayoutGrid | _QCLayoutEvosep = _QCLayoutGrid(
                qc_samples,  # type: ignore[arg-type]
                position_fun,
            )
        else:
            self._qc_layout = _QCLayoutEvosep(qc_samples)  # type: ignore[arg-type]

        # Create handler based on mode
        all_positions = _PositionFunctions.generate_all(
            trays=sampler.trays,
            rows=plate_layout.rows,
            cols=plate_layout.cols,
            position_fun=position_fun,
        )

        if layout_mode == "vial":
            self._vial_assigner: _VialAssigner | None = _VialAssigner(all_positions, self._qc_layout)
            self._plate_validator: _PlateValidator | None = None
        else:
            self._vial_assigner = None
            self._plate_validator = _PlateValidator(sampler.trays, self._qc_layout)

    def assign_positions(self, queue: VialQueue | PlateQueue) -> PlateQueue:
        if self._layout_mode == "vial":
            if not isinstance(queue, VialQueue):
                raise TypeError("Vial mode requires VialQueue")
            return self._vial_assigner.assign(queue)  # type: ignore[union-attr]
        else:
            if not isinstance(queue, PlateQueue):
                raise TypeError("Plate mode requires PlateQueue")
            return self._plate_validator.validate(queue)  # type: ignore[union-attr]

    def get_qc_position(self, sample_name: str) -> PositionDict:
        """Get QC sample position as dict.

        Args:
            sample_name: QC sample identifier

        Returns:
            Dict with "tray" and "grid_position" keys
        """
        tray, grid_position = self._qc_layout.get_position(sample_name)
        return {"tray": tray, "grid_position": grid_position}
