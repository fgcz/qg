"""Sampler classes for position assignment (new models).

Vial samplers: VialQueue → PlateQueue (assigns positions)
Plate samplers: PlateQueue → PlateQueue (validates positions)
"""

from __future__ import annotations

from itertools import product
from typing import Any

from qg.config_models import QCLayoutPattern
from qg.config_models_samplers import (
    EvosepConfig,
    EvosepPlateConfig,
    EvosepVialConfig,
    MClass48Config,
    MClass48PlateConfig,
    MClass48VialConfig,
    SamplersConfig,
    VanquishConfig,
    VanquishPlateConfig,
    VanquishVialConfig,
)
from qg.params_models import (
    Plate,
    PlateCell,
    PlateQueue,
    VialQueue,
)

PositionDict = dict[str, Any]


def _assign_trays_if_missing(queue: PlateQueue, available_trays: list[str | int]) -> PlateQueue:
    """Assign trays to plates that don't have one, using available tray positions in order."""
    plates_needing_tray = [p for p in queue.plates.values() if p.tray is None]
    if not plates_needing_tray:
        return queue

    if len(plates_needing_tray) > len(available_trays):
        raise ValueError(f"Not enough tray positions ({len(available_trays)}) for {len(plates_needing_tray)} plates")

    new_plates = dict(queue.plates)
    for plate, tray in zip(plates_needing_tray, available_trays, strict=False):
        new_plates[plate.plate_id] = Plate(
            plate_id=plate.plate_id,
            tray=tray,
            nr_samples=plate.nr_samples,
        )

    return PlateQueue(batches=queue.batches, plates=new_plates, cells=queue.cells)


def _grid_to_number(grid_pos: str, cols: int = 12) -> int:
    """Convert grid position like 'A1' to numeric position (1-based)."""
    if not grid_pos:
        return 0
    grid_pos = grid_pos.strip().upper()
    if not grid_pos:
        return 0
    row_letter = grid_pos[0]
    col_str = grid_pos[1:]
    try:
        row_num = ord(row_letter) - ord("A")
        col_num = int(col_str)
        return row_num * cols + col_num
    except (ValueError, IndexError):
        return 0


# =============================================================================
# Vanquish Samplers
# =============================================================================


class VanquishVialSampler:
    """Vanquish vial - generates positions row-major across plates."""

    def __init__(
        self,
        parent: VanquishConfig,
        container: VanquishVialConfig,
        qc_layout: QCLayoutPattern,
    ):
        self._parent = parent
        self._container = container
        self._qc_layout = qc_layout

    def _reserved_positions(self) -> set[tuple[str, str]]:
        return {
            (str(p["tray"]), str(p["grid_position"]))
            for p in (self._qc_layout.get_position(qc_id) for qc_id in self._qc_layout.positions)
        }

    def assign_positions(self, queue: VialQueue) -> PlateQueue:
        """Transform VialQueue to PlateQueue by assigning positions."""
        reserved = self._reserved_positions()
        n_samples = len(queue.samples)

        all_positions = [
            (plate, self._container.grid_position_format.format(row=row, col=col))
            for plate, row, col in product(
                self._parent.plates,
                self._container.sample_rows,
                self._container.cols,
            )
        ]
        available = [pos for pos in all_positions if pos not in reserved]

        if len(available) < n_samples:
            raise ValueError(f"Not enough positions (need {n_samples}, have {len(available)})")

        plates: dict[int, Plate] = {}
        cells: list[PlateCell] = []

        for i, sample in enumerate(queue.samples):
            tray, grid_position = available[i]
            plate_id = hash(tray) & 0xFFFFFFFF  # Simple plate_id from tray
            if plate_id not in plates:
                plates[plate_id] = Plate(plate_id=plate_id, tray=tray, nr_samples=0)
            cells.append(
                PlateCell(
                    sample=sample,
                    position=i + 1,
                    grid_position=grid_position,
                    plate_id=plate_id,
                )
            )

        # Update sample counts
        for plate_id in plates:
            plates[plate_id] = Plate(
                plate_id=plate_id,
                tray=plates[plate_id].tray,
                nr_samples=sum(1 for c in cells if c.plate_id == plate_id),
            )

        return PlateQueue(batches=queue.batches, plates=plates, cells=cells)

    def get_qc_position(self, sample_name: str) -> PositionDict:
        return self._qc_layout.get_position(sample_name)


class VanquishPlateSampler:
    """Vanquish plate - assigns trays and validates positions."""

    def __init__(
        self,
        parent: VanquishConfig,
        container: VanquishPlateConfig,
        qc_layout: QCLayoutPattern,
    ):
        self._parent = parent
        self._container = container
        self._qc_layout = qc_layout

    def _reserved_positions(self) -> set[tuple[str, str]]:
        return {
            (str(p["tray"]), str(p["grid_position"]))
            for p in (self._qc_layout.get_position(qc_id) for qc_id in self._qc_layout.positions)
        }

    def assign_positions(self, queue: PlateQueue) -> PlateQueue:
        """Assign trays to plates and validate positions don't conflict with QC."""
        queue = _assign_trays_if_missing(queue, self._parent.plates)
        reserved = self._reserved_positions()
        for cell in queue.cells:
            plate = queue.plates.get(cell.plate_id)
            tray = str(plate.tray) if plate else ""
            pos = (tray, str(cell.grid_position))
            if pos in reserved:
                raise ValueError(
                    f"Sample '{cell.sample.sample_name}' at {tray}:{cell.grid_position} conflicts with QC position"
                )
        return queue

    def get_qc_position(self, sample_name: str) -> PositionDict:
        return self._qc_layout.get_position(sample_name)


# =============================================================================
# MClass48 Samplers
# =============================================================================


class MClass48VialSampler:
    """MClass48 vial - generates positions row-major across plates."""

    def __init__(
        self,
        parent: MClass48Config,
        container: MClass48VialConfig,
        qc_layout: QCLayoutPattern,
    ):
        self._parent = parent
        self._container = container
        self._qc_layout = qc_layout

    def _reserved_positions(self) -> set[tuple[str, str]]:
        return {
            (str(p["tray"]), str(p["grid_position"]))
            for p in (self._qc_layout.get_position(qc_id) for qc_id in self._qc_layout.positions)
        }

    def assign_positions(self, queue: VialQueue) -> PlateQueue:
        """Transform VialQueue to PlateQueue by assigning positions."""
        reserved = self._reserved_positions()
        n_samples = len(queue.samples)

        all_positions = [
            (plate, self._container.grid_position_format.format(row=row, col=col))
            for plate, row, col in product(
                self._parent.plates,
                self._parent.sample_rows,
                self._parent.cols,
            )
        ]
        available = [pos for pos in all_positions if pos not in reserved]

        if len(available) < n_samples:
            raise ValueError(f"Not enough positions (need {n_samples}, have {len(available)})")

        plates: dict[int, Plate] = {}
        cells: list[PlateCell] = []

        for i, sample in enumerate(queue.samples):
            tray, grid_position = available[i]
            plate_id = hash(tray) & 0xFFFFFFFF
            if plate_id not in plates:
                plates[plate_id] = Plate(plate_id=plate_id, tray=tray, nr_samples=0)
            cells.append(
                PlateCell(
                    sample=sample,
                    position=i + 1,
                    grid_position=grid_position,
                    plate_id=plate_id,
                )
            )

        for plate_id in plates:
            plates[plate_id] = Plate(
                plate_id=plate_id,
                tray=plates[plate_id].tray,
                nr_samples=sum(1 for c in cells if c.plate_id == plate_id),
            )

        return PlateQueue(batches=queue.batches, plates=plates, cells=cells)

    def get_qc_position(self, sample_name: str) -> PositionDict:
        return self._qc_layout.get_position(sample_name)


class MClass48PlateSampler:
    """MClass48 plate - assigns trays and validates positions."""

    def __init__(
        self,
        parent: MClass48Config,
        container: MClass48PlateConfig,
        qc_layout: QCLayoutPattern,
    ):
        self._parent = parent
        self._container = container
        self._qc_layout = qc_layout

    def _reserved_positions(self) -> set[tuple[str, str]]:
        return {
            (str(p["tray"]), str(p["grid_position"]))
            for p in (self._qc_layout.get_position(qc_id) for qc_id in self._qc_layout.positions)
        }

    def assign_positions(self, queue: PlateQueue) -> PlateQueue:
        """Assign trays to plates and validate positions don't conflict with QC."""
        queue = _assign_trays_if_missing(queue, self._parent.plates)
        reserved = self._reserved_positions()
        for cell in queue.cells:
            plate = queue.plates.get(cell.plate_id)
            tray = str(plate.tray) if plate else ""
            pos = (tray, str(cell.grid_position))
            if pos in reserved:
                raise ValueError(
                    f"Sample '{cell.sample.sample_name}' at {tray}:{cell.grid_position} conflicts with QC position"
                )
        return queue

    def get_qc_position(self, sample_name: str) -> PositionDict:
        return self._qc_layout.get_position(sample_name)


# =============================================================================
# Evosep Samplers
# =============================================================================


class EvosepVialSampler:
    """Evosep vial - generates positions sequentially across slots."""

    def __init__(
        self,
        parent: EvosepConfig,
        container: EvosepVialConfig,
        qc_layout: QCLayoutPattern,
    ):
        self._parent = parent
        self._container = container
        self._qc_layout = qc_layout

    def assign_positions(self, queue: VialQueue) -> PlateQueue:
        """Transform VialQueue to PlateQueue by assigning positions."""
        n_samples = len(queue.samples)

        all_positions = [
            (slot, pos) for slot in self._parent.slots for pos in range(1, self._parent.positions_per_slot + 1)
        ]

        if len(all_positions) < n_samples:
            raise ValueError(f"Not enough positions (need {n_samples}, have {len(all_positions)})")

        plates: dict[int, Plate] = {}
        cells: list[PlateCell] = []

        for i, sample in enumerate(queue.samples):
            slot, position = all_positions[i]
            plate_id = slot
            if plate_id not in plates:
                plates[plate_id] = Plate(plate_id=plate_id, tray=slot, nr_samples=0)
            cells.append(
                PlateCell(
                    sample=sample,
                    position=position,
                    grid_position=position,
                    plate_id=plate_id,
                )
            )

        for plate_id in plates:
            plates[plate_id] = Plate(
                plate_id=plate_id,
                tray=plates[plate_id].tray,
                nr_samples=sum(1 for c in cells if c.plate_id == plate_id),
            )

        return PlateQueue(batches=queue.batches, plates=plates, cells=cells)

    def get_qc_position(self, sample_name: str) -> PositionDict:
        return self._qc_layout.get_position(sample_name)


class EvosepPlateSampler:
    """Evosep plate - assigns trays and converts grid positions to numeric."""

    def __init__(
        self,
        parent: EvosepConfig,
        container: EvosepPlateConfig,
        qc_layout: QCLayoutPattern,
    ):
        self._parent = parent
        self._container = container
        self._qc_layout = qc_layout

    def assign_positions(self, queue: PlateQueue) -> PlateQueue:
        """Assign trays to plates and convert grid positions to numeric if needed."""
        queue = _assign_trays_if_missing(queue, self._parent.slots)

        if self._container.grid_position_conversion not in ("grid_to_number", "row_major"):
            return queue

        new_cells = []
        for cell in queue.cells:
            numeric_pos = _grid_to_number(str(cell.grid_position))
            new_cells.append(
                PlateCell(
                    sample=cell.sample,
                    position=numeric_pos,
                    grid_position=numeric_pos,
                    plate_id=cell.plate_id,
                )
            )
        return PlateQueue(batches=queue.batches, plates=queue.plates, cells=new_cells)

    def get_qc_position(self, sample_name: str) -> PositionDict:
        return self._qc_layout.get_position(sample_name)


# =============================================================================
# Factory
# =============================================================================


VialSampler = VanquishVialSampler | MClass48VialSampler | EvosepVialSampler
PlateSampler = VanquishPlateSampler | MClass48PlateSampler | EvosepPlateSampler
Sampler = VialSampler | PlateSampler


def create_vial_sampler(
    sampler_name: str,
    config: SamplersConfig,
    qc_layout: QCLayoutPattern,
) -> VialSampler:
    """Create sampler for vial mode."""
    match sampler_name:
        case "Vanquish":
            return VanquishVialSampler(config.Vanquish, config.Vanquish.vial, qc_layout)
        case "MClass48":
            return MClass48VialSampler(config.MClass48, config.MClass48.vial, qc_layout)
        case "Evosep":
            return EvosepVialSampler(config.Evosep, config.Evosep.vial, qc_layout)
        case _:
            raise ValueError(f"Unknown sampler: {sampler_name}")


def create_plate_sampler(
    sampler_name: str,
    config: SamplersConfig,
    qc_layout: QCLayoutPattern,
) -> PlateSampler:
    """Create sampler for plate mode."""
    match sampler_name:
        case "Vanquish":
            return VanquishPlateSampler(config.Vanquish, config.Vanquish.plate, qc_layout)
        case "MClass48":
            return MClass48PlateSampler(config.MClass48, config.MClass48.plate, qc_layout)
        case "Evosep":
            return EvosepPlateSampler(config.Evosep, config.Evosep.plate, qc_layout)
        case _:
            raise ValueError(f"Unknown sampler: {sampler_name}")


class SamplerStrategy:
    """Strategy for assigning positions to samples."""

    def __init__(
        self,
        sampler_name: str,
        layout_mode: str,
        config: SamplersConfig,
        qc_layout: QCLayoutPattern,
    ):
        self._layout_mode = layout_mode
        if layout_mode == "vial":
            self._vial_sampler: VialSampler | None = create_vial_sampler(sampler_name, config, qc_layout)
            self._plate_sampler: PlateSampler | None = None
        else:
            self._vial_sampler = None
            self._plate_sampler = create_plate_sampler(sampler_name, config, qc_layout)

    def assign_positions(self, queue: VialQueue | PlateQueue) -> PlateQueue:
        """Assign positions. Vial → PlateQueue, Plate → validated PlateQueue."""
        if self._layout_mode == "vial":
            if not isinstance(queue, VialQueue):
                raise TypeError("Vial mode requires VialQueue")
            return self._vial_sampler.assign_positions(queue)
        else:
            if not isinstance(queue, PlateQueue):
                raise TypeError("Plate mode requires PlateQueue")
            return self._plate_sampler.assign_positions(queue)

    def get_qc_position(self, sample_name: str) -> PositionDict:
        """Get QC sample position."""
        sampler = self._vial_sampler or self._plate_sampler
        return sampler.get_qc_position(sample_name)
