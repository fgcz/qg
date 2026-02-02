# =============================================================================
# Position Generation (Layer 2: Domain Logic - Pure Python)
# =============================================================================
#
# This module implements position generation using pure functions and minimal
# state classes. No Pydantic/dataclasses - this is the algorithmic layer.
#
# Public API:
#   - SamplerStrategyV2: Main strategy class for position assignment
#
# All other functions and classes are private implementation details.
#

from __future__ import annotations

from collections.abc import Callable
from itertools import product
from typing import TYPE_CHECKING, Any

if TYPE_CHECKING:
    from qg.config_models_new.positions import (
        PlateLayout,
        QCSampleEvosep,
        QCSampleGrid,
        Sampler,
    )

from qg.params_models import (
    Plate,
    PlateCell,
    PlateQueue,
    VialQueue,
)

PositionDict = dict[str, Any]  # {"tray": plate, "grid_position": grid_position}


# =============================================================================
# Private: Helper Functions
# =============================================================================


def _assign_trays_if_missing(queue: PlateQueue, available_trays: list[str] | list[int]) -> PlateQueue:
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


# =============================================================================
# Private: Position Functions (Registry)
# =============================================================================


def _string_concat(row: str | int, col: int) -> str:
    """Grid samplers: 'A' + 1 → 'A1'."""
    return f"{row}{col}"


def _int_add(row: int, col: int) -> int:
    """Evosep: row + col → position (1-indexed offset)."""
    return row + col


_POSITION_FUNCTIONS: dict[str, Callable[[str | int, int], str | int]] = {
    "string_concat": _string_concat,
    "int_add": _int_add,
}


def _get_position_function(name: str) -> Callable[[str | int, int], str | int]:
    """Get a position function by name."""
    if name not in _POSITION_FUNCTIONS:
        raise KeyError(f"Unknown position function: {name!r}. Available: {list(_POSITION_FUNCTIONS.keys())}")
    return _POSITION_FUNCTIONS[name]


# =============================================================================
# Private: Position Generation
# =============================================================================


def _generate_all_positions(
    trays: list[str] | list[int],
    rows: list[str] | list[int],
    cols: list[int],
    position_fun: Callable[[str | int, int], str | int],
) -> list[tuple[str | int, str | int]]:
    """Generate all (tray, position) tuples using itertools.product."""
    return [(tray, position_fun(row, col)) for tray, row, col in product(trays, rows, cols)]


# =============================================================================
# Private: QCLayoutGrid (Stateless)
# =============================================================================


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
        if sample_id not in self._positions:
            raise KeyError(f"Unknown QC sample: {sample_id!r}. Available: {list(self._positions.keys())}")
        return self._positions[sample_id]

    def reserved_positions(self) -> set[tuple[str, str | int]]:
        return set(self._positions.values())


# =============================================================================
# Private: QCLayoutEvosep (Stateful)
# =============================================================================


class _QCLayoutEvosep:
    """Stateful QC layout for Evosep with consumable tip positions."""

    def __init__(self, samples: list[QCSampleEvosep]) -> None:
        self._samples: dict[str, QCSampleEvosep] = {s.sample_id: s for s in samples}
        self._counters: dict[str, int] = {s.sample_id: 0 for s in samples}

    def get_position(self, sample_id: str) -> tuple[int, int]:
        if sample_id not in self._samples:
            raise KeyError(f"Unknown QC sample: {sample_id!r}. Available: {list(self._samples.keys())}")

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
        for sample_id in self._counters:
            self._counters[sample_id] = 0


# =============================================================================
# SamplerStrategyV2 (Public API)
# =============================================================================


class SamplerStrategyV2:
    """Strategy for assigning positions to samples.

    This is the main public interface for position assignment. It combines:
    - Sampler configuration (trays, position function)
    - Plate layout (rows, cols)
    - QC layout (reserved positions)

    And provides methods to:
    - Assign positions to user samples (VialQueue → PlateQueue)
    - Validate plate positions (PlateQueue → PlateQueue)
    - Get positions for QC samples
    """

    def __init__(
        self,
        sampler: Sampler,
        plate_layout: PlateLayout,
        qc_samples: list[QCSampleGrid] | list[QCSampleEvosep],
        layout_mode: str = "vial",
    ) -> None:
        """Initialize sampler strategy.

        Args:
            sampler: Sampler configuration from config_models_new
            plate_layout: Plate layout from config_models_new
            qc_samples: QC sample positions (grid or evosep)
            layout_mode: "vial" or "plate"
        """
        self._sampler = sampler
        self._plate_layout = plate_layout
        self._layout_mode = layout_mode
        self._position_fun = _get_position_function(sampler.position_fun)

        # Create the appropriate QC layout based on sample type
        # Import here to avoid issues with TYPE_CHECKING block
        from qg.config_models_new.positions import QCSampleGrid

        if qc_samples and isinstance(qc_samples[0], QCSampleGrid):
            self._qc_layout: _QCLayoutGrid | _QCLayoutEvosep = _QCLayoutGrid(
                qc_samples,
                self._position_fun,  # type: ignore[arg-type]
            )
            self._is_evosep = False
        else:
            self._qc_layout = _QCLayoutEvosep(qc_samples)  # type: ignore[arg-type]
            self._is_evosep = True

        # Pre-generate all positions
        self._all_positions = _generate_all_positions(
            trays=sampler.trays,
            rows=plate_layout.rows,
            cols=plate_layout.cols,
            position_fun=self._position_fun,
        )

    def assign_positions(self, queue: VialQueue | PlateQueue) -> PlateQueue:
        """Assign positions. Vial → PlateQueue, Plate → validated PlateQueue."""
        if self._layout_mode == "vial":
            if not isinstance(queue, VialQueue):
                raise TypeError("Vial mode requires VialQueue")
            return self._assign_positions_vial(queue)
        else:
            if not isinstance(queue, PlateQueue):
                raise TypeError("Plate mode requires PlateQueue")
            return self._validate_positions_plate(queue)

    def _assign_positions_vial(self, queue: VialQueue) -> PlateQueue:
        """Transform VialQueue to PlateQueue by assigning positions."""
        reserved = self._qc_layout.reserved_positions()
        available = [p for p in self._all_positions if p not in reserved]

        n_samples = len(queue.samples)
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

        # Update sample counts
        for plate_id in plates:
            plates[plate_id] = Plate(
                plate_id=plate_id,
                tray=plates[plate_id].tray,
                nr_samples=sum(1 for c in cells if c.plate_id == plate_id),
            )

        return PlateQueue(batches=queue.batches, plates=plates, cells=cells)

    def _validate_positions_plate(self, queue: PlateQueue) -> PlateQueue:
        """Assign trays to plates and validate positions don't conflict with QC."""
        # Assign trays if missing
        queue = _assign_trays_if_missing(queue, self._sampler.trays)

        # Check for collisions with QC positions
        reserved = self._qc_layout.reserved_positions()
        for cell in queue.cells:
            plate = queue.plates.get(cell.plate_id)
            tray = plate.tray if plate else None
            pos = (tray, cell.grid_position)
            if pos in reserved:
                raise ValueError(
                    f"Sample '{cell.sample.sample_name}' at {tray}:{cell.grid_position} conflicts with QC position"
                )

        return queue

    def get_qc_position(self, sample_name: str) -> PositionDict:
        """Get QC sample position as dict.

        Args:
            sample_name: QC sample identifier

        Returns:
            Dict with "tray" and "grid_position" keys
        """
        tray, grid_position = self._qc_layout.get_position(sample_name)
        return {"tray": tray, "grid_position": grid_position}
