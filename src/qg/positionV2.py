# =============================================================================
# Position Generation V2 (Self-contained - NO dependencies on positions.py)
# =============================================================================
#
# 4 specialized classes for position assignment:
#   - _PlateValidatorWellConfig: Plate mode + well-plate samplers (Vanquish/MClass)
#   - _PlateValidatorTipConfig: Plate mode + tip-plate samplers (Evosep)
#   - _VialPlateAssignerWellConfig: Vial mode + well-plate samplers
#   - _VialPlateAssignerTipConfig: Vial mode + tip-plate samplers
#
# Factory: create_assembled_sampler() returns correct class

from __future__ import annotations

from collections import Counter
from typing import Protocol

from qg.config_models.loader import QGConfiguration
from qg.config_models.positions import (
    PlateLayout,
    Sampler,
)
from qg.config_models.structure import QueuePattern
from qg.params_models import (
    Plate,
    PlateCell,
    PlateQueue,
    VialQueue,
    VialSample,
)
from qg.qc_layout import QCLayoutTip, QCLayoutWell, create_qc_layout
from qg.utils import (
    LayoutMode,
    Position,
    generate_all_positions,
    get_position_function,
    group_by_key,
)

# =============================================================================
# Helper Functions
# =============================================================================


def _assign_trays_if_missing(queue: PlateQueue, available_trays: list[str] | list[int]) -> PlateQueue:
    """Assign trays to plates that don't have one."""
    plates_needing_tray = [p for p in queue.plates.values() if p.tray is None]
    if not plates_needing_tray:
        return queue

    if len(plates_needing_tray) > len(available_trays):
        raise ValueError(f"Not enough trays ({len(available_trays)}) for {len(plates_needing_tray)} plates")

    new_plates = dict(queue.plates)
    for plate, tray in zip(plates_needing_tray, available_trays, strict=False):
        new_plates[plate.plate_id] = Plate(
            plate_id=plate.plate_id,
            tray=tray,
            nr_samples=plate.nr_samples,
        )

    return PlateQueue(batches=queue.batches, plates=new_plates, cells=queue.cells)


def _build_plate_queue(
    batches: dict,
    pairs: list[tuple[VialSample, Position]],
) -> PlateQueue:
    """Build PlateQueue from sample-position pairs."""
    cells: list[PlateCell] = []
    tray_for_plate: dict[int, str | int] = {}

    for i, (sample, pos) in enumerate(pairs):
        plate_id = hash(pos.tray) & 0xFFFFFFFF
        tray_for_plate[plate_id] = pos.tray
        cells.append(
            PlateCell(
                sample=sample,
                position=i + 1,
                grid_position=pos.grid_position,
                plate_id=plate_id,
                row=pos.row,
                col=pos.col,
            )
        )

    counts = Counter(cell.plate_id for cell in cells)
    plates = {
        plate_id: Plate(plate_id=plate_id, tray=tray, nr_samples=counts[plate_id])
        for plate_id, tray in tray_for_plate.items()
    }
    return PlateQueue(batches=batches, plates=plates, cells=cells)


# =============================================================================
# Position Pools (DRY: shared position computation)
# =============================================================================


class _PositionPoolWell:
    """Position pool for well plates (Vanquish, MClass) — reusable positions.

    Uses precomputed QCLayoutWell for reserved positions and filters them out.
    """

    def __init__(
        self,
        sampler: Sampler,
        plate_layout: PlateLayout,
        qc_layout: QCLayoutWell,
    ) -> None:
        self.position_fun = get_position_function(sampler.position_fun)
        self.trays = sampler.trays

        # Store precomputed QC layout and its reserved positions
        self.qc_layout = qc_layout
        self.reserved = qc_layout.reserved

        # Generate all positions and filter out reserved
        self.all_positions = generate_all_positions(
            sampler.trays, plate_layout.rows, plate_layout.cols, self.position_fun
        )
        self.available = [p for p in self.all_positions if p not in self.reserved]
        self.by_tray = group_by_key(self.available, key=lambda p: p.tray)


class _PositionPoolTip:
    """Position pool for tip plates (Evosep) — consumable, single-use positions.

    Uses precomputed QCLayoutTip for reserved ranges (not filtered out - tips consumed sequentially).
    """

    def __init__(
        self,
        sampler: Sampler,
        plate_layout: PlateLayout,
        qc_layout: QCLayoutTip,
    ) -> None:
        self.position_fun = get_position_function(sampler.position_fun)
        self.trays = sampler.trays

        # Store precomputed QC layout and its reserved positions
        self.qc_layout = qc_layout
        self.reserved = qc_layout.reserved

        # Generate all positions (don't filter - Evosep uses consumable tips)
        self.all_positions = generate_all_positions(
            sampler.trays, plate_layout.rows, plate_layout.cols, self.position_fun
        )
        self.available = self.all_positions
        self.by_tray = group_by_key(self.available, key=lambda p: p.tray)


# =============================================================================
# Plate Validators (Plate Mode - validate existing positions)
# =============================================================================


class _PlateValidatorWellConfig:
    """Validates PlateQueue positions against QC reservations (well-plate samplers).

    For Plate mode: user provides PlateQueue with positions already assigned.
    This class validates that user positions don't conflict with QC positions,
    and splits alpha grid_position into row/col components.
    """

    def __init__(
        self,
        sampler: Sampler,
        plate_layout: PlateLayout,
        qc_layout: QCLayoutWell,
    ) -> None:
        self.pool = _PositionPoolWell(sampler, plate_layout, qc_layout)
        self.plate_layout = plate_layout

    @property
    def qc_layout(self) -> QCLayoutWell:
        return self.pool.qc_layout

    def _check_collisions(self, queue: PlateQueue) -> None:
        """Check that no user positions conflict with QC positions."""
        for cell in queue.cells:
            plate = queue.plates.get(cell.plate_id)
            tray = plate.tray if plate else None
            pos = Position(tray, cell.grid_position, row=cell.grid_position[0], col=int(cell.grid_position[1:]))
            if pos in self.pool.reserved:
                raise ValueError(
                    f"Sample '{cell.sample.sample_name}' at {tray}:{cell.grid_position} conflicts with QC position"
                )

    def assign(self, queue: PlateQueue, *, one_container_per_tray: bool = False) -> PlateQueue:  # noqa: ARG002
        """Assign trays to plates, validate no QC conflicts, populate row/col."""
        queue = _assign_trays_if_missing(queue, self.pool.trays)
        self._check_collisions(queue)
        # B-Fabric provides grid_position="A1" but row="" and col=0.
        # Split alpha grid_position into row and col components for format_table().
        split_cells = []
        for cell in queue.cells:
            if isinstance(cell.grid_position, str) and cell.row == "":
                row, col = self.plate_layout.split_alpha(cell.grid_position)
                split_cells.append(cell.model_copy(update={"row": row, "col": col}))
            else:
                split_cells.append(cell)
        return PlateQueue(batches=queue.batches, plates=queue.plates, cells=split_cells)


class _PlateValidatorTipConfig:
    """Validates PlateQueue positions for tip-plate plate mode.

    For Plate mode with Evosep: assigns trays and splits alpha grid_position
    into row/col components. B-Fabric provides alpha positions (e.g., "D8") which
    are now the internal representation — no numeric conversion needed.
    """

    def __init__(
        self,
        sampler: Sampler,
        plate_layout: PlateLayout,
        qc_layout: QCLayoutTip,
    ) -> None:
        self.pool = _PositionPoolTip(sampler, plate_layout, qc_layout)
        self.plate_layout = plate_layout

    @property
    def qc_layout(self) -> QCLayoutTip:
        return self.pool.qc_layout

    def assign(self, queue: PlateQueue, *, one_container_per_tray: bool = False) -> PlateQueue:  # noqa: ARG002
        """Assign trays to plates and populate row/col from alpha grid_position."""
        queue = _assign_trays_if_missing(queue, self.pool.trays)
        # B-Fabric provides grid_position="D8" but row="" and col=0.
        # Split alpha grid_position into row and col components for format_table().
        split_cells = []
        for cell in queue.cells:
            if isinstance(cell.grid_position, str) and cell.row == "":
                row, col = self.plate_layout.split_alpha(cell.grid_position)
                split_cells.append(cell.model_copy(update={"row": row, "col": col}))
            else:
                split_cells.append(cell)
        return PlateQueue(batches=queue.batches, plates=queue.plates, cells=split_cells)


# =============================================================================
# Vial Assigners (Vial Mode - assign new positions)
# =============================================================================


class _VialPlateAssignerWellConfig:
    """Assigns positions to VialQueue samples (well-plate samplers).

    For Vial mode: user provides VialQueue (samples without positions).
    This class assigns positions and returns PlateQueue.
    """

    def __init__(
        self,
        sampler: Sampler,
        plate_layout: PlateLayout,
        qc_layout: QCLayoutWell,
    ) -> None:
        self.pool = _PositionPoolWell(sampler, plate_layout, qc_layout)

    @property
    def qc_layout(self) -> QCLayoutWell:
        return self.pool.qc_layout

    def assign(self, queue: VialQueue, *, one_container_per_tray: bool = False) -> PlateQueue:
        """Transform VialQueue to PlateQueue by assigning positions."""
        n_samples = len(queue.samples)

        if one_container_per_tray:
            by_container = group_by_key(queue.samples, key=lambda s: s.container_id)
            n_containers = len(by_container)
            n_trays = len(self.pool.by_tray)
            if n_containers > n_trays:
                raise ValueError(
                    f"Not enough trays ({n_trays}) for {n_containers} containers. "
                    f"Set one_container_per_tray=False to allow mixing containers on the same tray."
                )
            pairs = []
            for samples, tray in zip(by_container.values(), self.pool.by_tray.keys(), strict=False):
                tray_positions = self.pool.by_tray[tray]
                if len(samples) > len(tray_positions):
                    raise ValueError(
                        f"Container has {len(samples)} samples but tray {tray} "
                        f"only has {len(tray_positions)} available positions."
                    )
                pairs.extend(zip(samples, tray_positions, strict=False))
        else:
            if n_samples > len(self.pool.available):
                raise ValueError(f"Not enough positions (need {n_samples}, have {len(self.pool.available)})")
            pairs = list(zip(queue.samples, self.pool.available, strict=False))

        return _build_plate_queue(queue.batches, pairs)


class _VialPlateAssignerTipConfig:
    """Assigns positions to VialQueue samples (tip-plate samplers).

    For Vial mode with Evosep: assigns sequential positions across slots.
    """

    def __init__(
        self,
        sampler: Sampler,
        plate_layout: PlateLayout,
        qc_layout: QCLayoutTip,
    ) -> None:
        self.pool = _PositionPoolTip(sampler, plate_layout, qc_layout)

    @property
    def qc_layout(self) -> QCLayoutTip:
        return self.pool.qc_layout

    def assign(self, queue: VialQueue, *, one_container_per_tray: bool = False) -> PlateQueue:  # noqa: ARG002
        """Transform VialQueue to PlateQueue by assigning positions."""
        n_samples = len(queue.samples)
        if n_samples > len(self.pool.available):
            raise ValueError(f"Not enough positions (need {n_samples}, have {len(self.pool.available)})")
        pairs = list(zip(queue.samples, self.pool.available, strict=False))
        return _build_plate_queue(queue.batches, pairs)


# =============================================================================
# Type Alias and Factory
# =============================================================================


class PlateValidator(Protocol):
    """Protocol for classes that validate PlateQueue positions (Plate mode)."""

    @property
    def qc_layout(self) -> QCLayoutWell | QCLayoutTip: ...

    def assign(self, queue: PlateQueue, *, one_container_per_tray: bool = False) -> PlateQueue:
        """Assign trays and validate positions."""
        ...


class VialAssigner(Protocol):
    """Protocol for classes that assign positions to VialQueue (Vial mode)."""

    @property
    def qc_layout(self) -> QCLayoutWell | QCLayoutTip: ...

    def assign(self, queue: VialQueue, *, one_container_per_tray: bool = False) -> PlateQueue:
        """Assign positions and trays."""
        ...


AssembledSampler = PlateValidator | VialAssigner


def create_assembled_sampler(
    sampler_name: str,
    layout_mode: LayoutMode,
    config: QGConfiguration,
    tech_area: str,
    pattern: QueuePattern,
    plate_layout_name: str,
) -> AssembledSampler:
    """Factory to create correct sampler class based on mode and sampler type.

    Args:
        sampler_name: Sampler name (e.g., "Vanquish", "MClass", "Evosep")
        layout_mode: LayoutMode.VIAL or LayoutMode.PLATE
        config: QGConfiguration with all config data
        tech_area: Technology area (e.g., "Proteomics")
        pattern: Queue pattern (used to resolve QC layout; empty patterns yield no reservations)
        plate_layout_name: Plate layout name (e.g., "Vanquish_54")

    Returns:
        One of 4 AssembledSampler classes based on layout_mode + sampler type
    """
    # Sampler and plate_layout existence validated in QueueParameters.create()
    sampler = config.samplers.get_sampler(sampler_name)
    plate_layout = config.plate_layouts.get_layout(plate_layout_name)
    position_fun = get_position_function(sampler.position_fun)

    # Create QC layout (empty when pattern has no QC references)
    qc_layout = create_qc_layout(
        config, tech_area, pattern, plate_layout.name, sampler_name, position_fun, plate_layout
    )

    # Return correct class based on layout_mode + sampler type
    is_tip = sampler.is_tip
    if layout_mode == LayoutMode.PLATE:
        if is_tip:
            return _PlateValidatorTipConfig(sampler, plate_layout, qc_layout)
        else:
            return _PlateValidatorWellConfig(sampler, plate_layout, qc_layout)
    else:  # vial
        if is_tip:
            return _VialPlateAssignerTipConfig(sampler, plate_layout, qc_layout)
        else:
            return _VialPlateAssignerWellConfig(sampler, plate_layout, qc_layout)
