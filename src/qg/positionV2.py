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
from qg.params_models import (
    Plate,
    PlateCell,
    PlateQueue,
    PlateQueueInput,
    PositionedQueueInput,
    QueueInput,
    QueueParameters,
    VialQueue,
    VialQueueInput,
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


def _trays_with_start_first(
    trays: list[str] | list[int],
    start_tray: str | int | None,
) -> list[str] | list[int]:
    """Return ``trays`` reordered so that ``start_tray`` comes first.

    The GUI emits string-valued trays (``mo.ui.dropdown`` options are strings)
    while tip samplers store integer trays — match leniently.
    """
    if start_tray is None or start_tray == "":
        return list(trays)
    for t in trays:
        if t == start_tray or str(t) == str(start_tray):
            return [t] + [other for other in trays if other != t]
    raise ValueError(f"start_tray={start_tray!r} not in available trays {trays}")


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
    plate_id_for_tray: dict[str | int, int] = {}

    for sample, pos in pairs:
        plate_id = plate_id_for_tray.setdefault(pos.tray, len(plate_id_for_tray) + 1)
        tray_for_plate[plate_id] = pos.tray
        cells.append(
            PlateCell(
                sample=sample,
                grid_position=pos.grid_position,
                plate_id=plate_id,
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
        start_position: str = "A1",
        start_tray: str | int | None = None,
    ) -> None:
        self.position_fun = get_position_function(sampler.position_fun)
        self.trays = sampler.trays

        # Store precomputed QC layout and its reserved positions
        self.qc_layout = qc_layout
        self.reserved = qc_layout.reserved

        # Resolve start_tray: default to first tray
        effective_start_tray = start_tray if start_tray is not None else sampler.trays[0]

        # Generate all positions and filter out reserved
        self.all_positions = generate_all_positions(
            sampler.trays, plate_layout.rows, plate_layout.cols, self.position_fun
        )
        self.available = [p for p in self.all_positions if p not in self.reserved]

        # Filter: skip trays before start_tray, apply start_position offset on start_tray
        tray_strs = [str(t) for t in sampler.trays]
        start_tray_idx = tray_strs.index(str(effective_start_tray))
        start_flat = plate_layout.alpha_to_flat(start_position)
        self.available = [
            p
            for p in self.available
            if tray_strs.index(str(p.tray)) > start_tray_idx
            or (
                tray_strs.index(str(p.tray)) == start_tray_idx
                and plate_layout.alpha_to_flat(p.grid_position) >= start_flat
            )
        ]

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
        start_position: str = "A1",
        start_tray: str | int | None = None,
    ) -> None:
        self.position_fun = get_position_function(sampler.position_fun)
        self.trays = sampler.trays

        # Store precomputed QC layout and its reserved positions
        self.qc_layout = qc_layout
        self.reserved = qc_layout.reserved

        # Resolve start_tray: default to first tray
        effective_start_tray = start_tray if start_tray is not None else sampler.trays[0]

        # Generate all positions (don't filter - Evosep uses consumable tips)
        self.all_positions = generate_all_positions(
            sampler.trays, plate_layout.rows, plate_layout.cols, self.position_fun
        )
        self.available = self.all_positions

        # Filter: skip trays before start_tray, apply start_position offset on start_tray
        tray_strs = [str(t) for t in sampler.trays]
        start_tray_idx = tray_strs.index(str(effective_start_tray))
        start_flat = plate_layout.alpha_to_flat(start_position)
        self.available = [
            p
            for p in self.available
            if tray_strs.index(str(p.tray)) > start_tray_idx
            or (
                tray_strs.index(str(p.tray)) == start_tray_idx
                and plate_layout.alpha_to_flat(p.grid_position) >= start_flat
            )
        ]

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
        start_tray: str | int | None = None,
    ) -> None:
        self.pool = _PositionPoolWell(sampler, plate_layout, qc_layout)
        self.plate_layout = plate_layout
        self._start_tray = start_tray

    @property
    def qc_layout(self) -> QCLayoutWell:
        return self.pool.qc_layout

    def _check_collisions(self, queue: PlateQueue) -> None:
        """Validate each well against the layout and reject QC-position conflicts."""
        for cell in queue.cells:
            plate = queue.plates.get(cell.plate_id)
            tray = plate.tray if plate else None
            row, col = self.plate_layout.split_alpha(cell.grid_position)
            pos = Position(tray, cell.grid_position, row=row, col=col)
            if pos in self.pool.reserved:
                raise ValueError(
                    f"Sample '{cell.sample.sample_name}' at {tray}:{cell.grid_position} conflicts with QC position"
                )

    def assign(self, queue: PlateQueue, *, one_container_per_tray: bool = False) -> PlateQueue:  # noqa: ARG002
        """Assign trays to plates and validate positions against QC reservations."""
        trays = _trays_with_start_first(self.pool.trays, self._start_tray)
        queue = _assign_trays_if_missing(queue, trays)
        self._check_collisions(queue)
        return queue


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
        start_tray: str | int | None = None,
    ) -> None:
        self.pool = _PositionPoolTip(sampler, plate_layout, qc_layout)
        self.plate_layout = plate_layout
        self._start_tray = start_tray

    @property
    def qc_layout(self) -> QCLayoutTip:
        return self.pool.qc_layout

    def assign(self, queue: PlateQueue, *, one_container_per_tray: bool = False) -> PlateQueue:  # noqa: ARG002
        """Assign trays to plates and validate every well against the layout.

        Tip plates have no QC-collision pass, so wells are validated here directly.
        """
        trays = _trays_with_start_first(self.pool.trays, self._start_tray)
        queue = _assign_trays_if_missing(queue, trays)
        for cell in queue.cells:
            self.plate_layout.split_alpha(cell.grid_position)
        return queue


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
        start_position: str = "A1",
        start_tray: str | int | None = None,
    ) -> None:
        self.pool = _PositionPoolWell(sampler, plate_layout, qc_layout, start_position, start_tray)

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
        start_position: str = "A1",
        start_tray: str | int | None = None,
    ) -> None:
        self.pool = _PositionPoolTip(sampler, plate_layout, qc_layout, start_position, start_tray)

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
    pattern_sample_ids: set[str],
    plate_layout_name: str,
    qc_layout_name: str,
    start_position: str = "A1",
    start_tray: str | int | None = None,
) -> AssembledSampler:
    """Factory to create correct sampler class based on mode and sampler type.

    Args:
        sampler_name: Sampler name (e.g., "Vanquish", "MClass", "Evosep")
        layout_mode: LayoutMode.VIAL or LayoutMode.PLATE
        config: QGConfiguration with all config data
        tech_area: Technology area (e.g., "Proteomics")
        pattern_sample_ids: Sample IDs referenced by the pattern (empty for noqc)
        plate_layout_name: Plate layout name (e.g., "Vanquish_54")
        qc_layout_name: QC layout name (e.g., "standard", "evosep_qc")
        start_position: Alpha grid position to start from on start tray (vial mode only)
        start_tray: Tray to start from. None = first tray. In vial mode it controls
            where vial assignment begins; in plate mode it relocates the user's plate
            to the chosen tray (used to escape QC-layout collisions on the default tray).

    Returns:
        One of 4 AssembledSampler classes based on layout_mode + sampler type
    """
    # Sampler and plate_layout existence validated in QueueParameters.create()
    sampler = config.samplers.get_sampler(sampler_name)
    plate_layout = config.plate_layouts.get_layout(plate_layout_name)
    position_fun = get_position_function(sampler.position_fun)

    # Create QC layout (empty when pattern has no QC references)
    qc_layout = create_qc_layout(
        config,
        tech_area,
        qc_layout_name,
        pattern_sample_ids,
        plate_layout.name,
        sampler_name,
        position_fun,
        plate_layout,
    )

    # Return correct class based on layout_mode + sampler type
    is_tip = sampler.is_tip
    if layout_mode == LayoutMode.PLATE:
        if is_tip:
            return _PlateValidatorTipConfig(sampler, plate_layout, qc_layout, start_tray)
        else:
            return _PlateValidatorWellConfig(sampler, plate_layout, qc_layout, start_tray)
    else:  # vial
        if is_tip:
            return _VialPlateAssignerTipConfig(sampler, plate_layout, qc_layout, start_position, start_tray)
        else:
            return _VialPlateAssignerWellConfig(sampler, plate_layout, qc_layout, start_position, start_tray)


def _create_input_sampler(
    config: QGConfiguration,
    parameters: QueueParameters,
    layout_mode: LayoutMode,
) -> AssembledSampler:
    """Create the configured sampler used to position a queue input."""
    pattern = config.queue_patterns.get_pattern(
        parameters.tech_area,
        parameters.queue_pattern,
    )
    sampler = config.samplers.get_sampler(parameters.sampler)
    start_tray = parameters.start_tray if parameters.start_tray != "" else sampler.trays[0]
    return create_assembled_sampler(
        sampler_name=parameters.sampler,
        layout_mode=layout_mode,
        config=config,
        tech_area=parameters.tech_area,
        pattern_sample_ids=pattern.get_all_sample_ids(),
        plate_layout_name=parameters.plate_layout,
        qc_layout_name=parameters.qc_layout_name,
        start_position=parameters.start_position,
        start_tray=start_tray,
    )


def _positioned(queue_input: QueueInput, plate_queue: PlateQueue) -> PositionedQueueInput:
    """Wrap a positioned plate queue, retaining the source provenance."""
    return PositionedQueueInput(
        parameters=queue_input.parameters,
        queue=plate_queue,
        qg_version=queue_input.qg_version,
        resolved_config=queue_input.resolved_config,
    )


def _position_vial_queue(queue_input: VialQueueInput) -> PositionedQueueInput:
    """Assign physical positions for a vial queue input.

    The vial assigner draws positions from a pool that already excludes reserved
    QC positions and fills in row/col, so its output is generation-ready without a
    separate plate-validation pass.
    """
    config = queue_input.resolved_config.to_configuration()
    assigner = _create_input_sampler(config, queue_input.parameters, LayoutMode.VIAL)
    plate_queue = assigner.assign(
        queue_input.queue,
        one_container_per_tray=queue_input.parameters.one_container_per_tray,
    )
    return _positioned(queue_input, plate_queue)


def _position_plate_queue(queue_input: PlateQueueInput) -> PositionedQueueInput:
    """Validate physical positions for a plate queue input.

    User-supplied plate positions still need collision-checking against QC
    reservations, tray assignment, and the alpha grid_position -> row/col split.
    """
    config = queue_input.resolved_config.to_configuration()
    validator = _create_input_sampler(config, queue_input.parameters, LayoutMode.PLATE)
    validated_queue = validator.assign(queue_input.queue)
    return _positioned(queue_input, validated_queue)
