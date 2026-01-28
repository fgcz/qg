"""Sampler classes for position assignment.

6 sampler classes (3 samplers x 2 container types):
- VanquishVialSampler, VanquishPlateSampler
- MClass48VialSampler, MClass48PlateSampler
- EvosepVialSampler, EvosepPlateSampler

Each sampler handles full position assignment including QC merging.
Uses composition and duck typing (no inheritance hierarchy).

All positions are returned in unified format:
- {"tray": str|int, "grid_position": str|int}
- Grid samplers: tray="Y"|"R"|"B"|"G"|"1"|"2", grid_position="A1"|"B2"|...
- Evosep samplers: tray=1|2|3|4, grid_position=1|2|...|96
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
from qg.params_models import QueueInput

# Type alias for position dict
PositionDict = dict[str, Any]


# =============================================================================
# Protocol and Helper
# =============================================================================


def _merge_positions(
    structure: list[str],
    user_positions: list[PositionDict],
    qc_positions: QCLayoutPattern,
) -> list[PositionDict]:
    """Merge user positions with QC positions based on structure."""
    result = []
    user_idx = 0
    for slot in structure:
        if slot == "default":
            result.append(user_positions[user_idx])
            user_idx += 1
        else:
            result.append(qc_positions.get_position(slot))
    return result


# =============================================================================
# Vanquish Samplers
# =============================================================================


class _VanquishVialSampler_prototype:
    """Vanquish vial - generates positions row-major across plates."""

    def __init__(
        self,
        parent: VanquishConfig,
        container: VanquishVialConfig,
        qc_layout_pattern: QCLayoutPattern,
    ):
        self._parent = parent
        self._container = container
        self._qc_positions = qc_layout_pattern
        self._positions: list[PositionDict] = []

    def get_qc_positions(self, sample_name: str) -> dict[str, str | int]:
        return self._qc_positions.get_position(sample_name)

    def _get_reserved_qc_positions(self) -> set[tuple[str, str]]:
        """Get set of (tray, grid_position) tuples reserved for QC samples."""
        return {
            (str(p["tray"]), str(p["grid_position"]))
            for p in (self._qc_positions.get_position(qc_id) for qc_id in self._qc_positions.positions)
        }

    def _generate_positions_default_samples(self, input_queue: QueueInput) -> list[PositionDict]:
        """Generate n positions row-major across all plates, skipping QC positions."""
        reserved = self._get_reserved_qc_positions()
        n_samples = len(input_queue.get_all_samples())

        # All (tray, grid_position) tuples in row-major order: plates -> rows -> cols
        all_positions = [
            (plate, self._container.grid_position_format.format(row=row, col=col))
            for plate, row, col in product(
                self._parent.plates,
                self._container.sample_rows,
                self._container.cols,
            )
        ]

        # Set difference while preserving order (can't use set() - loses order)
        available = [pos for pos in all_positions if pos not in reserved]

        if len(available) < n_samples:
            raise ValueError(f"Not enough positions available (requested {n_samples})")

        self._positions = [
            {"tray": tray, "grid_position": grid_position} for tray, grid_position in available[:n_samples]
        ]
        return self._positions

    def assign_positions_user_samples(self, input_queue: QueueInput) -> QueueInput:
        self._generate_positions_default_samples(input_queue)
        for index in range(len(self._positions)):
            pos = self._positions[index]
            input_queue.update_sample_position(index, pos["tray"], pos["grid_position"])
        return input_queue


class _VanquishPlateSampler_prototype:
    """Vanquish plate - positions from input samples."""

    def __init__(
        self,
        parent: VanquishConfig,
        container: VanquishPlateConfig,
        qc_layout_pattern: QCLayoutPattern,
    ):
        self._parent = parent
        self._container = container
        self._qc_positions = qc_layout_pattern

    def _get_reserved_qc_positions(self) -> set[tuple[str, str]]:
        """Get set of (tray, grid_position) tuples reserved for QC samples."""
        return {
            (str(p["tray"]), str(p["grid_position"]))
            for p in (self._qc_positions.get_position(qc_id) for qc_id in self._qc_positions.positions)
        }

    def assign_positions_user_samples(self, input_queue: QueueInput) -> QueueInput:
        """Validate that user sample positions don't collide with QC positions."""
        reserved = self._get_reserved_qc_positions()

        # Map position -> sample for error reporting
        samples = input_queue.get_all_samples()
        pos_to_sample = {(str(s.tray or ""), str(s.grid_position or "")): s for s in samples}

        # Set intersection to find collisions
        collisions = set(pos_to_sample.keys()) & reserved
        if collisions:
            pos = next(iter(collisions))
            sample = pos_to_sample[pos]
            raise ValueError(f"Sample '{sample.sample_name}' at {pos[0]}:{pos[1]} conflicts with reserved QC position")

        return input_queue

    def get_qc_positions(self, sample_name: str) -> dict[str, str | int]:
        return self._qc_positions.get_position(sample_name)


# =============================================================================
# MClass48 Samplers
# =============================================================================


class _MClass48VialSampler_prototype:
    """MClass48 vial - generates positions row-major across plates."""

    def __init__(
        self,
        parent: MClass48Config,
        container: MClass48VialConfig,
        qc_layout_pattern: QCLayoutPattern,
    ):
        self._parent = parent
        self._container = container
        self._qc_positions = qc_layout_pattern
        self._positions: list[PositionDict] = []

    def get_qc_positions(self, sample_name: str) -> dict[str, str | int]:
        return self._qc_positions.get_position(sample_name)

    def _get_reserved_qc_positions(self) -> set[tuple[str, str]]:
        """Get set of (tray, grid_position) tuples reserved for QC samples."""
        return {
            (str(p["tray"]), str(p["grid_position"]))
            for p in (self._qc_positions.get_position(qc_id) for qc_id in self._qc_positions.positions)
        }

    def _generate_positions_default_samples(self, input_queue: QueueInput) -> list[PositionDict]:
        """Generate n positions row-major across all plates, skipping QC positions."""
        reserved = self._get_reserved_qc_positions()
        n_samples = len(input_queue.get_all_samples())

        # All (tray, grid_position) tuples in row-major order: plates -> rows -> cols
        # Note: MClass48 uses sample_rows and cols from parent config (not container)
        all_positions = [
            (plate, self._container.grid_position_format.format(row=row, col=col))
            for plate, row, col in product(
                self._parent.plates,
                self._parent.sample_rows,
                self._parent.cols,
            )
        ]

        # Set difference while preserving order (can't use set() - loses order)
        available = [pos for pos in all_positions if pos not in reserved]

        if len(available) < n_samples:
            raise ValueError(f"Not enough positions available (requested {n_samples})")

        self._positions = [
            {"tray": tray, "grid_position": grid_position} for tray, grid_position in available[:n_samples]
        ]
        return self._positions

    def assign_positions_user_samples(self, input_queue: QueueInput) -> QueueInput:
        self._generate_positions_default_samples(input_queue)
        for index in range(len(self._positions)):
            pos = self._positions[index]
            input_queue.update_sample_position(index, pos["tray"], pos["grid_position"])
        return input_queue


class _MClass48PlateSampler_prototype:
    """MClass48 plate - positions from input samples."""

    def __init__(
        self,
        parent: MClass48Config,
        container: MClass48PlateConfig,
        qc_layout_pattern: QCLayoutPattern,
    ):
        self._parent = parent
        self._container = container
        self._qc_positions = qc_layout_pattern

    def _get_reserved_qc_positions(self) -> set[tuple[str, str]]:
        """Get set of (tray, grid_position) tuples reserved for QC samples."""
        return {
            (str(p["tray"]), str(p["grid_position"]))
            for p in (self._qc_positions.get_position(qc_id) for qc_id in self._qc_positions.positions)
        }

    def assign_positions_user_samples(self, input_queue: QueueInput) -> QueueInput:
        """Validate that user sample positions don't collide with QC positions."""
        reserved = self._get_reserved_qc_positions()

        # Map position -> sample for error reporting
        samples = input_queue.get_all_samples()
        pos_to_sample = {(str(s.tray or ""), str(s.grid_position or "")): s for s in samples}

        # Set intersection to find collisions
        collisions = set(pos_to_sample.keys()) & reserved
        if collisions:
            pos = next(iter(collisions))
            sample = pos_to_sample[pos]
            raise ValueError(f"Sample '{sample.sample_name}' at {pos[0]}:{pos[1]} conflicts with reserved QC position")

        return input_queue

    def get_qc_positions(self, sample_name: str) -> dict[str, str | int]:
        return self._qc_positions.get_position(sample_name)


# =============================================================================
# Evosep Samplers
# =============================================================================


def _grid_to_number(grid_pos: str, cols: int = 12) -> int:
    """Convert grid position like 'A1' to numeric position (1-based).

    Standard 96-well plate layout: A1=1, A2=2, ..., A12=12, B1=13, ...
    """
    if not grid_pos:
        return 0
    grid_pos = grid_pos.strip().upper()
    if not grid_pos:
        return 0
    row_letter = grid_pos[0]
    col_str = grid_pos[1:]
    try:
        row_num = ord(row_letter) - ord("A")  # A=0, B=1, ...
        col_num = int(col_str)  # 1-based
        return row_num * cols + col_num
    except (ValueError, IndexError):
        return 0


class _EvosepVialSampler_prototype:
    """Evosep vial - generates positions sequentially across slots."""

    def __init__(
        self,
        parent: EvosepConfig,
        container: EvosepVialConfig,
        qc_layout_pattern: QCLayoutPattern,
    ):
        self._parent = parent
        self._container = container
        self._qc_positions = qc_layout_pattern
        self._positions: list[PositionDict] = []

    def get_qc_positions(self, sample_name: str) -> dict[str, str | int]:
        return self._qc_positions.get_position(sample_name)

    def _generate_positions_default_samples(self, input_queue: QueueInput) -> list[PositionDict]:
        """Generate n positions sequentially across slots.

        Evosep uses slots 1-4 for user samples (96 positions each).
        QC samples are on separate trays (5, 6), so no collision possible.
        Returns: list of {"tray": slot, "grid_position": 1-96}
        """
        n_samples = len(input_queue.get_all_samples())

        # All (slot, position) tuples in sequential order: slots -> positions 1-96
        all_positions = [
            (slot, pos) for slot in self._parent.slots for pos in range(1, self._parent.positions_per_slot + 1)
        ]

        if len(all_positions) < n_samples:
            raise ValueError(f"Not enough positions available (requested {n_samples})")

        self._positions = [{"tray": slot, "grid_position": position} for slot, position in all_positions[:n_samples]]
        return self._positions

    def assign_positions_user_samples(self, input_queue: QueueInput) -> QueueInput:
        self._generate_positions_default_samples(input_queue)
        for index in range(len(self._positions)):
            pos = self._positions[index]
            input_queue.update_sample_position(index, pos["tray"], pos["grid_position"])
        return input_queue


class _EvosepPlateSampler_prototype:
    """Evosep plate - positions from input samples."""

    def __init__(
        self,
        parent: EvosepConfig,
        container: EvosepPlateConfig,
        qc_layout_pattern: QCLayoutPattern,
    ):
        self._parent = parent
        self._container = container
        self._qc_positions = qc_layout_pattern

    def assign_positions_user_samples(self, input_queue: QueueInput) -> QueueInput:
        """Convert grid positions to numeric and validate.

        For Evosep plate mode, grid_position (e.g., "A1") is converted to numeric (1-96).
        QC samples are on separate trays (5, 6), so no collision check needed.
        """
        samples = input_queue.get_all_samples()

        for index, sample in enumerate(samples):
            grid_pos = sample.grid_position or ""
            # Convert grid position to numeric if needed
            if self._container.grid_position_conversion in ("grid_to_number", "row_major"):
                numeric_pos = _grid_to_number(str(grid_pos))
            else:
                # Direct numeric: try to parse as int, fallback to 0
                try:
                    numeric_pos = int(grid_pos) if grid_pos else 0
                except ValueError:
                    numeric_pos = 0
            # Default to tray 1 for plate mode
            tray = sample.tray if sample.tray is not None else 1
            input_queue.update_sample_position(index, tray, numeric_pos)

        return input_queue

    def get_qc_positions(self, sample_name: str) -> dict[str, str | int]:
        return self._qc_positions.get_position(sample_name)


Sampler = (
    _VanquishVialSampler_prototype
    | _VanquishPlateSampler_prototype
    | _MClass48VialSampler_prototype
    | _MClass48PlateSampler_prototype
    | _EvosepVialSampler_prototype
    | _EvosepPlateSampler_prototype
)


def create_sampler(
    sampler_name: str,
    layout_mode: str,
    config: SamplersConfig,
    qc_layout_pattern: QCLayoutPattern,
) -> Sampler:
    """Factory: sampler_name + layout_mode + config -> Sampler instance.

    Args:
        sampler_name: Sampler name (e.g., "Vanquish", "MClass48", "Evosep")
        layout_mode: Layout mode ("vial" or "plate")
        config: Root samplers configuration from sampler.toml
        qc_layout_pattern: Validated QC layout for the pattern

    Returns:
        Sampler instance for the given name and layout mode

    Raises:
        ValueError: If sampler name or layout mode is unknown
    """
    match (sampler_name, layout_mode):
        case ("Vanquish", "vial"):
            return _VanquishVialSampler_prototype(config.Vanquish, config.Vanquish.vial, qc_layout_pattern)
        case ("Vanquish", "plate"):
            return _VanquishPlateSampler_prototype(config.Vanquish, config.Vanquish.plate, qc_layout_pattern)
        case ("MClass48", "vial"):
            return _MClass48VialSampler_prototype(config.MClass48, config.MClass48.vial, qc_layout_pattern)
        case ("MClass48", "plate"):
            return _MClass48PlateSampler_prototype(config.MClass48, config.MClass48.plate, qc_layout_pattern)
        case ("Evosep", "vial"):
            return _EvosepVialSampler_prototype(config.Evosep, config.Evosep.vial, qc_layout_pattern)
        case ("Evosep", "plate"):
            return _EvosepPlateSampler_prototype(config.Evosep, config.Evosep.plate, qc_layout_pattern)
        case _:
            raise ValueError(f"Unknown sampler: {sampler_name}.{layout_mode}")


class SamplerStrategy:
    """Strategy for assigning positions to samples in a queue.

    Usage (two-phase position assignment):
        1. Call assign_positions_user_samples() BEFORE generator.build_rows()
           - Assigns physical positions (tray, grid_position) to user samples
           - For vial mode: generates positions row-major (A1, A2, ...)
           - For plate mode: validates input positions don't conflict with QC

        2. Generator calls assign_positions_qc_samples() during build_rows()
           - Returns position list for full queue structure (user + QC)
           - User positions come from samples (already assigned in step 1)
           - QC positions come from qc_layout_pattern
    """

    sampler: Sampler

    def __init__(self, sampler_name: str, layout_mode: str, config: SamplersConfig, qc_layout_pattern: QCLayoutPattern):
        self.sampler = create_sampler(sampler_name, layout_mode, config, qc_layout_pattern)

    def assign_positions_user_samples(self, queue_input: QueueInput) -> QueueInput:
        """Assign physical positions to user samples. Call BEFORE generator.build_rows().

        For vial mode: Generates positions row-major across plates, skipping QC positions.
        For plate mode: Validates that input positions don't conflict with QC positions.

        Args:
            queue_input: QueueInput with samples to assign positions to.

        Returns:
            Same QueueInput with sample positions updated (mutates in place).
        """
        self.sampler.assign_positions_user_samples(queue_input)
        return queue_input

    def assign_positions_qc_samples(self, structure: list[str], queue_input: QueueInput) -> list[PositionDict]:
        """Assign positions to all slots in queue structure.

        Args:
            structure: List of sample IDs (e.g., ["QC01", "default", "default", "QC02"]).
                       "default" slots get positions from queue_input samples.
                       Other slots get QC positions from the sampler.
            queue_input: QueueInput with samples already having positions assigned.

        Returns:
            List of position dicts in structure order.
        """
        positions: list[PositionDict] = []
        user_idx = 0
        for sample_id in structure:
            if sample_id == "default":
                sample = queue_input.get_sample_by_index(user_idx)
                positions.append({"tray": sample.tray, "grid_position": sample.grid_position})
                user_idx += 1
            else:
                position = self.sampler.get_qc_positions(sample_id)
                positions.append(position)
        return positions
