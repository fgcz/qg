"""Sampler classes for position assignment.

6 sampler classes (3 samplers x 2 container types):
- VanquishVialSampler, VanquishPlateSampler
- MClass48VialSampler, MClass48PlateSampler
- EvosepVialSampler, EvosepPlateSampler

Each sampler handles full position assignment including QC merging.
Uses composition and duck typing (no inheritance hierarchy).
"""

from dataclasses import dataclass
from typing import Protocol, Sequence

from qg.config_models import QCPosition, QueuePattern, EvosepPosition
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
from qg.params_models import InputSample


# =============================================================================
# QC Layout Pattern (Validated)
# =============================================================================


def _format_qc_position(pos: QCPosition) -> str:
    """Format a QC position as a string."""
    if isinstance(pos, str):
        return pos
    elif isinstance(pos, EvosepPosition):
        return f"tray{pos.tray}:{pos.position_start}"
    elif isinstance(pos, dict):
        # Handle raw dict (from test fixtures)
        return f"tray{pos.get('tray', 0)}:{pos.get('position_start', 0)}"
    else:
        return str(pos)


def _collect_pattern_qc_ids(pattern: QueuePattern) -> set[str]:
    """Collect all unique QC sample IDs from a pattern."""
    qc_ids: set[str] = set()
    qc_ids.update(pattern.start)
    qc_ids.update(pattern.middle)
    qc_ids.update(pattern.end)
    if pattern.separation:
        qc_ids.update(pattern.separation)
    if pattern.middle_extended:
        qc_ids.update(pattern.middle_extended)
    return qc_ids


def _validate_unique_positions(positions: dict[str, str]) -> None:
    """Validate that all positions are unique.

    Raises:
        ValueError: If two different QC samples share the same position
    """
    seen: dict[str, str] = {}  # position -> qc_id
    for qc_id, pos in positions.items():
        if pos in seen:
            raise ValueError(
                f"Position conflict: '{qc_id}' and '{seen[pos]}' "
                f"both map to position '{pos}'"
            )
        seen[pos] = qc_id


@dataclass(frozen=True)
class QCLayoutPattern:
    """Validated QC layout for a specific queue pattern.

    Ensures that QC samples used in the pattern have unique positions.
    """

    positions: dict[str, str]  # QC sample ID -> formatted position string

    @classmethod
    def create(
        cls,
        pattern: QueuePattern,
        qc_layout: dict[str, QCPosition],
    ) -> "QCLayoutPattern":
        """Create and validate QC layout for a pattern.

        Args:
            pattern: Queue pattern with QC sample IDs
            qc_layout: Raw QC positions from config

        Returns:
            Validated QCLayoutPattern

        Raises:
            ValueError: If a QC sample is missing from qc_layout or positions conflict
        """
        # 1. Collect unique QC IDs from pattern
        qc_ids = _collect_pattern_qc_ids(pattern)

        # 2. Map to positions
        positions: dict[str, str] = {}
        for qc_id in qc_ids:
            if qc_id not in qc_layout:
                raise ValueError(f"QC sample '{qc_id}' not in qc_layout")
            positions[qc_id] = _format_qc_position(qc_layout[qc_id])

        # 3. Validate uniqueness
        _validate_unique_positions(positions)

        return cls(positions=positions)

    def get_position(self, qc_id: str) -> str:
        """Get position for a QC sample ID."""
        return self.positions.get(qc_id, "")


# =============================================================================
# Protocol and Helper
# =============================================================================


class SamplerProtocol(Protocol):
    """Duck-typed interface for all samplers."""

    def assign_positions(
        self,
        structure: list[str],
        samples: Sequence[InputSample],
    ) -> list[str]:
        """Assign positions to all slots (user samples merged with QC).

        Args:
            structure: List of sample IDs ("default" for user, QC IDs for QC)
            samples: Input samples (used for input-based position extraction)

        Returns:
            List of position strings, same length as structure
        """
        ...


def _merge_positions(
    structure: list[str],
    user_positions: list[str],
    qc_positions: QCLayoutPattern,
) -> list[str]:
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



class VanquishVialSampler:
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

    def assign_positions(
        self,
        structure: list[str],
        samples: Sequence[InputSample],
    ) -> list[str]:
        num_user = structure.count("default")
        user_positions = self._generate_positions(num_user)
        return _merge_positions(structure, user_positions, self._qc_positions)

    def _generate_positions(self, n: int) -> list[str]:
        """Generate n positions row-major across all plates.

        Note: Sample rows (A-E) don't overlap with QC row (F), so we can
        safely use all plates including the one where QC samples are located.
        """
        positions = []
        plate_idx, row_idx, col_idx = 0, 0, 0

        for _ in range(n):
            if plate_idx >= len(self._parent.plates):
                raise ValueError(f"Not enough positions available (requested {n})")

            pos = self._container.position_format.format(
                plate=self._parent.plates[plate_idx],
                row=self._container.sample_rows[row_idx],
                col=self._container.cols[col_idx],
            )
            positions.append(pos)

            # Advance (row-major order)
            col_idx += 1
            if col_idx >= len(self._container.cols):
                col_idx = 0
                row_idx += 1
                if row_idx >= len(self._container.sample_rows):
                    row_idx = 0
                    plate_idx += 1

        return positions


class VanquishPlateSampler:
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

    def assign_positions(
        self,
        structure: list[str],
        samples: Sequence[InputSample],
    ) -> list[str]:
        num_user = structure.count("default")
        if len(samples) < num_user:
            raise ValueError(
                f"Not enough input samples ({len(samples)}) for {num_user} positions"
            )
        user_positions = [s.grid_position or "" for s in samples[:num_user]]
        return _merge_positions(structure, user_positions, self._qc_positions)


# =============================================================================
# MClass48 Samplers
# =============================================================================


class MClass48VialSampler:
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

    def assign_positions(
        self,
        structure: list[str],
        samples: Sequence[InputSample],
    ) -> list[str]:
        num_user = structure.count("default")
        user_positions = self._generate_positions(num_user)
        return _merge_positions(structure, user_positions, self._qc_positions)

    def _generate_positions(self, n: int) -> list[str]:
        """Generate n positions row-major across all plates.

        Uses sample_rows and cols from parent config.
        """
        positions = []
        plate_idx, row_idx, col_idx = 0, 0, 0

        for _ in range(n):
            if plate_idx >= len(self._parent.plates):
                raise ValueError(f"Not enough positions available (requested {n})")

            pos = self._container.position_format.format(
                plate=self._parent.plates[plate_idx],
                row=self._parent.sample_rows[row_idx],
                col=self._parent.cols[col_idx],
            )
            positions.append(pos)

            # Advance (row-major order)
            col_idx += 1
            if col_idx >= len(self._parent.cols):
                col_idx = 0
                row_idx += 1
                if row_idx >= len(self._parent.sample_rows):
                    row_idx = 0
                    plate_idx += 1

        return positions


class MClass48PlateSampler:
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

    def assign_positions(
        self,
        structure: list[str],
        samples: Sequence[InputSample],
    ) -> list[str]:
        num_user = structure.count("default")
        if len(samples) < num_user:
            raise ValueError(
                f"Not enough input samples ({len(samples)}) for {num_user} positions"
            )
        user_positions = [s.grid_position or "" for s in samples[:num_user]]
        return _merge_positions(structure, user_positions, self._qc_positions)


# =============================================================================
# Evosep Samplers
# =============================================================================


class EvosepVialSampler:
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

    def assign_positions(
        self,
        structure: list[str],
        samples: Sequence[InputSample],
    ) -> list[str]:
        num_user = structure.count("default")
        user_positions = self._generate_positions(num_user)
        return _merge_positions(structure, user_positions, self._qc_positions)

    def _generate_positions(self, n: int) -> list[str]:
        """Generate n positions sequentially across slots.

        Format: "tray{slot}:{position}" (1-indexed positions).
        """
        positions = []
        slot_idx = 0
        position_in_slot = 1

        for _ in range(n):
            if slot_idx >= len(self._parent.slots):
                raise ValueError(f"Not enough positions available (requested {n})")

            slot = self._parent.slots[slot_idx]
            positions.append(f"tray{slot}:{position_in_slot}")

            # Advance
            position_in_slot += 1
            if position_in_slot > self._parent.positions_per_slot:
                position_in_slot = 1
                slot_idx += 1

        return positions


class EvosepPlateSampler:
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

    def assign_positions(
        self,
        structure: list[str],
        samples: Sequence[InputSample],
    ) -> list[str]:
        num_user = structure.count("default")
        if len(samples) < num_user:
            raise ValueError(
                f"Not enough input samples ({len(samples)}) for {num_user} positions"
            )
        user_positions = [s.grid_position or "" for s in samples[:num_user]]
        return _merge_positions(structure, user_positions, self._qc_positions)


# =============================================================================
# Type Alias and Factory
# =============================================================================


Sampler = (
    VanquishVialSampler
    | VanquishPlateSampler
    | MClass48VialSampler
    | MClass48PlateSampler
    | EvosepVialSampler
    | EvosepPlateSampler
)


def create_sampler(
    sampler_name: str,
    config: SamplersConfig,
    qc_layout_pattern: QCLayoutPattern,
) -> Sampler:
    """Factory: 'Vanquish.vial' + config + qc_layout_pattern -> Sampler instance.

    Args:
        sampler_name: Sampler identifier like "Vanquish.vial" or "Evosep.plate"
        config: Root samplers configuration from sampler.toml
        qc_layout_pattern: Validated QC layout for the pattern

    Returns:
        Sampler instance for the given name

    Raises:
        ValueError: If sampler name is unknown
    """
    match sampler_name:
        case "Vanquish.vial":
            return VanquishVialSampler(
                config.Vanquish, config.Vanquish.vial, qc_layout_pattern
            )
        case "Vanquish.plate":
            return VanquishPlateSampler(
                config.Vanquish, config.Vanquish.plate, qc_layout_pattern
            )
        case "MClass48.vial":
            return MClass48VialSampler(
                config.MClass48, config.MClass48.vial, qc_layout_pattern
            )
        case "MClass48.plate":
            return MClass48PlateSampler(
                config.MClass48, config.MClass48.plate, qc_layout_pattern
            )
        case "Evosep.vial":
            return EvosepVialSampler(
                config.Evosep, config.Evosep.vial, qc_layout_pattern
            )
        case "Evosep.plate":
            return EvosepPlateSampler(
                config.Evosep, config.Evosep.plate, qc_layout_pattern
            )
        case _:
            raise ValueError(f"Unknown sampler: {sampler_name}")
