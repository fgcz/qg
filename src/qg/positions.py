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

from collections.abc import Sequence
from typing import Any, Protocol

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
from qg.params_models import InputSample

# Type alias for position dict
PositionDict = dict[str, Any]


# =============================================================================
# Protocol and Helper
# =============================================================================


class SamplerProtocol(Protocol):
    """Duck-typed interface for all samplers."""

    def assign_positions(
        self,
        structure: list[str],
        samples: Sequence[InputSample],
    ) -> list[PositionDict]:
        """Assign positions to all slots (user samples merged with QC).

        Args:
            structure: List of sample IDs ("default" for user, QC IDs for QC)
            samples: Input samples (used for input-based position extraction)

        Returns:
            List of position dicts, same length as structure
        """
        ...


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
    ) -> list[PositionDict]:
        num_user = structure.count("default")
        user_positions = self._generate_positions(num_user)
        return _merge_positions(structure, user_positions, self._qc_positions)

    def _generate_positions(self, n: int) -> list[PositionDict]:
        """Generate n positions row-major across all plates.

        Note: Sample rows (A-E) don't overlap with QC row (F), so we can
        safely use all plates including the one where QC samples are located.

        Returns unified format: {"tray": plate, "grid_position": "A1"}
        """
        positions = []
        plate_idx, row_idx, col_idx = 0, 0, 0

        for _ in range(n):
            if plate_idx >= len(self._parent.plates):
                raise ValueError(f"Not enough positions available (requested {n})")

            row = self._container.sample_rows[row_idx]
            col = self._container.cols[col_idx]
            grid_position = self._container.grid_position_format.format(row=row, col=col)

            positions.append({
                "tray": self._parent.plates[plate_idx],
                "grid_position": grid_position,
            })

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
    ) -> list[PositionDict]:
        num_user = structure.count("default")
        if len(samples) < num_user:
            raise ValueError(
                f"Not enough input samples ({len(samples)}) for {num_user} positions"
            )
        # For plate mode, grid_position comes from input samples
        # Unified format: {"tray": plate, "grid_position": from_input}
        user_positions = [
            {"tray": "Y", "grid_position": s.grid_position or ""}
            for s in samples[:num_user]
        ]
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
    ) -> list[PositionDict]:
        num_user = structure.count("default")
        user_positions = self._generate_positions(num_user)
        return _merge_positions(structure, user_positions, self._qc_positions)

    def _generate_positions(self, n: int) -> list[PositionDict]:
        """Generate n positions row-major across all plates.

        Uses sample_rows and cols from parent config.
        Returns unified format: {"tray": plate, "grid_position": "A1"}
        """
        positions = []
        plate_idx, row_idx, col_idx = 0, 0, 0

        for _ in range(n):
            if plate_idx >= len(self._parent.plates):
                raise ValueError(f"Not enough positions available (requested {n})")

            row = self._parent.sample_rows[row_idx]
            col = self._parent.cols[col_idx]
            grid_position = self._container.grid_position_format.format(row=row, col=col)

            positions.append({
                "tray": self._parent.plates[plate_idx],
                "grid_position": grid_position,
            })

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
    ) -> list[PositionDict]:
        num_user = structure.count("default")
        if len(samples) < num_user:
            raise ValueError(
                f"Not enough input samples ({len(samples)}) for {num_user} positions"
            )
        # For plate mode, grid_position comes from input samples
        # Unified format: {"tray": plate, "grid_position": from_input}
        user_positions = [
            {"tray": "1", "grid_position": s.grid_position or ""}
            for s in samples[:num_user]
        ]
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
    ) -> list[PositionDict]:
        num_user = structure.count("default")
        user_positions = self._generate_positions(num_user)
        return _merge_positions(structure, user_positions, self._qc_positions)

    def _generate_positions(self, n: int) -> list[PositionDict]:
        """Generate n positions sequentially across slots.

        Returns unified format: {"tray": slot, "grid_position": 1-96}
        """
        positions = []
        slot_idx = 0
        position_in_slot = 1

        for _ in range(n):
            if slot_idx >= len(self._parent.slots):
                raise ValueError(f"Not enough positions available (requested {n})")

            slot = self._parent.slots[slot_idx]
            positions.append({
                "tray": slot,
                "grid_position": position_in_slot,
            })

            # Advance
            position_in_slot += 1
            if position_in_slot > self._parent.positions_per_slot:
                position_in_slot = 1
                slot_idx += 1

        return positions


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
    ) -> list[PositionDict]:
        num_user = structure.count("default")
        if len(samples) < num_user:
            raise ValueError(
                f"Not enough input samples ({len(samples)}) for {num_user} positions"
            )
        # For plate mode, convert grid_position (e.g., "A1") to numeric
        # Unified format: {"tray": slot, "grid_position": 1-96}
        user_positions = []
        for s in samples[:num_user]:
            grid_pos = s.grid_position or ""
            if self._container.grid_position_conversion == "grid_to_number":
                numeric_pos = _grid_to_number(grid_pos)
            else:
                # Try to parse as int, fallback to 0
                try:
                    numeric_pos = int(grid_pos) if grid_pos else 0
                except ValueError:
                    numeric_pos = 0
            user_positions.append({
                "tray": 1,  # Default to tray 1 for plate mode
                "grid_position": numeric_pos,
            })
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
