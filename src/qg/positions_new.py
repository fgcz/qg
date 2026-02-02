# =============================================================================
# Position Generation (Layer 2: Domain Logic - Pure Python)
# =============================================================================
#
# This module implements position generation using pure functions and minimal
# state classes. No Pydantic/dataclasses - this is the algorithmic layer.
#
# Architecture:
#   - Position functions: Registry of (row, col) -> position formatters
#   - generate_all_positions(): Pure function for grid enumeration
#   - QCLayoutGrid: Stateless - fixed well positions
#   - QCLayoutEvosep: Stateful - consumable tips with counters
#   - assign_user_positions(): Pure function for position assignment
#   - PositionGenerator: Orchestrator class
#

from __future__ import annotations

from collections.abc import Callable
from itertools import product
from typing import TYPE_CHECKING, Protocol

if TYPE_CHECKING:
    from qg.config_models_new.positions import (
        PlateLayout,
        QCSampleEvosep,
        QCSampleGrid,
        Sampler,
    )

# =============================================================================
# Position Functions (Registry)
# =============================================================================


def string_concat(row: str | int, col: int) -> str:
    """Grid samplers: 'A' + 1 → 'A1'."""
    return f"{row}{col}"


def int_add(row: int, col: int) -> int:
    """Evosep: row + col → position (1-indexed offset)."""
    return row + col


POSITION_FUNCTIONS: dict[str, Callable[[str | int, int], str | int]] = {
    "string_concat": string_concat,
    "int_add": int_add,
}


def get_position_function(name: str) -> Callable[[str | int, int], str | int]:
    """Get a position function by name.

    Args:
        name: Function name ("string_concat" or "int_add")

    Returns:
        The position function

    Raises:
        KeyError: If function name is not found
    """
    if name not in POSITION_FUNCTIONS:
        raise KeyError(f"Unknown position function: {name!r}. Available: {list(POSITION_FUNCTIONS.keys())}")
    return POSITION_FUNCTIONS[name]


# =============================================================================
# Position Generation (Pure Function)
# =============================================================================


def generate_all_positions(
    trays: list[str] | list[int],
    rows: list[str] | list[int],
    cols: list[int],
    position_fun: Callable[[str | int, int], str | int],
) -> list[tuple[str | int, str | int]]:
    """Generate all (tray, position) tuples using itertools.product.

    Args:
        trays: List of tray identifiers (e.g., ["Y", "R"] or [1, 2, 3])
        rows: List of row identifiers (e.g., ["A", "B", ...] or [1, 2, ...])
        cols: List of column identifiers (e.g., [1, 2, 3, ...])
        position_fun: Function to format (row, col) into position

    Returns:
        List of (tray, position) tuples in enumeration order
    """
    return [(tray, position_fun(row, col)) for tray, row, col in product(trays, rows, cols)]


# =============================================================================
# Format Position
# =============================================================================


def format_position(tray: str | int, position: str | int) -> str:
    """Format a (tray, position) tuple as a string for output.

    Args:
        tray: Tray identifier
        position: Position within tray

    Returns:
        Formatted position string (e.g., "Y:A1" or "1:42")
    """
    return f"{tray}:{position}"


# =============================================================================
# QC Layout Protocol
# =============================================================================


class QCLayoutProtocol(Protocol):
    """Protocol for QC layout classes (duck typing)."""

    def get_position(self, sample_id: str) -> tuple[str | int, str | int]:
        """Get position for a QC sample by ID."""
        ...

    def reserved_positions(self) -> set[tuple[str | int, str | int]]:
        """Get all positions reserved for QC samples."""
        ...


# =============================================================================
# QCLayoutGrid (Stateless)
# =============================================================================


class QCLayoutGrid:
    """Stateless QC layout for grid samplers with fixed well positions.

    Each QC sample has a fixed (plate, row, col) that never changes.
    """

    def __init__(
        self,
        samples: list[QCSampleGrid],
        position_fun: Callable[[str | int, int], str | int],
    ) -> None:
        """Initialize QC layout from config samples.

        Args:
            samples: List of QCSampleGrid from config
            position_fun: Function to format (row, col) into position
        """
        self._positions: dict[str, tuple[str, str | int]] = {
            s.sample_id: (s.plate, position_fun(s.row, s.col)) for s in samples
        }

    def get_position(self, sample_id: str) -> tuple[str, str | int]:
        """Get the fixed position for a QC sample.

        Args:
            sample_id: QC sample identifier

        Returns:
            (plate, position) tuple

        Raises:
            KeyError: If sample_id is not found
        """
        if sample_id not in self._positions:
            raise KeyError(f"Unknown QC sample: {sample_id!r}. Available: {list(self._positions.keys())}")
        return self._positions[sample_id]

    def reserved_positions(self) -> set[tuple[str, str | int]]:
        """Get all positions reserved for QC samples.

        Returns:
            Set of (plate, position) tuples
        """
        return set(self._positions.values())

    @property
    def sample_ids(self) -> list[str]:
        """Get all QC sample IDs."""
        return list(self._positions.keys())


# =============================================================================
# QCLayoutEvosep (Stateful)
# =============================================================================


class QCLayoutEvosep:
    """Stateful QC layout for Evosep with consumable tip positions.

    Each QC sample has a range of positions (tips). Each call to get_position()
    returns the next available position and increments the counter.
    """

    def __init__(self, samples: list[QCSampleEvosep]) -> None:
        """Initialize QC layout from config samples.

        Args:
            samples: List of QCSampleEvosep from config
        """
        self._samples: dict[str, QCSampleEvosep] = {s.sample_id: s for s in samples}
        self._counters: dict[str, int] = {s.sample_id: 0 for s in samples}

    def get_position(self, sample_id: str) -> tuple[int, int]:
        """Get the next position for a QC sample and increment counter.

        Args:
            sample_id: QC sample identifier

        Returns:
            (tray, position) tuple

        Raises:
            KeyError: If sample_id is not found
            ValueError: If position range is exhausted
        """
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
        """Get all positions reserved for QC samples (entire ranges).

        Returns:
            Set of (tray, position) tuples covering all ranges
        """
        return {(s.tray, pos) for s in self._samples.values() for pos in range(s.position_start, s.position_end + 1)}

    @property
    def sample_ids(self) -> list[str]:
        """Get all QC sample IDs."""
        return list(self._samples.keys())

    def remaining(self, sample_id: str) -> int:
        """Get remaining positions for a QC sample.

        Args:
            sample_id: QC sample identifier

        Returns:
            Number of remaining positions
        """
        s = self._samples[sample_id]
        used = self._counters[sample_id]
        total = s.position_end - s.position_start + 1
        return total - used

    def reset(self) -> None:
        """Reset all counters to initial state."""
        for sample_id in self._counters:
            self._counters[sample_id] = 0


# =============================================================================
# Validation Functions (Set Operations)
# =============================================================================


def check_collisions(
    user_positions: set[tuple[str | int, str | int]],
    reserved: set[tuple[str | int, str | int]],
) -> set[tuple[str | int, str | int]]:
    """Find collisions between user positions and reserved QC positions.

    Args:
        user_positions: Set of positions requested by user
        reserved: Set of positions reserved for QC samples

    Returns:
        Set of colliding positions (empty if no collisions)
    """
    return user_positions & reserved


def check_out_of_bounds(
    positions: set[tuple[str | int, str | int]],
    valid_positions: set[tuple[str | int, str | int]],
) -> set[tuple[str | int, str | int]]:
    """Find positions that are outside the valid grid.

    Args:
        positions: Set of positions to check
        valid_positions: Set of all valid positions in the grid

    Returns:
        Set of out-of-bounds positions (empty if all valid)
    """
    return positions - valid_positions


def available_positions(
    all_positions: set[tuple[str | int, str | int]],
    reserved: set[tuple[str | int, str | int]],
) -> set[tuple[str | int, str | int]]:
    """Get positions available for user samples.

    Args:
        all_positions: Set of all positions in the grid
        reserved: Set of positions reserved for QC samples

    Returns:
        Set of available positions
    """
    return all_positions - reserved


def validate_no_collisions(
    user_positions: set[tuple[str | int, str | int]],
    reserved: set[tuple[str | int, str | int]],
) -> None:
    """Validate that user positions don't collide with reserved QC positions.

    Args:
        user_positions: Set of positions requested by user
        reserved: Set of positions reserved for QC samples

    Raises:
        ValueError: If any collisions detected
    """
    collisions = user_positions & reserved
    if collisions:
        raise ValueError(f"Position collisions with QC samples: {collisions}")


def validate_in_bounds(
    positions: set[tuple[str | int, str | int]],
    valid_positions: set[tuple[str | int, str | int]],
) -> None:
    """Validate that all positions are within the grid bounds.

    Args:
        positions: Set of positions to validate
        valid_positions: Set of all valid positions in the grid

    Raises:
        ValueError: If any positions are out of bounds
    """
    out_of_bounds = positions - valid_positions
    if out_of_bounds:
        raise ValueError(f"Positions out of bounds: {out_of_bounds}")


# =============================================================================
# Position Assignment (Pure Function)
# =============================================================================


def assign_user_positions(
    num_samples: int,
    all_positions: list[tuple[str | int, str | int]],
    reserved: set[tuple[str | int, str | int]],
) -> list[tuple[str | int, str | int]]:
    """Assign positions to user samples, skipping reserved QC positions.

    Positions are assigned in row-major order (from itertools.product).

    Args:
        num_samples: Number of user samples to place
        all_positions: All positions in row-major enumeration order
        reserved: Set of positions reserved for QC samples

    Returns:
        List of (tray, position) tuples for user samples

    Raises:
        ValueError: If not enough positions available
    """
    # Filter available positions (set membership check is O(1))
    available = [p for p in all_positions if p not in reserved]

    if len(available) < num_samples:
        raise ValueError(f"Not enough positions: need {num_samples}, have {len(available)} available")

    return available[:num_samples]


# =============================================================================
# PositionGenerator (Orchestrator)
# =============================================================================


class PositionGenerator:
    """Orchestrates position generation for a queue run.

    This is the main entry point for position generation. It combines:
    - Sampler configuration (trays, position function)
    - Plate layout (rows, cols)
    - QC layout (reserved positions)

    And provides methods to:
    - Assign positions to user samples
    - Get positions for QC samples
    - Query capacity
    """

    def __init__(
        self,
        sampler: Sampler,
        plate_layout: PlateLayout,
        qc_layout: QCLayoutGrid | QCLayoutEvosep,
    ) -> None:
        """Initialize position generator.

        Args:
            sampler: Sampler configuration (trays, position_fun)
            plate_layout: Plate layout (rows, cols)
            qc_layout: QC layout (grid or evosep)
        """
        self.sampler = sampler
        self.plate_layout = plate_layout
        self.qc_layout = qc_layout

        self._position_fun = get_position_function(sampler.position_fun)

        # Pre-generate all positions (row-major order from product)
        self._all_positions = generate_all_positions(
            trays=sampler.trays,
            rows=plate_layout.rows,
            cols=plate_layout.cols,
            position_fun=self._position_fun,
        )

    def assign_positions(self, num_samples: int) -> list[tuple[str | int, str | int]]:
        """Assign positions for user samples in row-major order.

        Args:
            num_samples: Number of user samples to place

        Returns:
            List of (tray, position) tuples

        Raises:
            ValueError: If not enough positions available
        """
        reserved = self.qc_layout.reserved_positions()
        return assign_user_positions(num_samples, self._all_positions, reserved)

    def get_qc_position(self, sample_id: str) -> tuple[str | int, str | int]:
        """Get position for a QC sample.

        Args:
            sample_id: QC sample identifier

        Returns:
            (tray, position) tuple
        """
        return self.qc_layout.get_position(sample_id)

    def format_position(self, tray: str | int, position: str | int) -> str:
        """Format a position for output.

        Args:
            tray: Tray identifier
            position: Position within tray

        Returns:
            Formatted position string (e.g., "Y:A1")
        """
        return format_position(tray, position)

    @property
    def capacity(self) -> int:
        """Total positions minus reserved QC positions."""
        return len(self._all_positions) - len(self.qc_layout.reserved_positions())

    @property
    def total_positions(self) -> int:
        """Total positions (including QC reserved)."""
        return len(self._all_positions)

    @property
    def qc_reserved_count(self) -> int:
        """Number of positions reserved for QC samples."""
        return len(self.qc_layout.reserved_positions())

    @property
    def all_positions(self) -> list[tuple[str | int, str | int]]:
        """All positions in row-major order (read-only copy)."""
        return list(self._all_positions)

    def available_positions(self) -> set[tuple[str | int, str | int]]:
        """Available positions (all - reserved)."""
        return set(self._all_positions) - self.qc_layout.reserved_positions()

    def validate_user_positions(
        self,
        user_positions: set[tuple[str | int, str | int]],
    ) -> None:
        """Validate user-provided positions.

        Checks:
        1. All positions are within grid bounds
        2. No collisions with reserved QC positions

        Args:
            user_positions: Set of positions to validate

        Raises:
            ValueError: If validation fails
        """
        all_pos = set(self._all_positions)
        validate_in_bounds(user_positions, all_pos)
        validate_no_collisions(user_positions, self.qc_layout.reserved_positions())
