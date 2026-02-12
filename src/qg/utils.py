# =============================================================================
# Shared Utilities for Position Handling
# =============================================================================
#
# Common types and functions used by both position assignment (positionV2.py)
# and QC position lookup (qc_positions.py).

from __future__ import annotations

from collections.abc import Callable
from dataclasses import dataclass
from itertools import product
from typing import TypeVar

# =============================================================================
# Position
# =============================================================================


@dataclass(frozen=True)
class Position:
    """A position on a sampler tray.

    Equality and hashing use (tray, grid_position) only, so row/col
    don't affect set membership checks (QC conflict detection).
    """

    tray: str | int
    grid_position: str | int
    row: str | int = ""
    col: int = 0

    def __eq__(self, other: object) -> bool:
        if not isinstance(other, Position):
            return NotImplemented
        return self.tray == other.tray and self.grid_position == other.grid_position

    def __hash__(self) -> int:
        return hash((self.tray, self.grid_position))


# =============================================================================
# Position Functions
# =============================================================================


def string_concat(row: str | int, col: int) -> str:
    """Grid samplers: 'A' + 1 -> 'A1'."""
    return f"{row}{col}"


def int_add(row: int, col: int) -> int:
    """Evosep: 1 + 0 -> 1, 13 + 2 -> 15."""
    return row + col


_POSITION_FUNCTIONS: dict[str, Callable[[str | int, int], str | int]] = {
    "string_concat": string_concat,
    "int_add": int_add,
}


def get_position_function(name: str) -> Callable[[str | int, int], str | int]:
    """Get position function by name."""
    return _POSITION_FUNCTIONS[name]


def generate_all_positions(
    trays: list[str] | list[int],
    rows: list[str] | list[int],
    cols: list[int],
    position_fun: Callable[[str | int, int], str | int],
) -> list[Position]:
    """Generate all positions for given trays, rows, cols."""
    return [Position(tray, position_fun(row, col), row=row, col=col) for tray, row, col in product(trays, rows, cols)]


# =============================================================================
# Grouping Helper
# =============================================================================

T = TypeVar("T")
K = TypeVar("K")


def group_by_key(items: list[T], key: Callable[[T], K]) -> dict[K, list[T]]:
    """Group items by key function, preserving order."""
    result: dict[K, list[T]] = {}
    for item in items:
        result.setdefault(key(item), []).append(item)
    return result
