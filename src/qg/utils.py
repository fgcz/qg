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
    grid_position: str
    row: str = ""
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


def string_concat(row: str, col: int) -> str:
    """Combine row letter and column number: 'A' + 1 -> 'A1'."""
    return f"{row}{col}"


_POSITION_FUNCTIONS: dict[str, Callable[[str, int], str]] = {
    "string_concat": string_concat,
}


def get_position_function(name: str) -> Callable[[str, int], str]:
    """Get position function by name."""
    return _POSITION_FUNCTIONS[name]


def generate_all_positions(
    trays: list[str] | list[int],
    rows: list[str],
    cols: list[int],
    position_fun: Callable[[str, int], str],
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
