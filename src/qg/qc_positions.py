# =============================================================================
# QC Position Providers (Self-contained - NO dependencies on positions.py)
# =============================================================================
#
# 2 provider classes for QC position lookup:
#   - _QCPositionProviderGrid: Grid samplers (Vanquish/MClass) - stateless
#   - _QCPositionProviderEvosep: Evosep - stateful (consumable tips)
#
# Factory: create_qc_position_provider() returns correct class

from __future__ import annotations

from collections.abc import Callable
from typing import NamedTuple

from qg.config_models.loader import QGConfiguration
from qg.config_models.positions import (
    QCSampleEvosep,
    QCSampleGrid,
)
from qg.queue_structure import SlotEntry

# =============================================================================
# Position NamedTuple
# =============================================================================


class Position(NamedTuple):
    """A position on a sampler tray."""

    tray: str | int
    grid_position: str | int


# =============================================================================
# Position Functions Registry
# =============================================================================


class _PositionFunctions:
    """Registry for position functions (string_concat, int_add)."""

    @staticmethod
    def string_concat(row: str | int, col: int) -> str:
        """Grid samplers: 'A' + 1 -> 'A1'."""
        return f"{row}{col}"

    @staticmethod
    def int_add(row: int, col: int) -> int:
        """Evosep: 1 + 0 -> 1, 13 + 2 -> 15."""
        return row + col

    _registry: dict[str, Callable[[str | int, int], str | int]] = {
        "string_concat": string_concat.__func__,  # type: ignore[attr-defined]
        "int_add": int_add.__func__,  # type: ignore[attr-defined]
    }

    @classmethod
    def get(cls, name: str) -> Callable[[str | int, int], str | int]:
        """Get position function by name."""
        return cls._registry[name]


# =============================================================================
# QC Position Providers
# =============================================================================


class _QCPositionProviderGrid:
    """QC position provider for Grid samplers (stateless).

    Returns the same position for each QC sample every time - QC vials are reusable.
    """

    def __init__(
        self,
        qc_samples: list[QCSampleGrid],
        position_fun: Callable[[str | int, int], str | int],
        slot_entries: list[SlotEntry],  # noqa: ARG002
    ) -> None:
        # Precompute positions: {sample_id: Position}
        self._positions: dict[str, Position] = {
            s.sample_id: Position(s.tray, position_fun(s.row, s.col)) for s in qc_samples
        }

    def get_position(self, sample_id: str) -> Position:
        """Get QC position (always returns same position for Grid)."""
        return self._positions[sample_id]


class _QCPositionProviderEvosep:
    """QC position provider for Evosep (stateful, consumable tips).

    Each call increments the position counter - tips are consumed sequentially.
    Validates capacity upfront in constructor using slot_entries.
    """

    def __init__(
        self,
        qc_samples: list[QCSampleEvosep],
        slot_entries: list[SlotEntry],
    ) -> None:
        self._samples: dict[str, QCSampleEvosep] = {s.sample_id: s for s in qc_samples}
        self._counters: dict[str, int] = {s.sample_id: 0 for s in qc_samples}

        # Count QC samples needed from slot_entries
        qc_counts: dict[str, int] = {}
        for entry in slot_entries:
            if entry.sample_id != "default":
                qc_counts[entry.sample_id] = qc_counts.get(entry.sample_id, 0) + 1

        # Validate upfront: enough positions for all QC samples needed
        for sample_id, count_needed in qc_counts.items():
            if sample_id not in self._samples:
                raise ValueError(f"QC sample '{sample_id}' not found in QC layout")
            s = self._samples[sample_id]
            capacity = s.position_end - s.position_start + 1
            if count_needed > capacity:
                raise ValueError(
                    f"Not enough positions for QC sample '{sample_id}': "
                    f"need {count_needed}, have {capacity} (positions {s.position_start}-{s.position_end})"
                )

    def get_position(self, sample_id: str) -> Position:
        """Get next QC position (increments counter)."""
        s = self._samples[sample_id]
        idx = self._counters[sample_id]
        pos = s.position_start + idx
        self._counters[sample_id] = idx + 1
        return Position(s.tray, pos)


# =============================================================================
# Type Alias and Factory
# =============================================================================

QCPositionProvider = _QCPositionProviderGrid | _QCPositionProviderEvosep


def create_qc_position_provider(
    sampler_name: str,
    config: QGConfiguration,
    tech_area: str,
    qc_layout_name: str,
    plate_layout_name: str,
    slot_entries: list[SlotEntry],
) -> QCPositionProvider:
    """Factory to create QC position provider.

    Args:
        sampler_name: "Vanquish", "MClass", or "Evosep"
        config: QGConfiguration
        tech_area: e.g., "Proteomics"
        qc_layout_name: e.g., "standard"
        plate_layout_name: e.g., "Vanquish_54"
        slot_entries: List of SlotEntry - used by Evosep to validate capacity
    """
    sampler = config.samplers.get_sampler(sampler_name)
    position_fun = _PositionFunctions.get(sampler.position_fun)

    if sampler_name == "Evosep":
        qc_samples = config.qc_layouts_evosep.get_samples(tech_area, qc_layout_name, plate_layout_name)
        return _QCPositionProviderEvosep(qc_samples, slot_entries)
    else:
        qc_samples = config.qc_layouts_grid.get_samples(tech_area, qc_layout_name, plate_layout_name)
        return _QCPositionProviderGrid(qc_samples, position_fun, slot_entries)
