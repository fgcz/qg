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

from typing import Protocol

from qg.config_models.loader import QGConfiguration
from qg.config_models.structure import QueuePattern
from qg.qc_layout import QCLayoutEvosep, QCLayoutGrid, create_qc_layout
from qg.queue_structure import SlotEntry
from qg.utils import (
    Position,
    get_position_function,
)

# =============================================================================
# QC Position Provider Protocol
# =============================================================================


class QCPositionProvider(Protocol):
    """Protocol for providing QC positions."""

    def get_position(self, sample_id: str) -> Position:
        """Get position for a QC sample ID."""
        ...


# =============================================================================
# QC Position Providers
# =============================================================================


class _QCPositionProviderGrid:
    """QC position provider for Grid samplers (stateless).

    Returns the same position for each QC sample every time - QC vials are reusable.
    """

    def __init__(
        self,
        qc_layout: QCLayoutGrid,
        slot_entries: list[SlotEntry],  # noqa: ARG002
    ) -> None:
        self._positions = qc_layout.position_map

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
        qc_layout: QCLayoutEvosep,
        slot_entries: list[SlotEntry],
        default_sample_id: str,
    ) -> None:
        self._samples = qc_layout.sample_map
        self._counters: dict[str, int] = {sid: 0 for sid in self._samples}

        # Count QC samples needed from slot_entries
        qc_counts: dict[str, int] = {}
        for entry in slot_entries:
            if entry.sample_id != default_sample_id:
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
# Factory
# =============================================================================


def create_qc_position_provider(
    sampler_name: str,
    config: QGConfiguration,
    tech_area: str,
    pattern: QueuePattern,
    plate_layout_name: str,
    slot_entries: list[SlotEntry],
    default_sample_id: str,
) -> QCPositionProvider:
    """Factory to create QC position provider.

    Args:
        sampler_name: "Vanquish", "MClass", or "Evosep"
        config: QGConfiguration
        tech_area: e.g., "Proteomics"
        pattern: Queue pattern (used to resolve QC layout; empty patterns yield no QC positions)
        plate_layout_name: e.g., "Vanquish_54"
        slot_entries: List of SlotEntry - used by Evosep to validate capacity
        default_sample_id: The default sample ID for user samples
    """
    sampler = config.samplers.get_sampler(sampler_name)
    position_fun = get_position_function(sampler.position_fun)

    # Create QC layout (empty when pattern has no QC references)
    qc_layout = create_qc_layout(config, tech_area, pattern, plate_layout_name, sampler_name, position_fun)

    if sampler_name == "Evosep":
        return _QCPositionProviderEvosep(qc_layout, slot_entries, default_sample_id)
    else:
        return _QCPositionProviderGrid(qc_layout, slot_entries)
