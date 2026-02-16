# =============================================================================
# QC Position Providers (Self-contained - NO dependencies on positions.py)
# =============================================================================
#
# 2 provider classes for QC position lookup:
#   - _QCPositionProviderWell: Well-plate samplers (Vanquish/MClass) - stateless
#   - _QCPositionProviderTip: Tip-plate samplers (Evosep) - stateful (consumable tips)
#
# Factory: create_qc_position_provider() returns correct class

from __future__ import annotations

from typing import Protocol

from qg.config_models.loader import QGConfiguration
from qg.config_models.positions import PlateLayout
from qg.config_models.structure import QueuePattern
from qg.qc_layout import QCLayoutTip, QCLayoutWell, create_qc_layout
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


class _QCPositionProviderWell:
    """QC position provider for well-plate samplers (stateless).

    Returns the same position for each QC sample every time - QC vials are reusable.
    """

    def __init__(
        self,
        qc_layout: QCLayoutWell,
        slot_entries: list[SlotEntry],  # noqa: ARG002
    ) -> None:
        self._positions = qc_layout.position_map

    def get_position(self, sample_id: str) -> Position:
        """Get QC position (always returns same position for well plates)."""
        return self._positions[sample_id]


class _QCPositionProviderTip:
    """QC position provider for tip-plate samplers (stateful, consumable tips).

    Each call increments the position counter - tips are consumed sequentially.
    Validates capacity upfront in constructor using slot_entries.
    Internally uses flat (1-96) arithmetic, returns alpha Position objects.
    """

    def __init__(
        self,
        qc_layout: QCLayoutTip,
        slot_entries: list[SlotEntry],
        default_sample_id: str,
        plate_layout: PlateLayout,
    ) -> None:
        self._samples = qc_layout.sample_map
        self._plate_layout = plate_layout
        self._counters: dict[str, int] = {sid: 0 for sid in self._samples}

        # Pre-compute flat start positions for arithmetic
        self._start_flat: dict[str, int] = {
            sid: plate_layout.alpha_to_flat(s.position_start) for sid, s in self._samples.items()
        }

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
            start = plate_layout.alpha_to_flat(s.position_start)
            end = plate_layout.alpha_to_flat(s.position_end)
            capacity = end - start + 1
            if count_needed > capacity:
                raise ValueError(
                    f"Not enough positions for QC sample '{sample_id}': "
                    f"need {count_needed}, have {capacity} (positions {s.position_start}-{s.position_end})"
                )

    def get_position(self, sample_id: str) -> Position:
        """Get next QC position (increments counter). Returns alpha Position."""
        s = self._samples[sample_id]
        idx = self._counters[sample_id]
        flat_pos = self._start_flat[sample_id] + idx
        row, col = self._plate_layout.flat_to_row_col(flat_pos)
        self._counters[sample_id] = idx + 1
        return Position(s.tray, f"{row}{col}", row=row, col=col)


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
    plate_layout = config.plate_layouts.get_layout(plate_layout_name)

    # Create QC layout (empty when pattern has no QC references)
    qc_layout = create_qc_layout(
        config, tech_area, pattern, plate_layout_name, sampler_name, position_fun, plate_layout
    )

    if sampler_name == "Evosep":
        return _QCPositionProviderTip(qc_layout, slot_entries, default_sample_id, plate_layout)
    else:
        return _QCPositionProviderWell(qc_layout, slot_entries)
