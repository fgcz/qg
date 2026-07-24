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

from qg.config_models.positions import PlateLayout
from qg.qc_layout import QCLayoutTip, QCLayoutWell
from qg.queue_structure import SlotEntry
from qg.utils import Position

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
        self._counters: dict[str, int] = dict.fromkeys(self._samples, 0)

        # Pre-compute flat start/end positions for arithmetic and bounds checking
        self._start_flat: dict[str, int] = {
            sid: plate_layout.alpha_to_flat(s.position_start) for sid, s in self._samples.items()
        }
        self._end_flat: dict[str, int] = {
            sid: plate_layout.alpha_to_flat(s.position_end) for sid, s in self._samples.items()
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
            capacity = self._end_flat[sample_id] - self._start_flat[sample_id] + 1
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
        if flat_pos > self._end_flat[sample_id]:
            raise RuntimeError(
                f"QC sample '{sample_id}' exceeded allocated range ({s.position_start}-{s.position_end})"
            )
        row, col = self._plate_layout.flat_to_row_col(flat_pos)
        self._counters[sample_id] = idx + 1
        return Position(s.tray, f"{row}{col}", row=row, col=col)


# =============================================================================
# Factory
# =============================================================================


def create_qc_position_provider(
    qc_layout: QCLayoutWell | QCLayoutTip,
    slot_entries: list[SlotEntry],
    default_sample_id: str,
    plate_layout: PlateLayout,
) -> QCPositionProvider:
    """Factory to create QC position provider from a pre-computed QC layout.

    Args:
        qc_layout: Pre-computed QC layout (well or tip)
        slot_entries: List of SlotEntry - used by Evosep to validate capacity
        default_sample_id: The default sample ID for user samples
        plate_layout: Plate layout for tip position arithmetic
    """
    if isinstance(qc_layout, QCLayoutTip):
        return _QCPositionProviderTip(qc_layout, slot_entries, default_sample_id, plate_layout)
    else:
        return _QCPositionProviderWell(qc_layout, slot_entries)
