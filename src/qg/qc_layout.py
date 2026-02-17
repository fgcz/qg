# =============================================================================
# QC Layout Wrappers (Layer 2: precomputed reserved positions)
# =============================================================================
#
# Wraps raw QC sample lists into classes that precompute reserved positions.
# Factory function returns empty layouts when pattern has no QC references.

from __future__ import annotations

from collections.abc import Callable

from qg.config_models.loader import QGConfiguration
from qg.config_models.positions import PlateLayout, QCSampleTip, QCSampleWell
from qg.config_models.structure import QueuePattern
from qg.utils import Position


class QCLayoutWell:
    """Precomputed QC layout for well-plate samplers (Vanquish, MClass).

    Attributes:
        reserved: Set of positions reserved for QC samples.
        position_map: Mapping from sample_id to its fixed position.
        is_empty: True if no QC samples (e.g., noqc pattern).
    """

    def __init__(
        self,
        qc_samples: list[QCSampleWell],
        position_fun: Callable[[str, int], str],
    ) -> None:
        self.position_map: dict[str, Position] = {
            s.sample_id: Position(s.tray, position_fun(s.row, s.col), row=s.row, col=s.col) for s in qc_samples
        }
        self.reserved: set[Position] = set(self.position_map.values())
        self.is_empty: bool = len(qc_samples) == 0


class QCLayoutTip:
    """Precomputed QC layout for tip-plate samplers (consumable tips).

    Attributes:
        reserved: Set of all positions in QC tip ranges (alpha grid positions).
        sample_map: Mapping from sample_id to QCSampleTip config.
        is_empty: True if no QC samples (e.g., noqc pattern).
    """

    def __init__(self, qc_samples: list[QCSampleTip], plate_layout: PlateLayout | None = None) -> None:
        self.sample_map: dict[str, QCSampleTip] = {s.sample_id: s for s in qc_samples}
        self.reserved: set[Position] = set()
        if plate_layout is not None:
            for s in qc_samples:
                start_flat = plate_layout.alpha_to_flat(s.position_start)
                end_flat = plate_layout.alpha_to_flat(s.position_end)
                for flat in range(start_flat, end_flat + 1):
                    row, col = plate_layout.flat_to_row_col(flat)
                    self.reserved.add(Position(s.tray, f"{row}{col}", row=row, col=col))
        self.is_empty: bool = len(qc_samples) == 0


def create_qc_layout(
    config: QGConfiguration,
    tech_area: str,
    pattern: QueuePattern,
    plate_layout_name: str,
    sampler_name: str,
    position_fun: Callable[[str, int], str],
    plate_layout: PlateLayout | None = None,
) -> QCLayoutWell | QCLayoutTip:
    """Create a QC layout wrapper, returning an empty layout when the pattern has no QC references.

    Args:
        config: Configuration bundle.
        tech_area: Technology area (e.g., "Proteomics").
        pattern: Queue pattern (checked for QC sample references).
        plate_layout_name: Plate layout name (e.g., "Vanquish_54").
        sampler_name: Sampler name (used to look up Sampler config).
        position_fun: Position function for well-plate samplers (ignored for tip samplers).
        plate_layout: PlateLayout object (required for tip-plate alpha→flat conversion).

    Returns:
        QCLayoutWell or QCLayoutTip (empty if pattern has no QC sample IDs).
    """
    sampler = config.samplers.get_sampler(sampler_name)
    is_tip = sampler.is_tip

    # If the pattern references no QC samples, return an empty layout
    if not pattern.get_all_sample_ids():
        if is_tip:
            return QCLayoutTip([], plate_layout)
        return QCLayoutWell([], position_fun)

    qc_layout_name = pattern.qc_layout_name
    qc_samples = config.get_qc_samples(tech_area, qc_layout_name, plate_layout_name, sampler)

    if is_tip:
        return QCLayoutTip(qc_samples, plate_layout)
    return QCLayoutWell(qc_samples, position_fun)
