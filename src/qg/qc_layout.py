# =============================================================================
# QC Layout Wrappers (Layer 2: precomputed reserved positions)
# =============================================================================
#
# Wraps raw QC sample lists into classes that precompute reserved positions.
# Factory function returns empty layouts when pattern has no QC references.

from __future__ import annotations

from collections.abc import Callable

from qg.config_models.loader import QGConfiguration
from qg.config_models.positions import QCSampleEvosep, QCSampleGrid
from qg.config_models.structure import QueuePattern
from qg.utils import Position


class QCLayoutGrid:
    """Precomputed QC layout for grid samplers (Vanquish, MClass).

    Attributes:
        reserved: Set of positions reserved for QC samples.
        position_map: Mapping from sample_id to its fixed position.
        is_empty: True if no QC samples (e.g., noqc pattern).
    """

    def __init__(
        self,
        qc_samples: list[QCSampleGrid],
        position_fun: Callable[[str | int, int], str | int],
    ) -> None:
        self.position_map: dict[str, Position] = {
            s.sample_id: Position(s.tray, position_fun(s.row, s.col), row=s.row, col=s.col) for s in qc_samples
        }
        self.reserved: set[Position] = set(self.position_map.values())
        self.is_empty: bool = len(qc_samples) == 0


class QCLayoutEvosep:
    """Precomputed QC layout for Evosep (consumable tips).

    Attributes:
        reserved: Set of all positions in QC tip ranges.
        sample_map: Mapping from sample_id to QCSampleEvosep config.
        is_empty: True if no QC samples (e.g., noqc pattern).
    """

    def __init__(self, qc_samples: list[QCSampleEvosep]) -> None:
        self.sample_map: dict[str, QCSampleEvosep] = {s.sample_id: s for s in qc_samples}
        self.reserved: set[Position] = {
            Position(s.tray, pos) for s in qc_samples for pos in range(s.position_start, s.position_end + 1)
        }
        self.is_empty: bool = len(qc_samples) == 0


def create_qc_layout(
    config: QGConfiguration,
    tech_area: str,
    pattern: QueuePattern,
    plate_layout_name: str,
    sampler_name: str,
    position_fun: Callable[[str | int, int], str | int],
) -> QCLayoutGrid | QCLayoutEvosep:
    """Create a QC layout wrapper, returning an empty layout when the pattern has no QC references.

    Args:
        config: Configuration bundle.
        tech_area: Technology area (e.g., "Proteomics").
        pattern: Queue pattern (checked for QC sample references).
        plate_layout_name: Plate layout name (e.g., "Vanquish_54").
        sampler_name: Sampler name ("Evosep" uses evosep layout, others use grid).
        position_fun: Position function for grid samplers (ignored for Evosep).

    Returns:
        QCLayoutGrid or QCLayoutEvosep (empty if pattern has no QC sample IDs).
    """
    is_evosep = sampler_name == "Evosep"

    # If the pattern references no QC samples, return an empty layout
    if not pattern.get_all_sample_ids():
        if is_evosep:
            return QCLayoutEvosep([])
        return QCLayoutGrid([], position_fun)

    qc_layout_name = pattern.qc_layout_name
    qc_samples = config.get_qc_samples(tech_area, qc_layout_name, plate_layout_name, sampler_name)

    if is_evosep:
        return QCLayoutEvosep(qc_samples)
    return QCLayoutGrid(qc_samples, position_fun)
