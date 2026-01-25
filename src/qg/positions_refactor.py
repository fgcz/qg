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

from itertools import islice, product
from typing import Any

from qg.config_models import QCLayoutPattern
from qg.config_models_samplers import (
    SamplersConfig,
    VanquishConfig,
    VanquishPlateConfig,
    VanquishVialConfig,
)
from qg.params_models import QueueInput

# Type alias for position dict
PositionDict = dict[str, Any]


# =============================================================================
# Protocol and Helper
# =============================================================================


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


class _VanquishVialSampler_prototype:
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
        self._positions: list[PositionDict] = []

    def get_qc_positions(self, sample_name: str) -> dict[str, str | int]:
        return self._qc_positions.get_position(sample_name)

    def _get_reserved_qc_positions(self) -> set[tuple[str, str]]:
        """Get set of (tray, grid_position) tuples reserved for QC samples."""
        reserved: set[tuple[str, str]] = set()
        for qc_id in self._qc_positions.positions:
            unified = self._qc_positions.get_position(qc_id)
            reserved.add((str(unified["tray"]), str(unified["grid_position"])))
        return reserved

    def _generate_positions_default_samples(self, input_queue: QueueInput) -> list[PositionDict]:
        """Generate n positions row-major across all plates, skipping QC positions."""
        reserved = self._get_reserved_qc_positions()
        n_samples = len(input_queue.get_all_samples())

        # Generate all positions in row-major order: plates -> rows -> cols
        all_positions = (
            (plate, self._container.grid_position_format.format(row=row, col=col))
            for plate, row, col in product(
                self._parent.plates,
                self._container.sample_rows,
                self._container.cols,
            )
        )

        # Filter out reserved positions and take first n_samples
        available = ((tray, pos) for tray, pos in all_positions if (tray, pos) not in reserved)
        self._positions = [
            {"tray": tray, "grid_position": grid_position} for tray, grid_position in islice(available, n_samples)
        ]

        if len(self._positions) < n_samples:
            raise ValueError(f"Not enough positions available (requested {n_samples})")

        return self._positions

    def assign_positions_user_samples(self, input_queue: QueueInput) -> QueueInput:
        self._generate_positions_default_samples(input_queue)
        for index in range(len(self._positions)):
            pos = self._positions[index]
            input_queue.update_sample_position(index, pos["tray"], pos["grid_position"])
        return input_queue


class _VanquishPlateSampler_prototype:
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

    def _get_reserved_qc_positions(self) -> set[tuple[str, str]]:
        """Get set of (tray, grid_position) tuples reserved for QC samples."""
        reserved: set[tuple[str, str]] = set()
        for qc_id in self._qc_positions.positions:
            unified = self._qc_positions.get_position(qc_id)
            reserved.add((str(unified["tray"]), str(unified["grid_position"])))
        return reserved

    def assign_positions_user_samples(self, input_queue: QueueInput) -> QueueInput:
        """Validate that user sample positions don't collide with QC positions."""
        reserved = self._get_reserved_qc_positions()

        for sample in input_queue.get_all_samples():
            tray = str(sample.tray) if sample.tray else ""
            grid_pos = str(sample.grid_position) if sample.grid_position else ""
            if (tray, grid_pos) in reserved:
                raise ValueError(
                    f"Sample '{sample.sample_name}' at {tray}:{grid_pos} conflicts with reserved QC position"
                )

        return input_queue

    def get_qc_positions(self, sample_name: str) -> dict[str, str | int]:
        return self._qc_positions.get_position(sample_name)


Sampler = _VanquishVialSampler_prototype | _VanquishPlateSampler_prototype


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
            return _VanquishVialSampler_prototype(config.Vanquish, config.Vanquish.vial, qc_layout_pattern)
        case "Vanquish.plate":
            return _VanquishPlateSampler_prototype(config.Vanquish, config.Vanquish.plate, qc_layout_pattern)
        case _:
            raise ValueError(f"Unknown sampler: {sampler_name}")


class SamplerStrategy:
    sampler: Sampler

    def __init__(self, sampler_name: str, config: SamplersConfig, qc_layout_pattern: QCLayoutPattern):
        self.sampler = create_sampler(sampler_name, config, qc_layout_pattern)

    # TODO: implement get_positions for queue input -> just assign positions for user samples
    # this function is called before we call the generator!
    def assign_positions_user_samples(self, queue_input: QueueInput) -> QueueInput:
        # you implement it int terms of
        # Todo we do not wan to loose the sample groups
        # Todo for plate samplers i validates the Queue, if sample positions do not collide with the
        # layout
        self.sampler.assign_positions_user_samples(queue_input)

        return queue_input

    # TODO: implement a function which assigns positions to the qc samples only in a queue input,
    # you must not touch user samples however.
    # this function is used in the generator
    def assign_positions_qc_samples(self, structure: list[str], queue_input: QueueInput) -> list[PositionDict]:
        """Assign positions to all slots in queue structure.

        Args:
            structure: List of sample IDs (e.g., ["QC01", "default", "default", "QC02"]).
                       "default" slots get positions from queue_input samples.
                       Other slots get QC positions from the sampler.
            queue_input: QueueInput with samples already having positions assigned.

        Returns:
            List of position dicts in structure order.
        """
        positions: list[PositionDict] = []
        user_idx = 0
        for sample_id in structure:
            if sample_id == "default":
                sample = queue_input.get_sample_by_index(user_idx)
                positions.append({"tray": sample.tray, "grid_position": sample.grid_position})
                user_idx += 1
            else:
                position = self.sampler.get_qc_positions(sample_id)
                positions.append(position)
        return positions
