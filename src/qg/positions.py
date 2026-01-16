"""Position generators for different sampler types.

Each sampler implements generate_positions(n) -> list[str] as a pure, stateless function.
"""

from typing import Protocol


class SamplerPositionGenerator(Protocol):
    """Protocol for sampler position generators."""

    def generate_positions(self, n: int) -> list[str]:
        """Generate n positions for user samples. Pure, stateless."""
        ...


class VanquishSampler:
    """Vanquish sampler position generator (grid-based).

    Generates positions in row-major order across plates, skipping the QC plate.
    """

    def __init__(self, config: dict):
        self.plates = config.get("plates", ["Y", "R", "B", "G"])
        self.sample_rows = config.get("sample_rows", ["A", "B", "C", "D", "E"])
        self.cols = config.get("cols", [1, 2, 3, 4, 5, 6, 7, 8, 9])
        self.qc_plate = config.get("qc_plate", "B")
        self.position_format = config.get("position_format", "{plate}:{row}{col}")

    def generate_positions(self, n: int) -> list[str]:
        """Generate n user sample positions.

        Pure function - no internal state modified.

        Args:
            n: Number of positions to generate

        Returns:
            List of position strings like ["Y:A1", "Y:A2", ...]
        """
        positions = []
        plate_idx, row_idx, col_idx = 0, 0, 0

        # Skip QC plate initially
        while plate_idx < len(self.plates) and self.plates[plate_idx] == self.qc_plate:
            plate_idx += 1

        for _ in range(n):
            if plate_idx >= len(self.plates):
                raise ValueError(f"No more positions available (requested {n})")

            pos = self.position_format.format(
                plate=self.plates[plate_idx],
                row=self.sample_rows[row_idx],
                col=self.cols[col_idx],
            )
            positions.append(pos)

            # Advance (row-major order)
            col_idx += 1
            if col_idx >= len(self.cols):
                col_idx = 0
                row_idx += 1
                if row_idx >= len(self.sample_rows):
                    row_idx = 0
                    plate_idx += 1
                    # Skip QC plate
                    while (
                        plate_idx < len(self.plates)
                        and self.plates[plate_idx] == self.qc_plate
                    ):
                        plate_idx += 1

        return positions


class MClass48Sampler:
    """MClass48 sampler position generator (grid-based).

    Generates positions in row-major order across plates, skipping the QC plate.
    Same logic as VanquishSampler but with different default config.
    """

    def __init__(self, config: dict):
        self.plates = config.get("plates", ["1", "2"])
        self.sample_rows = config.get("sample_rows", ["A", "B", "C", "D", "E"])
        self.cols = config.get("cols", [1, 2, 3, 4, 5, 6, 7, 8])
        self.qc_plate = config.get("qc_plate", "1")
        self.position_format = config.get("position_format", "{plate}:{row},{col}")

    def generate_positions(self, n: int) -> list[str]:
        """Generate n user sample positions in row-major order.

        Args:
            n: Number of positions to generate

        Returns:
            List of position strings like ["1:A,1", "1:A,2", ...]
        """
        positions = []
        plate_idx, row_idx, col_idx = 0, 0, 0

        # Skip QC plate initially
        while plate_idx < len(self.plates) and self.plates[plate_idx] == self.qc_plate:
            plate_idx += 1

        for _ in range(n):
            if plate_idx >= len(self.plates):
                raise ValueError(f"No more positions available (requested {n})")

            pos = self.position_format.format(
                plate=self.plates[plate_idx],
                row=self.sample_rows[row_idx],
                col=self.cols[col_idx],
            )
            positions.append(pos)

            # Advance (row-major order)
            col_idx += 1
            if col_idx >= len(self.cols):
                col_idx = 0
                row_idx += 1
                if row_idx >= len(self.sample_rows):
                    row_idx = 0
                    plate_idx += 1
                    # Skip QC plate
                    while (
                        plate_idx < len(self.plates)
                        and self.plates[plate_idx] == self.qc_plate
                    ):
                        plate_idx += 1

        return positions


class EvosepSampler:
    """Evosep sampler position generator (tray-based, sequential fill).

    Generates positions sequentially within slots/trays.
    Position format: "tray{slot}:{position}" to match QC position format.
    """

    def __init__(self, config: dict):
        self.slots = config.get("slots", [1, 2, 3, 4])
        self.positions_per_slot = config.get("positions_per_slot", 96)

    def generate_positions(self, n: int) -> list[str]:
        """Generate n user sample positions sequentially across slots.

        Args:
            n: Number of positions to generate

        Returns:
            List of position strings like ["tray1:1", "tray1:2", ...]
        """
        positions = []
        slot_idx = 0
        position_in_slot = 1

        for _ in range(n):
            if slot_idx >= len(self.slots):
                raise ValueError(f"No more positions available (requested {n})")

            slot = self.slots[slot_idx]
            positions.append(f"tray{slot}:{position_in_slot}")

            # Advance
            position_in_slot += 1
            if position_in_slot > self.positions_per_slot:
                position_in_slot = 1
                slot_idx += 1

        return positions


def get_sampler(sampler_name: str, config: dict) -> SamplerPositionGenerator:
    """Factory function to get the right sampler generator.

    Args:
        sampler_name: Sampler name like "Vanquish.vial" or "MClass48.plate"
        config: Sampler configuration dict (merged base + container config)

    Returns:
        Sampler instance implementing SamplerPositionGenerator protocol
    """
    samplers: dict[str, type] = {
        "Vanquish": VanquishSampler,
        "MClass48": MClass48Sampler,
        "Evosep": EvosepSampler,
    }
    sampler_base = sampler_name.split(".")[0]
    if sampler_base not in samplers:
        raise ValueError(f"Unknown sampler: {sampler_base}")
    return samplers[sampler_base](config)
