"""Position generators for different sampler types.

Each position generator implements generate_positions(n) -> list[str] as a pure,
stateless function. Generators accept typed Pydantic models instead of dicts.
"""

from typing import Literal, Protocol

from qg.config_models_samplers import EvosepSampler, GridSampler


class SamplerPositionGenerator(Protocol):
    """Protocol for sampler position generators."""

    def generate_positions(self, n: int) -> list[str]:
        """Generate n positions for user samples. Pure, stateless."""
        ...


class GridPositionGenerator:
    """Grid-based sampler position generator (Vanquish, MClass48).

    Generates positions in row-major order across plates, skipping the QC plate.
    """

    def __init__(
        self,
        config: GridSampler,
        container: Literal["vial", "plate"] = "vial",
        sampler_name: str = "sampler",
        position_format_override: str | None = None,
    ):
        """Initialize from typed GridSampler config.

        Args:
            config: Typed GridSampler model
            container: Container type ("vial" or "plate")
            sampler_name: Name for error messages (e.g., "Vanquish", "MClass48")
            position_format_override: Optional format to override sampler default
        """
        container_config = getattr(config, container)
        if container_config is None:
            raise ValueError(f"Container '{container}' not defined for {sampler_name}")

        # Parent-level fields (always from config)
        self.plates = config.plates
        self.qc_plate = config.qc_plate

        # Container-level overrides (use container if set, else parent)
        self.sample_rows = container_config.sample_rows or config.sample_rows
        self.cols = container_config.cols or config.cols
        # Use override if provided, otherwise container default
        self.position_format = position_format_override or container_config.position_format

        # Validate required fields are set
        if self.sample_rows is None:
            raise ValueError("sample_rows must be set at sampler or container level")
        if self.cols is None:
            raise ValueError("cols must be set at sampler or container level")
        if self.position_format is None:
            raise ValueError("position_format must be set at container level")

    def generate_positions(self, n: int) -> list[str]:
        """Generate n user sample positions in row-major order.

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


class EvosepPositionGenerator:
    """Evosep sampler position generator (tray-based, sequential fill).

    Generates positions sequentially within slots/trays.
    Position format: "tray{slot}:{position}" to match QC position format.
    """

    def __init__(
        self, config: EvosepSampler, container: Literal["vial", "plate"] = "vial"
    ):
        """Initialize from typed EvosepSampler config.

        Args:
            config: Typed EvosepSampler model
            container: Container type ("vial" or "plate") - used for validation
        """
        container_config = getattr(config, container)
        if container_config is None:
            raise ValueError(f"Container '{container}' not defined for Evosep")

        self.slots = config.slots
        self.positions_per_slot = config.positions_per_slot

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


def get_position_generator(
    sampler_name: str,
    config: GridSampler | EvosepSampler,
    position_format_override: str | None = None,
) -> SamplerPositionGenerator:
    """Factory function to get the right position generator.

    Args:
        sampler_name: Sampler name like "Vanquish.vial" or "MClass48.plate"
        config: Typed sampler configuration model
        position_format_override: Optional position format to override sampler default

    Returns:
        Position generator implementing SamplerPositionGenerator protocol
    """
    parts = sampler_name.split(".")
    sampler_base = parts[0]
    container = parts[1] if len(parts) > 1 else "vial"

    if sampler_base in ("Vanquish", "MClass48"):
        return GridPositionGenerator(
            config, container, sampler_name=sampler_base,
            position_format_override=position_format_override,
        )
    elif sampler_base == "Evosep":
        return EvosepPositionGenerator(config, container)
    else:
        raise ValueError(f"Unknown sampler: {sampler_base}")
