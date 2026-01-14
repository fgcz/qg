"""Position generators for different sampler types."""

from abc import ABC, abstractmethod


class PositionGenerator(ABC):
    """Abstract base class for position generators."""

    @abstractmethod
    def next_position(self) -> str:
        """Get next position for a user sample."""
        pass


class VanquishPositionGenerator(PositionGenerator):
    """Generate positions for Vanquish sampler in vial mode."""

    def __init__(self, sampler_config: dict):
        self.plates = sampler_config.get("plates", ["Y", "R", "B", "G"])
        self.sample_rows = sampler_config.get("sample_rows", ["A", "B", "C", "D", "E"])
        self.cols = sampler_config.get("cols", [1, 2, 3, 4, 5, 6, 7, 8, 9])
        self.qc_plate = sampler_config.get("qc_plate", "B")
        self.position_format = sampler_config.get("position_format", "{plate}:{row}{col}")
        self.samples_per_plate = sampler_config.get("samples_per_plate", 45)

        # Current position state
        self._plate_idx = 0
        self._row_idx = 0
        self._col_idx = 0

        # Skip QC plate for sample positions
        if self.plates[self._plate_idx] == self.qc_plate:
            self._plate_idx += 1

    def next_position(self) -> str:
        """Get next position for a user sample."""
        # Skip QC plate
        while self._plate_idx < len(self.plates) and self.plates[self._plate_idx] == self.qc_plate:
            self._plate_idx += 1

        if self._plate_idx >= len(self.plates):
            raise ValueError("No more positions available")

        plate = self.plates[self._plate_idx]
        row = self.sample_rows[self._row_idx]
        col = self.cols[self._col_idx]

        position = self.position_format.format(plate=plate, row=row, col=col)

        # Advance to next position (row-major order)
        self._col_idx += 1
        if self._col_idx >= len(self.cols):
            self._col_idx = 0
            self._row_idx += 1
            if self._row_idx >= len(self.sample_rows):
                self._row_idx = 0
                self._plate_idx += 1
                # Skip QC plate
                while self._plate_idx < len(self.plates) and self.plates[self._plate_idx] == self.qc_plate:
                    self._plate_idx += 1

        return position


class MClass48PositionGenerator(PositionGenerator):
    """Generate positions for MClass48 sampler.

    Not yet implemented - positions come from input.
    """

    def __init__(self, sampler_config: dict):
        self.sampler_config = sampler_config
        raise NotImplementedError("MClass48 position generator not yet implemented")

    def next_position(self) -> str:
        raise NotImplementedError("MClass48 position generator not yet implemented")


class EvosepPositionGenerator(PositionGenerator):
    """Generate positions for Evosep sampler.

    Not yet implemented - positions come from input.
    """

    def __init__(self, sampler_config: dict):
        self.sampler_config = sampler_config
        raise NotImplementedError("Evosep position generator not yet implemented")

    def next_position(self) -> str:
        raise NotImplementedError("Evosep position generator not yet implemented")
