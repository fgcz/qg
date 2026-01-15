"""Tests for position strategies."""

import pytest

from qg.params_models import InputSample
from qg.positions import VanquishSampler
from qg.strategies import (
    GeneratedPositionStrategy,
    InputPositionStrategy,
    PositionStrategy,
    create_position_strategy,
)


class TestGeneratedPositionStrategy:
    """Tests for GeneratedPositionStrategy."""

    def test_generates_positions_using_sampler(self):
        """Strategy should delegate to sampler.generate_positions()."""
        config = {
            "plates": ["Y", "R", "B", "G"],
            "sample_rows": ["A", "B", "C", "D", "E"],
            "cols": [1, 2, 3, 4, 5, 6, 7, 8, 9],
            "qc_plate": "B",
            "position_format": "{plate}:{row}{col}",
        }
        sampler = VanquishSampler(config)
        strategy = GeneratedPositionStrategy(sampler=sampler)

        positions = strategy(5, [])

        assert len(positions) == 5
        assert positions[0] == "Y:A1"
        assert positions[1] == "Y:A2"
        assert positions[4] == "Y:A5"

    def test_ignores_input_samples(self):
        """Generated strategy doesn't use input samples."""
        config = {
            "plates": ["Y"],
            "sample_rows": ["A"],
            "cols": [1, 2, 3],
            "qc_plate": "B",
            "position_format": "{plate}:{row}{col}",
        }
        sampler = VanquishSampler(config)
        strategy = GeneratedPositionStrategy(sampler=sampler)

        samples = [
            InputSample(sample_name="s1", sample_id=1, grid_position="X:Z9"),
            InputSample(sample_name="s2", sample_id=2, grid_position="X:Z8"),
        ]
        positions = strategy(2, samples)

        # Positions come from sampler, not from samples
        assert positions == ["Y:A1", "Y:A2"]

    def test_conforms_to_protocol(self):
        """Strategy should satisfy PositionStrategy protocol."""
        config = {"plates": ["Y"], "sample_rows": ["A"], "cols": [1], "qc_plate": "B"}
        sampler = VanquishSampler(config)
        strategy = GeneratedPositionStrategy(sampler=sampler)

        # Type check - should be callable with correct signature
        assert callable(strategy)
        result = strategy(1, [])
        assert isinstance(result, list)


class TestInputPositionStrategy:
    """Tests for InputPositionStrategy."""

    def test_extracts_positions_from_samples(self):
        """Strategy should extract grid_position from input samples."""
        strategy = InputPositionStrategy()

        samples = [
            InputSample(sample_name="s1", sample_id=1, grid_position="Y:A1"),
            InputSample(sample_name="s2", sample_id=2, grid_position="Y:A2"),
            InputSample(sample_name="s3", sample_id=3, grid_position="Y:A3"),
        ]
        positions = strategy(3, samples)

        assert positions == ["Y:A1", "Y:A2", "Y:A3"]

    def test_handles_missing_grid_position(self):
        """Missing grid_position should become empty string."""
        strategy = InputPositionStrategy()

        samples = [
            InputSample(sample_name="s1", sample_id=1, grid_position="Y:A1"),
            InputSample(sample_name="s2", sample_id=2, grid_position=None),
            InputSample(sample_name="s3", sample_id=3, grid_position="Y:A3"),
        ]
        positions = strategy(3, samples)

        assert positions == ["Y:A1", "", "Y:A3"]

    def test_takes_only_requested_positions(self):
        """Strategy should only take num_positions samples."""
        strategy = InputPositionStrategy()

        samples = [
            InputSample(sample_name="s1", sample_id=1, grid_position="Y:A1"),
            InputSample(sample_name="s2", sample_id=2, grid_position="Y:A2"),
            InputSample(sample_name="s3", sample_id=3, grid_position="Y:A3"),
        ]
        positions = strategy(2, samples)

        assert positions == ["Y:A1", "Y:A2"]

    def test_raises_if_not_enough_samples(self):
        """Should raise ValueError if not enough samples provided."""
        strategy = InputPositionStrategy()

        samples = [
            InputSample(sample_name="s1", sample_id=1, grid_position="Y:A1"),
        ]

        with pytest.raises(ValueError, match="Not enough input samples"):
            strategy(5, samples)

    def test_conforms_to_protocol(self):
        """Strategy should satisfy PositionStrategy protocol."""
        strategy = InputPositionStrategy()

        assert callable(strategy)
        samples = [InputSample(sample_name="s1", sample_id=1, grid_position="Y:A1")]
        result = strategy(1, samples)
        assert isinstance(result, list)


class TestCreatePositionStrategy:
    """Tests for create_position_strategy factory."""

    def test_creates_generated_strategy_for_generated_source(self):
        """Should create GeneratedPositionStrategy when position_source='generated'."""
        config = {
            "position_source": "generated",
            "plates": ["Y", "R", "B", "G"],
            "sample_rows": ["A", "B", "C", "D", "E"],
            "cols": [1, 2, 3, 4, 5, 6, 7, 8, 9],
            "qc_plate": "B",
            "position_format": "{plate}:{row}{col}",
        }
        strategy = create_position_strategy("Vanquish.vial", config)

        assert isinstance(strategy, GeneratedPositionStrategy)
        # Verify it works
        positions = strategy(3, [])
        assert len(positions) == 3

    def test_creates_input_strategy_for_input_source(self):
        """Should create InputPositionStrategy when position_source='input'."""
        config = {"position_source": "input"}
        strategy = create_position_strategy("Vanquish.plate", config)

        assert isinstance(strategy, InputPositionStrategy)

    def test_defaults_to_generated_if_not_specified(self):
        """Should default to generated strategy if position_source not in config."""
        config = {
            "plates": ["Y"],
            "sample_rows": ["A"],
            "cols": [1],
            "qc_plate": "B",
            "position_format": "{plate}:{row}{col}",
        }
        strategy = create_position_strategy("Vanquish.vial", config)

        assert isinstance(strategy, GeneratedPositionStrategy)
