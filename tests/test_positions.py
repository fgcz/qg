"""Tests for positions.py - sampler position assignment."""

import pytest

from qg.config_models import EvosepPosition, QCLayoutPattern
from qg.config_models_samplers import (
    EvosepConfig,
    EvosepPlateConfig,
    EvosepVialConfig,
    MClass48Config,
    MClass48PlateConfig,
    MClass48VialConfig,
    SamplersConfig,
    VanquishConfig,
    VanquishPlateConfig,
    VanquishVialConfig,
)
from qg.params_models import InputSample, QueueInput, QueueParameters, SampleGroup
from qg.positions import SamplerStrategy

# =============================================================================
# Fixtures
# =============================================================================


@pytest.fixture
def vanquish_vial_config():
    """Create minimal VanquishVialConfig."""
    return VanquishVialConfig(
        container_type="Vial",
        position_source="generated",
        fill_order="row_major",
        grid_position_format="{row}{col}",
        sample_rows=["A", "B", "C", "D", "E"],
        cols=[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12],
    )


@pytest.fixture
def vanquish_plate_config():
    """Create minimal VanquishPlateConfig."""
    return VanquishPlateConfig(
        container_type="Plate",
        position_source="input",
        grid_position_format="{row}{col}",
        sample_rows=["A", "B", "C", "D", "E", "F", "G"],
        cols=[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12],
    )


@pytest.fixture
def vanquish_config(vanquish_vial_config, vanquish_plate_config):
    """Create VanquishConfig with both vial and plate."""
    return VanquishConfig(
        description="Vanquish sampler",
        plates=["Y", "R", "B", "G"],
        vial=vanquish_vial_config,
        plate=vanquish_plate_config,
    )


@pytest.fixture
def samplers_config(vanquish_config):
    """Create minimal SamplersConfig with Vanquish only."""
    return SamplersConfig(
        Vanquish=vanquish_config,
        MClass48=MClass48Config(
            description="MClass48",
            plates=["1", "2"],
            sample_rows=["A", "B", "C", "D", "E"],
            cols=[1, 2, 3, 4, 5, 6, 7, 8],
            vial=MClass48VialConfig(
                container_type="Vial",
                position_source="generated",
                fill_order="row_major",
                grid_position_format="{row}{col}",
            ),
            plate=MClass48PlateConfig(
                container_type="Plate",
                position_source="input",
                grid_position_format="{row}{col}",
            ),
        ),
        Evosep=EvosepConfig(
            description="Evosep",
            position_type="tray_position",
            slots=[1, 2, 3, 4],
            positions_per_slot=96,
            vial=EvosepVialConfig(
                container_type="Vial",
                position_source="generated",
                fill_order="sequential",
            ),
            plate=EvosepPlateConfig(
                container_type="Plate",
                position_source="input",
                grid_position_conversion="row_major",
            ),
        ),
    )


@pytest.fixture
def simple_qc_layout_pattern():
    """Create simple QCLayoutPattern with QC01 and clean positions.

    Note: positions use raw format {plate, row, col} which get_position()
    converts to unified format {tray, grid_position}.
    """
    positions = {
        "QC01": {"plate": "B", "row": "F", "col": 9},
        "clean": {"plate": "B", "row": "F", "col": 7},
        "QC03dia": {"plate": "B", "row": "F", "col": 8},
    }
    return QCLayoutPattern(positions)


@pytest.fixture
def queue_parameters():
    """Create minimal QueueParameters."""
    return QueueParameters(
        tech_area="Proteomics",
        instrument="ASTRAL_1",
        sampler="Vanquish",
        layout_mode="vial",
        output_format="xcalibur",
        queue_pattern="standard",
        polarity=[],
        date="20260124",
        user="testuser",
        method={},
        randomization="no",
        inj_vol_override=None,
        qc_frequency_override=None,
    )


def create_queue_input(n_samples: int, queue_parameters) -> QueueInput:
    """Helper to create QueueInput with n samples."""
    samples = [
        InputSample(sample_name=f"Sample_{i}", sample_id=1000 + i, tube_id=f"12345/{i}") for i in range(n_samples)
    ]
    group = SampleGroup(container_id=12345, samples=samples)
    return QueueInput(parameters=queue_parameters, sample_groups=[group])


# =============================================================================
# Tests for SamplerStrategy
# =============================================================================


class TestSamplerStrategy:
    """Tests for SamplerStrategy - the public interface for sampler operations."""

    def test_vial_sampler_assigns_positions(self, samplers_config, simple_qc_layout_pattern, queue_parameters):
        """Vial sampler assigns tray and grid_position to all samples."""
        strategy = SamplerStrategy("Vanquish", "vial", samplers_config, simple_qc_layout_pattern)
        queue_input = create_queue_input(3, queue_parameters)

        result = strategy.assign_positions_user_samples(queue_input)

        samples = result.get_all_samples()
        assert len(samples) == 3
        for sample in samples:
            assert sample.tray is not None
            assert sample.grid_position is not None
        assert samples[0].tray == "Y"
        assert samples[0].grid_position == "A1"
        assert samples[1].grid_position == "A2"
        assert samples[2].grid_position == "A3"

    def test_vial_sampler_row_major_order(self, samplers_config, simple_qc_layout_pattern, queue_parameters):
        """Vial sampler generates positions in row-major order."""
        strategy = SamplerStrategy("Vanquish", "vial", samplers_config, simple_qc_layout_pattern)
        queue_input = create_queue_input(15, queue_parameters)

        result = strategy.assign_positions_user_samples(queue_input)

        samples = result.get_all_samples()
        # First 12 samples in row A
        for i in range(12):
            assert samples[i].grid_position == f"A{i + 1}"
        # Samples 13-15 in row B
        assert samples[12].grid_position == "B1"
        assert samples[13].grid_position == "B2"
        assert samples[14].grid_position == "B3"

    def test_vial_sampler_spans_multiple_plates(self, samplers_config, simple_qc_layout_pattern, queue_parameters):
        """Vial sampler spans to next plate when current plate is full."""
        strategy = SamplerStrategy("Vanquish", "vial", samplers_config, simple_qc_layout_pattern)
        # 5 rows x 12 cols = 60 positions per plate
        queue_input = create_queue_input(65, queue_parameters)

        result = strategy.assign_positions_user_samples(queue_input)

        samples = result.get_all_samples()
        assert samples[0].tray == "Y"
        assert samples[59].tray == "Y"
        assert samples[59].grid_position == "E12"
        assert samples[60].tray == "R"
        assert samples[60].grid_position == "A1"

    def test_vial_sampler_skips_qc_positions(self, samplers_config, queue_parameters):
        """Vial sampler skips positions reserved for QC samples."""
        qc_layout = QCLayoutPattern({"QC01": {"plate": "Y", "row": "A", "col": 1}})
        strategy = SamplerStrategy("Vanquish", "vial", samplers_config, qc_layout)
        queue_input = create_queue_input(3, queue_parameters)

        result = strategy.assign_positions_user_samples(queue_input)
        samples = result.get_all_samples()

        # First sample should be A2 (not A1, which is reserved for QC01)
        assert samples[0].grid_position == "A2"
        assert samples[1].grid_position == "A3"
        assert samples[2].grid_position == "A4"

    def test_vial_sampler_skips_multiple_qc_positions(self, samplers_config, queue_parameters):
        """Vial sampler skips multiple reserved QC positions."""
        qc_layout = QCLayoutPattern(
            {
                "QC01": {"plate": "Y", "row": "A", "col": 1},
                "QC02": {"plate": "Y", "row": "A", "col": 3},
            }
        )
        strategy = SamplerStrategy("Vanquish", "vial", samplers_config, qc_layout)
        queue_input = create_queue_input(4, queue_parameters)

        result = strategy.assign_positions_user_samples(queue_input)
        samples = result.get_all_samples()

        # Positions should skip A1 and A3
        assert samples[0].grid_position == "A2"
        assert samples[1].grid_position == "A4"
        assert samples[2].grid_position == "A5"
        assert samples[3].grid_position == "A6"

    def test_plate_sampler_passes_through_positions(self, samplers_config, simple_qc_layout_pattern, queue_parameters):
        """Plate sampler passes through pre-set positions from input."""
        strategy = SamplerStrategy("Vanquish", "plate", samplers_config, simple_qc_layout_pattern)
        samples = [
            InputSample(sample_name="S1", sample_id=1001, grid_position="B1"),
            InputSample(sample_name="S2", sample_id=1002, grid_position="B2"),
        ]
        group = SampleGroup(container_id=12345, samples=samples)
        queue_input = QueueInput(parameters=queue_parameters, sample_groups=[group])

        result = strategy.assign_positions_user_samples(queue_input)

        assert result is queue_input
        assert result.get_all_samples()[0].grid_position == "B1"

    def test_plate_sampler_raises_on_qc_collision(self, samplers_config, queue_parameters):
        """Plate sampler raises ValueError if user sample collides with QC position."""
        qc_layout = QCLayoutPattern({"QC01": {"plate": "B", "row": "F", "col": 9}})
        strategy = SamplerStrategy("Vanquish", "plate", samplers_config, qc_layout)
        samples = [
            InputSample(sample_name="Conflict", sample_id=1001, tray="B", grid_position="F9"),
        ]
        group = SampleGroup(container_id=12345, samples=samples)
        queue_input = QueueInput(parameters=queue_parameters, sample_groups=[group])

        with pytest.raises(ValueError, match="conflicts with reserved QC position"):
            strategy.assign_positions_user_samples(queue_input)

    def test_plate_sampler_allows_non_conflicting_positions(self, samplers_config, queue_parameters):
        """Plate sampler allows positions that don't conflict with QC."""
        qc_layout = QCLayoutPattern({"QC01": {"plate": "B", "row": "F", "col": 9}})
        strategy = SamplerStrategy("Vanquish", "plate", samplers_config, qc_layout)
        samples = [
            InputSample(sample_name="S1", sample_id=1001, tray="Y", grid_position="A1"),
            InputSample(sample_name="S2", sample_id=1002, tray="B", grid_position="F8"),
        ]
        group = SampleGroup(container_id=12345, samples=samples)
        queue_input = QueueInput(parameters=queue_parameters, sample_groups=[group])

        result = strategy.assign_positions_user_samples(queue_input)

        assert result is queue_input

    def test_assign_positions_qc_samples_mixed_structure(
        self, samplers_config, simple_qc_layout_pattern, queue_parameters
    ):
        """assign_positions_qc_samples returns positions for QC and user slots."""
        strategy = SamplerStrategy("Vanquish", "vial", samplers_config, simple_qc_layout_pattern)
        queue_input = create_queue_input(3, queue_parameters)
        strategy.assign_positions_user_samples(queue_input)

        structure = ["QC01", "default", "default", "default", "clean"]
        positions = strategy.assign_positions_qc_samples(structure, queue_input)

        assert len(positions) == 5
        assert positions[0] == {"tray": "B", "grid_position": "F9"}  # QC01
        assert positions[1] == {"tray": "Y", "grid_position": "A1"}
        assert positions[2] == {"tray": "Y", "grid_position": "A2"}
        assert positions[3] == {"tray": "Y", "grid_position": "A3"}
        assert positions[4] == {"tray": "B", "grid_position": "F7"}  # clean

    def test_assign_positions_qc_samples_only_qc(self, samplers_config, simple_qc_layout_pattern, queue_parameters):
        """assign_positions_qc_samples handles structure with only QC slots."""
        strategy = SamplerStrategy("Vanquish", "vial", samplers_config, simple_qc_layout_pattern)
        queue_input = create_queue_input(0, queue_parameters)

        structure = ["QC01", "clean", "QC03dia"]
        positions = strategy.assign_positions_qc_samples(structure, queue_input)

        assert len(positions) == 3
        assert positions[0] == {"tray": "B", "grid_position": "F9"}
        assert positions[1] == {"tray": "B", "grid_position": "F7"}
        assert positions[2] == {"tray": "B", "grid_position": "F8"}

    def test_assign_positions_qc_samples_only_default(
        self, samplers_config, simple_qc_layout_pattern, queue_parameters
    ):
        """assign_positions_qc_samples handles structure with only default slots."""
        strategy = SamplerStrategy("Vanquish", "vial", samplers_config, simple_qc_layout_pattern)
        queue_input = create_queue_input(2, queue_parameters)
        strategy.assign_positions_user_samples(queue_input)

        structure = ["default", "default"]
        positions = strategy.assign_positions_qc_samples(structure, queue_input)

        assert len(positions) == 2
        assert positions[0] == {"tray": "Y", "grid_position": "A1"}
        assert positions[1] == {"tray": "Y", "grid_position": "A2"}

    def test_unknown_sampler_raises(self, samplers_config, simple_qc_layout_pattern):
        """Unknown sampler name raises ValueError."""
        with pytest.raises(ValueError, match="Unknown sampler"):
            SamplerStrategy("Unknown", "sampler", samplers_config, simple_qc_layout_pattern)


# =============================================================================
# Tests for MClass48 Samplers
# =============================================================================


@pytest.fixture
def mclass48_qc_layout_pattern():
    """Create QCLayoutPattern for MClass48 with positions on plate "1"."""
    positions = {
        "QC01": {"plate": "1", "row": "E", "col": 7},
        "clean": {"plate": "1", "row": "E", "col": 8},
    }
    return QCLayoutPattern(positions)


class TestMClass48SamplerStrategy:
    """Tests for MClass48 samplers via SamplerStrategy."""

    def test_mclass48_vial_assigns_positions(self, samplers_config, mclass48_qc_layout_pattern, queue_parameters):
        """MClass48 vial sampler assigns tray and grid_position to all samples."""
        strategy = SamplerStrategy("MClass48", "vial", samplers_config, mclass48_qc_layout_pattern)
        queue_input = create_queue_input(3, queue_parameters)

        result = strategy.assign_positions_user_samples(queue_input)

        samples = result.get_all_samples()
        assert len(samples) == 3
        for sample in samples:
            assert sample.tray is not None
            assert sample.grid_position is not None
        # MClass48 plates are ["1", "2"]
        assert samples[0].tray == "1"
        assert samples[0].grid_position == "A1"
        assert samples[1].grid_position == "A2"
        assert samples[2].grid_position == "A3"

    def test_mclass48_vial_row_major_order(self, samplers_config, mclass48_qc_layout_pattern, queue_parameters):
        """MClass48 vial sampler generates positions in row-major order."""
        strategy = SamplerStrategy("MClass48", "vial", samplers_config, mclass48_qc_layout_pattern)
        # MClass48 has 8 columns per row
        queue_input = create_queue_input(10, queue_parameters)

        result = strategy.assign_positions_user_samples(queue_input)

        samples = result.get_all_samples()
        # First 8 samples in row A
        for i in range(8):
            assert samples[i].grid_position == f"A{i + 1}"
        # Samples 9-10 in row B
        assert samples[8].grid_position == "B1"
        assert samples[9].grid_position == "B2"

    def test_mclass48_vial_spans_multiple_plates(self, samplers_config, mclass48_qc_layout_pattern, queue_parameters):
        """MClass48 vial sampler spans to next plate when current plate is full."""
        strategy = SamplerStrategy("MClass48", "vial", samplers_config, mclass48_qc_layout_pattern)
        # MClass48: 5 rows x 8 cols = 40 positions per plate
        queue_input = create_queue_input(45, queue_parameters)

        result = strategy.assign_positions_user_samples(queue_input)

        samples = result.get_all_samples()
        assert samples[0].tray == "1"
        # Position 38 is E6 (0-indexed: row 4, col 5), but E7 and E8 are reserved for QC
        # So after E6 comes plate 2
        assert samples[37].tray == "1"
        assert samples[37].grid_position == "E6"
        assert samples[38].tray == "2"
        assert samples[38].grid_position == "A1"

    def test_mclass48_vial_skips_qc_positions(self, samplers_config, queue_parameters):
        """MClass48 vial sampler skips positions reserved for QC samples."""
        qc_layout = QCLayoutPattern({"QC01": {"plate": "1", "row": "A", "col": 1}})
        strategy = SamplerStrategy("MClass48", "vial", samplers_config, qc_layout)
        queue_input = create_queue_input(3, queue_parameters)

        result = strategy.assign_positions_user_samples(queue_input)
        samples = result.get_all_samples()

        # First sample should be A2 (not A1, which is reserved for QC01)
        assert samples[0].grid_position == "A2"
        assert samples[1].grid_position == "A3"
        assert samples[2].grid_position == "A4"

    def test_mclass48_plate_validates_positions(self, samplers_config, mclass48_qc_layout_pattern, queue_parameters):
        """MClass48 plate sampler validates positions from input."""
        strategy = SamplerStrategy("MClass48", "plate", samplers_config, mclass48_qc_layout_pattern)
        samples = [
            InputSample(sample_name="S1", sample_id=1001, tray="1", grid_position="A1"),
            InputSample(sample_name="S2", sample_id=1002, tray="1", grid_position="A2"),
        ]
        group = SampleGroup(container_id=12345, samples=samples)
        queue_input = QueueInput(parameters=queue_parameters, sample_groups=[group])

        result = strategy.assign_positions_user_samples(queue_input)

        assert result is queue_input
        assert result.get_all_samples()[0].grid_position == "A1"

    def test_mclass48_plate_raises_on_qc_collision(self, samplers_config, queue_parameters):
        """MClass48 plate sampler raises ValueError if user sample collides with QC position."""
        qc_layout = QCLayoutPattern({"QC01": {"plate": "1", "row": "E", "col": 7}})
        strategy = SamplerStrategy("MClass48", "plate", samplers_config, qc_layout)
        samples = [
            InputSample(sample_name="Conflict", sample_id=1001, tray="1", grid_position="E7"),
        ]
        group = SampleGroup(container_id=12345, samples=samples)
        queue_input = QueueInput(parameters=queue_parameters, sample_groups=[group])

        with pytest.raises(ValueError, match="conflicts with reserved QC position"):
            strategy.assign_positions_user_samples(queue_input)

    def test_mclass48_assign_qc_samples(self, samplers_config, mclass48_qc_layout_pattern, queue_parameters):
        """MClass48 assign_positions_qc_samples returns positions for QC and user slots."""
        strategy = SamplerStrategy("MClass48", "vial", samplers_config, mclass48_qc_layout_pattern)
        queue_input = create_queue_input(2, queue_parameters)
        strategy.assign_positions_user_samples(queue_input)

        structure = ["QC01", "default", "default", "clean"]
        positions = strategy.assign_positions_qc_samples(structure, queue_input)

        assert len(positions) == 4
        assert positions[0] == {"tray": "1", "grid_position": "E7"}  # QC01
        assert positions[1] == {"tray": "1", "grid_position": "A1"}
        assert positions[2] == {"tray": "1", "grid_position": "A2"}
        assert positions[3] == {"tray": "1", "grid_position": "E8"}  # clean


# =============================================================================
# Tests for Evosep Samplers
# =============================================================================


@pytest.fixture
def evosep_qc_layout_pattern():
    """Create QCLayoutPattern for Evosep with positions on tray 5 (separate from user trays 1-4)."""
    positions = {
        "QC01": EvosepPosition(tray=5, position_start=1, position_end=48),
        "clean": EvosepPosition(tray=6, position_start=1, position_end=96),
    }
    evosep_counters = {"QC01": 1, "clean": 1}
    return QCLayoutPattern(positions, evosep_counters)


class TestEvosepSamplerStrategy:
    """Tests for Evosep samplers via SamplerStrategy."""

    def test_evosep_vial_assigns_positions(self, samplers_config, evosep_qc_layout_pattern, queue_parameters):
        """Evosep vial sampler assigns tray and grid_position to all samples."""
        strategy = SamplerStrategy("Evosep", "vial", samplers_config, evosep_qc_layout_pattern)
        queue_input = create_queue_input(3, queue_parameters)

        result = strategy.assign_positions_user_samples(queue_input)

        samples = result.get_all_samples()
        assert len(samples) == 3
        for sample in samples:
            assert sample.tray is not None
            assert sample.grid_position is not None
        # Evosep uses numeric trays (1-4) and positions (1-96)
        assert samples[0].tray == 1
        assert samples[0].grid_position == 1
        assert samples[1].grid_position == 2
        assert samples[2].grid_position == 3

    def test_evosep_vial_sequential_order(self, samplers_config, evosep_qc_layout_pattern, queue_parameters):
        """Evosep vial sampler generates positions sequentially."""
        strategy = SamplerStrategy("Evosep", "vial", samplers_config, evosep_qc_layout_pattern)
        # Test positions across a slot boundary
        queue_input = create_queue_input(100, queue_parameters)

        result = strategy.assign_positions_user_samples(queue_input)

        samples = result.get_all_samples()
        # First 96 samples on tray 1
        for i in range(96):
            assert samples[i].tray == 1
            assert samples[i].grid_position == i + 1
        # Samples 97-100 on tray 2
        assert samples[96].tray == 2
        assert samples[96].grid_position == 1
        assert samples[99].tray == 2
        assert samples[99].grid_position == 4

    def test_evosep_vial_spans_multiple_slots(self, samplers_config, evosep_qc_layout_pattern, queue_parameters):
        """Evosep vial sampler spans to next slot when current slot is full."""
        strategy = SamplerStrategy("Evosep", "vial", samplers_config, evosep_qc_layout_pattern)
        # 4 slots x 96 positions = 384 total
        queue_input = create_queue_input(200, queue_parameters)

        result = strategy.assign_positions_user_samples(queue_input)

        samples = result.get_all_samples()
        assert samples[0].tray == 1
        assert samples[95].tray == 1
        assert samples[95].grid_position == 96
        assert samples[96].tray == 2
        assert samples[96].grid_position == 1
        assert samples[191].tray == 2
        assert samples[191].grid_position == 96
        assert samples[192].tray == 3
        assert samples[192].grid_position == 1

    def test_evosep_vial_raises_when_full(self, samplers_config, evosep_qc_layout_pattern, queue_parameters):
        """Evosep vial sampler raises ValueError when all slots are full."""
        strategy = SamplerStrategy("Evosep", "vial", samplers_config, evosep_qc_layout_pattern)
        # 4 slots x 96 = 384, request more
        queue_input = create_queue_input(400, queue_parameters)

        with pytest.raises(ValueError, match="Not enough positions available"):
            strategy.assign_positions_user_samples(queue_input)

    def test_evosep_plate_converts_grid_to_numeric(self, samplers_config, evosep_qc_layout_pattern, queue_parameters):
        """Evosep plate sampler converts grid positions to numeric."""
        strategy = SamplerStrategy("Evosep", "plate", samplers_config, evosep_qc_layout_pattern)
        samples = [
            InputSample(sample_name="S1", sample_id=1001, tray=1, grid_position="A1"),
            InputSample(sample_name="S2", sample_id=1002, tray=1, grid_position="A12"),
            InputSample(sample_name="S3", sample_id=1003, tray=1, grid_position="B1"),
        ]
        group = SampleGroup(container_id=12345, samples=samples)
        queue_input = QueueInput(parameters=queue_parameters, sample_groups=[group])

        result = strategy.assign_positions_user_samples(queue_input)

        samples = result.get_all_samples()
        # A1 = 1, A12 = 12, B1 = 13 (row-major: row * 12 + col)
        assert samples[0].grid_position == 1
        assert samples[1].grid_position == 12
        assert samples[2].grid_position == 13

    def test_evosep_plate_preserves_tray(self, samplers_config, evosep_qc_layout_pattern, queue_parameters):
        """Evosep plate sampler preserves tray from input."""
        strategy = SamplerStrategy("Evosep", "plate", samplers_config, evosep_qc_layout_pattern)
        samples = [
            InputSample(sample_name="S1", sample_id=1001, tray=2, grid_position="A1"),
            InputSample(sample_name="S2", sample_id=1002, tray=3, grid_position="B1"),
        ]
        group = SampleGroup(container_id=12345, samples=samples)
        queue_input = QueueInput(parameters=queue_parameters, sample_groups=[group])

        result = strategy.assign_positions_user_samples(queue_input)

        samples = result.get_all_samples()
        assert samples[0].tray == 2
        assert samples[1].tray == 3

    def test_evosep_assign_qc_samples(self, samplers_config, evosep_qc_layout_pattern, queue_parameters):
        """Evosep assign_positions_qc_samples returns positions for QC and user slots."""
        strategy = SamplerStrategy("Evosep", "vial", samplers_config, evosep_qc_layout_pattern)
        queue_input = create_queue_input(2, queue_parameters)
        strategy.assign_positions_user_samples(queue_input)

        structure = ["QC01", "default", "default", "clean"]
        positions = strategy.assign_positions_qc_samples(structure, queue_input)

        assert len(positions) == 4
        # QC01 is on tray 5, starts at position 1
        assert positions[0] == {"tray": 5, "grid_position": 1}
        # User samples on tray 1
        assert positions[1] == {"tray": 1, "grid_position": 1}
        assert positions[2] == {"tray": 1, "grid_position": 2}
        # clean is on tray 6, starts at position 1
        assert positions[3] == {"tray": 6, "grid_position": 1}
