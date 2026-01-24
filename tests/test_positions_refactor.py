"""Tests for positions_refactor.py - sampler position assignment."""

import pytest

from qg.config_models import QCLayoutPattern
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
from qg.positions_refactor import (
    SamplerStrategy,
    _VanquishPlateSampler_prototype,
    _VanquishVialSampler_prototype,
    create_sampler,
)

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
        sampler="Vanquish.vial",
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
# Tests for _VanquishVialSampler_prototype
# =============================================================================


class TestVanquishVialSamplerPrototype:
    """Tests for _VanquishVialSampler_prototype."""

    def test_assign_user_positions_sets_tray_and_grid(
        self, vanquish_config, vanquish_vial_config, simple_qc_layout_pattern, queue_parameters
    ):
        """assign_positions_user_samples sets tray and grid_position on all samples."""
        sampler = _VanquishVialSampler_prototype(vanquish_config, vanquish_vial_config, simple_qc_layout_pattern)
        queue_input = create_queue_input(3, queue_parameters)

        result = sampler.assign_positions_user_samples(queue_input)

        samples = result.get_all_samples()
        assert len(samples) == 3

        # All samples should have tray and grid_position set
        for sample in samples:
            assert sample.tray is not None
            assert sample.grid_position is not None

        # First sample should be at Y:A1
        assert samples[0].tray == "Y"
        assert samples[0].grid_position == "A1"

    def test_position_generation_row_major_order(
        self, vanquish_config, vanquish_vial_config, simple_qc_layout_pattern, queue_parameters
    ):
        """Positions are generated in row-major order: A1, A2, ..., A12, B1, B2, ..."""
        sampler = _VanquishVialSampler_prototype(vanquish_config, vanquish_vial_config, simple_qc_layout_pattern)
        # Create enough samples to span multiple rows
        queue_input = create_queue_input(15, queue_parameters)

        result = sampler.assign_positions_user_samples(queue_input)

        samples = result.get_all_samples()

        # First 12 samples should be in row A (cols 1-12)
        for i in range(12):
            assert samples[i].tray == "Y"
            assert samples[i].grid_position == f"A{i + 1}"

        # Samples 13-15 should be in row B (cols 1-3)
        assert samples[12].tray == "Y"
        assert samples[12].grid_position == "B1"
        assert samples[13].tray == "Y"
        assert samples[13].grid_position == "B2"
        assert samples[14].tray == "Y"
        assert samples[14].grid_position == "B3"

    def test_positions_span_multiple_plates(
        self, vanquish_config, vanquish_vial_config, simple_qc_layout_pattern, queue_parameters
    ):
        """When more samples than fit on one plate, positions span to next plate."""
        sampler = _VanquishVialSampler_prototype(vanquish_config, vanquish_vial_config, simple_qc_layout_pattern)
        # 5 rows x 12 cols = 60 positions per plate
        # Create 65 samples to span to second plate
        queue_input = create_queue_input(65, queue_parameters)

        result = sampler.assign_positions_user_samples(queue_input)

        samples = result.get_all_samples()

        # First 60 samples on plate Y
        assert samples[0].tray == "Y"
        assert samples[59].tray == "Y"
        assert samples[59].grid_position == "E12"

        # Samples 61-65 on plate R
        assert samples[60].tray == "R"
        assert samples[60].grid_position == "A1"
        assert samples[64].tray == "R"
        assert samples[64].grid_position == "A5"

    def test_get_qc_positions_returns_correct_position(
        self, vanquish_config, vanquish_vial_config, simple_qc_layout_pattern
    ):
        """get_qc_positions returns position from QCLayoutPattern."""
        sampler = _VanquishVialSampler_prototype(vanquish_config, vanquish_vial_config, simple_qc_layout_pattern)

        pos = sampler.get_qc_positions("QC01")

        assert pos == {"tray": "B", "grid_position": "F9"}

    def test_position_generation_skips_qc_positions(self, vanquish_config, vanquish_vial_config, queue_parameters):
        """User positions skip positions reserved for QC samples."""
        # Create QC layout with position at Y:A1 (conflicts with first user position)
        qc_layout_with_conflict = QCLayoutPattern({"QC01": {"plate": "Y", "row": "A", "col": 1}})

        sampler = _VanquishVialSampler_prototype(vanquish_config, vanquish_vial_config, qc_layout_with_conflict)
        queue_input = create_queue_input(3, queue_parameters)

        result = sampler.assign_positions_user_samples(queue_input)
        samples = result.get_all_samples()

        # First sample should be A2 (not A1, which is reserved for QC01)
        assert samples[0].grid_position == "A2"
        assert samples[1].grid_position == "A3"
        assert samples[2].grid_position == "A4"

    def test_position_generation_skips_multiple_qc_positions(
        self, vanquish_config, vanquish_vial_config, queue_parameters
    ):
        """User positions skip multiple reserved QC positions."""
        # Create QC layout with positions at Y:A1 and Y:A3
        qc_layout_with_conflicts = QCLayoutPattern(
            {
                "QC01": {"plate": "Y", "row": "A", "col": 1},
                "QC02": {"plate": "Y", "row": "A", "col": 3},
            }
        )

        sampler = _VanquishVialSampler_prototype(vanquish_config, vanquish_vial_config, qc_layout_with_conflicts)
        queue_input = create_queue_input(4, queue_parameters)

        result = sampler.assign_positions_user_samples(queue_input)
        samples = result.get_all_samples()

        # Positions should skip A1 and A3: A2, A4, A5, A6
        assert samples[0].grid_position == "A2"
        assert samples[1].grid_position == "A4"
        assert samples[2].grid_position == "A5"
        assert samples[3].grid_position == "A6"


# =============================================================================
# Tests for _VanquishPlateSampler_prototype
# =============================================================================


class TestVanquishPlateSamplerPrototype:
    """Tests for _VanquishPlateSampler_prototype."""

    def test_assign_user_positions_returns_queue_input(
        self, vanquish_config, vanquish_plate_config, simple_qc_layout_pattern, queue_parameters
    ):
        """assign_positions_user_samples returns QueueInput (positions from input)."""
        sampler = _VanquishPlateSampler_prototype(vanquish_config, vanquish_plate_config, simple_qc_layout_pattern)
        # For plate mode, samples should have pre-set grid_positions
        samples = [
            InputSample(sample_name="S1", sample_id=1001, grid_position="B1"),
            InputSample(sample_name="S2", sample_id=1002, grid_position="B2"),
            InputSample(sample_name="S3", sample_id=1003, grid_position="B3"),
        ]
        group = SampleGroup(container_id=12345, samples=samples)
        queue_input = QueueInput(parameters=queue_parameters, sample_groups=[group])

        result = sampler.assign_positions_user_samples(queue_input)

        # Plate sampler just returns the queue_input (positions already set)
        assert result is queue_input
        assert result.get_all_samples()[0].grid_position == "B1"


# =============================================================================
# Tests for create_sampler factory
# =============================================================================


class TestCreateSampler:
    """Tests for create_sampler factory function."""

    def test_creates_vanquish_vial_sampler(self, samplers_config, simple_qc_layout_pattern):
        """Factory creates VanquishVialSampler for 'Vanquish.vial'."""
        sampler = create_sampler("Vanquish.vial", samplers_config, simple_qc_layout_pattern)

        assert isinstance(sampler, _VanquishVialSampler_prototype)

    def test_creates_vanquish_plate_sampler(self, samplers_config, simple_qc_layout_pattern):
        """Factory creates VanquishPlateSampler for 'Vanquish.plate'."""
        sampler = create_sampler("Vanquish.plate", samplers_config, simple_qc_layout_pattern)

        assert isinstance(sampler, _VanquishPlateSampler_prototype)

    def test_raises_for_unknown_sampler(self, samplers_config, simple_qc_layout_pattern):
        """Factory raises ValueError for unknown sampler name."""
        with pytest.raises(ValueError, match="Unknown sampler: UnknownSampler"):
            create_sampler("UnknownSampler", samplers_config, simple_qc_layout_pattern)


# =============================================================================
# Tests for SamplerStrategy
# =============================================================================


class TestSamplerStrategy:
    """Tests for SamplerStrategy wrapper class."""

    def test_assign_positions_user_samples_delegates_to_sampler(
        self, samplers_config, simple_qc_layout_pattern, queue_parameters
    ):
        """SamplerStrategy.assign_positions_user_samples delegates to underlying sampler."""
        strategy = SamplerStrategy("Vanquish.vial", samplers_config, simple_qc_layout_pattern)
        queue_input = create_queue_input(3, queue_parameters)

        result = strategy.assign_positions_user_samples(queue_input)

        samples = result.get_all_samples()
        # Verify positions were assigned
        assert samples[0].tray == "Y"
        assert samples[0].grid_position == "A1"
        assert samples[1].tray == "Y"
        assert samples[1].grid_position == "A2"
        assert samples[2].tray == "Y"
        assert samples[2].grid_position == "A3"

    def test_assign_positions_qc_samples_with_mixed_structure(
        self, samplers_config, simple_qc_layout_pattern, queue_parameters
    ):
        """assign_positions_qc_samples returns positions for QC and user slots."""
        strategy = SamplerStrategy("Vanquish.vial", samplers_config, simple_qc_layout_pattern)
        queue_input = create_queue_input(3, queue_parameters)

        # First assign user positions
        strategy.assign_positions_user_samples(queue_input)

        # Structure with QC and user slots
        structure = ["QC01", "default", "default", "default", "clean"]

        positions = strategy.assign_positions_qc_samples(structure, queue_input)

        assert len(positions) == 5

        # QC01 position from layout
        assert positions[0] == {"tray": "B", "grid_position": "F9"}

        # User positions from queue_input
        assert positions[1] == {"tray": "Y", "grid_position": "A1"}
        assert positions[2] == {"tray": "Y", "grid_position": "A2"}
        assert positions[3] == {"tray": "Y", "grid_position": "A3"}

        # clean position from layout
        assert positions[4] == {"tray": "B", "grid_position": "F7"}

    def test_assign_positions_qc_samples_only_qc(self, samplers_config, simple_qc_layout_pattern, queue_parameters):
        """assign_positions_qc_samples handles structure with only QC slots."""
        strategy = SamplerStrategy("Vanquish.vial", samplers_config, simple_qc_layout_pattern)
        queue_input = create_queue_input(0, queue_parameters)

        structure = ["QC01", "clean", "QC03dia"]

        positions = strategy.assign_positions_qc_samples(structure, queue_input)

        assert len(positions) == 3
        assert positions[0] == {"tray": "B", "grid_position": "F9"}  # QC01
        assert positions[1] == {"tray": "B", "grid_position": "F7"}  # clean
        assert positions[2] == {"tray": "B", "grid_position": "F8"}  # QC03dia

    def test_assign_positions_qc_samples_only_default(
        self, samplers_config, simple_qc_layout_pattern, queue_parameters
    ):
        """assign_positions_qc_samples handles structure with only default slots."""
        strategy = SamplerStrategy("Vanquish.vial", samplers_config, simple_qc_layout_pattern)
        queue_input = create_queue_input(2, queue_parameters)

        # First assign user positions
        strategy.assign_positions_user_samples(queue_input)

        structure = ["default", "default"]

        positions = strategy.assign_positions_qc_samples(structure, queue_input)

        assert len(positions) == 2
        assert positions[0] == {"tray": "Y", "grid_position": "A1"}
        assert positions[1] == {"tray": "Y", "grid_position": "A2"}
