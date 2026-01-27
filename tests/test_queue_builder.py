"""Tests for QueueBuilder."""

import polars as pl
import pytest

from qg.config import qg_config
from qg.params_models import QueueParameters
from qg.queue_builder import QueueBuilder


@pytest.fixture
def configs():
    """Load config bundle."""
    return qg_config()


@pytest.fixture
def sample_df():
    """Sample DataFrame with container_id column."""
    return pl.DataFrame(
        {
            "container_id": [1, 1, 1, 2, 2],
            "sample_name": ["S1", "S2", "S3", "S4", "S5"],
            "sample_id": [101, 102, 103, 104, 105],
            "tube_id": ["1/1", "1/2", "1/3", "2/1", "2/2"],
        }
    )


@pytest.fixture
def queue_parameters(configs):
    """Create valid QueueParameters."""
    return QueueParameters.create(
        configs,
        tech_area="Proteomics",
        instrument="ASTRAL_1",
        sampler="Vanquish",
        layout_mode="vial",
        output_format="xcalibur",
        queue_pattern="standard",
        polarity=["pos"],
        date="20260123",
        user="test",
    )


class TestQueueBuilderBuild:
    """Tests for QueueBuilder.build()."""

    def test_build_with_single_container(self, configs, queue_parameters):
        """Builder should create QueueInput for single container."""
        single_container_df = pl.DataFrame(
            {
                "container_id": [1, 1, 1],
                "sample_name": ["S1", "S2", "S3"],
                "sample_id": [101, 102, 103],
                "tube_id": ["1/1", "1/2", "1/3"],
            }
        )
        queue_input = (
            QueueBuilder(configs)
            .with_parameters(queue_parameters)
            .add_samples_from_dataframe(single_container_df)
            .build()
        )

        assert len(queue_input.sample_groups) == 1
        assert queue_input.sample_groups[0].container_id == 1
        assert len(queue_input.sample_groups[0].samples) == 3

    def test_build_with_multiple_containers(self, configs, queue_parameters, sample_df):
        """Builder should create QueueInput with multiple sample groups."""
        queue_input = (
            QueueBuilder(configs).with_parameters(queue_parameters).add_samples_from_dataframe(sample_df).build()
        )

        assert len(queue_input.sample_groups) == 2
        assert queue_input.sample_groups[0].container_id == 1
        assert queue_input.sample_groups[1].container_id == 2
        assert len(queue_input.get_all_samples()) == 5

    def test_build_without_parameters_raises(self, configs, sample_df):
        """Builder should raise if parameters not set."""
        builder = QueueBuilder(configs).add_samples_from_dataframe(sample_df)

        with pytest.raises(ValueError, match="Parameters not set"):
            builder.build()

    def test_build_without_samples_raises(self, configs, queue_parameters):
        """Builder should raise if no samples added."""
        builder = QueueBuilder(configs).with_parameters(queue_parameters)

        with pytest.raises(ValueError, match="No samples added"):
            builder.build()

    def test_build_twice_raises(self, configs, queue_parameters, sample_df):
        """Builder should raise if build() called twice."""
        builder = QueueBuilder(configs).with_parameters(queue_parameters).add_samples_from_dataframe(sample_df)
        builder.build()

        with pytest.raises(RuntimeError, match="Builder already used"):
            builder.build()


class TestQueueBuilderValidation:
    """Tests for QueueBuilder cross-validation."""

    def test_blocked_randomization_without_grouping_var_raises(self, configs, sample_df):
        """Blocked randomization requires grouping_var."""
        params = QueueParameters.create(
            configs,
            tech_area="Proteomics",
            instrument="ASTRAL_1",
            sampler="Vanquish",
            layout_mode="vial",
            output_format="xcalibur",
            queue_pattern="standard",
            polarity=["pos"],
            date="20260123",
            randomization="blocked",
        )

        with pytest.raises(ValueError, match="requires samples with grouping_var"):
            (QueueBuilder(configs).with_parameters(params).add_samples_from_dataframe(sample_df).build())

    def test_blocked_randomization_with_grouping_var_succeeds(self, configs):
        """Blocked randomization succeeds with grouping_var."""
        df = pl.DataFrame(
            {
                "container_id": [1, 1, 1],
                "sample_name": ["S1", "S2", "S3"],
                "sample_id": [101, 102, 103],
                "grouping_var": ["A", "B", "A"],
            }
        )
        params = QueueParameters.create(
            configs,
            tech_area="Proteomics",
            instrument="ASTRAL_1",
            sampler="Vanquish",
            layout_mode="vial",
            output_format="xcalibur",
            queue_pattern="standard",
            polarity=["pos"],
            date="20260123",
            randomization="blocked",
        )

        queue_input = QueueBuilder(configs).with_parameters(params).add_samples_from_dataframe(df).build()

        assert queue_input.parameters.randomization == "blocked"


class TestQueueBuilderDataFrame:
    """Tests for DataFrame handling."""

    def test_missing_container_id_column_raises(self, configs, queue_parameters):
        """DataFrame without container_id should raise."""
        df = pl.DataFrame(
            {
                "sample_name": ["S1"],
                "sample_id": [101],
            }
        )

        with pytest.raises(ValueError, match="must have 'container_id' column"):
            (QueueBuilder(configs).with_parameters(queue_parameters).add_samples_from_dataframe(df))

    def test_container_ids_extracted_from_dataframe(self, configs, queue_parameters, sample_df):
        """Container IDs should be extracted from DataFrame."""
        queue_input = (
            QueueBuilder(configs).with_parameters(queue_parameters).add_samples_from_dataframe(sample_df).build()
        )

        # sample_df has containers 1 and 2
        container_ids = [g.container_id for g in queue_input.sample_groups]
        assert container_ids == [1, 2]


class TestQueueBuilderPlateMode:
    """Tests for plate mode QueueInput building."""

    @pytest.fixture
    def plate_parameters(self, configs):
        """Create valid plate mode QueueParameters."""
        return QueueParameters.create(
            configs,
            tech_area="Proteomics",
            instrument="ASTRAL_1",
            sampler="Vanquish",
            layout_mode="plate",
            output_format="xcalibur",
            queue_pattern="standard",
            polarity=["pos"],
            date="20260123",
            user="test",
        )

    @pytest.fixture
    def plate_sample_df(self):
        """Sample DataFrame with plate fields."""
        return pl.DataFrame(
            {
                "container_id": [1, 1, 1],
                "sample_name": ["S1", "S2", "S3"],
                "sample_id": [101, 102, 103],
                "tray": ["Y", "Y", "Y"],
                "grid_position": ["A1", "A2", "A3"],
                "plate_id": [1001, 1001, 1001],
            }
        )

    def test_build_plate_mode_with_positions(self, configs, plate_parameters, plate_sample_df):
        """Plate mode should succeed when samples have grid_position."""
        queue_input = (
            QueueBuilder(configs).with_parameters(plate_parameters).add_samples_from_dataframe(plate_sample_df).build()
        )

        assert len(queue_input.sample_groups) == 1
        samples = queue_input.get_all_samples()
        assert samples[0].grid_position == "A1"
        assert samples[0].tray == "Y"

    def test_build_plate_mode_without_positions_raises(self, configs, plate_parameters):
        """Plate mode should raise when samples lack grid_position."""
        df_no_positions = pl.DataFrame(
            {
                "container_id": [1, 1],
                "sample_name": ["S1", "S2"],
                "sample_id": [101, 102],
            }
        )

        with pytest.raises(ValueError, match="Plate mode requires grid_position"):
            (
                QueueBuilder(configs)
                .with_parameters(plate_parameters)
                .add_samples_from_dataframe(df_no_positions)
                .build()
            )

    def test_build_plate_mode_with_empty_grid_position_raises(self, configs, plate_parameters):
        """Plate mode should raise when grid_position is empty string."""
        df_empty_positions = pl.DataFrame(
            {
                "container_id": [1, 1],
                "sample_name": ["S1", "S2"],
                "sample_id": [101, 102],
                "grid_position": ["A1", ""],  # Second sample has empty position
            }
        )

        with pytest.raises(ValueError, match="Plate mode requires grid_position"):
            (
                QueueBuilder(configs)
                .with_parameters(plate_parameters)
                .add_samples_from_dataframe(df_empty_positions)
                .build()
            )
