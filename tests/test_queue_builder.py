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
        sampler="Vanquish.vial",
        output_format="xcalibur",
        queue_pattern="standard",
        polarity=["pos"],
        date="20260123",
        user="test",
    )


class TestQueueBuilderBuild:
    """Tests for QueueBuilder.build()."""

    def test_build_with_single_container(self, configs, queue_parameters, sample_df):
        """Builder should create QueueInput for single container."""
        queue_input = (
            QueueBuilder(configs).with_parameters(queue_parameters).add_samples_from_dataframe(sample_df, [1]).build()
        )

        assert len(queue_input.sample_groups) == 1
        assert queue_input.sample_groups[0].container_id == 1
        assert len(queue_input.sample_groups[0].samples) == 3

    def test_build_with_multiple_containers(self, configs, queue_parameters, sample_df):
        """Builder should create QueueInput with multiple sample groups."""
        queue_input = (
            QueueBuilder(configs)
            .with_parameters(queue_parameters)
            .add_samples_from_dataframe(sample_df, [1, 2])
            .build()
        )

        assert len(queue_input.sample_groups) == 2
        assert queue_input.sample_groups[0].container_id == 1
        assert queue_input.sample_groups[1].container_id == 2
        assert len(queue_input.get_all_samples()) == 5

    def test_build_without_parameters_raises(self, configs, sample_df):
        """Builder should raise if parameters not set."""
        builder = QueueBuilder(configs).add_samples_from_dataframe(sample_df, [1])

        with pytest.raises(ValueError, match="Parameters not set"):
            builder.build()

    def test_build_without_samples_raises(self, configs, queue_parameters):
        """Builder should raise if no samples added."""
        builder = QueueBuilder(configs).with_parameters(queue_parameters)

        with pytest.raises(ValueError, match="No samples added"):
            builder.build()

    def test_build_twice_raises(self, configs, queue_parameters, sample_df):
        """Builder should raise if build() called twice."""
        builder = QueueBuilder(configs).with_parameters(queue_parameters).add_samples_from_dataframe(sample_df, [1])
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
            sampler="Vanquish.vial",
            output_format="xcalibur",
            queue_pattern="standard",
            polarity=["pos"],
            date="20260123",
            randomization="blocked",
        )

        with pytest.raises(ValueError, match="requires samples with grouping_var"):
            (QueueBuilder(configs).with_parameters(params).add_samples_from_dataframe(sample_df, [1]).build())

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
            sampler="Vanquish.vial",
            output_format="xcalibur",
            queue_pattern="standard",
            polarity=["pos"],
            date="20260123",
            randomization="blocked",
        )

        queue_input = QueueBuilder(configs).with_parameters(params).add_samples_from_dataframe(df, [1]).build()

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
            (QueueBuilder(configs).with_parameters(queue_parameters).add_samples_from_dataframe(df, [1]))

    def test_empty_container_ids_creates_empty_groups(self, configs, queue_parameters, sample_df):
        """Passing container IDs not in DataFrame creates no groups."""
        builder = (
            QueueBuilder(configs)
            .with_parameters(queue_parameters)
            .add_samples_from_dataframe(sample_df, [999])  # Non-existent container
        )

        with pytest.raises(ValueError, match="No samples added"):
            builder.build()
