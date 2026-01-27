"""Tests for QueueBuilder (new models)."""

import polars as pl
import pytest

from qg.config import qg_config
from qg.params_models_new import PlateQueueInput, QueueParameters, VialQueueInput
from qg.queue_builder_new import QueueBuilder


@pytest.fixture
def configs():
    return qg_config()


@pytest.fixture
def sample_df():
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
    return QueueParameters.create(
        configs,
        tech_area="Proteomics",
        instrument="ASTRAL_1",
        sampler="Vanquish",
        output_format="xcalibur",
        queue_pattern="standard",
        polarity=["pos"],
        date="20260123",
        user="test",
    )


class TestVialMode:
    def test_build_single_container(self, configs, queue_parameters):
        df = pl.DataFrame(
            {
                "container_id": [1, 1, 1],
                "sample_name": ["S1", "S2", "S3"],
                "sample_id": [101, 102, 103],
                "tube_id": ["1/1", "1/2", "1/3"],
            }
        )
        result = (
            QueueBuilder(configs)
            .with_parameters(queue_parameters, layout_mode="vial")
            .add_samples_from_dataframe(df)
            .build()
        )

        assert isinstance(result, VialQueueInput)
        assert len(result.queue.batches) == 1
        assert len(result.queue.cells) == 3

    def test_build_multiple_containers(self, configs, queue_parameters, sample_df):
        result = (
            QueueBuilder(configs)
            .with_parameters(queue_parameters, layout_mode="vial")
            .add_samples_from_dataframe(sample_df)
            .build()
        )

        assert isinstance(result, VialQueueInput)
        assert len(result.queue.batches) == 2
        assert len(result.queue.cells) == 5

    def test_samples_have_container_id_fk(self, configs, queue_parameters, sample_df):
        result = (
            QueueBuilder(configs)
            .with_parameters(queue_parameters, layout_mode="vial")
            .add_samples_from_dataframe(sample_df)
            .build()
        )
        container_1_samples = [s for s in result.queue.cells if s.container_id == 1]
        container_2_samples = [s for s in result.queue.cells if s.container_id == 2]
        assert len(container_1_samples) == 3
        assert len(container_2_samples) == 2


class TestPlateMode:
    @pytest.fixture
    def plate_df(self):
        return pl.DataFrame(
            {
                "container_id": [1, 1, 1],
                "sample_name": ["S1", "S2", "S3"],
                "sample_id": [101, 102, 103],
                "plate_id": [1001, 1001, 1001],
                "tray": ["Y", "Y", "Y"],
                "grid_position": ["A1", "A2", "A3"],
            }
        )

    def test_build_plate_input(self, configs, queue_parameters, plate_df):
        result = (
            QueueBuilder(configs)
            .with_parameters(queue_parameters, layout_mode="plate")
            .add_samples_from_dataframe(plate_df)
            .build()
        )

        assert isinstance(result, PlateQueueInput)
        assert len(result.queue.batches) == 1
        assert len(result.queue.plates) == 1
        assert len(result.queue.cells) == 3

    def test_plate_has_correct_sample_count(self, configs, queue_parameters, plate_df):
        result = (
            QueueBuilder(configs)
            .with_parameters(queue_parameters, layout_mode="plate")
            .add_samples_from_dataframe(plate_df)
            .build()
        )
        assert result.queue.plates[1001].nr_samples == 3

    def test_cells_have_positions(self, configs, queue_parameters, plate_df):
        result = (
            QueueBuilder(configs)
            .with_parameters(queue_parameters, layout_mode="plate")
            .add_samples_from_dataframe(plate_df)
            .build()
        )
        positions = [c.grid_position for c in result.queue.cells]
        assert positions == ["A1", "A2", "A3"]

    def test_missing_plate_columns_raises(self, configs, queue_parameters):
        df = pl.DataFrame(
            {
                "container_id": [1],
                "sample_name": ["S1"],
                "sample_id": [101],
            }
        )
        with pytest.raises(ValueError, match="Plate mode requires"):
            (
                QueueBuilder(configs)
                .with_parameters(queue_parameters, layout_mode="plate")
                .add_samples_from_dataframe(df)
            )


class TestBuilderErrors:
    def test_build_without_parameters_raises(self, configs, sample_df):
        builder = QueueBuilder(configs)
        with pytest.raises(RuntimeError, match="with_parameters"):
            builder.add_samples_from_dataframe(sample_df)

    def test_build_without_samples_raises(self, configs, queue_parameters):
        builder = QueueBuilder(configs).with_parameters(queue_parameters, layout_mode="vial")
        with pytest.raises(ValueError, match="No samples"):
            builder.build()

    def test_build_twice_raises(self, configs, queue_parameters, sample_df):
        builder = (
            QueueBuilder(configs)
            .with_parameters(queue_parameters, layout_mode="vial")
            .add_samples_from_dataframe(sample_df)
        )
        builder.build()
        with pytest.raises(RuntimeError, match="already used"):
            builder.build()

    def test_missing_container_id_raises(self, configs, queue_parameters):
        df = pl.DataFrame({"sample_name": ["S1"], "sample_id": [101]})
        with pytest.raises(ValueError, match="container_id"):
            (QueueBuilder(configs).with_parameters(queue_parameters, layout_mode="vial").add_samples_from_dataframe(df))


class TestRandomizationValidation:
    def test_blocked_without_grouping_var_raises(self, configs, sample_df):
        params = QueueParameters.create(
            configs,
            tech_area="Proteomics",
            instrument="ASTRAL_1",
            sampler="Vanquish",
            output_format="xcalibur",
            queue_pattern="standard",
            polarity=["pos"],
            date="20260123",
            randomization="blocked",
        )
        with pytest.raises(ValueError, match="grouping_var"):
            (
                QueueBuilder(configs)
                .with_parameters(params, layout_mode="vial")
                .add_samples_from_dataframe(sample_df)
                .build()
            )

    def test_blocked_with_grouping_var_succeeds(self, configs):
        df = pl.DataFrame(
            {
                "container_id": [1, 1],
                "sample_name": ["S1", "S2"],
                "sample_id": [101, 102],
                "grouping_var": ["A", "B"],
            }
        )
        params = QueueParameters.create(
            configs,
            tech_area="Proteomics",
            instrument="ASTRAL_1",
            sampler="Vanquish",
            output_format="xcalibur",
            queue_pattern="standard",
            polarity=["pos"],
            date="20260123",
            randomization="blocked",
        )
        result = (
            QueueBuilder(configs).with_parameters(params, layout_mode="vial").add_samples_from_dataframe(df).build()
        )
        assert result.parameters.randomization == "blocked"
