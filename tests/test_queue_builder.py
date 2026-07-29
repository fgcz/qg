"""Tests for QueueBuilder (new models)."""

from pathlib import Path

import polars as pl
import pytest

from qg import __version__
from qg.config_models.loader import qg_configuration
from qg.params_models import PlateQueueInput, QueueParameters, VialQueueInput
from qg.queue_builder import QueueBuilder

CONFIG_DIR = Path(__file__).parent.parent / "qg_configs"


@pytest.fixture
def config():
    qg_configuration.cache_clear()
    return qg_configuration(CONFIG_DIR)


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
def queue_parameters():
    return QueueParameters(
        tech_area="Proteomics",
        instrument="ASTRAL_1",
        sampler="Vanquish",
        output_format="xcalibur",
        queue_pattern="standard",
        queue_type="Vial",
        plate_layout="Vanquish_54",
        qc_layout_name="standard",
        polarity=["pos"],
        date="20260123",
        user="test",
    )


class TestVialMode:
    def test_build_single_container(self, config, queue_parameters):
        df = pl.DataFrame(
            {
                "container_id": [1, 1, 1],
                "sample_name": ["S1", "S2", "S3"],
                "sample_id": [101, 102, 103],
                "tube_id": ["1/1", "1/2", "1/3"],
            }
        )
        result = QueueBuilder(config).with_parameters(queue_parameters).add_samples_from_dataframe(df).build()

        assert isinstance(result, VialQueueInput)
        assert result.qg_version == __version__
        assert len(result.queue.batches) == 1
        assert len(result.queue.samples) == 3

    def test_build_multiple_containers(self, config, queue_parameters, sample_df):
        result = QueueBuilder(config).with_parameters(queue_parameters).add_samples_from_dataframe(sample_df).build()

        assert isinstance(result, VialQueueInput)
        assert len(result.queue.batches) == 2
        assert len(result.queue.samples) == 5

    def test_samples_have_container_id_fk(self, config, queue_parameters, sample_df):
        result = QueueBuilder(config).with_parameters(queue_parameters).add_samples_from_dataframe(sample_df).build()
        container_1_samples = [s for s in result.queue.samples if s.container_id == 1]
        container_2_samples = [s for s in result.queue.samples if s.container_id == 2]
        assert len(container_1_samples) == 3
        assert len(container_2_samples) == 2

    def test_bfabric_instance_round_trip(self, config, queue_parameters, sample_df):
        """`with_bfabric_instance` stamps QueueParameters.bfabric_instance and survives JSON round-trip."""
        base_url = "https://fgcz-bfabric.uzh.ch/bfabric"
        result = (
            QueueBuilder(config)
            .with_parameters(queue_parameters)
            .with_bfabric_instance(base_url)
            .add_samples_from_dataframe(sample_df)
            .build()
        )
        assert result.parameters.bfabric_instance == base_url

        round_tripped = VialQueueInput.model_validate_json(result.model_dump_json())
        assert round_tripped.parameters.bfabric_instance == base_url


class TestPlateMode:
    @pytest.fixture
    def queue_parameters(self):
        return QueueParameters(
            tech_area="Proteomics",
            instrument="ASTRAL_1",
            sampler="Vanquish",
            output_format="xcalibur",
            queue_pattern="standard",
            queue_type="Plate",
            plate_layout="Vanquish_54",
            qc_layout_name="standard",
            polarity=["pos"],
            date="20260123",
            user="test",
        )

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

    def test_build_plate_input(self, config, queue_parameters, plate_df):
        result = QueueBuilder(config).with_parameters(queue_parameters).add_samples_from_dataframe(plate_df).build()

        assert isinstance(result, PlateQueueInput)
        assert len(result.queue.batches) == 1
        assert len(result.queue.plates) == 1
        assert len(result.queue.cells) == 3

    def test_plate_has_correct_sample_count(self, config, queue_parameters, plate_df):
        result = QueueBuilder(config).with_parameters(queue_parameters).add_samples_from_dataframe(plate_df).build()
        assert result.queue.plates[1001].nr_samples == 3

    def test_cells_have_positions(self, config, queue_parameters, plate_df):
        result = QueueBuilder(config).with_parameters(queue_parameters).add_samples_from_dataframe(plate_df).build()
        positions = [c.grid_position for c in result.queue.cells]
        assert positions == ["A1", "A2", "A3"]

    def test_build_plate_with_null_tray(self, config, queue_parameters):
        """Reproduce B-Fabric plate data where tray is None."""
        df = pl.DataFrame(
            {
                "container_id": [1, 1, 1],
                "sample_name": ["S1", "S2", "S3"],
                "sample_id": [101, 102, 103],
                "plate_id": [1001, 1001, 1001],
                "tray": [None, None, None],
                "grid_position": ["A1", "A2", "A3"],
            }
        )
        result = QueueBuilder(config).with_parameters(queue_parameters).add_samples_from_dataframe(df).build()
        assert isinstance(result, PlateQueueInput)
        assert result.queue.plates[1001].tray is None
        assert len(result.queue.cells) == 3

    def test_missing_plate_columns_raises(self, config, queue_parameters):
        df = pl.DataFrame(
            {
                "container_id": [1],
                "sample_name": ["S1"],
                "sample_id": [101],
            }
        )
        with pytest.raises(ValueError, match="Plate mode requires"):
            (QueueBuilder(config).with_parameters(queue_parameters).add_samples_from_dataframe(df))


class TestBuilderErrors:
    def test_build_without_parameters_raises(self, config, sample_df):
        builder = QueueBuilder(config)
        with pytest.raises(RuntimeError, match="with_parameters"):
            builder.add_samples_from_dataframe(sample_df)

    def test_build_without_samples_succeeds(self, config, queue_parameters):
        builder = QueueBuilder(config).with_parameters(queue_parameters)
        result = builder.build()
        assert isinstance(result, VialQueueInput)
        assert result.queue.samples == []

    def test_build_twice_raises(self, config, queue_parameters, sample_df):
        builder = QueueBuilder(config).with_parameters(queue_parameters).add_samples_from_dataframe(sample_df)
        builder.build()
        with pytest.raises(RuntimeError, match="already used"):
            builder.build()

    def test_missing_container_id_raises(self, config, queue_parameters):
        df = pl.DataFrame({"sample_name": ["S1"], "sample_id": [101]})
        with pytest.raises(ValueError, match="container_id"):
            (QueueBuilder(config).with_parameters(queue_parameters).add_samples_from_dataframe(df))


class TestRandomizationValidation:
    def test_blocked_without_grouping_var_succeeds(self, config, sample_df):
        """Blocked randomization falls back to shuffle when no grouping_var."""
        params = QueueParameters(
            tech_area="Proteomics",
            instrument="ASTRAL_1",
            sampler="Vanquish",
            output_format="xcalibur",
            queue_pattern="standard",
            queue_type="Vial",
            plate_layout="Vanquish_54",
            qc_layout_name="standard",
            polarity=["pos"],
            date="20260123",
            randomization="blocked",
        )
        result = QueueBuilder(config).with_parameters(params).add_samples_from_dataframe(sample_df).build()
        assert result.parameters.randomization == "blocked"
        assert len(result.queue.samples) == 5

    def test_blocked_with_grouping_var_succeeds(self, config):
        df = pl.DataFrame(
            {
                "container_id": [1, 1],
                "sample_name": ["S1", "S2"],
                "sample_id": [101, 102],
                "grouping_var": ["A", "B"],
            }
        )
        params = QueueParameters(
            tech_area="Proteomics",
            instrument="ASTRAL_1",
            sampler="Vanquish",
            output_format="xcalibur",
            queue_pattern="standard",
            queue_type="Vial",
            plate_layout="Vanquish_54",
            qc_layout_name="standard",
            polarity=["pos"],
            date="20260123",
            randomization="blocked",
        )
        result = QueueBuilder(config).with_parameters(params).add_samples_from_dataframe(df).build()
        assert result.parameters.randomization == "blocked"
