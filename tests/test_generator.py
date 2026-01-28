"""Tests for QueueGenerator (new models)."""

from pathlib import Path

import polars as pl
import pytest

from qg.config import qg_config
from qg.generator import QueueGenerator
from qg.params_models import (
    ContainerBatch,
    QueueParameters,
    VialQueue,
    VialQueueInput,
    VialSample,
)
from qg.queue_builder import QueueBuilder

CONFIG_DIR = Path(__file__).parent.parent / "qg_configs"


@pytest.fixture
def configs():
    return qg_config(CONFIG_DIR)


def make_vial_samples(n: int, container_id: int = 99999, id_offset: int = 10000) -> list[VialSample]:
    return [
        VialSample(sample_name=f"sample_{i}", sample_id=id_offset + i, container_id=container_id)
        for i in range(1, n + 1)
    ]


def make_vial_queue_input(
    params: QueueParameters,
    samples: list[VialSample],
    container_id: int = 99999,
) -> VialQueueInput:
    return VialQueueInput(
        parameters=params,
        queue=VialQueue(
            batches={container_id: ContainerBatch(container_id=container_id)},
            samples=samples,
        ),
    )


class TestOutputFormats:
    @pytest.mark.parametrize("output_format", ["xcalibur", "chronos", "hystar"])
    def test_output_format_has_columns(self, configs, output_format: str):
        samples = make_vial_samples(1)
        params = QueueParameters(
            tech_area="Proteomics",
            instrument="ASTRAL_1",
            sampler="Vanquish",
            output_format=output_format,
            queue_pattern="noqc",
            polarity=["pos"],
            date="20260116",
            user="test",
        )
        queue_input = make_vial_queue_input(params, samples)

        generator = QueueGenerator(configs, queue_input, layout_mode="vial")
        result = generator.generate()

        assert isinstance(result, pl.DataFrame)
        assert len(result.columns) > 0

    @pytest.mark.parametrize("output_format", ["xcalibur", "chronos", "hystar"])
    def test_single_sample_row_count(self, configs, output_format: str):
        samples = make_vial_samples(1)
        params = QueueParameters(
            tech_area="Proteomics",
            instrument="ASTRAL_1",
            sampler="Vanquish",
            output_format=output_format,
            queue_pattern="noqc",
            polarity=["pos"],
            date="20260116",
            user="test",
        )
        queue_input = make_vial_queue_input(params, samples)

        generator = QueueGenerator(configs, queue_input, layout_mode="vial")
        result = generator.generate()

        assert len(result) == 1


class TestNoQCPattern:
    @pytest.mark.parametrize("num_samples", [1, 5])
    def test_noqc_row_count(self, configs, num_samples: int):
        samples = make_vial_samples(num_samples)
        params = QueueParameters(
            tech_area="Proteomics",
            instrument="ASTRAL_1",
            sampler="Vanquish",
            output_format="xcalibur",
            queue_pattern="noqc",
            polarity=["pos"],
            date="20260116",
            user="test",
        )
        queue_input = make_vial_queue_input(params, samples)

        generator = QueueGenerator(configs, queue_input, layout_mode="vial")
        result = generator.generate()

        assert len(result) == num_samples


class TestQCOnlyPattern:
    @pytest.mark.parametrize("num_samples", [5, 10])  # 0 excluded: empty groups edge case
    def test_qc_only_row_count(self, configs, num_samples: int):
        samples = make_vial_samples(num_samples)
        params = QueueParameters(
            tech_area="Proteomics",
            instrument="ASTRAL_1",
            sampler="Vanquish",
            output_format="xcalibur",
            queue_pattern="qc_only",
            polarity=["pos"],
            date="20260116",
            user="test",
        )
        queue_input = make_vial_queue_input(params, samples)

        generator = QueueGenerator(configs, queue_input, layout_mode="vial")
        result = generator.generate()

        expected = num_samples + 6  # 3 start + N + 3 end
        assert len(result) == expected


class TestRandomization:
    def test_random_mode_shuffles_samples(self, configs):
        import random as py_random

        py_random.seed(42)

        samples = make_vial_samples(10)
        original_order = [s.sample_id for s in samples]

        params = QueueParameters(
            tech_area="Proteomics",
            instrument="ASTRAL_1",
            sampler="Vanquish",
            output_format="xcalibur",
            queue_pattern="noqc",
            polarity=["pos"],
            date="20260116",
            user="test",
            randomization="random",
        )
        queue_input = make_vial_queue_input(params, samples)

        generator = QueueGenerator(configs, queue_input, layout_mode="vial")
        result = generator.build_rows()

        result_order = [int(row.sample_id) for row in result.rows if row.sample_type == "user"]

        assert result_order != original_order
        assert set(result_order) == set(original_order)

    def test_blocked_mode_creates_blocks(self, configs):
        import random as py_random

        py_random.seed(42)

        container_id = 99999
        samples = [
            VialSample(sample_name="a1", sample_id=1, grouping_var="A", container_id=container_id),
            VialSample(sample_name="a2", sample_id=2, grouping_var="A", container_id=container_id),
            VialSample(sample_name="b1", sample_id=3, grouping_var="B", container_id=container_id),
            VialSample(sample_name="b2", sample_id=4, grouping_var="B", container_id=container_id),
            VialSample(sample_name="c1", sample_id=5, grouping_var="C", container_id=container_id),
            VialSample(sample_name="c2", sample_id=6, grouping_var="C", container_id=container_id),
        ]

        params = QueueParameters(
            tech_area="Proteomics",
            instrument="ASTRAL_1",
            sampler="Vanquish",
            output_format="xcalibur",
            queue_pattern="noqc",
            polarity=["pos"],
            date="20260116",
            user="test",
            randomization="blocked",
        )
        queue_input = make_vial_queue_input(params, samples, container_id)

        generator = QueueGenerator(configs, queue_input, layout_mode="vial")
        result = generator.build_rows()

        result_ids = [int(row.sample_id) for row in result.rows if row.sample_type == "user"]

        block1 = set(result_ids[:3])
        block2 = set(result_ids[3:])

        assert block1 == {1, 3, 5}
        assert block2 == {2, 4, 6}

    def test_no_randomization_preserves_order(self, configs):
        samples = make_vial_samples(5)
        original_order = [s.sample_id for s in samples]

        params = QueueParameters(
            tech_area="Proteomics",
            instrument="ASTRAL_1",
            sampler="Vanquish",
            output_format="xcalibur",
            queue_pattern="noqc",
            polarity=["pos"],
            date="20260116",
            user="test",
            randomization="no",
        )
        queue_input = make_vial_queue_input(params, samples)

        generator = QueueGenerator(configs, queue_input, layout_mode="vial")
        result = generator.build_rows()

        result_order = [int(row.sample_id) for row in result.rows if row.sample_type == "user"]
        assert result_order == original_order


class TestPlateMode:
    """Integration tests for plate-mode queue generation (builder + generator)."""

    @pytest.fixture
    def plate_df_null_tray(self):
        """Plate DataFrame mimicking B-Fabric output where tray is None."""
        return pl.DataFrame(
            {
                "container_id": [39431] * 5,
                "sample_name": [f"Sample_{i}" for i in range(1, 6)],
                "sample_id": list(range(991775, 991780)),
                "plate_id": [5001] * 5,
                "tray": [None] * 5,
                "grid_position": ["A1", "A2", "A3", "A4", "A5"],
                "position": [1, 2, 3, 4, 5],
                "grouping_var": [None] * 5,
            }
        )

    def test_plate_null_tray_builder_succeeds(self, configs, plate_df_null_tray):
        params = QueueParameters.create(
            configs,
            tech_area="Metabolomics",
            instrument="EXPLORIS_3",
            sampler="Vanquish",
            output_format="xcalibur",
            queue_pattern="standard",
            polarity=["pos", "neg"],
            date="20260127",
            user="test",
        )
        result = (
            QueueBuilder(configs)
            .with_parameters(params, layout_mode="plate")
            .add_samples_from_dataframe(plate_df_null_tray)
            .build()
        )
        assert len(result.queue.cells) == 5
        assert result.queue.plates[5001].tray is None

    def test_plate_null_tray_full_pipeline(self, configs, plate_df_null_tray):
        params = QueueParameters.create(
            configs,
            tech_area="Metabolomics",
            instrument="EXPLORIS_3",
            sampler="Vanquish",
            output_format="xcalibur",
            queue_pattern="standard",
            polarity=["pos", "neg"],
            date="20260127",
            user="test",
        )
        queue_input = (
            QueueBuilder(configs)
            .with_parameters(params, layout_mode="plate")
            .add_samples_from_dataframe(plate_df_null_tray)
            .build()
        )
        generator = QueueGenerator(configs, queue_input, layout_mode="plate")
        result = generator.generate()

        assert isinstance(result, pl.DataFrame)
        assert len(result) > 5  # samples + QC rows

        # Verify tray was assigned by the plate sampler (was None from B-Fabric)
        rows = generator.build_rows()
        user_rows = [r for r in rows.rows if r.sample_type == "user"]
        assert all(r.tray is not None for r in user_rows), "Plate sampler should assign tray to plates with tray=None"
