"""Tests for QueueGenerator (new models using QGConfiguration)."""

from pathlib import Path

import polars as pl
import pytest

from qg.config_models.loader import qg_configuration
from qg.generator import QueueGenerator
from qg.params_models import (
    ContainerBatch,
    QueueParameters,
    VialQueue,
    VialQueueInput,
    VialSample,
)

CONFIG_DIR = Path(__file__).parent.parent / "qg_configs"


@pytest.fixture
def config():
    return qg_configuration(CONFIG_DIR)


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


class TestFormatTable:
    """Tests for format_table(), including literal: prefix handling."""

    def test_literal_prefix_produces_constant_column(self):
        """Columns with 'literal:VALUE' produce a constant column."""
        from qg.config_models.formatting import OutputFormat
        from qg.generator import QueueRow, QueueRowTable, format_table

        fmt = OutputFormat(
            description="test",
            file_extension=".csv",
            position_format="{tray}:{grid_position}",
            columns={
                "File Name": "file_name",
                "Lab": "literal:FGCZ",
                "Sample ID": "sample_id",
            },
        )
        rows = QueueRowTable(
            rows=[
                QueueRow(
                    run_number=1,
                    sample_type="user",
                    sample_id="100",
                    sample_name="s1",
                    tray="Y",
                    grid_position="A1",
                    row="A",
                    col=1,
                    inj_vol=2.0,
                    method="m.meth",
                    file_name="f1",
                    data_path="D:\\data",
                    container_id=1,
                ),
                QueueRow(
                    run_number=2,
                    sample_type="user",
                    sample_id="200",
                    sample_name="s2",
                    tray="Y",
                    grid_position="A2",
                    row="A",
                    col=2,
                    inj_vol=2.0,
                    method="m.meth",
                    file_name="f2",
                    data_path="D:\\data",
                    container_id=1,
                ),
            ]
        )

        df = format_table(rows, fmt)

        assert df.columns == ["File Name", "Lab", "Sample ID"]
        assert df["Lab"].to_list() == ["FGCZ", "FGCZ"]
        assert df["File Name"].to_list() == ["f1", "f2"]
        assert df["Sample ID"].to_list() == ["100", "200"]

    def test_literal_prefix_preserves_column_order(self):
        """Literal columns appear in the position defined by columns dict."""
        from qg.config_models.formatting import OutputFormat
        from qg.generator import QueueRow, QueueRowTable, format_table

        fmt = OutputFormat(
            description="test",
            file_extension=".csv",
            position_format="{tray}:{grid_position}",
            columns={
                "A": "file_name",
                "B": "literal:constant_b",
                "C": "sample_id",
                "D": "literal:constant_d",
            },
        )
        rows = QueueRowTable(
            rows=[
                QueueRow(
                    run_number=1,
                    sample_type="user",
                    sample_id="1",
                    sample_name="s",
                    tray="Y",
                    grid_position="A1",
                    row="A",
                    col=1,
                    inj_vol=1.0,
                    method="",
                    file_name="f",
                    data_path="",
                    container_id=1,
                ),
            ]
        )

        df = format_table(rows, fmt)

        assert df.columns == ["A", "B", "C", "D"]
        assert df["B"].item() == "constant_b"
        assert df["D"].item() == "constant_d"

    def test_no_literal_columns_still_works(self):
        """Format with only field references (no literal:) works as before."""
        from qg.config_models.formatting import OutputFormat
        from qg.generator import QueueRow, QueueRowTable, format_table

        fmt = OutputFormat(
            description="test",
            file_extension=".csv",
            position_format="{tray}:{grid_position}",
            columns={"Name": "file_name", "ID": "sample_id"},
        )
        rows = QueueRowTable(
            rows=[
                QueueRow(
                    run_number=1,
                    sample_type="user",
                    sample_id="42",
                    sample_name="s",
                    tray="Y",
                    grid_position="A1",
                    row="A",
                    col=1,
                    inj_vol=1.0,
                    method="",
                    file_name="myfile",
                    data_path="",
                    container_id=1,
                ),
            ]
        )

        df = format_table(rows, fmt)

        assert df.columns == ["Name", "ID"]
        assert df["Name"].item() == "myfile"
        assert df["ID"].item() == "42"


class TestOutputFormats:
    @pytest.mark.parametrize("output_format", ["xcalibur", "chronos", "hystar"])
    def test_output_format_has_columns(self, config, output_format: str):
        samples = make_vial_samples(1)
        params = QueueParameters(
            tech_area="Proteomics",
            instrument="ASTRAL_1",
            sampler="Vanquish",
            output_format=output_format,
            queue_pattern="noqc",
            queue_type="Vial",
            plate_layout="Vanquish_54",
            polarity=["pos"],
            date="20260116",
            user="test",
        )
        queue_input = make_vial_queue_input(params, samples)

        generator = QueueGenerator(config, queue_input, layout_mode="vial")
        result = generator.generate()

        assert isinstance(result, pl.DataFrame)
        assert len(result.columns) > 0

    def test_xcalibur_has_literal_l3_laboratory(self, config):
        """Xcalibur output includes L3 Laboratory column with constant 'FGCZ'."""
        samples = make_vial_samples(3)
        params = QueueParameters(
            tech_area="Proteomics",
            instrument="ASTRAL_1",
            sampler="Vanquish",
            output_format="xcalibur",
            queue_pattern="noqc",
            queue_type="Vial",
            plate_layout="Vanquish_54",
            polarity=["pos"],
            date="20260116",
            user="test",
        )
        queue_input = make_vial_queue_input(params, samples)

        generator = QueueGenerator(config, queue_input, layout_mode="vial")
        result = generator.generate()

        assert "L3 Laboratory" in result.columns
        assert result["L3 Laboratory"].to_list() == ["FGCZ"] * 3
        # Verify column order: L3 Laboratory between Inj Vol and Sample ID
        cols = result.columns
        assert cols.index("L3 Laboratory") == cols.index("Inj Vol") + 1
        assert cols.index("L3 Laboratory") == cols.index("Sample ID") - 1

    @pytest.mark.parametrize("output_format", ["xcalibur", "chronos", "hystar"])
    def test_single_sample_row_count(self, config, output_format: str):
        samples = make_vial_samples(1)
        params = QueueParameters(
            tech_area="Proteomics",
            instrument="ASTRAL_1",
            sampler="Vanquish",
            output_format=output_format,
            queue_pattern="noqc",
            queue_type="Vial",
            plate_layout="Vanquish_54",
            polarity=["pos"],
            date="20260116",
            user="test",
        )
        queue_input = make_vial_queue_input(params, samples)

        generator = QueueGenerator(config, queue_input, layout_mode="vial")
        result = generator.generate()

        assert len(result) == 1


class TestNoQCPattern:
    @pytest.mark.parametrize("num_samples", [1, 5])
    def test_noqc_row_count(self, config, num_samples: int):
        samples = make_vial_samples(num_samples)
        params = QueueParameters(
            tech_area="Proteomics",
            instrument="ASTRAL_1",
            sampler="Vanquish",
            output_format="xcalibur",
            queue_pattern="noqc",
            queue_type="Vial",
            plate_layout="Vanquish_54",
            polarity=["pos"],
            date="20260116",
            user="test",
        )
        queue_input = make_vial_queue_input(params, samples)

        generator = QueueGenerator(config, queue_input, layout_mode="vial")
        result = generator.generate()

        assert len(result) == num_samples


class TestQCOnlyPattern:
    @pytest.mark.parametrize("num_samples", [5, 10])
    def test_qc_only_row_count(self, config, num_samples: int):
        samples = make_vial_samples(num_samples)
        params = QueueParameters(
            tech_area="Proteomics",
            instrument="ASTRAL_1",
            sampler="Vanquish",
            output_format="xcalibur",
            queue_pattern="qc_only",
            queue_type="Vial",
            plate_layout="Vanquish_54",
            polarity=["pos"],
            date="20260116",
            user="test",
        )
        queue_input = make_vial_queue_input(params, samples)

        generator = QueueGenerator(config, queue_input, layout_mode="vial")
        result = generator.generate()

        # qc_only pattern: start(3) + samples + end(3) = 6 + num_samples
        expected = num_samples + 6
        assert len(result) == expected


class TestRandomization:
    def test_random_mode_shuffles_samples(self, config):
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
            queue_type="Vial",
            plate_layout="Vanquish_54",
            polarity=["pos"],
            date="20260116",
            user="test",
            randomization="random",
        )
        queue_input = make_vial_queue_input(params, samples)

        generator = QueueGenerator(config, queue_input, layout_mode="vial")
        result = generator.build_rows()

        result_order = [int(row.sample_id) for row in result.rows if row.sample_type == "user"]

        assert result_order != original_order
        assert set(result_order) == set(original_order)

    def test_blocked_mode_creates_blocks(self, config):
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
            queue_type="Vial",
            plate_layout="Vanquish_54",
            polarity=["pos"],
            date="20260116",
            user="test",
            randomization="blocked",
        )
        queue_input = make_vial_queue_input(params, samples, container_id)

        generator = QueueGenerator(config, queue_input, layout_mode="vial")
        result = generator.build_rows()

        result_ids = [int(row.sample_id) for row in result.rows if row.sample_type == "user"]

        # Each block has exactly one sample from each group
        group_ids = {"A": {1, 2}, "B": {3, 4}, "C": {5, 6}}
        block1 = set(result_ids[:3])
        block2 = set(result_ids[3:])

        for group_name, ids in group_ids.items():
            assert len(block1 & ids) == 1, f"Block 1 should have exactly 1 from group {group_name}"
            assert len(block2 & ids) == 1, f"Block 2 should have exactly 1 from group {group_name}"

        assert set(result_ids) == {1, 2, 3, 4, 5, 6}

    def test_no_randomization_preserves_order(self, config):
        samples = make_vial_samples(5)
        original_order = [s.sample_id for s in samples]

        params = QueueParameters(
            tech_area="Proteomics",
            instrument="ASTRAL_1",
            sampler="Vanquish",
            output_format="xcalibur",
            queue_pattern="noqc",
            queue_type="Vial",
            plate_layout="Vanquish_54",
            polarity=["pos"],
            date="20260116",
            user="test",
            randomization="no",
        )
        queue_input = make_vial_queue_input(params, samples)

        generator = QueueGenerator(config, queue_input, layout_mode="vial")
        result = generator.build_rows()

        result_order = [int(row.sample_id) for row in result.rows if row.sample_type == "user"]
        assert result_order == original_order


class TestDifferentSamplers:
    @pytest.mark.parametrize(
        "sampler,plate_layout",
        [("Vanquish", "Vanquish_54"), ("MClass", "MClass_48"), ("Evosep", "Evosep_96")],
    )
    def test_sampler_generates_queue(self, config, sampler: str, plate_layout: str):
        samples = make_vial_samples(3)
        params = QueueParameters(
            tech_area="Proteomics",
            instrument="ASTRAL_1",
            sampler=sampler,
            output_format="xcalibur",
            queue_pattern="noqc",
            queue_type="Vial",
            plate_layout=plate_layout,
            polarity=["pos"],
            date="20260116",
            user="test",
        )
        queue_input = make_vial_queue_input(params, samples)

        generator = QueueGenerator(config, queue_input, layout_mode="vial")
        result = generator.generate()

        assert isinstance(result, pl.DataFrame)
        assert len(result) == 3


class TestEvosepQCPattern:
    """Integration test for the evosep_qc pattern (Evosep: clean-QC03 bookends)."""

    def test_evosep_qc_slot_sequence(self, config):
        samples = make_vial_samples(5)
        params = QueueParameters(
            tech_area="Proteomics",
            instrument="ASTRAL_1",
            sampler="Evosep",
            output_format="chronos",
            queue_pattern="evosep_qc",
            queue_type="Vial",
            plate_layout="Evosep_96",
            polarity=["pos"],
            date="20260116",
            user="test",
        )
        queue_input = make_vial_queue_input(params, samples)

        generator = QueueGenerator(config, queue_input, layout_mode="vial")
        result = generator.build_rows()

        sample_ids = [row.sample_id for row in result.rows]
        sample_types = [row.sample_type for row in result.rows]

        # Pattern: start=[clean, QC03dia] + 5 samples + end=[clean, QC03dia]
        assert len(result.rows) == 9
        assert sample_types == ["qc", "qc", "user", "user", "user", "user", "user", "qc", "qc"]
        assert sample_ids[0] == "clean"
        assert sample_ids[1] == "QC03dia"
        assert sample_ids[-2] == "clean"
        assert sample_ids[-1] == "QC03dia"

    def test_evosep_qc_positions_on_tray6(self, config):
        samples = make_vial_samples(5)
        params = QueueParameters(
            tech_area="Proteomics",
            instrument="ASTRAL_1",
            sampler="Evosep",
            output_format="chronos",
            queue_pattern="evosep_qc",
            queue_type="Vial",
            plate_layout="Evosep_96",
            polarity=["pos"],
            date="20260116",
            user="test",
        )
        queue_input = make_vial_queue_input(params, samples)

        generator = QueueGenerator(config, queue_input, layout_mode="vial")
        result = generator.build_rows()

        qc_rows = [row for row in result.rows if row.sample_type == "qc"]

        for row in qc_rows:
            # All QC should be on tray 6
            assert row.tray == 6, f"{row.sample_id} should be on tray 6, got {row.tray}"

        # QC03dia positions should be <= 49
        qc03_rows = [row for row in qc_rows if row.sample_id == "QC03dia"]
        for row in qc03_rows:
            assert row.grid_position <= 49, f"QC03dia position {row.grid_position} should be <= 49"

        # Clean positions should be >= 50
        clean_rows = [row for row in qc_rows if row.sample_id == "clean"]
        for row in clean_rows:
            assert row.grid_position >= 50, f"clean position {row.grid_position} should be >= 50"

    def test_evosep_qc_user_samples_on_trays_1_to_5(self, config):
        samples = make_vial_samples(5)
        params = QueueParameters(
            tech_area="Proteomics",
            instrument="ASTRAL_1",
            sampler="Evosep",
            output_format="chronos",
            queue_pattern="evosep_qc",
            queue_type="Vial",
            plate_layout="Evosep_96",
            polarity=["pos"],
            date="20260116",
            user="test",
        )
        queue_input = make_vial_queue_input(params, samples)

        generator = QueueGenerator(config, queue_input, layout_mode="vial")
        result = generator.build_rows()

        user_rows = [row for row in result.rows if row.sample_type == "user"]
        for row in user_rows:
            assert row.tray in (1, 2, 3, 4, 5), f"User sample tray {row.tray} should be 1-5"

    def test_evosep_qc_no_middle_insertions(self, config):
        # With 50 samples, the high run_QC_after_n_samples (1000) should prevent middle QC
        samples = make_vial_samples(50)
        params = QueueParameters(
            tech_area="Proteomics",
            instrument="ASTRAL_1",
            sampler="Evosep",
            output_format="chronos",
            queue_pattern="evosep_qc",
            queue_type="Vial",
            plate_layout="Evosep_96",
            polarity=["pos"],
            date="20260116",
            user="test",
        )
        queue_input = make_vial_queue_input(params, samples)

        generator = QueueGenerator(config, queue_input, layout_mode="vial")
        result = generator.build_rows()

        # start(2) + 50 samples + end(2) = 54 total, no middle QC
        assert len(result.rows) == 54


class TestMetabolomicsPolarity:
    def test_polarity_expansion(self, config):
        samples = make_vial_samples(2)
        params = QueueParameters(
            tech_area="Metabolomics",
            instrument="EXPLORIS_3",
            sampler="Vanquish",
            output_format="xcalibur",
            queue_pattern="noqc",
            queue_type="Vial",
            plate_layout="Vanquish_54",
            polarity=["pos", "neg"],
            date="20260116",
            user="test",
        )
        queue_input = make_vial_queue_input(params, samples)

        generator = QueueGenerator(config, queue_input, layout_mode="vial")
        result = generator.build_rows()

        # 2 samples x 2 polarities = 4 rows
        assert len(result.rows) == 4

        # Check polarity alternation
        polarities = [row.polarity for row in result.rows]
        assert polarities == ["pos", "neg", "pos", "neg"]
