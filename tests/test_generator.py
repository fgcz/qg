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


def _output_col(config, format_name: str, internal_field: str) -> str:
    """Look up the output column name that maps to an internal field."""
    fmt = config.output_formats.get_format(format_name)
    for col_name, field in fmt.columns.items():
        if field == internal_field:
            return col_name
    msg = f"No column maps to '{internal_field}' in format '{format_name}'"
    raise KeyError(msg)


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

    @staticmethod
    def _plate_96():
        from qg.config_models.positions import PlateLayout

        return PlateLayout(name="Plate_96", rows=list("ABCDEFGH"), cols=list(range(1, 13)))

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

        df = format_table(rows, fmt, self._plate_96())

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

        df = format_table(rows, fmt, self._plate_96())

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

        df = format_table(rows, fmt, self._plate_96())

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
            qc_layout_name="standard",
            polarity=["pos"],
            date="20260116",
            user="test",
        )
        queue_input = make_vial_queue_input(params, samples)

        generator = QueueGenerator(config, queue_input)
        result = generator.generate()

        assert isinstance(result, pl.DataFrame)
        assert len(result.columns) > 0

    def test_xcalibur_literal_columns(self, config):
        """Xcalibur output includes literal columns with correct constant values."""
        samples = make_vial_samples(3)
        params = QueueParameters(
            tech_area="Proteomics",
            instrument="ASTRAL_1",
            sampler="Vanquish",
            output_format="xcalibur",
            queue_pattern="noqc",
            queue_type="Vial",
            plate_layout="Vanquish_54",
            qc_layout_name="standard",
            polarity=["pos"],
            date="20260116",
            user="test",
        )
        queue_input = make_vial_queue_input(params, samples)

        generator = QueueGenerator(config, queue_input)
        result = generator.generate()

        # Verify all literal columns from config have expected constant values
        xcalibur_fmt = config.output_formats.get_format("xcalibur")
        for col_name, mapping in xcalibur_fmt.columns.items():
            if mapping.startswith("literal:"):
                expected_value = mapping[len("literal:") :]
                assert col_name in result.columns, f"Missing literal column {col_name}"
                assert result[col_name].to_list() == [expected_value] * len(result)

        # Verify column order matches config
        assert result.columns == list(xcalibur_fmt.columns.keys())

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
            qc_layout_name="standard",
            polarity=["pos"],
            date="20260116",
            user="test",
        )
        queue_input = make_vial_queue_input(params, samples)

        generator = QueueGenerator(config, queue_input)
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
            qc_layout_name="standard",
            polarity=["pos"],
            date="20260116",
            user="test",
        )
        queue_input = make_vial_queue_input(params, samples)

        generator = QueueGenerator(config, queue_input)
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
            qc_layout_name="standard",
            polarity=["pos"],
            date="20260116",
            user="test",
        )
        queue_input = make_vial_queue_input(params, samples)

        generator = QueueGenerator(config, queue_input)
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
            qc_layout_name="standard",
            polarity=["pos"],
            date="20260116",
            user="test",
            randomization="random",
        )
        queue_input = make_vial_queue_input(params, samples)

        generator = QueueGenerator(config, queue_input)
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
            qc_layout_name="standard",
            polarity=["pos"],
            date="20260116",
            user="test",
            randomization="blocked",
        )
        queue_input = make_vial_queue_input(params, samples, container_id)

        generator = QueueGenerator(config, queue_input)
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
            qc_layout_name="standard",
            polarity=["pos"],
            date="20260116",
            user="test",
            randomization="no",
        )
        queue_input = make_vial_queue_input(params, samples)

        generator = QueueGenerator(config, queue_input)
        result = generator.build_rows()

        result_order = [int(row.sample_id) for row in result.rows if row.sample_type == "user"]
        assert result_order == original_order


class TestDifferentSamplers:
    @pytest.mark.parametrize(
        "sampler,plate_layout",
        [("Vanquish", "Vanquish_54"), ("MClass", "MClass_48"), ("Evosep", "Plate_96")],
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
            qc_layout_name="standard",
            polarity=["pos"],
            date="20260116",
            user="test",
        )
        queue_input = make_vial_queue_input(params, samples)

        generator = QueueGenerator(config, queue_input)
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
            plate_layout="Plate_96",
            qc_layout_name="evosep_qc",
            polarity=["pos"],
            date="20260116",
            user="test",
        )
        queue_input = make_vial_queue_input(params, samples)

        generator = QueueGenerator(config, queue_input)
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
            plate_layout="Plate_96",
            qc_layout_name="evosep_qc",
            polarity=["pos"],
            date="20260116",
            user="test",
        )
        queue_input = make_vial_queue_input(params, samples)

        generator = QueueGenerator(config, queue_input)
        result = generator.build_rows()

        qc_rows = [row for row in result.rows if row.sample_type == "qc"]

        user_rows = [row for row in result.rows if row.sample_type == "user"]

        # QC and user samples must be on different trays
        qc_trays = {row.tray for row in qc_rows}
        user_trays = {row.tray for row in user_rows}
        assert not (qc_trays & user_trays), f"QC trays {qc_trays} and user trays {user_trays} should not overlap"

        # QC03dia and clean must occupy distinct grid positions
        qc03_positions = {row.grid_position for row in qc_rows if row.sample_id == "QC03dia"}
        clean_positions = {row.grid_position for row in qc_rows if row.sample_id == "clean"}
        assert qc03_positions, "expected at least one QC03dia row"
        assert clean_positions, "expected at least one clean row"
        overlap = qc03_positions & clean_positions
        assert not overlap, f"QC03dia and clean share positions: {overlap}"

    def test_evosep_qc_user_samples_on_trays_1_to_5(self, config):
        samples = make_vial_samples(5)
        params = QueueParameters(
            tech_area="Proteomics",
            instrument="ASTRAL_1",
            sampler="Evosep",
            output_format="chronos",
            queue_pattern="evosep_qc",
            queue_type="Vial",
            plate_layout="Plate_96",
            qc_layout_name="evosep_qc",
            polarity=["pos"],
            date="20260116",
            user="test",
        )
        queue_input = make_vial_queue_input(params, samples)

        generator = QueueGenerator(config, queue_input)
        result = generator.build_rows()

        user_rows = [row for row in result.rows if row.sample_type == "user"]
        for row in user_rows:
            assert row.tray in (1, 2, 3, 4, 5), f"User sample tray {row.tray} should be 1-5"

    def test_evosep_qc_middle_insertions(self, config):
        # With 50 samples and run_QC_after_n_samples=12, expect middle QC insertions
        samples = make_vial_samples(50)
        params = QueueParameters(
            tech_area="Proteomics",
            instrument="ASTRAL_1",
            sampler="Evosep",
            output_format="chronos",
            queue_pattern="evosep_qc",
            queue_type="Vial",
            plate_layout="Plate_96",
            qc_layout_name="evosep_qc",
            polarity=["pos"],
            date="20260116",
            user="test",
        )
        queue_input = make_vial_queue_input(params, samples)

        generator = QueueGenerator(config, queue_input)
        result = generator.build_rows()

        # start(2) + 50 samples + 4 middle QC (every 12 samples) + end(2) = 58
        assert len(result.rows) == 58


class TestEvosepChronosOutputFormat:
    """Integration test: Evosep + Chronos output produces numeric Source Vial."""

    def test_chronos_source_vial_is_numeric(self, config):
        samples = make_vial_samples(3)
        params = QueueParameters(
            tech_area="Proteomics",
            instrument="ASTRAL_1",
            sampler="Evosep",
            output_format="chronos",
            queue_pattern="noqc",
            queue_type="Vial",
            plate_layout="Plate_96",
            qc_layout_name="standard",
            polarity=["pos"],
            date="20260116",
            user="test",
        )
        queue_input = make_vial_queue_input(params, samples)

        generator = QueueGenerator(config, queue_input)
        result = generator.generate()

        # grid_position column should contain numeric 1-96 (alpha_to_flat conversion)
        vial_col = _output_col(config, "chronos", "grid_position")
        assert vial_col in result.columns
        vials = result[vial_col].to_list()
        assert vials == ["1", "2", "3"]


class TestEvosepHystarOutputFormat:
    """Integration test: Evosep + HyStar output produces S{tray}-{row}{col} positions."""

    def test_hystar_position_format(self, config):
        samples = make_vial_samples(3)
        params = QueueParameters(
            tech_area="Proteomics",
            instrument="TIMSTOF_1",
            sampler="Evosep",
            output_format="hystar",
            queue_pattern="noqc",
            queue_type="Vial",
            plate_layout="Plate_96",
            qc_layout_name="standard",
            polarity=["pos"],
            date="20260116",
            user="test",
        )
        queue_input = make_vial_queue_input(params, samples)

        generator = QueueGenerator(config, queue_input)
        result = generator.generate()

        # Position should be S{tray}-{row}{col} format
        pos_col = _output_col(config, "hystar", "position")
        assert pos_col in result.columns
        positions = result[pos_col].to_list()
        assert positions[0] == "S1-A1"
        assert positions[1] == "S1-A2"
        assert positions[2] == "S1-A3"


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
            qc_layout_name="standard",
            polarity=["pos", "neg"],
            date="20260116",
            user="test",
        )
        queue_input = make_vial_queue_input(params, samples)

        generator = QueueGenerator(config, queue_input)
        result = generator.build_rows()

        # 2 samples x 2 polarities = 4 rows
        assert len(result.rows) == 4

        # Check polarity alternation
        polarities = [row.polarity for row in result.rows]
        assert polarities == ["pos", "neg", "pos", "neg"]


class TestChronosNonRowA:
    """Chronos alpha_to_flat must work beyond row A (regression gap from code review)."""

    def test_chronos_source_vial_beyond_row_a(self, config):
        """With 15 samples on Plate_96 (12 cols), rows A and B are used."""
        samples = make_vial_samples(15)
        params = QueueParameters(
            tech_area="Proteomics",
            instrument="ASTRAL_1",
            sampler="Evosep",
            output_format="chronos",
            queue_pattern="noqc",
            queue_type="Vial",
            plate_layout="Plate_96",
            qc_layout_name="standard",
            polarity=["pos"],
            date="20260116",
            user="test",
        )
        queue_input = make_vial_queue_input(params, samples)

        generator = QueueGenerator(config, queue_input)
        result = generator.generate()

        vial_col = _output_col(config, "chronos", "grid_position")
        vials = result[vial_col].to_list()
        # 15 samples: A1-A12 -> 1-12, B1-B3 -> 13-15
        assert vials == [str(i) for i in range(1, 16)]


class TestChronosFormat:
    """Chronos CSV must have exact column order, EvoSlot tray format, and correct literal values."""

    @staticmethod
    def _make_chronos_generator(config, num_samples: int = 3):
        samples = make_vial_samples(num_samples)
        params = QueueParameters(
            tech_area="Proteomics",
            instrument="ASTRAL_1",
            sampler="Evosep",
            output_format="chronos",
            queue_pattern="noqc",
            queue_type="Vial",
            plate_layout="Plate_96",
            qc_layout_name="standard",
            polarity=["pos"],
            date="20260116",
            user="test",
        )
        queue_input = make_vial_queue_input(params, samples)
        return QueueGenerator(config, queue_input)

    def test_chronos_column_order(self, config):
        """Chronos columns must match the output_formats config."""
        generator = self._make_chronos_generator(config)
        result = generator.generate()

        chronos_format = config.output_formats.get_format("chronos")
        expected_columns = list(chronos_format.columns.keys())
        assert result.columns == expected_columns

    def test_chronos_source_tray_evoslot_format(self, config):
        """Source Tray values must be 'EvoSlot N', not raw integers."""
        generator = self._make_chronos_generator(config)
        result = generator.generate()

        trays = result["Source Tray"].to_list()
        assert all(t.startswith("EvoSlot ") for t in trays)

    def test_chronos_empty_literal_columns(self, config):
        """Columns mapped to 'literal:' (empty literal) must contain empty strings."""
        generator = self._make_chronos_generator(config)
        result = generator.generate()

        chronos_format = config.output_formats.get_format("chronos")
        empty_literal_cols = [name for name, value in chronos_format.columns.items() if value == "literal:"]
        assert empty_literal_cols, "Config should have at least one empty-literal column"
        for col_name in empty_literal_cols:
            values = result[col_name].to_list()
            assert all(v == "" for v in values), f"{col_name} should be empty, got {values}"

    def test_chronos_non_empty_literal_columns(self, config):
        """Non-empty literal columns must have the value defined in config."""
        generator = self._make_chronos_generator(config)
        result = generator.generate()

        chronos_format = config.output_formats.get_format("chronos")
        non_empty_literals = {
            name: value[len("literal:") :]
            for name, value in chronos_format.columns.items()
            if value.startswith("literal:") and value != "literal:"
        }
        assert non_empty_literals, "Config should have at least one non-empty literal column"
        for col_name, expected in non_empty_literals.items():
            values = result[col_name].to_list()
            assert all(v == expected for v in values), f"{col_name} should be '{expected}', got {values}"


class TestChronosWriter:
    """Chronos CSV writer must prepend a 1-based row counter column."""

    def test_chronos_writer_prepends_counter(self, config):
        """write() output has empty-header counter 1..N as first CSV column."""
        import csv
        import io

        samples = make_vial_samples(5)
        params = QueueParameters(
            tech_area="Proteomics",
            instrument="ASTRAL_1",
            sampler="Evosep",
            output_format="chronos",
            queue_pattern="noqc",
            queue_type="Vial",
            plate_layout="Plate_96",
            qc_layout_name="standard",
            polarity=["pos"],
            date="20260116",
            user="test",
        )
        queue_input = make_vial_queue_input(params, samples)

        generator = QueueGenerator(config, queue_input)
        csv_text = generator.write()

        reader = csv.reader(io.StringIO(csv_text))
        header = next(reader)
        # First column header should be empty (unnamed counter)
        assert header[0] == "", f"First column header should be empty, got '{header[0]}'"
        # Data rows: first column should be 1, 2, 3, 4, 5
        for i, row in enumerate(reader, start=1):
            assert row[0] == str(i), f"Row {i} counter should be {i}, got '{row[0]}'"


class TestQueueInputRoundTrip:
    """Round-trip: write_queue_input -> read_queue_input preserves data."""

    def test_vial_queue_input_round_trip(self, tmp_path):
        """VialQueueInput survives JSON serialization round-trip."""
        from qg.params_models import read_queue_input, write_queue_input

        samples = make_vial_samples(3, container_id=12345)
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
            date="20260116",
            user="test",
        )
        original = make_vial_queue_input(params, samples, container_id=12345)

        path = tmp_path / "vial_test.json"
        write_queue_input(original, path)
        restored = read_queue_input(path)

        assert isinstance(restored, VialQueueInput)
        assert len(restored.queue.samples) == 3
        assert restored.parameters.instrument == "ASTRAL_1"
        assert restored.queue.samples[0].sample_name == "sample_1"
        assert restored.queue.samples[0].container_id == 12345

    def test_plate_queue_input_round_trip(self, tmp_path):
        """PlateQueueInput survives JSON serialization round-trip."""
        from qg.params_models import (
            Plate,
            PlateCell,
            PlateQueue,
            PlateQueueInput,
            read_queue_input,
            write_queue_input,
        )

        sample = VialSample(sample_name="S1", sample_id=100, container_id=99)
        cell = PlateCell(sample=sample, position=1, grid_position="D8", plate_id=1, row="D", col=8)
        plate = Plate(plate_id=1, tray="Y", nr_samples=1)
        queue = PlateQueue(
            batches={99: ContainerBatch(container_id=99)},
            plates={1: plate},
            cells=[cell],
        )
        params = QueueParameters(
            tech_area="Proteomics",
            instrument="ASTRAL_1",
            sampler="Evosep",
            output_format="chronos",
            queue_pattern="noqc",
            queue_type="Plate",
            plate_layout="Plate_96",
            qc_layout_name="standard",
            polarity=["pos"],
            date="20260116",
            user="test",
        )
        original = PlateQueueInput(parameters=params, queue=queue)

        path = tmp_path / "plate_test.json"
        write_queue_input(original, path)
        restored = read_queue_input(path)

        assert isinstance(restored, PlateQueueInput)
        assert restored.queue.cells[0].grid_position == "D8"
        assert restored.queue.cells[0].row == "D"
        assert restored.queue.cells[0].col == 8
        assert restored.queue.cells[0].sample.sample_name == "S1"
        assert restored.queue.plates[1].tray == "Y"
