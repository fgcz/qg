# =============================================================================
# Tests for positions_new.py - SamplerStrategyV2
# =============================================================================
#
# Tests adapted from test_positions.py for SamplerStrategy

import pytest

from qg.config_models_new.positions import PlateLayout, QCSampleEvosep, QCSampleGrid, Sampler
from qg.params_models import ContainerBatch, VialQueue, VialSample
from qg.positions_new import SamplerStrategyV2


def create_vial_queue(n_samples: int) -> VialQueue:
    """Helper to create VialQueue with n samples."""
    samples = [
        VialSample(sample_name=f"Sample_{i}", sample_id=1000 + i, tube_id=f"12345/{i}", container_id=12345)
        for i in range(n_samples)
    ]
    batches = {12345: ContainerBatch(container_id=12345)}
    return VialQueue(batches=batches, samples=samples)


class TestSamplerStrategyV2:
    """Tests for SamplerStrategyV2 - adapted from TestSamplerStrategy."""

    @pytest.fixture
    def vanquish_sampler(self) -> Sampler:
        return Sampler(
            name="Vanquish",
            trays=["Y", "R", "B", "G"],
            position_fun="string_concat",
        )

    @pytest.fixture
    def vanquish_layout(self) -> PlateLayout:
        return PlateLayout(
            name="Vanquish_60",
            rows=["A", "B", "C", "D", "E"],
            cols=[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12],
        )

    @pytest.fixture
    def simple_qc_samples(self) -> list[QCSampleGrid]:
        return [
            QCSampleGrid(
                tech_area="proteomics",
                qc_layout_name="standard",
                plate_layout="Vanquish_60",
                sample_id="QC01",
                plate="B",
                row="F",
                col=9,
            ),
            QCSampleGrid(
                tech_area="proteomics",
                qc_layout_name="standard",
                plate_layout="Vanquish_60",
                sample_id="clean",
                plate="B",
                row="F",
                col=7,
            ),
        ]

    def test_assigns_positions(
        self,
        vanquish_sampler: Sampler,
        vanquish_layout: PlateLayout,
        simple_qc_samples: list[QCSampleGrid],
    ) -> None:
        strategy = SamplerStrategyV2(vanquish_sampler, vanquish_layout, simple_qc_samples)
        queue = create_vial_queue(3)

        result = strategy.assign_positions(queue)

        assert len(result.cells) == 3
        for cell in result.cells:
            assert cell.grid_position is not None
        assert result.cells[0].grid_position == "A1"
        assert result.cells[1].grid_position == "A2"
        assert result.cells[2].grid_position == "A3"

    def test_row_major_order(
        self,
        vanquish_sampler: Sampler,
        vanquish_layout: PlateLayout,
        simple_qc_samples: list[QCSampleGrid],
    ) -> None:
        strategy = SamplerStrategyV2(vanquish_sampler, vanquish_layout, simple_qc_samples)
        queue = create_vial_queue(15)

        result = strategy.assign_positions(queue)

        # First 12 samples in row A
        for i in range(12):
            assert result.cells[i].grid_position == f"A{i + 1}"
        # Samples 13-15 in row B
        assert result.cells[12].grid_position == "B1"
        assert result.cells[13].grid_position == "B2"
        assert result.cells[14].grid_position == "B3"

    def test_spans_multiple_plates(
        self,
        vanquish_sampler: Sampler,
        vanquish_layout: PlateLayout,
        simple_qc_samples: list[QCSampleGrid],
    ) -> None:
        strategy = SamplerStrategyV2(vanquish_sampler, vanquish_layout, simple_qc_samples)
        # 5 rows x 12 cols = 60 positions per plate
        queue = create_vial_queue(65)

        result = strategy.assign_positions(queue)

        # Check plates via the plates dict
        trays = [result.plates[c.plate_id].tray for c in result.cells]
        assert trays[0] == "Y"
        assert trays[59] == "Y"
        assert result.cells[59].grid_position == "E12"
        assert trays[60] == "R"
        assert result.cells[60].grid_position == "A1"

    def test_skips_qc_positions(
        self,
        vanquish_sampler: Sampler,
        vanquish_layout: PlateLayout,
    ) -> None:
        qc_samples = [
            QCSampleGrid(
                tech_area="proteomics",
                qc_layout_name="standard",
                plate_layout="Vanquish_60",
                sample_id="QC01",
                plate="Y",
                row="A",
                col=1,
            ),
        ]
        strategy = SamplerStrategyV2(vanquish_sampler, vanquish_layout, qc_samples)
        queue = create_vial_queue(3)

        result = strategy.assign_positions(queue)

        # First sample should be A2 (not A1, which is reserved for QC01)
        assert result.cells[0].grid_position == "A2"
        assert result.cells[1].grid_position == "A3"
        assert result.cells[2].grid_position == "A4"

    def test_skips_multiple_qc_positions(
        self,
        vanquish_sampler: Sampler,
        vanquish_layout: PlateLayout,
    ) -> None:
        qc_samples = [
            QCSampleGrid(
                tech_area="proteomics",
                qc_layout_name="standard",
                plate_layout="Vanquish_60",
                sample_id="QC01",
                plate="Y",
                row="A",
                col=1,
            ),
            QCSampleGrid(
                tech_area="proteomics",
                qc_layout_name="standard",
                plate_layout="Vanquish_60",
                sample_id="QC02",
                plate="Y",
                row="A",
                col=3,
            ),
        ]
        strategy = SamplerStrategyV2(vanquish_sampler, vanquish_layout, qc_samples)
        queue = create_vial_queue(4)

        result = strategy.assign_positions(queue)

        # Positions should skip A1 and A3
        assert result.cells[0].grid_position == "A2"
        assert result.cells[1].grid_position == "A4"
        assert result.cells[2].grid_position == "A5"
        assert result.cells[3].grid_position == "A6"

    def test_get_qc_position(
        self,
        vanquish_sampler: Sampler,
        vanquish_layout: PlateLayout,
        simple_qc_samples: list[QCSampleGrid],
    ) -> None:
        strategy = SamplerStrategyV2(vanquish_sampler, vanquish_layout, simple_qc_samples)

        pos = strategy.get_qc_position("QC01")

        assert pos == {"tray": "B", "grid_position": "F9"}


class TestSamplerStrategyV2MClass48:
    """Tests for SamplerStrategyV2 with MClass48-like config."""

    @pytest.fixture
    def mclass48_sampler(self) -> Sampler:
        return Sampler(
            name="MClass48",
            trays=["1", "2"],
            position_fun="string_concat",
        )

    @pytest.fixture
    def mclass48_layout(self) -> PlateLayout:
        return PlateLayout(
            name="MClass48_40",
            rows=["A", "B", "C", "D", "E"],
            cols=[1, 2, 3, 4, 5, 6, 7, 8],
        )

    @pytest.fixture
    def mclass48_qc_samples(self) -> list[QCSampleGrid]:
        return [
            QCSampleGrid(
                tech_area="proteomics",
                qc_layout_name="standard",
                plate_layout="MClass48_40",
                sample_id="QC01",
                plate="1",
                row="E",
                col=7,
            ),
            QCSampleGrid(
                tech_area="proteomics",
                qc_layout_name="standard",
                plate_layout="MClass48_40",
                sample_id="clean",
                plate="1",
                row="E",
                col=8,
            ),
        ]

    def test_assigns_positions(
        self,
        mclass48_sampler: Sampler,
        mclass48_layout: PlateLayout,
        mclass48_qc_samples: list[QCSampleGrid],
    ) -> None:
        strategy = SamplerStrategyV2(mclass48_sampler, mclass48_layout, mclass48_qc_samples)
        queue = create_vial_queue(3)

        result = strategy.assign_positions(queue)

        assert len(result.cells) == 3
        assert result.cells[0].grid_position == "A1"
        assert result.cells[1].grid_position == "A2"
        assert result.cells[2].grid_position == "A3"

    def test_row_major_order(
        self,
        mclass48_sampler: Sampler,
        mclass48_layout: PlateLayout,
        mclass48_qc_samples: list[QCSampleGrid],
    ) -> None:
        strategy = SamplerStrategyV2(mclass48_sampler, mclass48_layout, mclass48_qc_samples)
        # MClass48 has 8 columns per row
        queue = create_vial_queue(10)

        result = strategy.assign_positions(queue)

        # First 8 samples in row A
        for i in range(8):
            assert result.cells[i].grid_position == f"A{i + 1}"
        # Samples 9-10 in row B
        assert result.cells[8].grid_position == "B1"
        assert result.cells[9].grid_position == "B2"

    def test_skips_qc_positions(
        self,
        mclass48_sampler: Sampler,
        mclass48_layout: PlateLayout,
    ) -> None:
        qc_samples = [
            QCSampleGrid(
                tech_area="proteomics",
                qc_layout_name="standard",
                plate_layout="MClass48_40",
                sample_id="QC01",
                plate="1",
                row="A",
                col=1,
            ),
        ]
        strategy = SamplerStrategyV2(mclass48_sampler, mclass48_layout, qc_samples)
        queue = create_vial_queue(3)

        result = strategy.assign_positions(queue)

        # First sample should be A2 (not A1, which is reserved for QC01)
        assert result.cells[0].grid_position == "A2"
        assert result.cells[1].grid_position == "A3"
        assert result.cells[2].grid_position == "A4"


class TestSamplerStrategyV2Evosep:
    """Tests for SamplerStrategyV2 with Evosep sampler."""

    @pytest.fixture
    def evosep_sampler(self) -> Sampler:
        return Sampler(
            name="Evosep",
            trays=[1, 2, 3, 4],
            position_fun="int_add",
        )

    @pytest.fixture
    def evosep_layout(self) -> PlateLayout:
        return PlateLayout(
            name="Evosep_96",
            rows=[0],
            cols=list(range(1, 97)),  # 1-96
        )

    @pytest.fixture
    def evosep_qc_samples(self) -> list[QCSampleEvosep]:
        return [
            QCSampleEvosep(
                tech_area="proteomics",
                qc_layout_name="standard",
                plate_layout="Evosep_96",
                sample_id="QC01",
                tray=5,
                position_start=1,
                position_end=48,
            ),
            QCSampleEvosep(
                tech_area="proteomics",
                qc_layout_name="standard",
                plate_layout="Evosep_96",
                sample_id="clean",
                tray=6,
                position_start=1,
                position_end=96,
            ),
        ]

    def test_assigns_positions(
        self,
        evosep_sampler: Sampler,
        evosep_layout: PlateLayout,
        evosep_qc_samples: list[QCSampleEvosep],
    ) -> None:
        strategy = SamplerStrategyV2(evosep_sampler, evosep_layout, evosep_qc_samples)
        queue = create_vial_queue(3)

        result = strategy.assign_positions(queue)

        assert len(result.cells) == 3
        # Evosep uses numeric positions
        assert result.cells[0].grid_position == 1
        assert result.cells[1].grid_position == 2
        assert result.cells[2].grid_position == 3

    def test_sequential_order(
        self,
        evosep_sampler: Sampler,
        evosep_layout: PlateLayout,
        evosep_qc_samples: list[QCSampleEvosep],
    ) -> None:
        strategy = SamplerStrategyV2(evosep_sampler, evosep_layout, evosep_qc_samples)
        # Test positions across a slot boundary
        queue = create_vial_queue(100)

        result = strategy.assign_positions(queue)

        # First 96 samples on tray 1
        for i in range(96):
            assert result.cells[i].grid_position == i + 1
        # Samples 97-100 on tray 2
        tray_97 = result.plates[result.cells[96].plate_id].tray
        assert tray_97 == 2
        assert result.cells[96].grid_position == 1

    def test_raises_when_full(
        self,
        evosep_sampler: Sampler,
        evosep_layout: PlateLayout,
        evosep_qc_samples: list[QCSampleEvosep],
    ) -> None:
        strategy = SamplerStrategyV2(evosep_sampler, evosep_layout, evosep_qc_samples)
        # 4 slots x 96 = 384, request more
        queue = create_vial_queue(400)

        with pytest.raises(ValueError, match="Not enough positions"):
            strategy.assign_positions(queue)

    def test_get_qc_position(
        self,
        evosep_sampler: Sampler,
        evosep_layout: PlateLayout,
        evosep_qc_samples: list[QCSampleEvosep],
    ) -> None:
        strategy = SamplerStrategyV2(evosep_sampler, evosep_layout, evosep_qc_samples)

        pos = strategy.get_qc_position("QC01")

        assert pos == {"tray": 5, "grid_position": 1}

    def test_get_qc_position_increments(
        self,
        evosep_sampler: Sampler,
        evosep_layout: PlateLayout,
        evosep_qc_samples: list[QCSampleEvosep],
    ) -> None:
        strategy = SamplerStrategyV2(evosep_sampler, evosep_layout, evosep_qc_samples)

        pos1 = strategy.get_qc_position("QC01")
        pos2 = strategy.get_qc_position("QC01")

        assert pos1 == {"tray": 5, "grid_position": 1}
        assert pos2 == {"tray": 5, "grid_position": 2}
