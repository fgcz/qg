"""Tests for positions_new.py - sampler position assignment."""

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
from qg.params_models_new import ContainerBatch, Plate, PlateCell, PlateQueue, VialQueue, VialSample
from qg.positions_new import SamplerStrategy


@pytest.fixture
def vanquish_vial_config():
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
    return VanquishPlateConfig(
        container_type="Plate",
        position_source="input",
        grid_position_format="{row}{col}",
        sample_rows=["A", "B", "C", "D", "E", "F", "G"],
        cols=[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12],
    )


@pytest.fixture
def vanquish_config(vanquish_vial_config, vanquish_plate_config):
    return VanquishConfig(
        description="Vanquish sampler",
        plates=["Y", "R", "B", "G"],
        vial=vanquish_vial_config,
        plate=vanquish_plate_config,
    )


@pytest.fixture
def samplers_config(vanquish_config):
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
    positions = {
        "QC01": {"plate": "B", "row": "F", "col": 9},
        "clean": {"plate": "B", "row": "F", "col": 7},
        "QC03dia": {"plate": "B", "row": "F", "col": 8},
    }
    return QCLayoutPattern(positions)


def create_vial_queue(n_samples: int) -> VialQueue:
    """Helper to create VialQueue with n samples."""
    samples = [
        VialSample(sample_name=f"Sample_{i}", sample_id=1000 + i, tube_id=f"12345/{i}", container_id=12345)
        for i in range(n_samples)
    ]
    batches = {12345: ContainerBatch(container_id=12345)}
    return VialQueue(batches=batches, cells=samples)


def create_plate_queue(samples_data: list[dict]) -> PlateQueue:
    """Helper to create PlateQueue with given samples."""
    batches = {12345: ContainerBatch(container_id=12345)}
    plates: dict[int, Plate] = {}
    cells: list[PlateCell] = []

    for i, data in enumerate(samples_data):
        tray = data.get("tray", "Y")
        plate_id = hash(tray) & 0xFFFFFFFF
        if plate_id not in plates:
            plates[plate_id] = Plate(plate_id=plate_id, tray=tray, nr_samples=0)
        sample = VialSample(
            sample_name=data["sample_name"],
            sample_id=data["sample_id"],
            container_id=12345,
        )
        cells.append(
            PlateCell(
                sample=sample,
                position=i + 1,
                grid_position=data["grid_position"],
                plate_id=plate_id,
            )
        )

    for plate_id in plates:
        plates[plate_id] = Plate(
            plate_id=plate_id,
            tray=plates[plate_id].tray,
            nr_samples=sum(1 for c in cells if c.plate_id == plate_id),
        )

    return PlateQueue(batches=batches, plates=plates, cells=cells)


class TestSamplerStrategy:
    def test_vial_sampler_assigns_positions(self, samplers_config, simple_qc_layout_pattern):
        strategy = SamplerStrategy("Vanquish", "vial", samplers_config, simple_qc_layout_pattern)
        queue = create_vial_queue(3)

        result = strategy.assign_positions(queue)

        assert len(result.cells) == 3
        for cell in result.cells:
            assert cell.grid_position is not None
        assert result.cells[0].grid_position == "A1"
        assert result.cells[1].grid_position == "A2"
        assert result.cells[2].grid_position == "A3"

    def test_vial_sampler_row_major_order(self, samplers_config, simple_qc_layout_pattern):
        strategy = SamplerStrategy("Vanquish", "vial", samplers_config, simple_qc_layout_pattern)
        queue = create_vial_queue(15)

        result = strategy.assign_positions(queue)

        # First 12 samples in row A
        for i in range(12):
            assert result.cells[i].grid_position == f"A{i + 1}"
        # Samples 13-15 in row B
        assert result.cells[12].grid_position == "B1"
        assert result.cells[13].grid_position == "B2"
        assert result.cells[14].grid_position == "B3"

    def test_vial_sampler_spans_multiple_plates(self, samplers_config, simple_qc_layout_pattern):
        strategy = SamplerStrategy("Vanquish", "vial", samplers_config, simple_qc_layout_pattern)
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

    def test_vial_sampler_skips_qc_positions(self, samplers_config):
        qc_layout = QCLayoutPattern({"QC01": {"plate": "Y", "row": "A", "col": 1}})
        strategy = SamplerStrategy("Vanquish", "vial", samplers_config, qc_layout)
        queue = create_vial_queue(3)

        result = strategy.assign_positions(queue)

        # First sample should be A2 (not A1, which is reserved for QC01)
        assert result.cells[0].grid_position == "A2"
        assert result.cells[1].grid_position == "A3"
        assert result.cells[2].grid_position == "A4"

    def test_vial_sampler_skips_multiple_qc_positions(self, samplers_config):
        qc_layout = QCLayoutPattern(
            {
                "QC01": {"plate": "Y", "row": "A", "col": 1},
                "QC02": {"plate": "Y", "row": "A", "col": 3},
            }
        )
        strategy = SamplerStrategy("Vanquish", "vial", samplers_config, qc_layout)
        queue = create_vial_queue(4)

        result = strategy.assign_positions(queue)

        # Positions should skip A1 and A3
        assert result.cells[0].grid_position == "A2"
        assert result.cells[1].grid_position == "A4"
        assert result.cells[2].grid_position == "A5"
        assert result.cells[3].grid_position == "A6"

    def test_plate_sampler_passes_through_positions(self, samplers_config, simple_qc_layout_pattern):
        strategy = SamplerStrategy("Vanquish", "plate", samplers_config, simple_qc_layout_pattern)
        queue = create_plate_queue(
            [
                {"sample_name": "S1", "sample_id": 1001, "tray": "Y", "grid_position": "B1"},
                {"sample_name": "S2", "sample_id": 1002, "tray": "Y", "grid_position": "B2"},
            ]
        )

        result = strategy.assign_positions(queue)

        assert result.cells[0].grid_position == "B1"

    def test_plate_sampler_raises_on_qc_collision(self, samplers_config):
        qc_layout = QCLayoutPattern({"QC01": {"plate": "B", "row": "F", "col": 9}})
        strategy = SamplerStrategy("Vanquish", "plate", samplers_config, qc_layout)
        queue = create_plate_queue(
            [
                {"sample_name": "Conflict", "sample_id": 1001, "tray": "B", "grid_position": "F9"},
            ]
        )

        with pytest.raises(ValueError, match="conflicts with QC"):
            strategy.assign_positions(queue)

    def test_plate_sampler_allows_non_conflicting_positions(self, samplers_config):
        qc_layout = QCLayoutPattern({"QC01": {"plate": "B", "row": "F", "col": 9}})
        strategy = SamplerStrategy("Vanquish", "plate", samplers_config, qc_layout)
        queue = create_plate_queue(
            [
                {"sample_name": "S1", "sample_id": 1001, "tray": "Y", "grid_position": "A1"},
                {"sample_name": "S2", "sample_id": 1002, "tray": "B", "grid_position": "F8"},
            ]
        )

        result = strategy.assign_positions(queue)

        assert len(result.cells) == 2

    def test_get_qc_position(self, samplers_config, simple_qc_layout_pattern):
        strategy = SamplerStrategy("Vanquish", "vial", samplers_config, simple_qc_layout_pattern)

        pos = strategy.get_qc_position("QC01")

        assert pos == {"tray": "B", "grid_position": "F9"}

    def test_unknown_sampler_raises(self, samplers_config, simple_qc_layout_pattern):
        with pytest.raises(ValueError, match="Unknown sampler"):
            SamplerStrategy("Unknown", "vial", samplers_config, simple_qc_layout_pattern)


class TestMClass48SamplerStrategy:
    @pytest.fixture
    def mclass48_qc_layout_pattern(self):
        positions = {
            "QC01": {"plate": "1", "row": "E", "col": 7},
            "clean": {"plate": "1", "row": "E", "col": 8},
        }
        return QCLayoutPattern(positions)

    def test_mclass48_vial_assigns_positions(self, samplers_config, mclass48_qc_layout_pattern):
        strategy = SamplerStrategy("MClass48", "vial", samplers_config, mclass48_qc_layout_pattern)
        queue = create_vial_queue(3)

        result = strategy.assign_positions(queue)

        assert len(result.cells) == 3
        assert result.cells[0].grid_position == "A1"
        assert result.cells[1].grid_position == "A2"
        assert result.cells[2].grid_position == "A3"

    def test_mclass48_vial_row_major_order(self, samplers_config, mclass48_qc_layout_pattern):
        strategy = SamplerStrategy("MClass48", "vial", samplers_config, mclass48_qc_layout_pattern)
        # MClass48 has 8 columns per row
        queue = create_vial_queue(10)

        result = strategy.assign_positions(queue)

        # First 8 samples in row A
        for i in range(8):
            assert result.cells[i].grid_position == f"A{i + 1}"
        # Samples 9-10 in row B
        assert result.cells[8].grid_position == "B1"
        assert result.cells[9].grid_position == "B2"

    def test_mclass48_vial_skips_qc_positions(self, samplers_config):
        qc_layout = QCLayoutPattern({"QC01": {"plate": "1", "row": "A", "col": 1}})
        strategy = SamplerStrategy("MClass48", "vial", samplers_config, qc_layout)
        queue = create_vial_queue(3)

        result = strategy.assign_positions(queue)

        # First sample should be A2 (not A1, which is reserved for QC01)
        assert result.cells[0].grid_position == "A2"
        assert result.cells[1].grid_position == "A3"
        assert result.cells[2].grid_position == "A4"

    def test_mclass48_plate_raises_on_qc_collision(self, samplers_config):
        qc_layout = QCLayoutPattern({"QC01": {"plate": "1", "row": "E", "col": 7}})
        strategy = SamplerStrategy("MClass48", "plate", samplers_config, qc_layout)
        queue = create_plate_queue(
            [
                {"sample_name": "Conflict", "sample_id": 1001, "tray": "1", "grid_position": "E7"},
            ]
        )

        with pytest.raises(ValueError, match="conflicts with QC"):
            strategy.assign_positions(queue)


class TestEvosepSamplerStrategy:
    @pytest.fixture
    def evosep_qc_layout_pattern(self):
        positions = {
            "QC01": EvosepPosition(tray=5, position_start=1, position_end=48),
            "clean": EvosepPosition(tray=6, position_start=1, position_end=96),
        }
        evosep_counters = {"QC01": 1, "clean": 1}
        return QCLayoutPattern(positions, evosep_counters)

    def test_evosep_vial_assigns_positions(self, samplers_config, evosep_qc_layout_pattern):
        strategy = SamplerStrategy("Evosep", "vial", samplers_config, evosep_qc_layout_pattern)
        queue = create_vial_queue(3)

        result = strategy.assign_positions(queue)

        assert len(result.cells) == 3
        # Evosep uses numeric positions
        assert result.cells[0].grid_position == 1
        assert result.cells[1].grid_position == 2
        assert result.cells[2].grid_position == 3

    def test_evosep_vial_sequential_order(self, samplers_config, evosep_qc_layout_pattern):
        strategy = SamplerStrategy("Evosep", "vial", samplers_config, evosep_qc_layout_pattern)
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

    def test_evosep_vial_raises_when_full(self, samplers_config, evosep_qc_layout_pattern):
        strategy = SamplerStrategy("Evosep", "vial", samplers_config, evosep_qc_layout_pattern)
        # 4 slots x 96 = 384, request more
        queue = create_vial_queue(400)

        with pytest.raises(ValueError, match="Not enough positions"):
            strategy.assign_positions(queue)

    def test_evosep_plate_converts_grid_to_numeric(self, samplers_config, evosep_qc_layout_pattern):
        strategy = SamplerStrategy("Evosep", "plate", samplers_config, evosep_qc_layout_pattern)
        queue = create_plate_queue(
            [
                {"sample_name": "S1", "sample_id": 1001, "tray": 1, "grid_position": "A1"},
                {"sample_name": "S2", "sample_id": 1002, "tray": 1, "grid_position": "A12"},
                {"sample_name": "S3", "sample_id": 1003, "tray": 1, "grid_position": "B1"},
            ]
        )

        result = strategy.assign_positions(queue)

        # A1 = 1, A12 = 12, B1 = 13 (row-major: row * 12 + col)
        assert result.cells[0].grid_position == 1
        assert result.cells[1].grid_position == 12
        assert result.cells[2].grid_position == 13

    def test_get_qc_position_evosep(self, samplers_config, evosep_qc_layout_pattern):
        strategy = SamplerStrategy("Evosep", "vial", samplers_config, evosep_qc_layout_pattern)

        pos = strategy.get_qc_position("QC01")

        assert pos == {"tray": 5, "grid_position": 1}
