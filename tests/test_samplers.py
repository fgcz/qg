"""Integration tests for Samplers with real config."""

from pathlib import Path

import pytest

from qg.config import qg_config
from qg.config_models import QCLayoutPattern
from qg.params_models import ContainerBatch, Plate, PlateCell, PlateQueue, VialQueue, VialSample
from qg.positions import SamplerStrategy
from qg.queue_structure import build_multi_container_queue_structure

CONFIG_DIR = Path(__file__).parent.parent / "qg_configs"

CONTAINER_ID = 12345


@pytest.fixture
def configs():
    """Load real configs for integration tests."""
    qg_config.cache_clear()
    return qg_config(CONFIG_DIR)


def create_vial_queue(num_samples: int, container_id: int = CONTAINER_ID) -> VialQueue:
    """Create a VialQueue with the given number of samples."""
    samples = [
        VialSample(sample_name=f"S{i}", sample_id=1000 + i, container_id=container_id) for i in range(num_samples)
    ]
    batches = {container_id: ContainerBatch(container_id=container_id)}
    return VialQueue(batches=batches, samples=samples)


def create_plate_queue(
    samples_with_positions: list[dict],
    container_id: int = CONTAINER_ID,
) -> PlateQueue:
    """Create a PlateQueue with pre-assigned positions.

    Each dict: {"sample_name", "sample_id", "tray", "grid_position"}.
    """
    batches = {container_id: ContainerBatch(container_id=container_id)}
    plates: dict[int, Plate] = {}
    cells: list[PlateCell] = []

    for i, data in enumerate(samples_with_positions):
        tray = data["tray"]
        plate_id = hash(tray) & 0xFFFFFFFF
        if plate_id not in plates:
            plates[plate_id] = Plate(plate_id=plate_id, tray=tray, nr_samples=0)
        sample = VialSample(
            sample_name=data["sample_name"],
            sample_id=data["sample_id"],
            container_id=container_id,
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


class TestVanquishVialSamplerIntegration:
    """Integration test for VanquishVialSampler with real config.

    Flow: load config → build queue structure → assign positions → verify.
    """

    def test_standard_pattern_with_5_samples(self, configs):
        """VanquishVialSampler generates correct positions for standard pattern."""
        pattern = configs.queue_patterns.get_pattern("Proteomics", "standard")
        qc_layout = configs.qc_layouts.get_layout("Proteomics", "Vanquish.vial")
        qc_layout_pattern = QCLayoutPattern.create(pattern, qc_layout)
        strategy = SamplerStrategy("Vanquish", "vial", configs.samplers, qc_layout_pattern)

        # Build queue structure
        groups = [(CONTAINER_ID, 5)]
        slot_entries = build_multi_container_queue_structure(groups, pattern)
        structure = [s.sample_id for s in slot_entries]

        assert structure == [
            "QC03dia",
            "QC01",  # start
            "default",
            "default",
            "default",
            "default",
            "default",  # 5 samples
            "clean",
            "QC01",
            "QC03dia",
            "clean",  # end
        ]

        # Assign user positions
        num_user_samples = structure.count("default")
        queue = create_vial_queue(num_user_samples)
        result = strategy.assign_positions(queue)

        assert len(result.cells) == 5
        # User positions: Y plate, row A, row-major
        trays = [result.plates[c.plate_id].tray for c in result.cells]
        assert all(t == "Y" for t in trays)
        assert result.cells[0].grid_position == "A1"
        assert result.cells[1].grid_position == "A2"
        assert result.cells[2].grid_position == "A3"
        assert result.cells[3].grid_position == "A4"
        assert result.cells[4].grid_position == "A5"

        # QC positions from qc_layouts_grid.csv [Proteomics.Vanquish.vial]
        assert strategy.get_qc_position("QC03dia") == {"tray": "B", "grid_position": "F8"}
        assert strategy.get_qc_position("QC01") == {"tray": "B", "grid_position": "F9"}
        assert strategy.get_qc_position("clean") == {"tray": "B", "grid_position": "F7"}


class TestVanquishVialSamplerMetabolomicsIntegration:
    """Integration test for VanquishVialSampler with Metabolomics config."""

    def test_standard_pattern_with_5_samples(self, configs):
        """VanquishVialSampler generates correct positions for Metabolomics.standard."""
        pattern = configs.queue_patterns.get_pattern("Metabolomics", "standard")
        qc_layout = configs.qc_layouts.get_layout("Metabolomics", "Vanquish.vial")
        qc_layout_pattern = QCLayoutPattern.create(pattern, qc_layout)
        strategy = SamplerStrategy("Vanquish", "vial", configs.samplers, qc_layout_pattern)

        groups = [(CONTAINER_ID, 5)]
        slot_entries = build_multi_container_queue_structure(groups, pattern)
        structure = [s.sample_id for s in slot_entries]

        assert structure == [
            # start (11)
            "blank",
            "108mix_AA",
            "pooledQC",
            "blank",
            "pooledQCDil1",
            "pooledQCDil2",
            "pooledQCDil3",
            "pooledQCDil4",
            "pooledQCDil5",
            "pooledQCDil6",
            "blank",
            # 5 samples
            "default",
            "default",
            "default",
            "default",
            "default",
            # end (3)
            "108mix_AA",
            "pooledQC",
            "blank",
        ]

        num_user_samples = structure.count("default")
        queue = create_vial_queue(num_user_samples)
        result = strategy.assign_positions(queue)

        assert len(result.cells) == 5
        trays = [result.plates[c.plate_id].tray for c in result.cells]
        assert all(t == "Y" for t in trays)
        assert result.cells[0].grid_position == "A1"
        assert result.cells[1].grid_position == "A2"
        assert result.cells[2].grid_position == "A3"
        assert result.cells[3].grid_position == "A4"
        assert result.cells[4].grid_position == "A5"

        # QC positions from qc_layouts_grid.csv [Metabolomics.Vanquish.vial]
        assert strategy.get_qc_position("blank") == {"tray": "Y", "grid_position": "F1"}
        assert strategy.get_qc_position("108mix_AA") == {"tray": "Y", "grid_position": "F9"}
        assert strategy.get_qc_position("pooledQC") == {"tray": "Y", "grid_position": "F8"}
        assert strategy.get_qc_position("pooledQCDil1") == {"tray": "Y", "grid_position": "F2"}
        assert strategy.get_qc_position("pooledQCDil2") == {"tray": "Y", "grid_position": "F3"}
        assert strategy.get_qc_position("pooledQCDil3") == {"tray": "Y", "grid_position": "F4"}
        assert strategy.get_qc_position("pooledQCDil4") == {"tray": "Y", "grid_position": "F5"}
        assert strategy.get_qc_position("pooledQCDil5") == {"tray": "Y", "grid_position": "F6"}
        assert strategy.get_qc_position("pooledQCDil6") == {"tray": "Y", "grid_position": "F7"}


class TestEvosepVialSamplerIntegration:
    """Integration test for EvosepVialSampler with real config.

    Evosep uses consumable tips, so each QC injection consumes
    the next position in the range (sequential counters).
    """

    def test_standard_pattern_with_5_samples(self, configs):
        """EvosepVialSampler generates correct positions for standard pattern."""
        pattern = configs.queue_patterns.get_pattern("Proteomics", "standard")
        qc_layout = configs.qc_layouts.get_layout("Proteomics", "Evosep.vial")
        qc_layout_pattern = QCLayoutPattern.create(pattern, qc_layout)
        strategy = SamplerStrategy("Evosep", "vial", configs.samplers, qc_layout_pattern)

        groups = [(CONTAINER_ID, 5)]
        slot_entries = build_multi_container_queue_structure(groups, pattern)
        structure = [s.sample_id for s in slot_entries]

        assert structure == [
            "QC03dia",
            "QC01",  # start
            "default",
            "default",
            "default",
            "default",
            "default",  # 5 samples
            "clean",
            "QC01",
            "QC03dia",
            "clean",  # end
        ]

        # Assign user positions
        num_user_samples = structure.count("default")
        queue = create_vial_queue(num_user_samples)
        result = strategy.assign_positions(queue)

        assert len(result.cells) == 5
        # User positions: sequential on tray 1
        for i, cell in enumerate(result.cells):
            assert result.plates[cell.plate_id].tray == 1
            assert cell.grid_position == i + 1

        # QC positions: sequential counters from ranges
        # QC03dia range: tray 5, positions 49-96 → first call returns 49, second 50
        assert strategy.get_qc_position("QC03dia") == {"tray": 5, "grid_position": 49}
        assert strategy.get_qc_position("QC03dia") == {"tray": 5, "grid_position": 50}
        # QC01 range: tray 5, positions 1-48 → first call returns 1, second 2
        assert strategy.get_qc_position("QC01") == {"tray": 5, "grid_position": 1}
        assert strategy.get_qc_position("QC01") == {"tray": 5, "grid_position": 2}
        # clean range: tray 6, positions 1-96
        assert strategy.get_qc_position("clean") == {"tray": 6, "grid_position": 1}
        assert strategy.get_qc_position("clean") == {"tray": 6, "grid_position": 2}


class TestMClass48VialSamplerIntegration:
    """Integration test for MClass48VialSampler with real config."""

    def test_standard_pattern_with_5_samples(self, configs):
        """MClass48VialSampler generates correct positions for standard pattern."""
        pattern = configs.queue_patterns.get_pattern("Proteomics", "standard")
        qc_layout = configs.qc_layouts.get_layout("Proteomics", "MClass48.vial")
        qc_layout_pattern = QCLayoutPattern.create(pattern, qc_layout)
        strategy = SamplerStrategy("MClass48", "vial", configs.samplers, qc_layout_pattern)

        groups = [(CONTAINER_ID, 5)]
        slot_entries = build_multi_container_queue_structure(groups, pattern)
        structure = [s.sample_id for s in slot_entries]

        assert len(structure) == 11  # start(2) + 5 samples + end(4)

        num_user_samples = structure.count("default")
        queue = create_vial_queue(num_user_samples)
        result = strategy.assign_positions(queue)

        assert len(result.cells) == 5
        # User positions: plate "1", row A, row-major
        trays = [result.plates[c.plate_id].tray for c in result.cells]
        assert all(t == "1" for t in trays)
        assert result.cells[0].grid_position == "A1"
        assert result.cells[1].grid_position == "A2"
        assert result.cells[2].grid_position == "A3"
        assert result.cells[3].grid_position == "A4"
        assert result.cells[4].grid_position == "A5"

        # QC positions from qc_layouts_grid.csv [Proteomics.MClass48.vial]
        assert strategy.get_qc_position("QC03dia") == {"tray": "1", "grid_position": "F7"}
        assert strategy.get_qc_position("QC01") == {"tray": "1", "grid_position": "F8"}
        assert strategy.get_qc_position("clean") == {"tray": "1", "grid_position": "F6"}


class TestVanquishPlateSamplerIntegration:
    """Integration test for VanquishPlateSampler with real config.

    Plate mode: user samples have pre-assigned grid_position from input.
    """

    def test_standard_pattern_with_5_samples(self, configs):
        """VanquishPlateSampler validates input positions for user samples."""
        pattern = configs.queue_patterns.get_pattern("Proteomics", "standard")
        qc_layout = configs.qc_layouts.get_layout("Proteomics", "Vanquish.plate")
        qc_layout_pattern = QCLayoutPattern.create(pattern, qc_layout)
        strategy = SamplerStrategy("Vanquish", "plate", configs.samplers, qc_layout_pattern)

        groups = [(CONTAINER_ID, 5)]
        slot_entries = build_multi_container_queue_structure(groups, pattern)
        structure = [s.sample_id for s in slot_entries]

        assert len(structure) == 11

        num_user_samples = structure.count("default")
        queue = create_plate_queue(
            [
                {"sample_name": f"S{i}", "sample_id": 1000 + i, "tray": "Y", "grid_position": f"B{i + 1}"}
                for i in range(num_user_samples)
            ]
        )
        result = strategy.assign_positions(queue)

        # Plate mode passes through positions unchanged
        assert len(result.cells) == 5
        assert result.cells[0].grid_position == "B1"
        assert result.cells[1].grid_position == "B2"
        assert result.cells[2].grid_position == "B3"
        assert result.cells[3].grid_position == "B4"
        assert result.cells[4].grid_position == "B5"

        # QC positions from qc_layouts_grid.csv [Proteomics.Vanquish.plate]
        assert strategy.get_qc_position("QC03dia") == {"tray": "B", "grid_position": "H10"}
        assert strategy.get_qc_position("QC01") == {"tray": "B", "grid_position": "H9"}
        assert strategy.get_qc_position("clean") == {"tray": "B", "grid_position": "H1"}


class TestEvosepPlateSamplerIntegration:
    """Integration test for EvosepPlateSampler with real config.

    Plate mode: grid positions get converted to numeric.
    """

    def test_standard_pattern_with_5_samples(self, configs):
        """EvosepPlateSampler converts grid positions to numeric."""
        pattern = configs.queue_patterns.get_pattern("Proteomics", "standard")
        qc_layout = configs.qc_layouts.get_layout("Proteomics", "Evosep.plate")
        qc_layout_pattern = QCLayoutPattern.create(pattern, qc_layout)
        strategy = SamplerStrategy("Evosep", "plate", configs.samplers, qc_layout_pattern)

        groups = [(CONTAINER_ID, 5)]
        slot_entries = build_multi_container_queue_structure(groups, pattern)
        structure = [s.sample_id for s in slot_entries]

        assert len(structure) == 11

        num_user_samples = structure.count("default")
        queue = create_plate_queue(
            [
                {"sample_name": f"S{i}", "sample_id": 1000 + i, "tray": 1, "grid_position": f"A{i + 1}"}
                for i in range(num_user_samples)
            ]
        )
        result = strategy.assign_positions(queue)

        # Grid positions converted to numeric: A1=1, A2=2, ...
        assert len(result.cells) == 5
        assert result.cells[0].grid_position == 1
        assert result.cells[1].grid_position == 2
        assert result.cells[2].grid_position == 3
        assert result.cells[3].grid_position == 4
        assert result.cells[4].grid_position == 5

        # QC positions from qc_layouts_evosep.csv [Proteomics.Evosep.plate]
        assert strategy.get_qc_position("QC03dia") == {"tray": 5, "grid_position": 49}
        assert strategy.get_qc_position("QC01") == {"tray": 5, "grid_position": 1}
        assert strategy.get_qc_position("clean") == {"tray": 6, "grid_position": 1}


class TestVanquishVialSamplerLipidomicsIntegration:
    """Integration test for VanquishVialSampler with Lipidomics config."""

    def test_standard_pattern_with_5_samples(self, configs):
        """VanquishVialSampler generates correct positions for Lipidomics.standard."""
        pattern = configs.queue_patterns.get_pattern("Lipidomics", "standard")
        qc_layout = configs.qc_layouts.get_layout("Lipidomics", "Vanquish.vial")
        qc_layout_pattern = QCLayoutPattern.create(pattern, qc_layout)
        strategy = SamplerStrategy("Vanquish", "vial", configs.samplers, qc_layout_pattern)

        groups = [(CONTAINER_ID, 5)]
        slot_entries = build_multi_container_queue_structure(groups, pattern)
        structure = [s.sample_id for s in slot_entries]

        assert structure == [
            # start (11)
            "blank",
            "EquiSPLASH",
            "pooledQC",
            "blank",
            "pooledQCDil1",
            "pooledQCDil2",
            "pooledQCDil3",
            "pooledQCDil4",
            "pooledQCDil5",
            "pooledQCDil6",
            "blank",
            # 5 samples
            "default",
            "default",
            "default",
            "default",
            "default",
            # end (3)
            "EquiSPLASH",
            "pooledQC",
            "blank",
        ]

        num_user_samples = structure.count("default")
        queue = create_vial_queue(num_user_samples)
        result = strategy.assign_positions(queue)

        assert len(result.cells) == 5
        trays = [result.plates[c.plate_id].tray for c in result.cells]
        assert all(t == "Y" for t in trays)
        assert result.cells[0].grid_position == "A1"
        assert result.cells[1].grid_position == "A2"
        assert result.cells[2].grid_position == "A3"
        assert result.cells[3].grid_position == "A4"
        assert result.cells[4].grid_position == "A5"

        # QC positions from qc_layouts_grid.csv [Lipidomics.Vanquish.vial]
        assert strategy.get_qc_position("blank") == {"tray": "Y", "grid_position": "F1"}
        assert strategy.get_qc_position("EquiSPLASH") == {"tray": "Y", "grid_position": "F9"}
        assert strategy.get_qc_position("pooledQC") == {"tray": "Y", "grid_position": "F8"}
        assert strategy.get_qc_position("pooledQCDil1") == {"tray": "Y", "grid_position": "F2"}
        assert strategy.get_qc_position("pooledQCDil2") == {"tray": "Y", "grid_position": "F3"}
        assert strategy.get_qc_position("pooledQCDil3") == {"tray": "Y", "grid_position": "F4"}
        assert strategy.get_qc_position("pooledQCDil4") == {"tray": "Y", "grid_position": "F5"}
        assert strategy.get_qc_position("pooledQCDil5") == {"tray": "Y", "grid_position": "F6"}
        assert strategy.get_qc_position("pooledQCDil6") == {"tray": "Y", "grid_position": "F7"}
