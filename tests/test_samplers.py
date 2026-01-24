"""Integration tests for Samplers with real config."""

from pathlib import Path

import pytest

from qg.config import qg_config
from qg.params_models import InputSample
from qg.positions import QCLayoutPattern, create_sampler
from qg.queue_structure import build_multi_container_queue_structure

CONFIG_DIR = Path(__file__).parent.parent / "qg_configs"


@pytest.fixture
def configs():
    """Load real configs for integration tests."""
    qg_config.cache_clear()
    return qg_config(CONFIG_DIR)


class TestVanquishVialSamplerIntegration:
    """Integration test for VanquishVialSampler with real config.

    Demonstrates the full flow:
    1. Load pattern and qc_layout from config
    2. Create QCLayoutPattern (validates coverage + uniqueness)
    3. Create sampler via factory
    4. Build queue structure
    5. Assign positions
    """

    def test_standard_pattern_with_5_samples(self, configs):
        """VanquishVialSampler generates correct positions for standard pattern."""
        # 1. Load pattern
        pattern = configs.queue_patterns.get_pattern("Proteomics", "standard")

        # 2. Load QC layout for Vanquish.vial
        qc_layout = configs.qc_layouts.get_layout("Proteomics", "Vanquish.vial")

        # 3. Create validated QCLayoutPattern
        qc_layout_pattern = QCLayoutPattern.create(pattern, qc_layout)

        # 4. Create sampler
        sampler = create_sampler("Vanquish.vial", configs.samplers, qc_layout_pattern)

        # 5. Build queue structure: [(container_id, num_samples)]
        groups = [(12345, 5)]
        slot_entries = build_multi_container_queue_structure(groups, pattern)
        structure = [s.sample_id for s in slot_entries]

        # 6. Create user samples (vial mode: no grid_position needed)
        num_user_samples = structure.count("default")
        samples = [InputSample(sample_name=f"S{i}", sample_id=1000 + i) for i in range(num_user_samples)]

        # 7. Assign positions
        positions = sampler.assign_positions(structure, samples)

        # Structure: start(2) + 5 samples + end(4) = 11 slots
        # start = ["QC03dia", "QC01"], end = ["clean", "QC01", "QC03dia", "clean"]
        assert len(positions) == 11
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

        # QC positions from qc_layouts_grid.csv [Proteomics.Vanquish.vial]
        # Unified format: {"tray": plate, "grid_position": "row+col"}
        assert positions[0] == {"tray": "B", "grid_position": "F8"}  # QC03dia
        assert positions[1] == {"tray": "B", "grid_position": "F9"}  # QC01
        assert positions[7] == {"tray": "B", "grid_position": "F7"}  # clean
        assert positions[8] == {"tray": "B", "grid_position": "F9"}  # QC01
        assert positions[9] == {"tray": "B", "grid_position": "F8"}  # QC03dia
        assert positions[10] == {"tray": "B", "grid_position": "F7"}  # clean

        # User positions: Y plate, row A, row-major
        # Unified format: {"tray": plate, "grid_position": "row+col"}
        assert positions[2] == {"tray": "Y", "grid_position": "A1"}
        assert positions[3] == {"tray": "Y", "grid_position": "A2"}
        assert positions[4] == {"tray": "Y", "grid_position": "A3"}
        assert positions[5] == {"tray": "Y", "grid_position": "A4"}
        assert positions[6] == {"tray": "Y", "grid_position": "A5"}


class TestVanquishVialSamplerMetabolomicsIntegration:
    """Integration test for VanquishVialSampler with Metabolomics config."""

    def test_standard_pattern_with_5_samples(self, configs):
        """VanquishVialSampler generates correct positions for Metabolomics.standard."""
        # 1. Load pattern
        pattern = configs.queue_patterns.get_pattern("Metabolomics", "standard")

        # 2. Load QC layout for Vanquish.vial
        qc_layout = configs.qc_layouts.get_layout("Metabolomics", "Vanquish.vial")

        # 3. Create validated QCLayoutPattern
        qc_layout_pattern = QCLayoutPattern.create(pattern, qc_layout)

        # 4. Create sampler
        sampler = create_sampler("Vanquish.vial", configs.samplers, qc_layout_pattern)

        # 5. Build queue structure: [(container_id, num_samples)]
        groups = [(12345, 5)]
        slot_entries = build_multi_container_queue_structure(groups, pattern)
        structure = [s.sample_id for s in slot_entries]

        # 6. Create user samples (vial mode: no grid_position needed)
        num_user_samples = structure.count("default")
        samples = [InputSample(sample_name=f"S{i}", sample_id=1000 + i) for i in range(num_user_samples)]

        # 7. Assign positions
        positions = sampler.assign_positions(structure, samples)

        # Structure: start(11) + 5 samples + end(3) = 19 slots
        # start = ["blank", "108mix_AA", "pooledQC", "blank",
        #          "pooledQCDil1-6", "blank"]
        # end = ["108mix_AA", "pooledQC", "blank"]
        assert len(positions) == 19
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

        # QC positions from qc_layouts_grid.csv [Metabolomics.Vanquish.vial]
        # Unified format: {"tray": plate, "grid_position": "row+col"}
        # Start block
        assert positions[0] == {"tray": "Y", "grid_position": "F1"}  # blank
        assert positions[1] == {"tray": "Y", "grid_position": "F9"}  # 108mix_AA
        assert positions[2] == {"tray": "Y", "grid_position": "F8"}  # pooledQC
        assert positions[3] == {"tray": "Y", "grid_position": "F1"}  # blank
        assert positions[4] == {"tray": "Y", "grid_position": "F2"}  # pooledQCDil1
        assert positions[5] == {"tray": "Y", "grid_position": "F3"}  # pooledQCDil2
        assert positions[6] == {"tray": "Y", "grid_position": "F4"}  # pooledQCDil3
        assert positions[7] == {"tray": "Y", "grid_position": "F5"}  # pooledQCDil4
        assert positions[8] == {"tray": "Y", "grid_position": "F6"}  # pooledQCDil5
        assert positions[9] == {"tray": "Y", "grid_position": "F7"}  # pooledQCDil6
        assert positions[10] == {"tray": "Y", "grid_position": "F1"}  # blank

        # User positions: Y plate, row A, row-major
        # Unified format: {"tray": plate, "grid_position": "row+col"}
        assert positions[11] == {"tray": "Y", "grid_position": "A1"}
        assert positions[12] == {"tray": "Y", "grid_position": "A2"}
        assert positions[13] == {"tray": "Y", "grid_position": "A3"}
        assert positions[14] == {"tray": "Y", "grid_position": "A4"}
        assert positions[15] == {"tray": "Y", "grid_position": "A5"}

        # End block
        assert positions[16] == {"tray": "Y", "grid_position": "F9"}  # 108mix_AA
        assert positions[17] == {"tray": "Y", "grid_position": "F8"}  # pooledQC
        assert positions[18] == {"tray": "Y", "grid_position": "F1"}  # blank


class TestEvosepVialSamplerIntegration:
    """Integration test for EvosepVialSampler with real config.

    Demonstrates the full flow:
    1. Load pattern and qc_layout from config
    2. Create QCLayoutPattern (validates coverage + uniqueness)
    3. Create sampler via factory
    4. Build queue structure
    5. Assign positions

    Key insight: Evosep uses consumable tips, so each QC injection consumes
    the next position in the range. Multiple QC01 injections use positions
    1, 2, 3, etc. (not repeating position 1).
    """

    def test_standard_pattern_with_5_samples(self, configs):
        """EvosepVialSampler generates correct positions for standard pattern."""
        # 1. Load pattern
        pattern = configs.queue_patterns.get_pattern("Proteomics", "standard")

        # 2. Load QC layout for Evosep.vial
        qc_layout = configs.qc_layouts.get_layout("Proteomics", "Evosep.vial")

        # 3. Create validated QCLayoutPattern
        qc_layout_pattern = QCLayoutPattern.create(pattern, qc_layout)

        # 4. Create sampler
        sampler = create_sampler("Evosep.vial", configs.samplers, qc_layout_pattern)

        # 5. Build queue structure: [(container_id, num_samples)]
        groups = [(12345, 5)]
        slot_entries = build_multi_container_queue_structure(groups, pattern)
        structure = [s.sample_id for s in slot_entries]

        # 6. Create user samples (vial mode: no grid_position needed)
        num_user_samples = structure.count("default")
        samples = [InputSample(sample_name=f"S{i}", sample_id=1000 + i) for i in range(num_user_samples)]

        # 7. Assign positions
        positions = sampler.assign_positions(structure, samples)

        # Structure: start(2) + 5 samples + end(4) = 11 slots
        # start = ["QC03dia", "QC01"], end = ["clean", "QC01", "QC03dia", "clean"]
        assert len(positions) == 11
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

        # QC positions from qc_layouts_evosep.csv [Proteomics.Evosep]
        # Evosep uses SEQUENTIAL positions from range (consumable tips)
        # QC01 range: tray 5, positions 1-48
        # QC03dia range: tray 5, positions 49-96
        # clean range: tray 6, positions 1-96
        # Unified format: {"tray": slot, "grid_position": position_number}

        # First QC03dia uses position 49, second uses 50
        assert positions[0] == {"tray": 5, "grid_position": 49}  # QC03dia #1
        # First QC01 uses position 1, second uses 2
        assert positions[1] == {"tray": 5, "grid_position": 1}  # QC01 #1

        # User positions: sequential across slots
        assert positions[2] == {"tray": 1, "grid_position": 1}
        assert positions[3] == {"tray": 1, "grid_position": 2}
        assert positions[4] == {"tray": 1, "grid_position": 3}
        assert positions[5] == {"tray": 1, "grid_position": 4}
        assert positions[6] == {"tray": 1, "grid_position": 5}

        # End block QC positions - SEQUENTIAL from their ranges
        assert positions[7] == {"tray": 6, "grid_position": 1}  # clean #1
        assert positions[8] == {"tray": 5, "grid_position": 2}  # QC01 #2 (next in range!)
        assert positions[9] == {"tray": 5, "grid_position": 50}  # QC03dia #2 (next in range!)
        assert positions[10] == {"tray": 6, "grid_position": 2}  # clean #2 (next in range!)


class TestMClass48VialSamplerIntegration:
    """Integration test for MClass48VialSampler with real config."""

    def test_standard_pattern_with_5_samples(self, configs):
        """MClass48VialSampler generates correct positions for standard pattern."""
        # 1. Load pattern
        pattern = configs.queue_patterns.get_pattern("Proteomics", "standard")

        # 2. Load QC layout for MClass48.vial
        qc_layout = configs.qc_layouts.get_layout("Proteomics", "MClass48.vial")

        # 3. Create validated QCLayoutPattern
        qc_layout_pattern = QCLayoutPattern.create(pattern, qc_layout)

        # 4. Create sampler
        sampler = create_sampler("MClass48.vial", configs.samplers, qc_layout_pattern)

        # 5. Build queue structure
        groups = [(12345, 5)]
        slot_entries = build_multi_container_queue_structure(groups, pattern)
        structure = [s.sample_id for s in slot_entries]

        # 6. Create user samples (vial mode: no grid_position needed)
        num_user_samples = structure.count("default")
        samples = [InputSample(sample_name=f"S{i}", sample_id=1000 + i) for i in range(num_user_samples)]

        # 7. Assign positions
        positions = sampler.assign_positions(structure, samples)

        # Structure: start(2) + 5 samples + end(4) = 11 slots
        assert len(positions) == 11

        # QC positions from qc_layouts_grid.csv [Proteomics.MClass48.vial]
        # MClass48 uses plate "1" instead of "B", QC in row F
        assert positions[0] == {"tray": "1", "grid_position": "F7"}  # QC03dia
        assert positions[1] == {"tray": "1", "grid_position": "F8"}  # QC01
        assert positions[7] == {"tray": "1", "grid_position": "F6"}  # clean
        assert positions[8] == {"tray": "1", "grid_position": "F8"}  # QC01
        assert positions[9] == {"tray": "1", "grid_position": "F7"}  # QC03dia
        assert positions[10] == {"tray": "1", "grid_position": "F6"}  # clean

        # User positions: plate "1", row A, row-major
        assert positions[2] == {"tray": "1", "grid_position": "A1"}
        assert positions[3] == {"tray": "1", "grid_position": "A2"}
        assert positions[4] == {"tray": "1", "grid_position": "A3"}
        assert positions[5] == {"tray": "1", "grid_position": "A4"}
        assert positions[6] == {"tray": "1", "grid_position": "A5"}


class TestVanquishPlateSamplerIntegration:
    """Integration test for VanquishPlateSampler with real config.

    Plate mode: user samples have pre-assigned grid_position from input.
    """

    def test_standard_pattern_with_5_samples(self, configs):
        """VanquishPlateSampler uses input positions for user samples."""
        # 1. Load pattern
        pattern = configs.queue_patterns.get_pattern("Proteomics", "standard")

        # 2. Load QC layout for Vanquish.plate
        qc_layout = configs.qc_layouts.get_layout("Proteomics", "Vanquish.plate")

        # 3. Create validated QCLayoutPattern
        qc_layout_pattern = QCLayoutPattern.create(pattern, qc_layout)

        # 4. Create sampler
        sampler = create_sampler("Vanquish.plate", configs.samplers, qc_layout_pattern)

        # 5. Build queue structure
        groups = [(12345, 5)]
        slot_entries = build_multi_container_queue_structure(groups, pattern)
        structure = [s.sample_id for s in slot_entries]

        # 6. Create user samples WITH pre-assigned grid_position (plate mode)
        num_user_samples = structure.count("default")
        samples = [
            InputSample(sample_name=f"S{i}", sample_id=1000 + i, grid_position=f"B{i + 1}")
            for i in range(num_user_samples)
        ]

        # 7. Assign positions
        positions = sampler.assign_positions(structure, samples)

        # Structure: start(2) + 5 samples + end(4) = 11 slots
        assert len(positions) == 11

        # QC positions from qc_layouts_grid.csv [Proteomics.Vanquish.plate]
        # Vanquish.plate uses QC in row H, plate B
        assert positions[0] == {"tray": "B", "grid_position": "H10"}  # QC03dia
        assert positions[1] == {"tray": "B", "grid_position": "H9"}  # QC01
        assert positions[7] == {"tray": "B", "grid_position": "H1"}  # clean
        assert positions[8] == {"tray": "B", "grid_position": "H9"}  # QC01
        assert positions[9] == {"tray": "B", "grid_position": "H10"}  # QC03dia
        assert positions[10] == {"tray": "B", "grid_position": "H1"}  # clean

        # User positions: from input grid_position (plate mode reads from samples)
        assert positions[2] == {"tray": "Y", "grid_position": "B1"}
        assert positions[3] == {"tray": "Y", "grid_position": "B2"}
        assert positions[4] == {"tray": "Y", "grid_position": "B3"}
        assert positions[5] == {"tray": "Y", "grid_position": "B4"}
        assert positions[6] == {"tray": "Y", "grid_position": "B5"}


class TestEvosepPlateSamplerIntegration:
    """Integration test for EvosepPlateSampler with real config.

    Plate mode: user samples have pre-assigned grid_position from input,
    which gets converted to numeric position.
    """

    def test_standard_pattern_with_5_samples(self, configs):
        """EvosepPlateSampler converts grid positions to numeric."""
        # 1. Load pattern
        pattern = configs.queue_patterns.get_pattern("Proteomics", "standard")

        # 2. Load QC layout for Evosep.plate
        qc_layout = configs.qc_layouts.get_layout("Proteomics", "Evosep.plate")

        # 3. Create validated QCLayoutPattern
        qc_layout_pattern = QCLayoutPattern.create(pattern, qc_layout)

        # 4. Create sampler
        sampler = create_sampler("Evosep.plate", configs.samplers, qc_layout_pattern)

        # 5. Build queue structure
        groups = [(12345, 5)]
        slot_entries = build_multi_container_queue_structure(groups, pattern)
        structure = [s.sample_id for s in slot_entries]

        # 6. Create user samples WITH pre-assigned grid_position (plate mode)
        # Grid positions like "A1", "A2" get converted to numeric (1, 2, ...)
        num_user_samples = structure.count("default")
        samples = [
            InputSample(sample_name=f"S{i}", sample_id=1000 + i, grid_position=f"A{i + 1}")
            for i in range(num_user_samples)
        ]

        # 7. Assign positions
        positions = sampler.assign_positions(structure, samples)

        # Structure: start(2) + 5 samples + end(4) = 11 slots
        assert len(positions) == 11

        # QC positions from qc_layouts_evosep.csv [Proteomics.Evosep.plate]
        # Same ranges as vial mode
        assert positions[0] == {"tray": 5, "grid_position": 49}  # QC03dia #1
        assert positions[1] == {"tray": 5, "grid_position": 1}  # QC01 #1
        assert positions[7] == {"tray": 6, "grid_position": 1}  # clean #1
        assert positions[8] == {"tray": 5, "grid_position": 2}  # QC01 #2
        assert positions[9] == {"tray": 5, "grid_position": 50}  # QC03dia #2
        assert positions[10] == {"tray": 6, "grid_position": 2}  # clean #2

        # User positions: grid_position converted to numeric
        # A1=1, A2=2, A3=3, A4=4, A5=5
        assert positions[2] == {"tray": 1, "grid_position": 1}
        assert positions[3] == {"tray": 1, "grid_position": 2}
        assert positions[4] == {"tray": 1, "grid_position": 3}
        assert positions[5] == {"tray": 1, "grid_position": 4}
        assert positions[6] == {"tray": 1, "grid_position": 5}


class TestVanquishVialSamplerLipidomicsIntegration:
    """Integration test for VanquishVialSampler with Lipidomics config."""

    def test_standard_pattern_with_5_samples(self, configs):
        """VanquishVialSampler generates correct positions for Lipidomics.standard."""
        # 1. Load pattern
        pattern = configs.queue_patterns.get_pattern("Lipidomics", "standard")

        # 2. Load QC layout for Vanquish.vial
        qc_layout = configs.qc_layouts.get_layout("Lipidomics", "Vanquish.vial")

        # 3. Create validated QCLayoutPattern
        qc_layout_pattern = QCLayoutPattern.create(pattern, qc_layout)

        # 4. Create sampler
        sampler = create_sampler("Vanquish.vial", configs.samplers, qc_layout_pattern)

        # 5. Build queue structure
        groups = [(12345, 5)]
        slot_entries = build_multi_container_queue_structure(groups, pattern)
        structure = [s.sample_id for s in slot_entries]

        # 6. Create user samples
        num_user_samples = structure.count("default")
        samples = [InputSample(sample_name=f"S{i}", sample_id=1000 + i) for i in range(num_user_samples)]

        # 7. Assign positions
        positions = sampler.assign_positions(structure, samples)

        # Structure: start(11) + 5 samples + end(3) = 19 slots
        # start = ["blank", "EquiSPLASH", "pooledQC", "blank",
        #          "pooledQCDil1-6", "blank"]
        # end = ["EquiSPLASH", "pooledQC", "blank"]
        assert len(positions) == 19
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

        # QC positions from qc_layouts_grid.csv [Lipidomics.Vanquish.vial]
        # Start block
        assert positions[0] == {"tray": "Y", "grid_position": "F1"}  # blank
        assert positions[1] == {"tray": "Y", "grid_position": "F9"}  # EquiSPLASH
        assert positions[2] == {"tray": "Y", "grid_position": "F8"}  # pooledQC
        assert positions[3] == {"tray": "Y", "grid_position": "F1"}  # blank
        assert positions[4] == {"tray": "Y", "grid_position": "F2"}  # pooledQCDil1
        assert positions[5] == {"tray": "Y", "grid_position": "F3"}  # pooledQCDil2
        assert positions[6] == {"tray": "Y", "grid_position": "F4"}  # pooledQCDil3
        assert positions[7] == {"tray": "Y", "grid_position": "F5"}  # pooledQCDil4
        assert positions[8] == {"tray": "Y", "grid_position": "F6"}  # pooledQCDil5
        assert positions[9] == {"tray": "Y", "grid_position": "F7"}  # pooledQCDil6
        assert positions[10] == {"tray": "Y", "grid_position": "F1"}  # blank

        # User positions: Y plate, row A, row-major
        assert positions[11] == {"tray": "Y", "grid_position": "A1"}
        assert positions[12] == {"tray": "Y", "grid_position": "A2"}
        assert positions[13] == {"tray": "Y", "grid_position": "A3"}
        assert positions[14] == {"tray": "Y", "grid_position": "A4"}
        assert positions[15] == {"tray": "Y", "grid_position": "A5"}

        # End block
        assert positions[16] == {"tray": "Y", "grid_position": "F9"}  # EquiSPLASH
        assert positions[17] == {"tray": "Y", "grid_position": "F8"}  # pooledQC
        assert positions[18] == {"tray": "Y", "grid_position": "F1"}  # blank
