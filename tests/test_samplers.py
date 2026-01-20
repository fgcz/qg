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
        samples = [
            InputSample(sample_name=f"S{i}", sample_id=1000 + i)
            for i in range(num_user_samples)
        ]

        # 7. Assign positions
        positions = sampler.assign_positions(structure, samples)

        # Structure: start(2) + 5 samples + end(4) = 11 slots
        # start = ["QC03dia", "QC01"], end = ["clean", "QC01", "QC03dia", "clean"]
        assert len(positions) == 11
        assert structure == [
            "QC03dia", "QC01",  # start
            "default", "default", "default", "default", "default",  # 5 samples
            "clean", "QC01", "QC03dia", "clean",  # end
        ]

        # QC positions from qc_layouts.toml [Proteomics.Vanquish.vial] - now dicts
        assert positions[0] == {"plate": "B", "row": "F", "col": 8}  # QC03dia
        assert positions[1] == {"plate": "B", "row": "F", "col": 9}  # QC01
        assert positions[7] == {"plate": "B", "row": "F", "col": 7}  # clean
        assert positions[8] == {"plate": "B", "row": "F", "col": 9}  # QC01
        assert positions[9] == {"plate": "B", "row": "F", "col": 8}  # QC03dia
        assert positions[10] == {"plate": "B", "row": "F", "col": 7}  # clean

        # User positions: Y plate, row A, row-major - now dicts
        assert positions[2] == {"plate": "Y", "row": "A", "col": 1}
        assert positions[3] == {"plate": "Y", "row": "A", "col": 2}
        assert positions[4] == {"plate": "Y", "row": "A", "col": 3}
        assert positions[5] == {"plate": "Y", "row": "A", "col": 4}
        assert positions[6] == {"plate": "Y", "row": "A", "col": 5}

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
        samples = [
            InputSample(sample_name=f"S{i}", sample_id=1000 + i)
            for i in range(num_user_samples)
        ]

        # 7. Assign positions
        positions = sampler.assign_positions(structure, samples)

        # Structure: start(11) + 5 samples + end(3) = 19 slots
        # start = ["blank", "108mix_AA", "pooledQC", "blank",
        #          "pooledQCDil1-6", "blank"]
        # end = ["108mix_AA", "pooledQC", "blank"]
        assert len(positions) == 19
        assert structure == [
            # start (11)
            "blank", "108mix_AA", "pooledQC", "blank",
            "pooledQCDil1", "pooledQCDil2", "pooledQCDil3",
            "pooledQCDil4", "pooledQCDil5", "pooledQCDil6",
            "blank",
            # 5 samples
            "default", "default", "default", "default", "default",
            # end (3)
            "108mix_AA", "pooledQC", "blank",
        ]

        # QC positions from qc_layouts.toml [Metabolomics.Vanquish.vial] - now dicts
        # Start block
        assert positions[0] == {"plate": "Y", "row": "F", "col": 1}   # blank
        assert positions[1] == {"plate": "Y", "row": "F", "col": 9}   # 108mix_AA
        assert positions[2] == {"plate": "Y", "row": "F", "col": 8}   # pooledQC
        assert positions[3] == {"plate": "Y", "row": "F", "col": 1}   # blank
        assert positions[4] == {"plate": "Y", "row": "F", "col": 2}   # pooledQCDil1
        assert positions[5] == {"plate": "Y", "row": "F", "col": 3}   # pooledQCDil2
        assert positions[6] == {"plate": "Y", "row": "F", "col": 4}   # pooledQCDil3
        assert positions[7] == {"plate": "Y", "row": "F", "col": 5}   # pooledQCDil4
        assert positions[8] == {"plate": "Y", "row": "F", "col": 6}   # pooledQCDil5
        assert positions[9] == {"plate": "Y", "row": "F", "col": 7}   # pooledQCDil6
        assert positions[10] == {"plate": "Y", "row": "F", "col": 1}  # blank

        # User positions: Y plate, row A, row-major - now dicts
        assert positions[11] == {"plate": "Y", "row": "A", "col": 1}
        assert positions[12] == {"plate": "Y", "row": "A", "col": 2}
        assert positions[13] == {"plate": "Y", "row": "A", "col": 3}
        assert positions[14] == {"plate": "Y", "row": "A", "col": 4}
        assert positions[15] == {"plate": "Y", "row": "A", "col": 5}

        # End block
        assert positions[16] == {"plate": "Y", "row": "F", "col": 9}  # 108mix_AA
        assert positions[17] == {"plate": "Y", "row": "F", "col": 8}  # pooledQC
        assert positions[18] == {"plate": "Y", "row": "F", "col": 1}  # blank


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

        # 2. Load QC layout for Evosep
        qc_layout = configs.qc_layouts.get_layout("Proteomics", "Evosep")

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
        samples = [
            InputSample(sample_name=f"S{i}", sample_id=1000 + i)
            for i in range(num_user_samples)
        ]

        # 7. Assign positions
        positions = sampler.assign_positions(structure, samples)

        # Structure: start(2) + 5 samples + end(4) = 11 slots
        # start = ["QC03dia", "QC01"], end = ["clean", "QC01", "QC03dia", "clean"]
        assert len(positions) == 11
        assert structure == [
            "QC03dia", "QC01",  # start
            "default", "default", "default", "default", "default",  # 5 samples
            "clean", "QC01", "QC03dia", "clean",  # end
        ]

        # QC positions from qc_layouts.toml [Proteomics.Evosep]
        # Evosep uses SEQUENTIAL positions from range (consumable tips)
        # QC01 range: tray 5, positions 1-48
        # QC03dia range: tray 5, positions 49-96
        # clean range: tray 6, positions 1-96

        # First QC03dia uses position 49, second uses 50
        assert positions[0] == {"tray": 5, "position": 49}  # QC03dia #1
        # First QC01 uses position 1, second uses 2
        assert positions[1] == {"tray": 5, "position": 1}   # QC01 #1

        # User positions: sequential across slots
        assert positions[2] == {"tray": 1, "position": 1}
        assert positions[3] == {"tray": 1, "position": 2}
        assert positions[4] == {"tray": 1, "position": 3}
        assert positions[5] == {"tray": 1, "position": 4}
        assert positions[6] == {"tray": 1, "position": 5}

        # End block QC positions - SEQUENTIAL from their ranges
        assert positions[7] == {"tray": 6, "position": 1}   # clean #1
        assert positions[8] == {"tray": 5, "position": 2}   # QC01 #2 (next in range!)
        assert positions[9] == {"tray": 5, "position": 50}  # QC03dia #2 (next in range!)
        assert positions[10] == {"tray": 6, "position": 2}  # clean #2 (next in range!)
