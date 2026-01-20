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

        # QC positions from qc_layouts.toml [Proteomics.Vanquish.vial]
        assert positions[0] == "B:F8"  # QC03dia
        assert positions[1] == "B:F9"  # QC01
        assert positions[7] == "B:F7"  # clean
        assert positions[8] == "B:F9"  # QC01
        assert positions[9] == "B:F8"  # QC03dia
        assert positions[10] == "B:F7"  # clean

        # User positions: Y plate, row A, row-major (Y:A1, Y:A2, ...)
        assert positions[2] == "Y:A1"
        assert positions[3] == "Y:A2"
        assert positions[4] == "Y:A3"
        assert positions[5] == "Y:A4"
        assert positions[6] == "Y:A5"

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

        # QC positions from qc_layouts.toml [Metabolomics.Vanquish.vial]
        # Start block
        assert positions[0] == "Y:F1"   # blank
        assert positions[1] == "Y:F9"   # 108mix_AA
        assert positions[2] == "Y:F8"   # pooledQC
        assert positions[3] == "Y:F1"   # blank
        assert positions[4] == "Y:F2"   # pooledQCDil1
        assert positions[5] == "Y:F3"   # pooledQCDil2
        assert positions[6] == "Y:F4"   # pooledQCDil3
        assert positions[7] == "Y:F5"   # pooledQCDil4
        assert positions[8] == "Y:F6"   # pooledQCDil5
        assert positions[9] == "Y:F7"   # pooledQCDil6
        assert positions[10] == "Y:F1"  # blank

        # User positions: Y plate, row A, row-major (Y:A1, Y:A2, ...)
        assert positions[11] == "Y:A1"
        assert positions[12] == "Y:A2"
        assert positions[13] == "Y:A3"
        assert positions[14] == "Y:A4"
        assert positions[15] == "Y:A5"

        # End block
        assert positions[16] == "Y:F9"  # 108mix_AA
        assert positions[17] == "Y:F8"  # pooledQC
        assert positions[18] == "Y:F1"  # blank
