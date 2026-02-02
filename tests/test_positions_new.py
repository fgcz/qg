# =============================================================================
# Tests for positions_new.py - SamplerStrategyV2
# =============================================================================
#
# Tests use QGConfiguration from qg_configs_new/

from pathlib import Path

import pytest

from qg.config_models_new.loader import qg_configuration
from qg.params_models import ContainerBatch, VialQueue, VialSample
from qg.positions_new import SamplerStrategyV2

CONFIG_DIR = Path(__file__).parent.parent / "qg_configs_new"


@pytest.fixture
def config():
    """Load QGConfiguration from qg_configs_new/."""
    return qg_configuration(CONFIG_DIR)


def create_vial_queue(n_samples: int) -> VialQueue:
    """Helper to create VialQueue with n samples."""
    samples = [
        VialSample(sample_name=f"Sample_{i}", sample_id=1000 + i, tube_id=f"12345/{i}", container_id=12345)
        for i in range(n_samples)
    ]
    batches = {12345: ContainerBatch(container_id=12345)}
    return VialQueue(batches=batches, samples=samples)


class TestSamplerStrategyV2Vanquish:
    """Tests for SamplerStrategyV2 with Vanquish sampler."""

    def test_assigns_positions(self, config) -> None:
        strategy = SamplerStrategyV2("Vanquish", "vial", config, "Proteomics", "standard")
        queue = create_vial_queue(3)

        result = strategy.assign_positions(queue)

        assert len(result.cells) == 3
        for cell in result.cells:
            assert cell.grid_position is not None
        # First available positions (skipping QC reserved positions)
        assert result.cells[0].grid_position == "A1"
        assert result.cells[1].grid_position == "A2"
        assert result.cells[2].grid_position == "A3"

    def test_row_major_order(self, config) -> None:
        strategy = SamplerStrategyV2("Vanquish", "vial", config, "Proteomics", "standard")
        queue = create_vial_queue(15)

        result = strategy.assign_positions(queue)

        # First 9 samples in row A (Vanquish_54 has 9 cols)
        for i in range(9):
            assert result.cells[i].grid_position == f"A{i + 1}"
        # Samples 10-15 in row B
        assert result.cells[9].grid_position == "B1"
        assert result.cells[10].grid_position == "B2"

    def test_spans_multiple_plates(self, config) -> None:
        strategy = SamplerStrategyV2("Vanquish", "vial", config, "Proteomics", "standard")
        # Vanquish_54 = 6 rows x 9 cols = 54 positions per plate
        queue = create_vial_queue(60)

        result = strategy.assign_positions(queue)

        # Check plates via the plates dict
        trays = [result.plates[c.plate_id].tray for c in result.cells]
        assert trays[0] == "Y"
        # After filling first plate (minus QC positions), should move to next tray

    def test_get_qc_position(self, config) -> None:
        strategy = SamplerStrategyV2("Vanquish", "vial", config, "Proteomics", "standard")

        # Get a QC position defined in qc_layouts_grid.csv
        pos = strategy.get_qc_position("QC01")

        assert "tray" in pos
        assert "grid_position" in pos


class TestSamplerStrategyV2MClass:
    """Tests for SamplerStrategyV2 with MClass sampler."""

    def test_assigns_positions(self, config) -> None:
        strategy = SamplerStrategyV2("MClass", "vial", config, "Proteomics", "standard")
        queue = create_vial_queue(3)

        result = strategy.assign_positions(queue)

        assert len(result.cells) == 3
        assert result.cells[0].grid_position == "A1"
        assert result.cells[1].grid_position == "A2"
        assert result.cells[2].grid_position == "A3"

    def test_row_major_order(self, config) -> None:
        strategy = SamplerStrategyV2("MClass", "vial", config, "Proteomics", "standard")
        # MClass_48 has 8 columns per row
        queue = create_vial_queue(10)

        result = strategy.assign_positions(queue)

        # First 8 samples in row A
        for i in range(8):
            assert result.cells[i].grid_position == f"A{i + 1}"
        # Samples 9-10 in row B
        assert result.cells[8].grid_position == "B1"
        assert result.cells[9].grid_position == "B2"


class TestSamplerStrategyV2Evosep:
    """Tests for SamplerStrategyV2 with Evosep sampler."""

    def test_assigns_positions(self, config) -> None:
        strategy = SamplerStrategyV2("Evosep", "vial", config, "Proteomics", "standard")
        queue = create_vial_queue(3)

        result = strategy.assign_positions(queue)

        assert len(result.cells) == 3
        # Evosep uses numeric positions
        assert result.cells[0].grid_position == 1
        assert result.cells[1].grid_position == 2
        assert result.cells[2].grid_position == 3

    def test_sequential_order(self, config) -> None:
        strategy = SamplerStrategyV2("Evosep", "vial", config, "Proteomics", "standard")
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

    def test_raises_when_full(self, config) -> None:
        strategy = SamplerStrategyV2("Evosep", "vial", config, "Proteomics", "standard")
        # 4 slots x 96 = 384, request more
        queue = create_vial_queue(400)

        with pytest.raises(ValueError, match="Not enough positions"):
            strategy.assign_positions(queue)

    def test_get_qc_position(self, config) -> None:
        strategy = SamplerStrategyV2("Evosep", "vial", config, "Proteomics", "standard")

        pos = strategy.get_qc_position("QC01")

        assert "tray" in pos
        assert "grid_position" in pos

    def test_get_qc_position_increments(self, config) -> None:
        strategy = SamplerStrategyV2("Evosep", "vial", config, "Proteomics", "standard")

        pos1 = strategy.get_qc_position("QC01")
        pos2 = strategy.get_qc_position("QC01")

        # Evosep QC positions should increment (consumable tips)
        assert pos1["grid_position"] != pos2["grid_position"]


class TestSamplerStrategyV2NoQC:
    """Tests with noqc pattern (no QC positions reserved)."""

    def test_noqc_uses_all_positions(self, config) -> None:
        strategy = SamplerStrategyV2("Vanquish", "vial", config, "Proteomics", "noqc")
        queue = create_vial_queue(5)

        result = strategy.assign_positions(queue)

        assert len(result.cells) == 5
        # All positions available starting from A1
        assert result.cells[0].grid_position == "A1"
        assert result.cells[1].grid_position == "A2"
        assert result.cells[2].grid_position == "A3"
        assert result.cells[3].grid_position == "A4"
        assert result.cells[4].grid_position == "A5"
