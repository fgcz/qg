# =============================================================================
# Tests for positionV2.py
# =============================================================================
#
# - assign() tests use create_assembled_sampler
# - get_qc_position() tests use create_qc_position_provider

from dataclasses import dataclass
from pathlib import Path

import pytest

from qg.config_models.formatting import SamplesConfig
from qg.config_models.loader import qg_configuration
from qg.params_models import ContainerBatch, Plate, PlateCell, PlateQueue, VialQueue, VialSample
from qg.positionV2 import create_assembled_sampler
from qg.qc_positions import create_qc_position_provider

CONFIG_DIR = Path(__file__).parent.parent / "qg_configs"


@pytest.fixture
def config():
    """Load QGConfiguration from qg_configs/."""
    return qg_configuration(CONFIG_DIR)


def create_vial_queue(n_samples: int) -> VialQueue:
    """Helper to create VialQueue with n samples."""
    samples = [
        VialSample(sample_name=f"Sample_{i}", sample_id=1000 + i, tube_id=f"12345/{i}", container_id=12345)
        for i in range(n_samples)
    ]
    batches = {12345: ContainerBatch(container_id=12345)}
    return VialQueue(batches=batches, samples=samples)


# =============================================================================
# Tests for assign() - using positionV2.create_assembled_sampler
# =============================================================================


class TestAssignVanquish:
    """Tests for assign() with Vanquish sampler."""

    def test_assigns_positions(self, config) -> None:
        pattern = config.queue_patterns.get_pattern("Proteomics", "standard")
        sampler = create_assembled_sampler("Vanquish", "vial", config, "Proteomics", pattern, "Vanquish_54")
        queue = create_vial_queue(3)

        result = sampler.assign(queue)

        assert len(result.cells) == 3
        for cell in result.cells:
            assert cell.grid_position is not None
        # First available positions (skipping QC reserved positions)
        assert result.cells[0].grid_position == "A1"
        assert result.cells[1].grid_position == "A2"
        assert result.cells[2].grid_position == "A3"

    def test_row_major_order(self, config) -> None:
        pattern = config.queue_patterns.get_pattern("Proteomics", "standard")
        sampler = create_assembled_sampler("Vanquish", "vial", config, "Proteomics", pattern, "Vanquish_54")
        queue = create_vial_queue(15)

        result = sampler.assign(queue)

        # First 9 samples in row A (Vanquish_54 has 9 cols)
        for i in range(9):
            assert result.cells[i].grid_position == f"A{i + 1}"
        # Samples 10-15 in row B
        assert result.cells[9].grid_position == "B1"
        assert result.cells[10].grid_position == "B2"

    def test_spans_multiple_plates(self, config) -> None:
        pattern = config.queue_patterns.get_pattern("Proteomics", "standard")
        sampler = create_assembled_sampler("Vanquish", "vial", config, "Proteomics", pattern, "Vanquish_54")
        # Vanquish_54 = 6 rows x 9 cols = 54 positions per plate
        queue = create_vial_queue(60)

        result = sampler.assign(queue)

        # Check plates via the plates dict
        trays = [result.plates[c.plate_id].tray for c in result.cells]
        assert trays[0] == "Y"
        # After filling first plate (minus QC positions), should move to next tray


class TestAssignMClass:
    """Tests for assign() with MClass sampler."""

    def test_assigns_positions(self, config) -> None:
        pattern = config.queue_patterns.get_pattern("Proteomics", "standard")
        sampler = create_assembled_sampler("MClass", "vial", config, "Proteomics", pattern, "MClass_48")
        queue = create_vial_queue(3)

        result = sampler.assign(queue)

        assert len(result.cells) == 3
        assert result.cells[0].grid_position == "A1"
        assert result.cells[1].grid_position == "A2"
        assert result.cells[2].grid_position == "A3"

    def test_row_major_order(self, config) -> None:
        pattern = config.queue_patterns.get_pattern("Proteomics", "standard")
        sampler = create_assembled_sampler("MClass", "vial", config, "Proteomics", pattern, "MClass_48")
        # MClass_48 has 8 columns per row
        queue = create_vial_queue(10)

        result = sampler.assign(queue)

        # First 8 samples in row A
        for i in range(8):
            assert result.cells[i].grid_position == f"A{i + 1}"
        # Samples 9-10 in row B
        assert result.cells[8].grid_position == "B1"
        assert result.cells[9].grid_position == "B2"


class TestAssignEvosep:
    """Tests for assign() with Evosep sampler."""

    def test_assigns_positions(self, config) -> None:
        pattern = config.queue_patterns.get_pattern("Proteomics", "standard")
        sampler = create_assembled_sampler("Evosep", "vial", config, "Proteomics", pattern, "Evosep_96")
        queue = create_vial_queue(3)

        result = sampler.assign(queue)

        assert len(result.cells) == 3
        # Evosep uses numeric positions
        assert result.cells[0].grid_position == 1
        assert result.cells[1].grid_position == 2
        assert result.cells[2].grid_position == 3

    def test_sequential_order(self, config) -> None:
        pattern = config.queue_patterns.get_pattern("Proteomics", "standard")
        sampler = create_assembled_sampler("Evosep", "vial", config, "Proteomics", pattern, "Evosep_96")
        # Test positions across a slot boundary
        queue = create_vial_queue(100)

        result = sampler.assign(queue)

        # First 96 samples on tray 1
        for i in range(96):
            assert result.cells[i].grid_position == i + 1
        # Samples 97-100 on tray 2
        tray_97 = result.plates[result.cells[96].plate_id].tray
        assert tray_97 == 2
        assert result.cells[96].grid_position == 1

    def test_raises_when_full(self, config) -> None:
        pattern = config.queue_patterns.get_pattern("Proteomics", "standard")
        sampler = create_assembled_sampler("Evosep", "vial", config, "Proteomics", pattern, "Evosep_96")
        # 4 slots x 96 = 384, request more
        queue = create_vial_queue(400)

        with pytest.raises(ValueError):
            sampler.assign(queue)


class TestGridSamplerExhaustion:
    """Tests for grid sampler position exhaustion."""

    def test_vanquish_raises_when_full(self, config) -> None:
        """Vanquish_54 has 4 trays × 54 = 216 positions."""
        pattern = config.queue_patterns.get_pattern("Proteomics", "noqc")
        sampler = create_assembled_sampler("Vanquish", "vial", config, "Proteomics", pattern, "Vanquish_54")
        queue = create_vial_queue(220)  # More than 216

        with pytest.raises(ValueError, match="Not enough positions"):
            sampler.assign(queue)

    def test_mclass_raises_when_full(self, config) -> None:
        """MClass_48 has 4 trays × 48 = 192 positions."""
        pattern = config.queue_patterns.get_pattern("Proteomics", "noqc")
        sampler = create_assembled_sampler("MClass", "vial", config, "Proteomics", pattern, "MClass_48")
        queue = create_vial_queue(200)  # More than 192

        with pytest.raises(ValueError, match="Not enough positions"):
            sampler.assign(queue)


class TestAssignNoQC:
    """Tests with noqc pattern (no QC positions reserved)."""

    def test_noqc_uses_all_positions(self, config) -> None:
        pattern = config.queue_patterns.get_pattern("Proteomics", "noqc")
        sampler = create_assembled_sampler("Vanquish", "vial", config, "Proteomics", pattern, "Vanquish_54")
        queue = create_vial_queue(5)

        result = sampler.assign(queue)

        assert len(result.cells) == 5
        # All positions available starting from A1
        assert result.cells[0].grid_position == "A1"
        assert result.cells[1].grid_position == "A2"
        assert result.cells[2].grid_position == "A3"
        assert result.cells[3].grid_position == "A4"
        assert result.cells[4].grid_position == "A5"


def create_multi_container_vial_queue(containers: list[tuple[int, int]]) -> VialQueue:
    """Create VialQueue with samples from multiple containers.

    Args:
        containers: List of (container_id, n_samples) tuples
    """
    samples = []
    batches = {}
    sample_id = 1000
    for container_id, n_samples in containers:
        batches[container_id] = ContainerBatch(container_id=container_id)
        for i in range(n_samples):
            samples.append(
                VialSample(
                    sample_name=f"C{container_id}_S{i}",
                    sample_id=sample_id,
                    tube_id=f"{container_id}/{i}",
                    container_id=container_id,
                )
            )
            sample_id += 1
    return VialQueue(batches=batches, samples=samples)


class TestOneContainerPerTray:
    """Tests for one_container_per_tray mode."""

    def test_one_container_per_tray_assigns_to_different_trays(self, config) -> None:
        """Each container's samples should be on a different tray."""
        pattern = config.queue_patterns.get_pattern("Proteomics", "noqc")
        sampler = create_assembled_sampler("Vanquish", "vial", config, "Proteomics", pattern, "Vanquish_54")
        # Two containers with 3 samples each
        queue = create_multi_container_vial_queue([(100, 3), (200, 3)])

        result = sampler.assign(queue, one_container_per_tray=True)

        assert len(result.cells) == 6
        # Container 100 samples should all be on tray Y (first tray)
        container_100_trays = {result.plates[c.plate_id].tray for c in result.cells if c.sample.container_id == 100}
        # Container 200 samples should all be on tray R (second tray)
        container_200_trays = {result.plates[c.plate_id].tray for c in result.cells if c.sample.container_id == 200}
        assert len(container_100_trays) == 1, "Container 100 should be on one tray"
        assert len(container_200_trays) == 1, "Container 200 should be on one tray"
        assert container_100_trays != container_200_trays, "Containers should be on different trays"

    def test_one_container_per_tray_false_mixes_on_same_tray(self, config) -> None:
        """Without one_container_per_tray, samples fill sequentially across trays."""
        pattern = config.queue_patterns.get_pattern("Proteomics", "noqc")
        sampler = create_assembled_sampler("Vanquish", "vial", config, "Proteomics", pattern, "Vanquish_54")
        # Two containers with 3 samples each
        queue = create_multi_container_vial_queue([(100, 3), (200, 3)])

        result = sampler.assign(queue, one_container_per_tray=False)

        assert len(result.cells) == 6
        # All 6 samples should be on the same tray (Y) since they fit
        trays = {result.plates[c.plate_id].tray for c in result.cells}
        assert len(trays) == 1, "All samples should be on the same tray when one_container_per_tray=False"

    def test_one_container_per_tray_raises_when_not_enough_trays(self, config) -> None:
        """Should raise error if more containers than available trays."""
        pattern = config.queue_patterns.get_pattern("Proteomics", "noqc")
        sampler = create_assembled_sampler("Vanquish", "vial", config, "Proteomics", pattern, "Vanquish_54")
        # Vanquish has 4 trays, try with 5 containers
        queue = create_multi_container_vial_queue([(100, 1), (200, 1), (300, 1), (400, 1), (500, 1)])

        with pytest.raises(ValueError):
            sampler.assign(queue, one_container_per_tray=True)

    def test_one_container_per_tray_raises_when_container_too_large(self, config) -> None:
        """Should raise error if a container has more samples than tray capacity."""
        pattern = config.queue_patterns.get_pattern("Proteomics", "noqc")
        sampler = create_assembled_sampler("Vanquish", "vial", config, "Proteomics", pattern, "Vanquish_54")
        # Vanquish_54 has 54 positions per tray, try with 60 samples in one container
        queue = create_multi_container_vial_queue([(100, 60)])

        with pytest.raises(ValueError):
            sampler.assign(queue, one_container_per_tray=True)


# =============================================================================
# Tests for get_qc_position() - using positionV2.create_qc_position_provider
# =============================================================================


@dataclass
class MockSlotEntry:
    """Mock SlotEntry for testing QC position providers."""

    sample_id: str
    container_id: int = 0


class TestGetQCPosition:
    """Tests for QC position providers."""

    def test_vanquish_get_qc_position(self, config) -> None:
        # Grid ignores slot_entries, so we pass empty list
        pattern = config.queue_patterns.get_pattern("Proteomics", "standard")
        provider = create_qc_position_provider(
            "Vanquish",
            config,
            "Proteomics",
            pattern,
            "Vanquish_54",
            slot_entries=[],
            default_sample_id=SamplesConfig.DEFAULT_SAMPLE_ID,
        )

        pos = provider.get_position("QC01")

        assert pos.tray is not None
        assert pos.grid_position is not None

    def test_evosep_get_qc_position(self, config) -> None:
        # Evosep needs slot_entries to validate capacity
        pattern = config.queue_patterns.get_pattern("Proteomics", "standard")
        slot_entries = [MockSlotEntry("QC01")]
        provider = create_qc_position_provider(
            "Evosep",
            config,
            "Proteomics",
            pattern,
            "Evosep_96",
            slot_entries=slot_entries,
            default_sample_id=SamplesConfig.DEFAULT_SAMPLE_ID,
        )

        pos = provider.get_position("QC01")

        assert pos.tray is not None
        assert pos.grid_position is not None

    def test_evosep_get_qc_position_increments(self, config) -> None:
        # Evosep needs slot_entries - we need 2 QC01 entries since we call get_position twice
        pattern = config.queue_patterns.get_pattern("Proteomics", "standard")
        slot_entries = [MockSlotEntry("QC01"), MockSlotEntry("QC01")]
        provider = create_qc_position_provider(
            "Evosep",
            config,
            "Proteomics",
            pattern,
            "Evosep_96",
            slot_entries=slot_entries,
            default_sample_id=SamplesConfig.DEFAULT_SAMPLE_ID,
        )

        pos1 = provider.get_position("QC01")
        pos2 = provider.get_position("QC01")

        # Evosep QC positions should increment (consumable tips)
        assert pos1.grid_position != pos2.grid_position


# =============================================================================
# Tests for Evosep Plate mode - alphanumeric to numeric conversion
# =============================================================================


def _create_plate_queue_with_alpha_positions(positions: list[str]) -> PlateQueue:
    """Helper to create a PlateQueue with alphanumeric grid positions (e.g., A1, D8)."""
    cells = [
        PlateCell(
            sample=VialSample(
                sample_name=f"Sample_{i}",
                sample_id=1000 + i,
                tube_id=f"12345/{i}",
                container_id=12345,
            ),
            position=i + 1,
            grid_position=pos,
            plate_id=1,
        )
        for i, pos in enumerate(positions)
    ]
    plates = {1: Plate(plate_id=1, tray=None, nr_samples=len(cells))}
    batches = {12345: ContainerBatch(container_id=12345)}
    return PlateQueue(batches=batches, plates=plates, cells=cells)


class TestEvosepPlateAlphaToNumeric:
    """Tests for Evosep plate mode: alphanumeric -> numeric position conversion."""

    def test_converts_alpha_positions_to_numeric(self, config) -> None:
        """A1->1, D8->44, H12->96."""
        pattern = config.queue_patterns.get_pattern("Proteomics", "standard")
        sampler = create_assembled_sampler("Evosep", "plate", config, "Proteomics", pattern, "Evosep_96")
        queue = _create_plate_queue_with_alpha_positions(["A1", "D8", "H12"])

        result = sampler.assign(queue)

        assert result.cells[0].grid_position == 1
        assert result.cells[1].grid_position == 44
        assert result.cells[2].grid_position == 96

    def test_already_numeric_positions_unchanged(self, config) -> None:
        """Positions that are already numeric should pass through unchanged."""
        pattern = config.queue_patterns.get_pattern("Proteomics", "standard")
        sampler = create_assembled_sampler("Evosep", "plate", config, "Proteomics", pattern, "Evosep_96")
        cells = [
            PlateCell(
                sample=VialSample(sample_name="Sample_0", sample_id=1000, tube_id="12345/0", container_id=12345),
                position=1,
                grid_position=44,
                plate_id=1,
            )
        ]
        plates = {1: Plate(plate_id=1, tray=None, nr_samples=1)}
        batches = {12345: ContainerBatch(container_id=12345)}
        queue = PlateQueue(batches=batches, plates=plates, cells=cells)

        result = sampler.assign(queue)

        assert result.cells[0].grid_position == 44

    def test_all_corner_positions(self, config) -> None:
        """Test all four corners of 96-well plate."""
        pattern = config.queue_patterns.get_pattern("Proteomics", "standard")
        sampler = create_assembled_sampler("Evosep", "plate", config, "Proteomics", pattern, "Evosep_96")
        queue = _create_plate_queue_with_alpha_positions(["A1", "A12", "H1", "H12"])

        result = sampler.assign(queue)

        assert result.cells[0].grid_position == 1  # 1 + (1-1) = 1
        assert result.cells[1].grid_position == 12  # 1 + (12-1) = 12
        assert result.cells[2].grid_position == 85  # 85 + (1-1) = 85
        assert result.cells[3].grid_position == 96  # 85 + (12-1) = 96
