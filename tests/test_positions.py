# =============================================================================
# Tests for positionV2.py
# =============================================================================
#
# - assign() tests use create_assembled_sampler
# - get_qc_position() tests use create_qc_position_provider

from dataclasses import dataclass
from pathlib import Path

import pytest

from qg.config_models.loader import qg_configuration
from qg.config_models.positions import PlateLayout
from qg.config_models.structure import QueuePattern, SamplesConfig
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
        sampler = create_assembled_sampler(
            "Vanquish",
            "vial",
            config,
            "Proteomics",
            pattern.get_all_sample_ids(),
            "Vanquish_54",
            "standard",
        )
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
        sampler = create_assembled_sampler(
            "Vanquish",
            "vial",
            config,
            "Proteomics",
            pattern.get_all_sample_ids(),
            "Vanquish_54",
            "standard",
        )
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
        sampler = create_assembled_sampler(
            "Vanquish",
            "vial",
            config,
            "Proteomics",
            pattern.get_all_sample_ids(),
            "Vanquish_54",
            "standard",
        )
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
        sampler = create_assembled_sampler(
            "MClass", "vial", config, "Proteomics", pattern.get_all_sample_ids(), "MClass_48", "standard"
        )
        queue = create_vial_queue(3)

        result = sampler.assign(queue)

        assert len(result.cells) == 3
        assert result.cells[0].grid_position == "A1"
        assert result.cells[1].grid_position == "A2"
        assert result.cells[2].grid_position == "A3"

    def test_row_major_order(self, config) -> None:
        pattern = config.queue_patterns.get_pattern("Proteomics", "standard")
        sampler = create_assembled_sampler(
            "MClass", "vial", config, "Proteomics", pattern.get_all_sample_ids(), "MClass_48", "standard"
        )
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
        sampler = create_assembled_sampler(
            "Evosep", "vial", config, "Proteomics", pattern.get_all_sample_ids(), "Plate_96", "standard"
        )
        queue = create_vial_queue(3)

        result = sampler.assign(queue)

        assert len(result.cells) == 3
        # Evosep now uses alpha positions (unified with grid samplers)
        assert result.cells[0].grid_position == "A1"
        assert result.cells[1].grid_position == "A2"
        assert result.cells[2].grid_position == "A3"

    def test_sequential_order(self, config) -> None:
        pattern = config.queue_patterns.get_pattern("Proteomics", "standard")
        sampler = create_assembled_sampler(
            "Evosep", "vial", config, "Proteomics", pattern.get_all_sample_ids(), "Plate_96", "standard"
        )
        # Test positions across a slot boundary (row-major: A1..A12, B1..B12, ...)
        queue = create_vial_queue(100)

        result = sampler.assign(queue)

        # First 12 samples: A1-A12 on tray 1
        assert result.cells[0].grid_position == "A1"
        assert result.cells[11].grid_position == "A12"
        # Position 13: B1
        assert result.cells[12].grid_position == "B1"
        # Position 96: H12 (last on tray 1)
        assert result.cells[95].grid_position == "H12"
        # Samples 97-100 on tray 2
        tray_97 = result.plates[result.cells[96].plate_id].tray
        assert tray_97 == 2
        assert result.cells[96].grid_position == "A1"

    def test_raises_when_full(self, config) -> None:
        pattern = config.queue_patterns.get_pattern("Proteomics", "standard")
        sampler = create_assembled_sampler(
            "Evosep", "vial", config, "Proteomics", pattern.get_all_sample_ids(), "Plate_96", "standard"
        )
        # 6 trays x 96 = 576, request more
        queue = create_vial_queue(600)

        with pytest.raises(ValueError):
            sampler.assign(queue)


class TestGridSamplerExhaustion:
    """Tests for grid sampler position exhaustion."""

    def test_vanquish_raises_when_full(self, config) -> None:
        """Vanquish_54 has 4 trays × 54 = 216 positions."""
        pattern = QueuePattern(description="test", run_QC_after_n_samples=1, start=[], middle=[], end=[])
        sampler = create_assembled_sampler(
            "Vanquish",
            "vial",
            config,
            "Proteomics",
            pattern.get_all_sample_ids(),
            "Vanquish_54",
            "standard",
        )
        queue = create_vial_queue(220)  # More than 216

        with pytest.raises(ValueError, match="Not enough positions"):
            sampler.assign(queue)

    def test_mclass_raises_when_full(self, config) -> None:
        """MClass_48 has 4 trays × 48 = 192 positions."""
        pattern = QueuePattern(description="test", run_QC_after_n_samples=1, start=[], middle=[], end=[])
        sampler = create_assembled_sampler(
            "MClass", "vial", config, "Proteomics", pattern.get_all_sample_ids(), "MClass_48", "standard"
        )
        queue = create_vial_queue(200)  # More than 192

        with pytest.raises(ValueError, match="Not enough positions"):
            sampler.assign(queue)


class TestAssignNoQC:
    """Tests with noqc pattern (no QC positions reserved)."""

    def test_noqc_uses_all_positions(self, config) -> None:
        pattern = QueuePattern(description="test", run_QC_after_n_samples=1, start=[], middle=[], end=[])
        sampler = create_assembled_sampler(
            "Vanquish",
            "vial",
            config,
            "Proteomics",
            pattern.get_all_sample_ids(),
            "Vanquish_54",
            "standard",
        )
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
        pattern = QueuePattern(description="test", run_QC_after_n_samples=1, start=[], middle=[], end=[])
        sampler = create_assembled_sampler(
            "Vanquish",
            "vial",
            config,
            "Proteomics",
            pattern.get_all_sample_ids(),
            "Vanquish_54",
            "standard",
        )
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
        pattern = QueuePattern(description="test", run_QC_after_n_samples=1, start=[], middle=[], end=[])
        sampler = create_assembled_sampler(
            "Vanquish",
            "vial",
            config,
            "Proteomics",
            pattern.get_all_sample_ids(),
            "Vanquish_54",
            "standard",
        )
        # Two containers with 3 samples each
        queue = create_multi_container_vial_queue([(100, 3), (200, 3)])

        result = sampler.assign(queue, one_container_per_tray=False)

        assert len(result.cells) == 6
        # All 6 samples should be on the same tray (Y) since they fit
        trays = {result.plates[c.plate_id].tray for c in result.cells}
        assert len(trays) == 1, "All samples should be on the same tray when one_container_per_tray=False"

    def test_one_container_per_tray_raises_when_not_enough_trays(self, config) -> None:
        """Should raise error if more containers than available trays."""
        pattern = QueuePattern(description="test", run_QC_after_n_samples=1, start=[], middle=[], end=[])
        sampler = create_assembled_sampler(
            "Vanquish",
            "vial",
            config,
            "Proteomics",
            pattern.get_all_sample_ids(),
            "Vanquish_54",
            "standard",
        )
        # Vanquish has 4 trays, try with 5 containers
        queue = create_multi_container_vial_queue([(100, 1), (200, 1), (300, 1), (400, 1), (500, 1)])

        with pytest.raises(ValueError):
            sampler.assign(queue, one_container_per_tray=True)

    def test_one_container_per_tray_raises_when_container_too_large(self, config) -> None:
        """Should raise error if a container has more samples than tray capacity."""
        pattern = QueuePattern(description="test", run_QC_after_n_samples=1, start=[], middle=[], end=[])
        sampler = create_assembled_sampler(
            "Vanquish",
            "vial",
            config,
            "Proteomics",
            pattern.get_all_sample_ids(),
            "Vanquish_54",
            "standard",
        )
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
        sampler = create_assembled_sampler(
            "Vanquish",
            "vial",
            config,
            "Proteomics",
            pattern.get_all_sample_ids(),
            "Vanquish_54",
            "standard",
        )
        plate_layout = config.plate_layouts.get_layout("Vanquish_54")
        provider = create_qc_position_provider(
            qc_layout=sampler.qc_layout,
            slot_entries=[],
            default_sample_id=SamplesConfig.DEFAULT_SAMPLE_ID,
            plate_layout=plate_layout,
        )

        pos = provider.get_position("QC01")

        assert pos.tray is not None
        assert pos.grid_position is not None

    def test_evosep_get_qc_position(self, config) -> None:
        # Evosep needs slot_entries to validate capacity
        pattern = config.queue_patterns.get_pattern("Testing", "test1_pattern")
        sampler = create_assembled_sampler(
            "Evosep", "vial", config, "Testing", pattern.get_all_sample_ids(), "Plate_96", "standard"
        )
        plate_layout = config.plate_layouts.get_layout("Plate_96")
        slot_entries = [MockSlotEntry("QC01")]
        provider = create_qc_position_provider(
            qc_layout=sampler.qc_layout,
            slot_entries=slot_entries,
            default_sample_id=SamplesConfig.DEFAULT_SAMPLE_ID,
            plate_layout=plate_layout,
        )

        pos = provider.get_position("QC01")

        assert pos.tray is not None
        assert pos.grid_position is not None

    def test_evosep_get_qc_position_increments(self, config) -> None:
        # Evosep needs slot_entries - we need 2 QC01 entries since we call get_position twice
        pattern = config.queue_patterns.get_pattern("Testing", "test1_pattern")
        sampler = create_assembled_sampler(
            "Evosep", "vial", config, "Testing", pattern.get_all_sample_ids(), "Plate_96", "standard"
        )
        plate_layout = config.plate_layouts.get_layout("Plate_96")
        slot_entries = [MockSlotEntry("QC01"), MockSlotEntry("QC01")]
        provider = create_qc_position_provider(
            qc_layout=sampler.qc_layout,
            slot_entries=slot_entries,
            default_sample_id=SamplesConfig.DEFAULT_SAMPLE_ID,
            plate_layout=plate_layout,
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
            grid_position=pos,
            plate_id=1,
        )
        for i, pos in enumerate(positions)
    ]
    plates = {1: Plate(plate_id=1, tray=None, nr_samples=len(cells))}
    batches = {12345: ContainerBatch(container_id=12345)}
    return PlateQueue(batches=batches, plates=plates, cells=cells)


class TestEvosepPlateAlphaPassthrough:
    """Tests for Evosep plate mode: alpha positions pass through and are validated.

    Row/column geometry is no longer stored on the cell; it is derived from
    ``grid_position`` during generation (see ``test_generator.py``).
    """

    def test_alpha_positions_pass_through(self, config) -> None:
        """Alpha positions stay as alpha (no conversion to numeric)."""
        pattern = config.queue_patterns.get_pattern("Proteomics", "standard")
        sampler = create_assembled_sampler(
            "Evosep", "plate", config, "Proteomics", pattern.get_all_sample_ids(), "Plate_96", "standard"
        )
        queue = _create_plate_queue_with_alpha_positions(["A1", "D8", "H12"])

        result = sampler.assign(queue)

        assert result.cells[0].grid_position == "A1"
        assert result.cells[1].grid_position == "D8"
        assert result.cells[2].grid_position == "H12"

    def test_all_corner_positions(self, config) -> None:
        """Test all four corners of 96-well plate stay as alpha."""
        pattern = config.queue_patterns.get_pattern("Proteomics", "standard")
        sampler = create_assembled_sampler(
            "Evosep", "plate", config, "Proteomics", pattern.get_all_sample_ids(), "Plate_96", "standard"
        )
        queue = _create_plate_queue_with_alpha_positions(["A1", "A12", "H1", "H12"])

        result = sampler.assign(queue)

        assert result.cells[0].grid_position == "A1"
        assert result.cells[1].grid_position == "A12"
        assert result.cells[2].grid_position == "H1"
        assert result.cells[3].grid_position == "H12"

    def test_out_of_layout_position_rejected(self, config) -> None:
        """A submitted tip position outside the plate layout fails validation."""
        sampler = create_assembled_sampler(
            "Evosep",
            "plate",
            config,
            "Proteomics",
            set(),
            "Plate_96",
            "standard",
        )
        queue = _create_plate_queue_with_alpha_positions(["A99"])

        with pytest.raises(ValueError, match=r"A99"):
            sampler.assign(queue)


@pytest.mark.parametrize(
    ("sampler_name", "layout"),
    [
        ("Vanquish", "Vanquish_54"),
        ("MClass", "MClass_48"),
        ("Evosep", "Plate_96"),
    ],
)
def test_plate_mode_rejects_unknown_plate(config, sampler_name: str, layout: str) -> None:
    """All plate samplers reject a cell whose plate foreign key is absent."""
    sampler = create_assembled_sampler(
        sampler_name,
        "plate",
        config,
        "Proteomics",
        set(),
        layout,
        "standard",
    )
    queue = _create_plate_queue_with_alpha_positions(["A1"])
    queue.cells[0].plate_id = 999

    with pytest.raises(ValueError, match=r"unknown plate 999"):
        sampler.assign(queue)


# =============================================================================
# Tests for Well-plate Plate mode - row/col splitting
# =============================================================================


class TestWellPlateModeValidation:
    """Tests for well-plate plate mode: grid_position preserved and validated.

    Row/column geometry is derived from ``grid_position`` during generation, so
    ``assign`` only preserves the coordinate and rejects wells outside the layout.
    """

    @pytest.mark.parametrize("sampler,layout", [("Vanquish", "Vanquish_54"), ("MClass", "MClass_48")])
    def test_out_of_layout_position_rejected(self, config, sampler: str, layout: str) -> None:
        """A submitted well outside the selected layout fails validation."""
        pattern = config.queue_patterns.get_pattern("Proteomics", "standard")
        assembled = create_assembled_sampler(
            sampler, "plate", config, "Proteomics", pattern.get_all_sample_ids(), layout, "standard"
        )
        # Column 99 does not exist in either well-plate layout.
        queue = _create_plate_queue_with_alpha_positions(["A99"])

        with pytest.raises(ValueError, match=r"A99"):
            assembled.assign(queue)

    @pytest.mark.parametrize("sampler,layout", [("Vanquish", "Vanquish_54"), ("MClass", "MClass_48")])
    def test_grid_position_preserved(self, config, sampler: str, layout: str) -> None:
        """Alpha grid_position is preserved after splitting."""
        pattern = config.queue_patterns.get_pattern("Proteomics", "standard")
        assembled = create_assembled_sampler(
            sampler, "plate", config, "Proteomics", pattern.get_all_sample_ids(), layout, "standard"
        )
        queue = _create_plate_queue_with_alpha_positions(["A1", "B3", "E6"])

        result = assembled.assign(queue)

        assert result.cells[0].grid_position == "A1"
        assert result.cells[1].grid_position == "B3"
        assert result.cells[2].grid_position == "E6"


# =============================================================================
# split_alpha: the single canonical, validated coordinate parser
# =============================================================================


class TestSplitAlphaValidation:
    """`split_alpha` is the one parser; `alpha_to_flat` derives from it."""

    _LAYOUT = PlateLayout(name="P96", rows=list("ABCDEFGH"), cols=list(range(1, 13)))

    def test_parses_and_normalizes(self) -> None:
        assert self._LAYOUT.split_alpha("A1") == ("A", 1)
        assert self._LAYOUT.split_alpha("D8") == ("D", 8)
        # Two-digit column and lower-case row both parse to the canonical form.
        assert self._LAYOUT.split_alpha("A12") == ("A", 12)
        assert self._LAYOUT.split_alpha("h12") == ("H", 12)

    @pytest.mark.parametrize("bad", ["", "A", "1", "AA", "A1B", "A0", " ", "Z1", "A13", "A99"])
    def test_rejects_invalid_or_out_of_layout(self, bad: str) -> None:
        """Malformed, unknown-row, zero-column, and out-of-range wells all raise."""
        with pytest.raises(ValueError, match=r"P96"):
            self._LAYOUT.split_alpha(bad)

    def test_alpha_to_flat_agrees_with_split_alpha(self) -> None:
        """Both parse identically, and flat numbering is unchanged for valid wells."""
        assert self._LAYOUT.alpha_to_flat("A1") == 1
        assert self._LAYOUT.alpha_to_flat("A12") == 12
        assert self._LAYOUT.alpha_to_flat("B1") == 13
        assert self._LAYOUT.alpha_to_flat("H12") == 96
        # alpha_to_flat delegates its parse, so it rejects the same bad input.
        with pytest.raises(ValueError, match=r"P96"):
            self._LAYOUT.alpha_to_flat("A99")


# =============================================================================
# Round-trip tests for PlateLayout position conversions
# =============================================================================


class TestPlateLayoutRoundTrip:
    """Round-trip: alpha -> flat -> (row, col) -> alpha for all plate layouts."""

    @pytest.mark.parametrize(
        "layout_name",
        ["Plate_96", "Vanquish_54", "MClass_48"],
    )
    def test_alpha_flat_round_trip_all_positions(self, config, layout_name: str) -> None:
        """Every position in the layout survives alpha -> flat -> row_col -> alpha."""
        layout = config.plate_layouts.get_layout(layout_name)

        for row in layout.rows:
            for col in layout.cols:
                alpha = f"{row}{col}"
                flat = layout.alpha_to_flat(alpha)
                row_back, col_back = layout.flat_to_row_col(flat)
                alpha_back = f"{row_back}{col_back}"

                assert alpha_back == alpha, f"{layout_name}: {alpha} -> flat {flat} -> {alpha_back}"

    @pytest.mark.parametrize(
        "layout_name",
        ["Plate_96", "Vanquish_54", "MClass_48"],
    )
    def test_flat_range_is_contiguous(self, config, layout_name: str) -> None:
        """Flat indices should be 1..capacity with no gaps."""
        layout = config.plate_layouts.get_layout(layout_name)

        flats = sorted(layout.alpha_to_flat(f"{r}{c}") for r in layout.rows for c in layout.cols)

        assert flats == list(range(1, layout.capacity + 1))


# =============================================================================
# Tests for start_position offset
# =============================================================================


class TestStartPosition:
    """Tests for start_position offset in Vial mode."""

    def test_default_a1_matches_no_offset(self, config) -> None:
        """start_position='A1' should produce identical results to omitting it."""
        pattern = QueuePattern(description="test", run_QC_after_n_samples=1, start=[], middle=[], end=[])
        sampler_default = create_assembled_sampler(
            "Vanquish", "vial", config, "Proteomics", pattern.get_all_sample_ids(), "Vanquish_54", "standard"
        )
        sampler_a1 = create_assembled_sampler(
            "Vanquish",
            "vial",
            config,
            "Proteomics",
            pattern.get_all_sample_ids(),
            "Vanquish_54",
            "standard",
            start_position="A1",
        )
        queue = create_vial_queue(5)

        result_default = sampler_default.assign(queue)
        result_a1 = sampler_a1.assign(queue)

        for c1, c2 in zip(result_default.cells, result_a1.cells, strict=True):
            assert c1.grid_position == c2.grid_position

    def test_start_position_skips_earlier_positions(self, config) -> None:
        """Starting at B1 should skip all A-row positions on first tray."""
        pattern = QueuePattern(description="test", run_QC_after_n_samples=1, start=[], middle=[], end=[])
        sampler = create_assembled_sampler(
            "Vanquish",
            "vial",
            config,
            "Proteomics",
            pattern.get_all_sample_ids(),
            "Vanquish_54",
            "standard",
            start_position="B1",
        )
        queue = create_vial_queue(3)

        result = sampler.assign(queue)

        # First position should be B1 (all A-row positions skipped)
        assert result.cells[0].grid_position == "B1"
        assert result.cells[1].grid_position == "B2"
        assert result.cells[2].grid_position == "B3"

    def test_start_position_mid_row(self, config) -> None:
        """Starting at A5 should skip A1-A4."""
        pattern = QueuePattern(description="test", run_QC_after_n_samples=1, start=[], middle=[], end=[])
        sampler = create_assembled_sampler(
            "Vanquish",
            "vial",
            config,
            "Proteomics",
            pattern.get_all_sample_ids(),
            "Vanquish_54",
            "standard",
            start_position="A5",
        )
        queue = create_vial_queue(3)

        result = sampler.assign(queue)

        assert result.cells[0].grid_position == "A5"
        assert result.cells[1].grid_position == "A6"
        assert result.cells[2].grid_position == "A7"

    def test_start_position_reduces_capacity(self, config) -> None:
        """Starting late should raise when not enough positions remain."""
        pattern = QueuePattern(description="test", run_QC_after_n_samples=1, start=[], middle=[], end=[])
        sampler = create_assembled_sampler(
            "Vanquish",
            "vial",
            config,
            "Proteomics",
            pattern.get_all_sample_ids(),
            "Vanquish_54",
            "standard",
            start_position="F9",
        )
        # F9 is the last position on first tray; only 1 position on tray Y + full trays R,G,B
        # Total: 1 + 54 + 54 + 54 = 163 available positions
        queue = create_vial_queue(200)

        with pytest.raises(ValueError, match="Not enough positions"):
            sampler.assign(queue)

    def test_start_position_only_affects_first_tray(self, config) -> None:
        """Second tray should start from A1 regardless of start_position."""
        pattern = QueuePattern(description="test", run_QC_after_n_samples=1, start=[], middle=[], end=[])
        sampler = create_assembled_sampler(
            "Vanquish",
            "vial",
            config,
            "Proteomics",
            pattern.get_all_sample_ids(),
            "Vanquish_54",
            "standard",
            start_position="F1",
        )
        # F1-F9 = 9 positions on first tray, then second tray starts at A1
        queue = create_vial_queue(12)

        result = sampler.assign(queue)

        # First 9 cells on tray Y: F1-F9
        assert result.cells[0].grid_position == "F1"
        assert result.cells[8].grid_position == "F9"
        # Cell 10 should be on second tray starting at A1
        tray_10 = result.plates[result.cells[9].plate_id].tray
        assert tray_10 == "R"
        assert result.cells[9].grid_position == "A1"

    def test_start_position_evosep(self, config) -> None:
        """Start position should work for Evosep (tip-plate) too."""
        pattern = QueuePattern(description="test", run_QC_after_n_samples=1, start=[], middle=[], end=[])
        sampler = create_assembled_sampler(
            "Evosep",
            "vial",
            config,
            "Proteomics",
            pattern.get_all_sample_ids(),
            "Plate_96",
            "standard",
            start_position="B1",
        )
        queue = create_vial_queue(3)

        result = sampler.assign(queue)

        # Should skip A1-A12 (12 positions in row A for Plate_96)
        assert result.cells[0].grid_position == "B1"
        assert result.cells[1].grid_position == "B2"
        assert result.cells[2].grid_position == "B3"


# =============================================================================
# Tests for start_tray selection
# =============================================================================


class TestStartTray:
    """Tests for start_tray selection in Vial mode."""

    def test_default_first_tray(self, config) -> None:
        """Omitting start_tray should behave same as explicitly passing first tray."""
        pattern = QueuePattern(description="test", run_QC_after_n_samples=1, start=[], middle=[], end=[])
        sampler_default = create_assembled_sampler(
            "Vanquish", "vial", config, "Proteomics", pattern.get_all_sample_ids(), "Vanquish_54", "standard"
        )
        sampler_explicit = create_assembled_sampler(
            "Vanquish",
            "vial",
            config,
            "Proteomics",
            pattern.get_all_sample_ids(),
            "Vanquish_54",
            "standard",
            start_tray="Y",
        )
        queue = create_vial_queue(5)
        result_default = sampler_default.assign(queue)
        result_explicit = sampler_explicit.assign(queue)

        for c1, c2 in zip(result_default.cells, result_explicit.cells, strict=True):
            assert c1.grid_position == c2.grid_position

    def test_start_tray_skips_earlier_trays(self, config) -> None:
        """Starting at tray R should skip tray Y entirely."""
        pattern = QueuePattern(description="test", run_QC_after_n_samples=1, start=[], middle=[], end=[])
        sampler = create_assembled_sampler(
            "Vanquish",
            "vial",
            config,
            "Proteomics",
            pattern.get_all_sample_ids(),
            "Vanquish_54",
            "standard",
            start_tray="R",
        )
        queue = create_vial_queue(5)
        result = sampler.assign(queue)

        trays = {result.plates[c.plate_id].tray for c in result.cells}
        assert "Y" not in trays
        # First sample should be on tray R at A1
        first_tray = result.plates[result.cells[0].plate_id].tray
        assert first_tray == "R"
        assert result.cells[0].grid_position == "A1"

    def test_start_tray_and_start_position_combined(self, config) -> None:
        """Starting at tray R, position B3 should place first sample at R:B3."""
        pattern = QueuePattern(description="test", run_QC_after_n_samples=1, start=[], middle=[], end=[])
        sampler = create_assembled_sampler(
            "Vanquish",
            "vial",
            config,
            "Proteomics",
            pattern.get_all_sample_ids(),
            "Vanquish_54",
            "standard",
            start_position="B3",
            start_tray="R",
        )
        queue = create_vial_queue(3)
        result = sampler.assign(queue)

        first_tray = result.plates[result.cells[0].plate_id].tray
        assert first_tray == "R"
        assert result.cells[0].grid_position == "B3"
        assert result.cells[1].grid_position == "B4"

    def test_start_tray_reduces_capacity(self, config) -> None:
        """Starting on later tray reduces total available positions."""
        pattern = QueuePattern(description="test", run_QC_after_n_samples=1, start=[], middle=[], end=[])
        # Vanquish has 4 trays × 54 positions. Starting on tray G (3rd) leaves 2 trays = 108 positions.
        sampler = create_assembled_sampler(
            "Vanquish",
            "vial",
            config,
            "Proteomics",
            pattern.get_all_sample_ids(),
            "Vanquish_54",
            "standard",
            start_tray="G",
        )
        # 109 samples should fail (only 108 available on trays G + B)
        queue = create_vial_queue(109)
        with pytest.raises(ValueError, match="Not enough positions"):
            sampler.assign(queue)

    def test_start_tray_evosep(self, config) -> None:
        """Start tray should work for Evosep (tip-plate) too."""
        pattern = QueuePattern(description="test", run_QC_after_n_samples=1, start=[], middle=[], end=[])
        sampler = create_assembled_sampler(
            "Evosep",
            "vial",
            config,
            "Proteomics",
            pattern.get_all_sample_ids(),
            "Plate_96",
            "standard",
            start_tray=3,
        )
        queue = create_vial_queue(3)
        result = sampler.assign(queue)

        first_tray = result.plates[result.cells[0].plate_id].tray
        assert first_tray == 3
        assert result.cells[0].grid_position == "A1"
