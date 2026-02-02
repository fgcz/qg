# =============================================================================
# Tests for positions_new.py
# =============================================================================

import pytest

from qg.config_models_new.positions import PlateLayout, QCSampleEvosep, QCSampleGrid, Sampler
from qg.positions_new import (
    POSITION_FUNCTIONS,
    PositionGenerator,
    QCLayoutEvosep,
    QCLayoutGrid,
    assign_user_positions,
    available_positions,
    check_collisions,
    check_out_of_bounds,
    format_position,
    generate_all_positions,
    get_position_function,
    int_add,
    string_concat,
    validate_in_bounds,
    validate_no_collisions,
)

# =============================================================================
# Test Position Functions
# =============================================================================


class TestPositionFunctions:
    """Tests for position formatting functions."""

    def test_string_concat_letter_row(self) -> None:
        """String concat with letter row."""
        assert string_concat("A", 1) == "A1"
        assert string_concat("B", 12) == "B12"

    def test_string_concat_int_row(self) -> None:
        """String concat works with int row too."""
        assert string_concat(1, 2) == "12"

    def test_int_add(self) -> None:
        """Int add for Evosep."""
        assert int_add(1, 0) == 1
        assert int_add(1, 5) == 6
        assert int_add(10, 3) == 13

    def test_registry_contains_both_functions(self) -> None:
        """Registry has both functions."""
        assert "string_concat" in POSITION_FUNCTIONS
        assert "int_add" in POSITION_FUNCTIONS

    def test_get_position_function_valid(self) -> None:
        """Get valid position functions."""
        assert get_position_function("string_concat") is string_concat
        assert get_position_function("int_add") is int_add

    def test_get_position_function_invalid(self) -> None:
        """Get invalid position function raises KeyError."""
        with pytest.raises(KeyError, match="Unknown position function"):
            get_position_function("invalid")


# =============================================================================
# Test generate_all_positions
# =============================================================================


class TestGenerateAllPositions:
    """Tests for generate_all_positions()."""

    def test_single_tray_grid(self) -> None:
        """Single tray with grid layout."""
        positions = generate_all_positions(
            trays=["Y"],
            rows=["A", "B"],
            cols=[1, 2],
            position_fun=string_concat,
        )
        assert positions == [
            ("Y", "A1"),
            ("Y", "A2"),
            ("Y", "B1"),
            ("Y", "B2"),
        ]

    def test_multiple_trays(self) -> None:
        """Multiple trays enumerate correctly."""
        positions = generate_all_positions(
            trays=["Y", "R"],
            rows=["A"],
            cols=[1, 2],
            position_fun=string_concat,
        )
        assert positions == [
            ("Y", "A1"),
            ("Y", "A2"),
            ("R", "A1"),
            ("R", "A2"),
        ]

    def test_evosep_int_positions(self) -> None:
        """Evosep with int-based positions."""
        positions = generate_all_positions(
            trays=[1, 2],
            rows=[1],  # Single row, col is offset
            cols=[0, 1, 2],
            position_fun=int_add,
        )
        assert positions == [
            (1, 1),
            (1, 2),
            (1, 3),
            (2, 1),
            (2, 2),
            (2, 3),
        ]

    def test_empty_inputs(self) -> None:
        """Empty inputs return empty list."""
        assert generate_all_positions([], ["A"], [1], string_concat) == []
        assert generate_all_positions(["Y"], [], [1], string_concat) == []
        assert generate_all_positions(["Y"], ["A"], [], string_concat) == []


# =============================================================================
# Test format_position
# =============================================================================


class TestFormatPosition:
    """Tests for format_position()."""

    def test_string_tray_string_position(self) -> None:
        """String tray and position."""
        assert format_position("Y", "A1") == "Y:A1"

    def test_int_tray_int_position(self) -> None:
        """Int tray and position (Evosep)."""
        assert format_position(1, 42) == "1:42"

    def test_mixed_types(self) -> None:
        """Mixed types work."""
        assert format_position("R", 5) == "R:5"


# =============================================================================
# Test QCLayoutGrid
# =============================================================================


class TestQCLayoutGrid:
    """Tests for QCLayoutGrid (stateless fixed positions)."""

    @pytest.fixture
    def sample_qc_samples(self) -> list[QCSampleGrid]:
        """Create sample QC samples."""
        return [
            QCSampleGrid(
                tech_area="proteomics",
                qc_layout_name="standard",
                plate_layout="Vanquish_54",
                sample_id="QC01",
                plate="Y",
                row="A",
                col=1,
            ),
            QCSampleGrid(
                tech_area="proteomics",
                qc_layout_name="standard",
                plate_layout="Vanquish_54",
                sample_id="QC02",
                plate="Y",
                row="B",
                col=2,
            ),
        ]

    def test_get_position(self, sample_qc_samples: list[QCSampleGrid]) -> None:
        """Get position returns correct fixed position."""
        layout = QCLayoutGrid(sample_qc_samples, string_concat)
        assert layout.get_position("QC01") == ("Y", "A1")
        assert layout.get_position("QC02") == ("Y", "B2")

    def test_get_position_unknown_id(self, sample_qc_samples: list[QCSampleGrid]) -> None:
        """Get position for unknown ID raises KeyError."""
        layout = QCLayoutGrid(sample_qc_samples, string_concat)
        with pytest.raises(KeyError, match="Unknown QC sample"):
            layout.get_position("UNKNOWN")

    def test_reserved_positions(self, sample_qc_samples: list[QCSampleGrid]) -> None:
        """Reserved positions returns all QC positions."""
        layout = QCLayoutGrid(sample_qc_samples, string_concat)
        reserved = layout.reserved_positions()
        assert reserved == {("Y", "A1"), ("Y", "B2")}

    def test_sample_ids(self, sample_qc_samples: list[QCSampleGrid]) -> None:
        """Sample IDs property."""
        layout = QCLayoutGrid(sample_qc_samples, string_concat)
        assert set(layout.sample_ids) == {"QC01", "QC02"}

    def test_repeated_get_position_returns_same(self, sample_qc_samples: list[QCSampleGrid]) -> None:
        """Stateless: repeated calls return same position."""
        layout = QCLayoutGrid(sample_qc_samples, string_concat)
        assert layout.get_position("QC01") == ("Y", "A1")
        assert layout.get_position("QC01") == ("Y", "A1")
        assert layout.get_position("QC01") == ("Y", "A1")


# =============================================================================
# Test QCLayoutEvosep
# =============================================================================


class TestQCLayoutEvosep:
    """Tests for QCLayoutEvosep (stateful consumable tips)."""

    @pytest.fixture
    def sample_qc_samples(self) -> list[QCSampleEvosep]:
        """Create sample QC samples with ranges."""
        return [
            QCSampleEvosep(
                tech_area="proteomics",
                qc_layout_name="standard",
                plate_layout="Evosep_96",
                sample_id="QC01",
                tray=1,
                position_start=1,
                position_end=5,
            ),
            QCSampleEvosep(
                tech_area="proteomics",
                qc_layout_name="standard",
                plate_layout="Evosep_96",
                sample_id="QC02",
                tray=1,
                position_start=6,
                position_end=10,
            ),
        ]

    def test_get_position_increments(self, sample_qc_samples: list[QCSampleEvosep]) -> None:
        """Get position increments counter each call."""
        layout = QCLayoutEvosep(sample_qc_samples)
        assert layout.get_position("QC01") == (1, 1)
        assert layout.get_position("QC01") == (1, 2)
        assert layout.get_position("QC01") == (1, 3)

    def test_get_position_independent_counters(self, sample_qc_samples: list[QCSampleEvosep]) -> None:
        """Each sample has independent counter."""
        layout = QCLayoutEvosep(sample_qc_samples)
        assert layout.get_position("QC01") == (1, 1)
        assert layout.get_position("QC02") == (1, 6)
        assert layout.get_position("QC01") == (1, 2)
        assert layout.get_position("QC02") == (1, 7)

    def test_get_position_exhausted(self, sample_qc_samples: list[QCSampleEvosep]) -> None:
        """Get position raises when range exhausted."""
        layout = QCLayoutEvosep(sample_qc_samples)
        for _ in range(5):  # Use all 5 positions
            layout.get_position("QC01")
        with pytest.raises(ValueError, match="Position range exhausted"):
            layout.get_position("QC01")

    def test_get_position_unknown_id(self, sample_qc_samples: list[QCSampleEvosep]) -> None:
        """Get position for unknown ID raises KeyError."""
        layout = QCLayoutEvosep(sample_qc_samples)
        with pytest.raises(KeyError, match="Unknown QC sample"):
            layout.get_position("UNKNOWN")

    def test_reserved_positions(self, sample_qc_samples: list[QCSampleEvosep]) -> None:
        """Reserved positions covers entire ranges."""
        layout = QCLayoutEvosep(sample_qc_samples)
        reserved = layout.reserved_positions()
        # QC01: 1-5, QC02: 6-10
        expected = {(1, p) for p in range(1, 11)}
        assert reserved == expected

    def test_sample_ids(self, sample_qc_samples: list[QCSampleEvosep]) -> None:
        """Sample IDs property."""
        layout = QCLayoutEvosep(sample_qc_samples)
        assert set(layout.sample_ids) == {"QC01", "QC02"}

    def test_remaining(self, sample_qc_samples: list[QCSampleEvosep]) -> None:
        """Remaining positions count."""
        layout = QCLayoutEvosep(sample_qc_samples)
        assert layout.remaining("QC01") == 5
        layout.get_position("QC01")
        assert layout.remaining("QC01") == 4
        layout.get_position("QC01")
        assert layout.remaining("QC01") == 3

    def test_reset(self, sample_qc_samples: list[QCSampleEvosep]) -> None:
        """Reset restores counters."""
        layout = QCLayoutEvosep(sample_qc_samples)
        layout.get_position("QC01")
        layout.get_position("QC01")
        assert layout.remaining("QC01") == 3
        layout.reset()
        assert layout.remaining("QC01") == 5
        assert layout.get_position("QC01") == (1, 1)


# =============================================================================
# Test Validation Functions (Set Operations)
# =============================================================================


class TestCheckCollisions:
    """Tests for check_collisions() using set intersection."""

    def test_no_collisions(self) -> None:
        """No collisions returns empty set."""
        user = {("Y", "A1"), ("Y", "A2")}
        reserved = {("Y", "B1"), ("Y", "B2")}
        assert check_collisions(user, reserved) == set()

    def test_with_collisions(self) -> None:
        """Collisions returned as set."""
        user = {("Y", "A1"), ("Y", "A2"), ("Y", "B1")}
        reserved = {("Y", "B1"), ("Y", "B2")}
        assert check_collisions(user, reserved) == {("Y", "B1")}

    def test_all_collide(self) -> None:
        """All positions collide."""
        user = {("Y", "A1"), ("Y", "A2")}
        reserved = {("Y", "A1"), ("Y", "A2"), ("Y", "B1")}
        assert check_collisions(user, reserved) == {("Y", "A1"), ("Y", "A2")}

    def test_empty_user(self) -> None:
        """Empty user positions."""
        assert check_collisions(set(), {("Y", "A1")}) == set()

    def test_empty_reserved(self) -> None:
        """Empty reserved positions."""
        assert check_collisions({("Y", "A1")}, set()) == set()


class TestCheckOutOfBounds:
    """Tests for check_out_of_bounds() using set difference."""

    def test_all_in_bounds(self) -> None:
        """All positions in bounds returns empty set."""
        positions = {("Y", "A1"), ("Y", "A2")}
        valid = {("Y", "A1"), ("Y", "A2"), ("Y", "B1"), ("Y", "B2")}
        assert check_out_of_bounds(positions, valid) == set()

    def test_some_out_of_bounds(self) -> None:
        """Some positions out of bounds."""
        positions = {("Y", "A1"), ("Y", "C1"), ("Z", "A1")}
        valid = {("Y", "A1"), ("Y", "A2"), ("Y", "B1"), ("Y", "B2")}
        assert check_out_of_bounds(positions, valid) == {("Y", "C1"), ("Z", "A1")}

    def test_all_out_of_bounds(self) -> None:
        """All positions out of bounds."""
        positions = {("Z", "A1"), ("Z", "A2")}
        valid = {("Y", "A1"), ("Y", "A2")}
        assert check_out_of_bounds(positions, valid) == positions

    def test_empty_positions(self) -> None:
        """Empty positions to check."""
        assert check_out_of_bounds(set(), {("Y", "A1")}) == set()


class TestAvailablePositions:
    """Tests for available_positions() using set difference."""

    def test_basic(self) -> None:
        """Basic available positions calculation."""
        all_pos = {("Y", "A1"), ("Y", "A2"), ("Y", "B1"), ("Y", "B2")}
        reserved = {("Y", "A1"), ("Y", "B2")}
        assert available_positions(all_pos, reserved) == {("Y", "A2"), ("Y", "B1")}

    def test_no_reserved(self) -> None:
        """No reserved positions."""
        all_pos = {("Y", "A1"), ("Y", "A2")}
        assert available_positions(all_pos, set()) == all_pos

    def test_all_reserved(self) -> None:
        """All positions reserved."""
        all_pos = {("Y", "A1"), ("Y", "A2")}
        assert available_positions(all_pos, all_pos) == set()


class TestValidateNoCollisions:
    """Tests for validate_no_collisions() - raises on collision."""

    def test_no_collisions_passes(self) -> None:
        """No collisions passes validation."""
        user = {("Y", "A1"), ("Y", "A2")}
        reserved = {("Y", "B1"), ("Y", "B2")}
        validate_no_collisions(user, reserved)  # Should not raise

    def test_collision_raises(self) -> None:
        """Collision raises ValueError."""
        user = {("Y", "A1"), ("Y", "B1")}
        reserved = {("Y", "B1"), ("Y", "B2")}
        with pytest.raises(ValueError, match="Position collisions"):
            validate_no_collisions(user, reserved)


class TestValidateInBounds:
    """Tests for validate_in_bounds() - raises on out of bounds."""

    def test_in_bounds_passes(self) -> None:
        """In bounds passes validation."""
        positions = {("Y", "A1"), ("Y", "A2")}
        valid = {("Y", "A1"), ("Y", "A2"), ("Y", "B1")}
        validate_in_bounds(positions, valid)  # Should not raise

    def test_out_of_bounds_raises(self) -> None:
        """Out of bounds raises ValueError."""
        positions = {("Y", "A1"), ("Y", "C1")}
        valid = {("Y", "A1"), ("Y", "A2"), ("Y", "B1")}
        with pytest.raises(ValueError, match="Positions out of bounds"):
            validate_in_bounds(positions, valid)


# =============================================================================
# Test assign_user_positions
# =============================================================================


class TestAssignUserPositions:
    """Tests for assign_user_positions()."""

    def test_no_reserved(self) -> None:
        """Assign with no reserved positions."""
        all_positions = [("Y", "A1"), ("Y", "A2"), ("Y", "B1"), ("Y", "B2")]
        result = assign_user_positions(2, all_positions, set())
        assert result == [("Y", "A1"), ("Y", "A2")]

    def test_skips_reserved(self) -> None:
        """Assign skips reserved positions."""
        all_positions = [("Y", "A1"), ("Y", "A2"), ("Y", "B1"), ("Y", "B2")]
        reserved = {("Y", "A1"), ("Y", "B1")}
        result = assign_user_positions(2, all_positions, reserved)
        assert result == [("Y", "A2"), ("Y", "B2")]

    def test_not_enough_positions(self) -> None:
        """Raises when not enough positions available."""
        all_positions = [("Y", "A1"), ("Y", "A2")]
        reserved = {("Y", "A1")}
        with pytest.raises(ValueError, match="Not enough positions"):
            assign_user_positions(3, all_positions, reserved)

    def test_exact_count(self) -> None:
        """Exact number of positions requested."""
        all_positions = [("Y", "A1"), ("Y", "A2"), ("Y", "B1")]
        reserved = {("Y", "A2")}
        result = assign_user_positions(2, all_positions, reserved)
        assert result == [("Y", "A1"), ("Y", "B1")]

    def test_zero_samples(self) -> None:
        """Zero samples returns empty list."""
        all_positions = [("Y", "A1"), ("Y", "A2")]
        result = assign_user_positions(0, all_positions, set())
        assert result == []


# =============================================================================
# Test PositionGenerator
# =============================================================================


class TestPositionGenerator:
    """Tests for PositionGenerator (orchestrator)."""

    @pytest.fixture
    def grid_sampler(self) -> Sampler:
        """Create a grid sampler (Vanquish-like)."""
        return Sampler(
            name="Vanquish",
            description="Vanquish autosampler",
            trays=["Y", "R"],
            position_fun="string_concat",
        )

    @pytest.fixture
    def grid_layout(self) -> PlateLayout:
        """Create a grid plate layout."""
        return PlateLayout(
            name="Vanquish_6",
            rows=["A", "B", "C"],
            cols=[1, 2],
        )

    @pytest.fixture
    def grid_qc_samples(self) -> list[QCSampleGrid]:
        """Create QC samples for grid."""
        return [
            QCSampleGrid(
                tech_area="proteomics",
                qc_layout_name="standard",
                plate_layout="Vanquish_6",
                sample_id="QC01",
                plate="Y",
                row="A",
                col=1,
            ),
        ]

    @pytest.fixture
    def evosep_sampler(self) -> Sampler:
        """Create an Evosep sampler."""
        return Sampler(
            name="Evosep",
            description="Evosep autosampler",
            trays=[1, 2],
            position_fun="int_add",
        )

    @pytest.fixture
    def evosep_layout(self) -> PlateLayout:
        """Create an Evosep plate layout."""
        return PlateLayout(
            name="Evosep_8",
            rows=[1],  # Single row for Evosep
            cols=[0, 1, 2, 3],  # Offsets
        )

    @pytest.fixture
    def evosep_qc_samples(self) -> list[QCSampleEvosep]:
        """Create QC samples for Evosep."""
        return [
            QCSampleEvosep(
                tech_area="proteomics",
                qc_layout_name="standard",
                plate_layout="Evosep_8",
                sample_id="QC01",
                tray=1,
                position_start=1,
                position_end=2,
            ),
        ]

    def test_grid_assign_positions(
        self,
        grid_sampler: Sampler,
        grid_layout: PlateLayout,
        grid_qc_samples: list[QCSampleGrid],
    ) -> None:
        """Grid sampler assigns positions correctly."""
        qc_layout = QCLayoutGrid(grid_qc_samples, string_concat)
        gen = PositionGenerator(grid_sampler, grid_layout, qc_layout)

        positions = gen.assign_positions(3)
        # QC01 at Y:A1 is reserved, so we get A2, B1, B2
        assert positions == [("Y", "A2"), ("Y", "B1"), ("Y", "B2")]

    def test_grid_get_qc_position(
        self,
        grid_sampler: Sampler,
        grid_layout: PlateLayout,
        grid_qc_samples: list[QCSampleGrid],
    ) -> None:
        """Grid sampler gets QC position."""
        qc_layout = QCLayoutGrid(grid_qc_samples, string_concat)
        gen = PositionGenerator(grid_sampler, grid_layout, qc_layout)

        assert gen.get_qc_position("QC01") == ("Y", "A1")

    def test_grid_capacity(
        self,
        grid_sampler: Sampler,
        grid_layout: PlateLayout,
        grid_qc_samples: list[QCSampleGrid],
    ) -> None:
        """Grid sampler capacity calculation."""
        qc_layout = QCLayoutGrid(grid_qc_samples, string_concat)
        gen = PositionGenerator(grid_sampler, grid_layout, qc_layout)

        # 2 trays * 3 rows * 2 cols = 12 total, 1 reserved = 11 capacity
        assert gen.total_positions == 12
        assert gen.qc_reserved_count == 1
        assert gen.capacity == 11

    def test_evosep_assign_positions(
        self,
        evosep_sampler: Sampler,
        evosep_layout: PlateLayout,
        evosep_qc_samples: list[QCSampleEvosep],
    ) -> None:
        """Evosep sampler assigns positions correctly."""
        qc_layout = QCLayoutEvosep(evosep_qc_samples)
        gen = PositionGenerator(evosep_sampler, evosep_layout, qc_layout)

        positions = gen.assign_positions(3)
        # QC01 at tray 1, positions 1-2 are reserved
        # Available: (1, 3), (1, 4), (2, 1), (2, 2), (2, 3), (2, 4)
        assert positions == [(1, 3), (1, 4), (2, 1)]

    def test_evosep_get_qc_position_increments(
        self,
        evosep_sampler: Sampler,
        evosep_layout: PlateLayout,
        evosep_qc_samples: list[QCSampleEvosep],
    ) -> None:
        """Evosep QC position increments."""
        qc_layout = QCLayoutEvosep(evosep_qc_samples)
        gen = PositionGenerator(evosep_sampler, evosep_layout, qc_layout)

        assert gen.get_qc_position("QC01") == (1, 1)
        assert gen.get_qc_position("QC01") == (1, 2)

    def test_format_position_method(
        self,
        grid_sampler: Sampler,
        grid_layout: PlateLayout,
        grid_qc_samples: list[QCSampleGrid],
    ) -> None:
        """Format position method."""
        qc_layout = QCLayoutGrid(grid_qc_samples, string_concat)
        gen = PositionGenerator(grid_sampler, grid_layout, qc_layout)

        assert gen.format_position("Y", "A1") == "Y:A1"
        assert gen.format_position(1, 42) == "1:42"

    def test_all_positions_property(
        self,
        grid_sampler: Sampler,
        grid_layout: PlateLayout,
        grid_qc_samples: list[QCSampleGrid],
    ) -> None:
        """All positions property returns copy."""
        qc_layout = QCLayoutGrid(grid_qc_samples, string_concat)
        gen = PositionGenerator(grid_sampler, grid_layout, qc_layout)

        pos1 = gen.all_positions
        pos2 = gen.all_positions
        assert pos1 == pos2
        assert pos1 is not pos2  # Should be a copy

    def test_available_positions(
        self,
        grid_sampler: Sampler,
        grid_layout: PlateLayout,
        grid_qc_samples: list[QCSampleGrid],
    ) -> None:
        """Available positions (set difference)."""
        qc_layout = QCLayoutGrid(grid_qc_samples, string_concat)
        gen = PositionGenerator(grid_sampler, grid_layout, qc_layout)

        avail = gen.available_positions()
        assert isinstance(avail, set)
        assert len(avail) == 11  # 12 total - 1 reserved
        assert ("Y", "A1") not in avail  # Reserved

    def test_validate_user_positions_valid(
        self,
        grid_sampler: Sampler,
        grid_layout: PlateLayout,
        grid_qc_samples: list[QCSampleGrid],
    ) -> None:
        """Valid user positions pass validation."""
        qc_layout = QCLayoutGrid(grid_qc_samples, string_concat)
        gen = PositionGenerator(grid_sampler, grid_layout, qc_layout)

        user_positions = {("Y", "A2"), ("Y", "B1")}
        gen.validate_user_positions(user_positions)  # Should not raise

    def test_validate_user_positions_collision(
        self,
        grid_sampler: Sampler,
        grid_layout: PlateLayout,
        grid_qc_samples: list[QCSampleGrid],
    ) -> None:
        """User positions colliding with QC raises."""
        qc_layout = QCLayoutGrid(grid_qc_samples, string_concat)
        gen = PositionGenerator(grid_sampler, grid_layout, qc_layout)

        user_positions = {("Y", "A1"), ("Y", "A2")}  # A1 is reserved
        with pytest.raises(ValueError, match="Position collisions"):
            gen.validate_user_positions(user_positions)

    def test_validate_user_positions_out_of_bounds(
        self,
        grid_sampler: Sampler,
        grid_layout: PlateLayout,
        grid_qc_samples: list[QCSampleGrid],
    ) -> None:
        """User positions out of bounds raises."""
        qc_layout = QCLayoutGrid(grid_qc_samples, string_concat)
        gen = PositionGenerator(grid_sampler, grid_layout, qc_layout)

        user_positions = {("Y", "A2"), ("Z", "X9")}  # Z:X9 doesn't exist
        with pytest.raises(ValueError, match="Positions out of bounds"):
            gen.validate_user_positions(user_positions)


# =============================================================================
# Integration Test: Full Workflow
# =============================================================================


class TestIntegration:
    """Integration tests for complete workflows."""

    def test_grid_workflow(self) -> None:
        """Complete grid sampler workflow."""
        # Setup
        sampler = Sampler(
            name="Vanquish",
            trays=["Y"],
            position_fun="string_concat",
        )
        layout = PlateLayout(name="Vanquish_6", rows=["A", "B"], cols=[1, 2, 3])
        qc_samples = [
            QCSampleGrid(
                tech_area="proteomics",
                qc_layout_name="standard",
                plate_layout="Vanquish_6",
                sample_id="QC01",
                plate="Y",
                row="A",
                col=1,
            ),
            QCSampleGrid(
                tech_area="proteomics",
                qc_layout_name="standard",
                plate_layout="Vanquish_6",
                sample_id="QC02",
                plate="Y",
                row="B",
                col=3,
            ),
        ]
        qc_layout = QCLayoutGrid(qc_samples, string_concat)
        gen = PositionGenerator(sampler, layout, qc_layout)

        # Verify capacity
        assert gen.total_positions == 6
        assert gen.qc_reserved_count == 2
        assert gen.capacity == 4

        # Assign user samples
        user_positions = gen.assign_positions(4)
        assert len(user_positions) == 4
        assert ("Y", "A1") not in user_positions  # Reserved for QC01
        assert ("Y", "B3") not in user_positions  # Reserved for QC02

        # Get QC positions
        assert gen.get_qc_position("QC01") == ("Y", "A1")
        assert gen.get_qc_position("QC02") == ("Y", "B3")

    def test_evosep_workflow(self) -> None:
        """Complete Evosep sampler workflow."""
        # Setup
        sampler = Sampler(
            name="Evosep",
            trays=[1],
            position_fun="int_add",
        )
        layout = PlateLayout(name="Evosep_10", rows=[1], cols=list(range(10)))
        qc_samples = [
            QCSampleEvosep(
                tech_area="proteomics",
                qc_layout_name="standard",
                plate_layout="Evosep_10",
                sample_id="QC01",
                tray=1,
                position_start=1,
                position_end=3,
            ),
        ]
        qc_layout = QCLayoutEvosep(qc_samples)
        gen = PositionGenerator(sampler, layout, qc_layout)

        # Verify capacity
        assert gen.total_positions == 10
        assert gen.qc_reserved_count == 3  # Positions 1, 2, 3
        assert gen.capacity == 7

        # Assign user samples
        user_positions = gen.assign_positions(5)
        assert len(user_positions) == 5
        # Positions 1-3 reserved, so we get 4-8
        assert user_positions == [(1, 4), (1, 5), (1, 6), (1, 7), (1, 8)]

        # Get QC positions (stateful, increments)
        assert gen.get_qc_position("QC01") == (1, 1)
        assert gen.get_qc_position("QC01") == (1, 2)
        assert gen.get_qc_position("QC01") == (1, 3)
