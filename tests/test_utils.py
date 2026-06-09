"""Tests for qg.utils module — Position, generate_all_positions, group_by_key, string_concat."""

from __future__ import annotations

import dataclasses

import pytest

from qg.utils import Position, generate_all_positions, group_by_key, string_concat


class TestPosition:
    def test_equal_same_tray_grid_different_row_col(self):
        a = Position("T1", "A1", "A", 1)
        b = Position("T1", "A1", "B", 2)
        assert a == b

    def test_not_equal_different_grid(self):
        a = Position("T1", "A1", "A", 1)
        b = Position("T1", "B2", "B", 2)
        assert a != b

    def test_not_equal_different_tray(self):
        a = Position("T1", "A1", "A", 1)
        b = Position("T2", "A1", "A", 1)
        assert a != b

    def test_eq_returns_not_implemented_for_non_position(self):
        p = Position("T1", "A1", "A", 1)
        assert p.__eq__("not a position") is NotImplemented

    def test_set_membership_ignores_row_col(self):
        a = Position("T1", "A1", "A", 1)
        b = Position("T1", "A1", "B", 2)
        assert len({a, b}) == 1

    def test_hash_consistency(self):
        a = Position("T1", "A1", "A", 1)
        b = Position("T1", "A1", "B", 2)
        assert hash(a) == hash(b)

    def test_frozen(self):
        p = Position("T1", "A1", "A", 1)
        with pytest.raises(dataclasses.FrozenInstanceError):
            p.tray = "T2"  # type: ignore[misc]


class TestGenerateAllPositions:
    def test_basic_generation(self):
        positions = generate_all_positions(["T1", "T2"], ["A", "B"], [1, 2, 3], string_concat)
        assert len(positions) == 12
        assert all(isinstance(p, Position) for p in positions)

    def test_empty_inputs(self):
        assert generate_all_positions([], ["A"], [1], string_concat) == []
        assert generate_all_positions(["T1"], [], [1], string_concat) == []
        assert generate_all_positions(["T1"], ["A"], [], string_concat) == []


class TestGroupByKey:
    def test_basic_grouping(self):
        result = group_by_key([1, 2, 3, 4, 5], lambda x: x % 2)
        assert result == {1: [1, 3, 5], 0: [2, 4]}

    def test_preserves_order(self):
        result = group_by_key([3, 1, 4, 1, 5], lambda x: x)
        assert list(result.keys()) == [3, 1, 4, 5]
        assert result[1] == [1, 1]

    def test_empty_list(self):
        assert group_by_key([], lambda x: x) == {}


class TestStringConcat:
    def test_basic(self):
        assert string_concat("A", 1) == "A1"
