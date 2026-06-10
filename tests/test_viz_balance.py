"""Tests for the balance-scoring helpers (qg.viz.balance)."""

import polars as pl

from qg.generator import QueueRow, QueueRowTable
from qg.viz.balance import correlation_ratio, plate_balance, queue_balance


class TestCorrelationRatio:
    def test_interleaved_groups_score_near_zero(self):
        # Perfectly alternating groups: each group's mean position sits at the
        # centre, so almost no variance is explained by group.
        positions = list(range(1, 13))
        groups = ["A", "B"] * 6
        assert correlation_ratio(positions, groups) < 0.05

    def test_contiguous_blocks_score_high(self):
        # Groups in solid blocks: most variance is between group means.
        positions = list(range(1, 13))
        groups = ["A"] * 6 + ["B"] * 6
        interleaved = correlation_ratio(positions, ["A", "B"] * 6)
        blocked = correlation_ratio(positions, groups)
        assert blocked > 0.7
        assert blocked > interleaved

    def test_single_group_is_undefined(self):
        assert correlation_ratio([1, 2, 3], ["A", "A", "A"]) is None

    def test_all_null_groups_is_undefined(self):
        assert correlation_ratio([1, 2, 3], [None, None, None]) is None

    def test_null_groups_are_dropped(self):
        # Nulls carry no group signal and must not affect the score.
        positions = [1, 2, 3, 4]
        with_nulls = correlation_ratio(positions, ["A", "B", None, None])
        without = correlation_ratio([1, 2], ["A", "B"])
        assert with_nulls == without

    def test_zero_variance_returns_zero(self):
        assert correlation_ratio([5, 5, 5, 5], ["A", "A", "B", "B"]) == 0.0


def _row(
    n: int, *, slot_kind: str = "user", grouping_var: str | None = None, col: int = 1, container_id: int = 1
) -> QueueRow:
    return QueueRow(
        run_number=n,
        slot_kind=slot_kind,
        sample_id=str(n),
        sample_name=f"s{n}",
        sample_type="Unknown" if slot_kind == "user" else "QC",
        tray="A",
        grid_position=f"A{col}",
        row="A",
        col=col,
        grouping_var=grouping_var,
        inj_vol=2.0,
        file_name=f"f{n}",
        container_id=container_id,
    )


def _df(rows: list[QueueRow]) -> pl.DataFrame:
    return QueueRowTable(rows=rows).to_table()


class TestQueueBalance:
    def test_blocked_order_scores_lower_than_clustered(self):
        clustered = _df([_row(i, grouping_var=g, col=i) for i, g in enumerate(["A", "A", "B", "B"], start=1)])
        blocked = _df([_row(i, grouping_var=g, col=i) for i, g in enumerate(["A", "B", "A", "B"], start=1)])
        assert queue_balance(blocked) < queue_balance(clustered)

    def test_qc_rows_excluded(self):
        # A QC row (no grouping) interleaved must not change the user-only score.
        rows = [
            _row(1, grouping_var="A", col=1),
            _row(2, slot_kind="qc", col=2),
            _row(3, grouping_var="B", col=3),
        ]
        assert queue_balance(_df(rows)) == correlation_ratio([1, 3], ["A", "B"])

    def test_no_grouping_var_returns_none(self):
        assert queue_balance(_df([_row(1), _row(2)])) is None

    def test_scored_within_each_container_then_averaged(self):
        # Same eight injections, two ways. Split across two projects, each is
        # internally well-interleaved, so the averaged score is low. Merged into
        # one project, the distinct group labels separate in time, inflating the
        # global score — exactly the inflation per-container averaging removes.
        groups = ["case", "control", "case", "control", "g1", "g2", "g1", "g2"]
        split = _df(
            [_row(i, grouping_var=g, col=i, container_id=10 if i <= 4 else 20) for i, g in enumerate(groups, start=1)]
        )
        merged = _df([_row(i, grouping_var=g, col=i, container_id=10) for i, g in enumerate(groups, start=1)])
        assert queue_balance(merged) > 0.7
        assert queue_balance(split) < queue_balance(merged)

    def test_single_group_container_skipped_in_average(self):
        # Container 10 is perfectly blocked (η²=1); container 20 has one group
        # (undefined) and must be skipped, so the mean equals container 10's score.
        rows = [
            _row(i, grouping_var=g, col=i, container_id=10) for i, g in enumerate(["A", "A", "B", "B"], start=1)
        ] + [_row(i, grouping_var="X", col=i, container_id=20) for i in range(5, 8)]
        assert queue_balance(_df(rows)) == correlation_ratio([1, 2, 3, 4], ["A", "A", "B", "B"])


class TestPlateBalance:
    def test_scores_group_by_plate_reading_order(self):
        rows = [_row(i, grouping_var=g, col=i) for i, g in enumerate(["A", "A", "B", "B"], start=1)]
        assert plate_balance(_df(rows)) > 0.7

    def test_no_grouping_var_returns_none(self):
        assert plate_balance(_df([_row(1, col=1), _row(2, col=2)])) is None

    def test_scored_within_each_container_then_averaged(self):
        # Each project's wells are internally interleaved; ranking plate position
        # within each container and averaging stays low, whereas merging the two
        # projects into one plate separates the group labels by position.
        groups = ["case", "control", "case", "control", "g1", "g2", "g1", "g2"]
        split = _df(
            [_row(i, grouping_var=g, col=i, container_id=10 if i <= 4 else 20) for i, g in enumerate(groups, start=1)]
        )
        merged = _df([_row(i, grouping_var=g, col=i, container_id=10) for i, g in enumerate(groups, start=1)])
        assert plate_balance(split) < plate_balance(merged)
