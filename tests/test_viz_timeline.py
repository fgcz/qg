"""Tests for the acquisition-timeline visualization (qg.viz.timeline)."""

import plotly.graph_objects as go
import polars as pl

from qg.generator import QueueRow, QueueRowTable
from qg.viz.timeline import build_timeline_figure


def _row(n: int, *, slot_kind: str = "user", grouping_var: str | None = None) -> QueueRow:
    return QueueRow(
        run_number=n,
        slot_kind=slot_kind,
        sample_id=str(n),
        sample_name=f"s{n}",
        sample_type="Unknown" if slot_kind == "user" else "QC",
        tray="A",
        grid_position=f"A{n}",
        row="A",
        col=n,
        grouping_var=grouping_var,
        inj_vol=2.0,
        file_name=f"f{n}",
        container_id=1,
    )


def _df(rows: list[QueueRow]) -> pl.DataFrame:
    return QueueRowTable(rows=rows).to_table()


def _total_tiles(fig: go.Figure) -> int:
    return sum(len(t.x) for t in fig.data)


class TestBuildTimelineFigure:
    def test_group_mode_one_tile_per_injection(self):
        rows = [
            _row(1, grouping_var="A"),
            _row(2, grouping_var="B"),
            _row(3, slot_kind="qc"),
            _row(4, grouping_var="A"),
        ]
        fig = build_timeline_figure(_df(rows), color_by="grouping_var")

        assert isinstance(fig, go.Figure)
        assert _total_tiles(fig) == 4
        legend = {t.name for t in fig.data if t.name}
        assert {"A", "B", "QC / blank"} <= legend

    def test_group_mode_buckets_ungrouped_samples(self):
        rows = [_row(1, grouping_var="A"), _row(2)]  # second sample has no group
        fig = build_timeline_figure(_df(rows), color_by="grouping_var")

        legend = {t.name for t in fig.data if t.name}
        assert "(none)" in legend

    def test_qc_cadence_mode_splits_qc_from_samples(self):
        rows = [
            _row(1, grouping_var="A"),
            _row(2, grouping_var="B"),
            _row(3, slot_kind="qc"),
            _row(4, grouping_var="A"),
            _row(5, grouping_var="B"),
            _row(6, slot_kind="qc"),
        ]
        fig = build_timeline_figure(_df(rows), color_by="qc_cadence")

        assert _total_tiles(fig) == 6
        legend = {t.name for t in fig.data if t.name}
        assert legend == {"Sample", "QC / blank"}
        # the second QC's hover reports the gap (3 injections) since the previous QC.
        qc_trace = next(t for t in fig.data if t.name == "QC / blank")
        assert any("Injections since previous QC: 3" in h for h in qc_trace.text)

    def test_single_injection_queue(self):
        # The caller guards emptiness; the smallest real queue is one injection.
        fig = build_timeline_figure(_df([_row(1, grouping_var="A")]), color_by="grouping_var")
        assert isinstance(fig, go.Figure)
        assert _total_tiles(fig) == 1
