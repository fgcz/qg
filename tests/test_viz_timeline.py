"""Tests for the acquisition-timeline visualization (qg.viz.timeline)."""

import plotly.graph_objects as go
import polars as pl

from qg.generator import QueueRow, QueueRowTable
from qg.viz.plate import _CATEGORICAL_PALETTE
from qg.viz.timeline import _BLANK_COLOR, build_timeline_figure


def _row(
    n: int,
    *,
    slot_kind: str = "user",
    grouping_var: str | None = None,
    sample_type: str | None = None,
    qc_class: str | None = None,
    polarity: str = "",
) -> QueueRow:
    return QueueRow(
        run_number=n,
        slot_kind=slot_kind,
        sample_id=str(n),
        sample_name=f"s{n}",
        sample_type=sample_type or ("Unknown" if slot_kind == "user" else "QC"),
        qc_class=qc_class,
        tray="A",
        grid_position=f"A{n}",
        row="A",
        col=n,
        grouping_var=grouping_var,
        inj_vol=2.0,
        file_name=f"f{n}",
        polarity=polarity,
        container_id=1,
    )


def _df(rows: list[QueueRow]) -> pl.DataFrame:
    return QueueRowTable(rows=rows).to_table()


def _total_tiles(fig: go.Figure) -> int:
    return sum(len(t.x) for t in fig.data)


def _legend(fig: go.Figure) -> set[str]:
    return {t.name for t in fig.data if t.name}


def _color_of(fig: go.Figure, name: str) -> str:
    return next(t.marker.color for t in fig.data if t.name == name)


class TestBuildTimelineFigure:
    def test_group_mode_one_tile_per_injection(self):
        rows = [
            _row(1, grouping_var="A"),
            _row(2, grouping_var="B"),
            _row(3, slot_kind="qc"),  # sample_type QC, no qc_class -> falls back to "QC"
            _row(4, grouping_var="A"),
        ]
        fig = build_timeline_figure(_df(rows), color_by="grouping_var")

        assert isinstance(fig, go.Figure)
        assert _total_tiles(fig) == 4
        assert {"A", "B", "QC"} <= _legend(fig)

    def test_group_mode_buckets_ungrouped_samples(self):
        rows = [_row(1, grouping_var="A"), _row(2)]  # second sample has no group
        fig = build_timeline_figure(_df(rows), color_by="grouping_var")
        assert "(none)" in _legend(fig)

    def test_group_mode_colors_qc_by_class(self):
        # QC injections of different types get their own legend entry / colour.
        rows = [
            _row(1, grouping_var="A"),
            _row(2, slot_kind="qc", sample_type="Blank"),  # -> "Blank"
            _row(3, slot_kind="qc", sample_type="QC", qc_class="Pooled QC"),
            _row(4, slot_kind="qc", sample_type="QC", qc_class="QC dilution series"),
            _row(5, grouping_var="B"),
        ]
        fig = build_timeline_figure(_df(rows), color_by="grouping_var")
        legend = _legend(fig)
        assert {"Blank", "Pooled QC", "QC dilution series", "A", "B"} <= legend
        # The old single grey "QC / blank" lump is gone in this mode.
        assert "QC / blank" not in legend

    def test_qc_class_falls_back_to_sample_type(self):
        # A QC sample without qc_class is labelled by its sample_type.
        rows = [_row(1, grouping_var="A"), _row(2, slot_kind="qc", sample_type="Blank")]
        fig = build_timeline_figure(_df(rows), color_by="grouping_var")
        assert "Blank" in _legend(fig)

    def test_group_and_qc_colours_never_collide(self):
        rows = [
            _row(1, grouping_var="A"),
            _row(2, grouping_var="B"),
            _row(3, slot_kind="qc", sample_type="QC", qc_class="Pooled QC"),
            _row(4, slot_kind="qc", sample_type="QC", qc_class="System-suitability QC"),
            _row(5, slot_kind="qc", sample_type="Blank"),
        ]
        fig = build_timeline_figure(_df(rows), color_by="grouping_var")
        # Groups use the shared categorical palette (so timeline colours match the plate view).
        assert _color_of(fig, "A") in _CATEGORICAL_PALETTE
        assert _color_of(fig, "B") in _CATEGORICAL_PALETTE
        assert _color_of(fig, "Blank") == _BLANK_COLOR
        # No two classes (groups or QC) ever share a colour — guards the blue-on-blue bug.
        colours = [_color_of(fig, n) for n in ("A", "B", "Pooled QC", "System-suitability QC", "Blank")]
        assert len(set(colours)) == len(colours)

    def test_polarity_track_added_for_dual_polarity(self):
        rows = [
            _row(1, grouping_var="A", polarity="pos"),
            _row(2, grouping_var="A", polarity="neg"),
            _row(3, grouping_var="B", polarity="pos"),
            _row(4, grouping_var="B", polarity="neg"),
        ]
        fig = build_timeline_figure(_df(rows), color_by="grouping_var")
        # A second, stacked track exists with the polarity legend.
        assert {"positive", "negative"} <= _legend(fig)
        assert sorted({t.yaxis for t in fig.data}) == ["y", "y2"]
        assert len(fig._grid_ref) == 2

    def test_no_polarity_track_for_single_polarity(self):
        rows = [_row(1, grouping_var="A", polarity="pos"), _row(2, grouping_var="B", polarity="pos")]
        fig = build_timeline_figure(_df(rows), color_by="grouping_var")
        assert not ({"positive", "negative"} & _legend(fig))
        assert len(fig._grid_ref) == 1

    def test_polarity_track_under_qc_cadence_mode(self):
        rows = [
            _row(1, grouping_var="A", polarity="pos"),
            _row(2, grouping_var="A", polarity="neg"),
            _row(3, slot_kind="qc", polarity="pos"),
            _row(4, slot_kind="qc", polarity="neg"),
        ]
        fig = build_timeline_figure(_df(rows), color_by="qc_cadence")
        assert {"Sample", "QC / blank", "positive", "negative"} <= _legend(fig)
        assert len(fig._grid_ref) == 2

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
        assert _legend(fig) == {"Sample", "QC / blank"}
        # the second QC's hover reports the gap (3 injections) since the previous QC.
        qc_trace = next(t for t in fig.data if t.name == "QC / blank")
        assert any("Injections since previous QC: 3" in h for h in qc_trace.text)

    def test_single_injection_queue(self):
        # The caller guards emptiness; the smallest real queue is one injection.
        fig = build_timeline_figure(_df([_row(1, grouping_var="A")]), color_by="grouping_var")
        assert isinstance(fig, go.Figure)
        assert _total_tiles(fig) == 1
