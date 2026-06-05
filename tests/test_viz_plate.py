"""Tests for the plate-layout visualization helpers (qg.viz.plate)."""

import plotly.graph_objects as go
import polars as pl

from qg.config_models.positions import PlateLayout
from qg.generator import QueueRow, QueueRowTable
from qg.viz.plate import build_plate_figure, build_plate_wells


def _row(
    n: int,
    *,
    tray: str,
    row: str,
    col: int,
    slot_kind: str = "user",
    sample_type: str = "Unknown",
    container_id: int = 1,
    file_name: str | None = None,
) -> QueueRow:
    return QueueRow(
        run_number=n,
        slot_kind=slot_kind,
        sample_id=str(n),
        sample_name=f"s{n}",
        sample_type=sample_type,
        tray=tray,
        grid_position=f"{row}{col}",
        row=row,
        col=col,
        inj_vol=2.0,
        file_name=file_name or f"f{n}",
        container_id=container_id,
    )


def _queue_df(rows: list[QueueRow]) -> pl.DataFrame:
    return QueueRowTable(rows=rows).to_table()


class TestBuildPlateWells:
    def test_one_record_per_well_with_category_and_hover(self):
        df = _queue_df(
            [
                _row(1, tray="A", row="A", col=1, slot_kind="user"),
                _row(2, tray="A", row="B", col=2, slot_kind="qc", sample_type="QC"),
            ]
        )

        wells = build_plate_wells(df)

        assert wells.height == 2
        assert {"category", "n_orders", "hover", "grid_position"} <= set(wells.columns)
        # user slot -> "Sample", QC slot keeps its sample_type.
        by_pos = dict(zip(wells["grid_position"], wells["category"], strict=True))
        assert by_pos == {"A1": "Sample", "B2": "QC"}
        # single order everywhere
        assert wells["n_orders"].to_list() == [1, 1]
        # hover carries the rendered detail string
        assert all("Name: s" in h for h in wells["hover"])

    def test_multiple_injections_collapse_to_single_well(self):
        # Same well injected twice (e.g. two polarities / repeats) -> one record, n_inj=2.
        df = _queue_df(
            [
                _row(1, tray="A", row="A", col=1, file_name="f1"),
                _row(2, tray="A", row="A", col=1, file_name="f2"),
            ]
        )

        wells = build_plate_wells(df)

        assert wells.height == 1
        assert wells["n_inj"].item() == 2
        # both file names are listed in the hover
        assert "f1" in wells["hover"].item() and "f2" in wells["hover"].item()

    def test_well_shared_across_orders_flagged(self):
        df = _queue_df(
            [
                _row(1, tray="A", row="A", col=1, container_id=10),
                _row(2, tray="A", row="A", col=1, container_id=20),
            ]
        )

        wells = build_plate_wells(df)

        assert wells.height == 1
        assert wells["n_orders"].item() == 2
        assert wells["order_label"].item() == "shared"


class TestBuildPlateFigure:
    def _layout(self) -> PlateLayout:
        return PlateLayout(name="test", rows=["A", "B", "C"], cols=[1, 2, 3])

    def test_one_subplot_per_tray(self):
        df = _queue_df(
            [
                _row(1, tray="A", row="A", col=1),
                _row(2, tray="B", row="B", col=2),
            ]
        )
        wells = build_plate_wells(df)

        fig = build_plate_figure(wells, self._layout(), orders=[1])

        assert isinstance(fig, go.Figure)
        # subplot titles name each tray
        titles = {a.text for a in fig.layout.annotations}
        assert {"Tray A", "Tray B"} <= titles
        assert fig.layout.template.layout.colorway is not None  # plotly_white template applied

    def test_trays_wrap_at_two_panels_per_row(self):
        # 1-2 trays stay on a single row; 3+ wrap to a 2-column grid.
        def grid_shape(n_trays: int) -> tuple[int, int]:
            df = _queue_df([_row(i, tray=str(i), row="A", col=1) for i in range(1, n_trays + 1)])
            fig = build_plate_figure(build_plate_wells(df), self._layout(), orders=[1])
            ref = fig._grid_ref
            return (len(ref), len(ref[0]))

        assert grid_shape(2) == (1, 2)
        assert grid_shape(3) == (2, 2)  # never more than 2 panels per row
        assert grid_shape(5) == (3, 2)

    def test_cell_size_scales_figure(self):
        df = _queue_df([_row(1, tray="A", row="A", col=1)])
        wells = build_plate_wells(df)

        small = build_plate_figure(wells, self._layout(), orders=[1], cell=16)
        large = build_plate_figure(wells, self._layout(), orders=[1], cell=48)

        assert small.layout.width < large.layout.width
        assert small.layout.height < large.layout.height

    def test_single_order_uses_circles_no_order_legend(self):
        df = _queue_df([_row(1, tray="A", row="A", col=1)])
        wells = build_plate_wells(df)

        fig = build_plate_figure(wells, self._layout(), orders=[1])

        legend_names = {t.name for t in fig.data if t.name}
        # color legend present, no per-order shape legend for a single-order queue
        assert "Sample" in legend_names
        assert not any(n.startswith("Order ") for n in legend_names)

    def test_multi_order_adds_order_shape_legend(self):
        df = _queue_df(
            [
                _row(1, tray="A", row="A", col=1, container_id=10),
                _row(2, tray="A", row="B", col=2, container_id=20),
            ]
        )
        wells = build_plate_wells(df)

        fig = build_plate_figure(wells, self._layout(), orders=[10, 20])

        legend_names = {t.name for t in fig.data if t.name}
        assert {"Order 10", "Order 20"} <= legend_names
