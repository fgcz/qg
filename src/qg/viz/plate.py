"""Plate-layout visualization for generated queues.

Pure functions (no marimo, no config access) so they are unit-testable outside
the notebook. ``build_plate_wells`` aggregates queue rows into one record per
occupied well; ``build_plate_figure`` renders those wells onto the plate grid,
one subplot per tray, with color encoding sample category and marker shape
encoding the order.
"""

from __future__ import annotations

import math

import plotly.graph_objects as go
import polars as pl
from plotly.subplots import make_subplots

from qg.config_models.positions import PlateLayout

# Color encodes sample category (fallback for any unexpected value).
_COLORS = {
    "Sample": "#4C78A8",
    "QC": "#F58518",
    "Std Bracket": "#54A24B",
    "Blank": "#B279A2",
    "Unknown": "#9D9D9D",
}
_FALLBACK = "#333333"
# Colorblind-safe palette cycled over the distinct values of an arbitrary
# categorical column (e.g. ``grouping_var``) when coloring by covariate.
_CATEGORICAL_PALETTE = [
    "#4C78A8",
    "#E45756",
    "#59A14F",
    "#B279A2",
    "#F58518",
    "#76B7B2",
    "#EDC948",
    "#FF9DA7",
    "#9C755F",
    "#BAB0AC",
]
_NONE_LABEL = "(none)"
_NONE_COLOR = "#D0D0D0"
# Shape encodes the order: order 1 = circle, order 2 = square, ... so a
# single-order queue (all circles) matches the first order of a multi-order one.
_SYMBOL_POOL = ["circle", "square", "triangle-up", "diamond", "star", "pentagon", "hexagon", "triangle-down"]
_SHARED_SYMBOL = "circle-open"  # wells reused across orders (e.g. shared QC positions)


def build_plate_wells(geom: pl.DataFrame) -> pl.DataFrame:
    """Aggregate geometry-bearing queue rows into one record per occupied well.

    Args:
        geom: Queue rows already filtered to those with real row/column
            geometry (``row != ""`` and ``col != 0``). Must carry the
            ``QueueRow`` columns ``slot_kind``, ``sample_type``, ``tray``,
            ``row``, ``col``, ``grid_position``, ``sample_id``, ``sample_name``,
            ``inj_vol``, ``container_id`` and ``file_name``.

    Returns:
        One row per ``(tray, row, col)`` well with a ``category`` (sample type,
        with user slots mapped to ``"Sample"``), an ``n_orders`` count, and a
        ready-to-render HTML ``hover`` string.
    """
    return (
        geom.with_columns(
            # category: user slots are always sample_type="Unknown", so split on slot_kind first.
            pl.when(pl.col("slot_kind") == "user")
            .then(pl.lit("Sample"))
            .otherwise(pl.col("sample_type"))
            .alias("category")
        )
        .group_by(["tray", "row", "col"])
        .agg(
            pl.col("category").first().alias("category"),
            pl.col("grid_position").first().alias("grid_position"),
            pl.col("sample_id").first().alias("sample_id"),
            pl.col("sample_name").first().alias("sample_name"),
            pl.col("grouping_var").first().alias("grouping_var"),
            pl.col("inj_vol").first().alias("inj_vol"),
            pl.col("container_id").first().alias("container_id"),
            pl.col("container_id").n_unique().alias("n_orders"),
            pl.col("file_name").unique().alias("file_names"),
            pl.len().alias("n_inj"),
        )
        .with_columns(
            pl.when(pl.col("n_orders") == 1)
            .then(pl.col("container_id").cast(pl.Utf8))
            .otherwise(pl.lit("shared"))
            .alias("order_label")
        )
        .with_columns(
            (
                "Tray "
                + pl.col("tray").cast(pl.Utf8)
                + " "
                + pl.col("grid_position")
                + "<br>ID: "
                + pl.col("sample_id").cast(pl.Utf8)
                + "<br>Name: "
                + pl.col("sample_name")
                + "<br>Type: "
                + pl.col("category")
                + "<br>Group: "
                + pl.col("grouping_var").fill_null("—")
                + "<br>Order: "
                + pl.col("order_label")
                + "<br>Inj vol: "
                + pl.col("inj_vol").cast(pl.Utf8)
                + "<br>Injections: "
                + pl.col("n_inj").cast(pl.Utf8)
                + "<br>File:<br>"
                + pl.col("file_names").list.join("<br>")
            ).alias("hover")
        )
    )


def _resolve_coloring(wells: pl.DataFrame, color_by: str) -> tuple[pl.DataFrame, list[str], dict[str, str], str]:
    """Compute the per-well color key, the ordered legend categories, the color
    map, and the legend title for the requested ``color_by`` column.

    ``color_by == "category"`` keeps the fixed sample-type palette; any other
    column is treated as an arbitrary categorical covariate (e.g. ``grouping_var``)
    colored from :data:`_CATEGORICAL_PALETTE`, with null values bucketed under
    :data:`_NONE_LABEL`.
    """
    if color_by == "category":
        wells = wells.with_columns(pl.col("category").alias("_clr"))
        present = set(wells["_clr"].to_list())
        ordered = [c for c in _COLORS if c in present]
        return wells, ordered, dict(_COLORS), "Sample type"

    wells = wells.with_columns(pl.col(color_by).cast(pl.Utf8).fill_null(_NONE_LABEL).alias("_clr"))
    present = set(wells["_clr"].to_list())
    real = sorted(v for v in present if v != _NONE_LABEL)
    color_map = {v: _CATEGORICAL_PALETTE[i % len(_CATEGORICAL_PALETTE)] for i, v in enumerate(real)}
    ordered = list(real)
    if _NONE_LABEL in present:
        color_map[_NONE_LABEL] = _NONE_COLOR
        ordered.append(_NONE_LABEL)
    return wells, ordered, color_map, "Group"


def build_plate_figure(
    wells: pl.DataFrame, layout: PlateLayout, orders: list[int], cell: int = 30, color_by: str = "category"
) -> go.Figure:
    """Render occupied wells onto the plate grid, one subplot per tray.

    Args:
        wells: Per-well frame from :func:`build_plate_wells`.
        layout: Resolved plate layout providing the grid ``rows`` and ``cols``.
        orders: Sorted list of container ids present in the queue; their order
            determines the marker shape assigned to each.
        cell: Pixels allotted to each well; sets the overall figure size and
            scales the marker sizes. Smaller values produce a more compact plot.
        color_by: Well column driving the color encoding. ``"category"`` (default)
            colors by sample type; any other column (e.g. ``"grouping_var"``) is
            colored as an arbitrary categorical covariate.

    Returns:
        A plotly figure sized to the grid so wells stay roughly square.
    """
    wells, color_categories, color_map, legend_title = _resolve_coloring(wells, color_by)
    multi_order = len(orders) > 1
    order_symbol = {o: _SYMBOL_POOL[i % len(_SYMBOL_POOL)] for i, o in enumerate(orders)}
    has_shared = multi_order and bool((wells["n_orders"] > 1).any())

    def _symbols(container_ids: list[int], n_orders: list[int]) -> str | list[str]:
        if not multi_order:
            return "circle"
        return [order_symbol[c] if n == 1 else _SHARED_SYMBOL for c, n in zip(container_ids, n_orders, strict=False)]

    trays = sorted(wells["tray"].unique().to_list(), key=str)
    cols = list(layout.cols)
    rows = list(layout.rows)
    grid_x = [c for _r in rows for c in cols]
    grid_y = [_r for _r in rows for _c in cols]

    # Size the figure to the grid so wells stay roughly square instead of being
    # stretched across the full container width (one short, compact panel per tray).
    # Wrap the per-tray panels at MAX_PANELS_PER_ROW so they don't run off to the
    # right; trays fill the grid row-major (tray 1 top-left, tray 2 top-right, ...).
    n_trays, n_cols, n_rows = len(trays), len(cols), len(rows)
    max_panels_per_row = 2
    grid_cols = min(n_trays, max_panels_per_row)
    grid_rows = math.ceil(n_trays / max_panels_per_row)
    # Scale the markers with the cell size so wells stay proportional when resized.
    data_marker = max(6, round(cell * 0.45))
    grid_marker = max(4, round(cell * 0.36))
    margin = {"l": 55, "r": 15, "t": 55, "b": 45}
    legend_px = 160
    hgap_px = 60  # room for the right column's row-axis labels
    vgap_px = 70  # room for each lower row's column-axis labels and subplot title
    fig_w = margin["l"] + grid_cols * n_cols * cell + (grid_cols - 1) * hgap_px + margin["r"] + legend_px
    fig_h = margin["t"] + grid_rows * n_rows * cell + (grid_rows - 1) * vgap_px + margin["b"]
    hspace = hgap_px / fig_w if grid_cols > 1 else 0.0
    vspace = vgap_px / fig_h if grid_rows > 1 else 0.0

    fig = make_subplots(
        rows=grid_rows,
        cols=grid_cols,
        subplot_titles=[f"Tray {t}" for t in trays],
        horizontal_spacing=hspace,
        vertical_spacing=vspace,
    )
    for i, tray in enumerate(trays):
        sub_row = i // max_panels_per_row + 1
        sub_col = i % max_panels_per_row + 1
        # Faint empty grid behind the occupied wells.
        fig.add_trace(
            go.Scatter(
                x=grid_x,
                y=grid_y,
                mode="markers",
                marker={"size": grid_marker, "color": "#EEEEEE", "line": {"color": "#CCCCCC", "width": 1}},
                hoverinfo="skip",
                showlegend=False,
            ),
            row=sub_row,
            col=sub_col,
        )
        tray_wells = wells.filter(pl.col("tray").cast(pl.Utf8) == str(tray))
        present = set(tray_wells["_clr"].unique().to_list())
        for category in (c for c in color_categories if c in present):
            cat_wells = tray_wells.filter(pl.col("_clr") == category)
            fig.add_trace(
                go.Scatter(
                    x=cat_wells["col"].to_list(),
                    y=cat_wells["row"].to_list(),
                    mode="markers",
                    marker={
                        "size": data_marker,
                        "color": color_map.get(category, _FALLBACK),
                        "symbol": _symbols(cat_wells["container_id"].to_list(), cat_wells["n_orders"].to_list()),
                    },
                    showlegend=False,  # legends are provided by the dummy traces below
                    text=cat_wells["hover"].to_list(),
                    hovertemplate="%{text}<extra></extra>",
                ),
                row=sub_row,
                col=sub_col,
            )
        fig.update_xaxes(title_text="Column", tickmode="array", tickvals=cols, row=sub_row, col=sub_col)
        fig.update_yaxes(
            title_text="Row", categoryorder="array", categoryarray=list(reversed(rows)), row=sub_row, col=sub_col
        )

    # Color legend, decoupled from the data traces so swatches stay clean.
    for category in color_categories:
        fig.add_trace(
            go.Scatter(
                x=[None],
                y=[None],
                mode="markers",
                marker={"size": 12, "color": color_map.get(category, _FALLBACK), "symbol": "circle"},
                name=category,
                legendgroup="color",
                legendgrouptitle_text=legend_title,
                showlegend=True,
            ),
            row=1,
            col=1,
        )
    # Order legend (shape), only when more than one order is present.
    if multi_order:
        for order in orders:
            fig.add_trace(
                go.Scatter(
                    x=[None],
                    y=[None],
                    mode="markers",
                    marker={"size": 12, "color": "#666666", "symbol": order_symbol[order]},
                    name=f"Order {order}",
                    legendgroup="order",
                    legendgrouptitle_text="Order",
                    showlegend=True,
                ),
                row=1,
                col=1,
            )
        if has_shared:
            fig.add_trace(
                go.Scatter(
                    x=[None],
                    y=[None],
                    mode="markers",
                    marker={"size": 12, "color": "#666666", "symbol": _SHARED_SYMBOL},
                    name="Shared",
                    legendgroup="order",
                    legendgrouptitle_text="Order",
                    showlegend=True,
                ),
                row=1,
                col=1,
            )
    fig.update_layout(width=fig_w, height=fig_h, autosize=False, margin=margin, template="plotly_white")
    return fig
