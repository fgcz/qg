"""Acquisition-order timeline for generated queues (app-agnostic, no marimo).

One tile per injection along the acquisition axis (``run_number``), recolored to
answer the questions a queue operator cares about:

* ``color_by="grouping_var"`` ("Injection class") — what is each injection, and is
  any biological group front- or back-loaded? User samples are coloured by group
  (matching the plate view); QC/blank injections are coloured by **type**
  (``qc_class`` from ``samples.csv``, falling back to ``sample_type``). Group and
  QC colours are drawn from one shared palette so no two classes ever collide, and
  ``Blank`` is always grey.
* ``color_by="qc_cadence"`` — are QC injections regularly spaced for drift
  monitoring, including across the ``separation`` blocks between containers? QC
  injections are highlighted and their hover reports the gap (in injections)
  since the previous QC.

When the queue spans more than one ionization polarity (metabolomics/lipidomics),
a second **polarity** track (pos/neg) is stacked below the class track under both
modes. Single-polarity runs (proteomics) keep a single track.

Pure plotly so it is unit-testable outside the notebook.
"""

from __future__ import annotations

import plotly.graph_objects as go
import polars as pl
from plotly.subplots import make_subplots

# Reuse the single categorical palette so group colors match the plate view.
from qg.viz.plate import _CATEGORICAL_PALETTE, _NONE_COLOR, _NONE_LABEL

_QC_LABEL = "QC / blank"
_QC_COLOR = "#9D9D9D"
_SAMPLE_LABEL = "Sample"
_SAMPLE_COLOR = "#CFCFCF"
_QC_HIGHLIGHT = "#F58518"

# "Blank" injections are always grey (the "empty" injection); every other class —
# biological group or QC type — is drawn from the shared categorical palette. Groups
# take the first palette slots (so timeline colours match the plate view) and QC
# types take the remaining slots, guaranteeing group and QC colours never collide.
_BLANK_LABEL = "Blank"
_BLANK_COLOR = "#BFBFBF"

_POLARITY_COLORS = {"pos": "#9ECAE1", "neg": "#08519C"}
_POLARITY_LABELS = {"pos": "positive", "neg": "negative"}


def _hover(label_expr: pl.Expr) -> pl.Expr:
    return (
        "Injection "
        + pl.col("run_number").cast(pl.Utf8)
        + "<br>Name: "
        + pl.col("sample_name")
        + "<br>Type: "
        + pl.col("sample_type")
        + label_expr
    ).alias("hover")


def _add_category_bars(
    fig: go.Figure, df: pl.DataFrame, ordered: list[str], color_map: dict[str, str], *, row: int = 1
) -> None:
    """One bar trace per color category, each a strip of unit-height tiles."""
    for label in ordered:
        sub = df.filter(pl.col("_clr") == label)
        if sub.is_empty():
            continue
        fig.add_trace(
            go.Bar(
                x=sub["run_number"].to_list(),
                y=[1] * sub.height,
                width=1.0,
                marker={"color": color_map.get(label, _NONE_COLOR), "line": {"color": "white", "width": 0.3}},
                name=label,
                text=sub["hover"].to_list(),
                textposition="none",  # details live in the hover, not crammed onto unreadable tiles
                hovertemplate="%{text}<extra></extra>",
            ),
            row=row,
            col=1,
        )


def _add_polarity_bars(fig: go.Figure, df: pl.DataFrame, row: int) -> None:
    """Stacked pos/neg track: one tile per injection colored by ionization polarity."""
    for pol in ("pos", "neg"):
        sub = df.filter(pl.col("polarity") == pol)
        if sub.is_empty():
            continue
        hover = "Injection " + pl.col("run_number").cast(pl.Utf8) + "<br>Polarity: " + pl.lit(_POLARITY_LABELS[pol])
        sub = sub.with_columns(hover.alias("_pol_hover"))
        fig.add_trace(
            go.Bar(
                x=sub["run_number"].to_list(),
                y=[1] * sub.height,
                width=1.0,
                marker={"color": _POLARITY_COLORS[pol], "line": {"color": "white", "width": 0.3}},
                name=_POLARITY_LABELS[pol],
                text=sub["_pol_hover"].to_list(),
                textposition="none",
                hovertemplate="%{text}<extra></extra>",
            ),
            row=row,
            col=1,
        )


def _grouping_var_encoding(df: pl.DataFrame) -> tuple[pl.DataFrame, list[str], dict[str, str], str]:
    """Colour user samples by group and QC by type, from one collision-free palette."""
    if "qc_class" not in df.columns:
        df = df.with_columns(pl.lit(None, dtype=pl.Utf8).alias("qc_class"))
    df = df.with_columns(
        pl.when(pl.col("slot_kind") == "user")
        .then(pl.col("grouping_var").fill_null(_NONE_LABEL))
        .otherwise(pl.col("qc_class").fill_null(pl.col("sample_type")))
        .alias("_clr"),
        _hover("<br>Group: " + pl.col("grouping_var").fill_null("—")),
    )

    groups = sorted(v for v in df.filter(pl.col("slot_kind") == "user")["_clr"].unique().to_list() if v != _NONE_LABEL)
    qc_classes = sorted(c for c in df.filter(pl.col("slot_kind") == "qc")["_clr"].unique().to_list() if c != _BLANK_LABEL)

    color_map: dict[str, str] = {}
    # Groups take the first palette slots (so timeline group colours match the plate
    # view); QC types take the slots *after* the groups, so the two never collide.
    for i, g in enumerate(groups):
        color_map[g] = _CATEGORICAL_PALETTE[i % len(_CATEGORICAL_PALETTE)]
    for j, c in enumerate(qc_classes):
        color_map[c] = _CATEGORICAL_PALETTE[(len(groups) + j) % len(_CATEGORICAL_PALETTE)]
    color_map[_BLANK_LABEL] = _BLANK_COLOR
    color_map[_NONE_LABEL] = _NONE_COLOR

    # Legend order: QC scaffolding first (Blank, then QC types), then biological groups.
    present = set(df["_clr"].to_list())
    ordered = [c for c in ([_BLANK_LABEL] + qc_classes + groups) if c in present]
    if _NONE_LABEL in present:
        ordered.append(_NONE_LABEL)
    return df, ordered, color_map, "Acquisition order — injection class across the run"


def _qc_cadence_encoding(df: pl.DataFrame) -> tuple[pl.DataFrame, list[str], dict[str, str], str]:
    """Highlight QC injections and report the gap (in injections) since the previous QC."""
    qc_runs = df.filter(pl.col("slot_kind") == "qc")["run_number"].to_list()
    prev = dict(zip(qc_runs, _gaps(qc_runs), strict=True))
    gap_expr = (
        pl.when(pl.col("slot_kind") == "qc")
        .then(
            "<br>Injections since previous QC: "
            + pl.col("run_number").replace_strict(prev, default=None).cast(pl.Utf8, strict=False).fill_null("—")
        )
        .otherwise(pl.lit(""))
    )
    df = df.with_columns(
        pl.when(pl.col("slot_kind") == "qc").then(pl.lit(_QC_LABEL)).otherwise(pl.lit(_SAMPLE_LABEL)).alias("_clr"),
        _hover(gap_expr),
    )
    ordered = [_SAMPLE_LABEL, _QC_LABEL]
    color_map = {_SAMPLE_LABEL: _SAMPLE_COLOR, _QC_LABEL: _QC_HIGHLIGHT}
    return df, ordered, color_map, "QC cadence — highlighted QC injections along the run"


def build_timeline_figure(df: pl.DataFrame, color_by: str = "grouping_var") -> go.Figure:
    """Render the acquisition-order strip for ``df`` (a generated queue table).

    Args:
        df: Queue rows carrying ``run_number``, ``slot_kind``, ``sample_type``,
            ``sample_name``, ``grouping_var``, ``polarity`` and (optionally) ``qc_class``.
        color_by: ``"grouping_var"`` colors user samples by group and QC by type;
            ``"qc_cadence"`` highlights QC injections and reports their spacing.

    Returns:
        A plotly figure: x = injection order 1..N. One class track always; a second
        polarity track is stacked below it when the queue spans >1 polarity.
    """
    df = df.sort("run_number")

    if color_by == "qc_cadence":
        df, ordered, color_map, title = _qc_cadence_encoding(df)
    else:
        df, ordered, color_map, title = _grouping_var_encoding(df)

    pols = [p for p in df["polarity"].unique().to_list() if p] if "polarity" in df.columns else []
    has_polarity = len(pols) > 1
    n_rows = 2 if has_polarity else 1

    fig = make_subplots(
        rows=n_rows,
        cols=1,
        shared_xaxes=True,
        row_heights=[0.6, 0.4] if has_polarity else [1.0],
        vertical_spacing=0.18,
    )
    _add_category_bars(fig, df, ordered, color_map, row=1)
    if has_polarity:
        _add_polarity_bars(fig, df, row=2)

    fig.update_layout(
        barmode="overlay",
        template="plotly_white",
        height=300 if has_polarity else 200,
        margin={"l": 40, "r": 15, "t": 36, "b": 70},
        title={"text": title, "x": 0.0, "font": {"size": 12}},
        # Flat horizontal legend below the plot so it never grows into a tall,
        # scrollbar-clipped column in the app. Items wrap across the width.
        legend={"orientation": "h", "yanchor": "top", "y": -0.25, "xanchor": "left", "x": 0.0, "font": {"size": 11}},
        bargap=0,
    )
    fig.update_xaxes(title_text="acquisition order (injection 1..N)", tickformat="d", row=n_rows, col=1)
    fig.update_yaxes(visible=False, range=[0, 1])
    return fig


def _gaps(positions: list[int]) -> list[int | None]:
    """Gap (in injections) from each position to the previous one; first is None.

    Same length as ``positions`` (``[]`` for empty, ``[None]`` for a single QC).
    """
    return [None if i == 0 else positions[i] - positions[i - 1] for i in range(len(positions))]
