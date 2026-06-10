"""Acquisition-order timeline for generated queues (app-agnostic, no marimo).

One tile per injection along the acquisition axis (``run_number``), recolored to
answer two questions a queue operator cares about:

* ``color_by="grouping_var"`` — is any biological group front- or back-loaded in
  the run? User samples are colored by group; QC/blank injections are drawn grey
  so the interleaving stays legible.
* ``color_by="qc_cadence"`` — are QC injections regularly spaced for drift
  monitoring, including across the ``separation`` blocks between containers? QC
  injections are highlighted and their hover reports the gap (in injections)
  since the previous QC.

Pure plotly so it is unit-testable outside the notebook.
"""

from __future__ import annotations

import plotly.graph_objects as go
import polars as pl

# Reuse the single categorical palette so group colors match the plate view.
from qg.viz.plate import _CATEGORICAL_PALETTE, _NONE_COLOR, _NONE_LABEL

_QC_LABEL = "QC / blank"
_QC_COLOR = "#9D9D9D"
_SAMPLE_LABEL = "Sample"
_SAMPLE_COLOR = "#CFCFCF"
_QC_HIGHLIGHT = "#F58518"


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


def _add_category_bars(fig: go.Figure, df: pl.DataFrame, ordered: list[str], color_map: dict[str, str]) -> None:
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
                hovertemplate="%{text}<extra></extra>",
            )
        )


def build_timeline_figure(df: pl.DataFrame, color_by: str = "grouping_var") -> go.Figure:
    """Render the acquisition-order strip for ``df`` (a generated queue table).

    Args:
        df: Queue rows carrying ``run_number``, ``slot_kind``, ``sample_type``,
            ``sample_name`` and ``grouping_var``.
        color_by: ``"grouping_var"`` colors user samples by group (QC grey);
            ``"qc_cadence"`` highlights QC injections and reports their spacing.

    Returns:
        A plotly figure: x = injection order 1..N, one unit-height tile each.
    """
    df = df.sort("run_number")

    if color_by == "qc_cadence":
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
        title = "QC cadence — highlighted QC injections along the run"
    else:
        df = df.with_columns(
            pl.when(pl.col("slot_kind") == "qc")
            .then(pl.lit(_QC_LABEL))
            .otherwise(pl.col("grouping_var").fill_null(_NONE_LABEL))
            .alias("_clr"),
            _hover("<br>Group: " + pl.col("grouping_var").fill_null("—")),
        )
        groups = sorted(v for v in df["_clr"].unique().to_list() if v not in (_QC_LABEL, _NONE_LABEL))
        color_map = {g: _CATEGORICAL_PALETTE[i % len(_CATEGORICAL_PALETTE)] for i, g in enumerate(groups)}
        ordered = list(groups)
        if _NONE_LABEL in df["_clr"].to_list():
            color_map[_NONE_LABEL] = _NONE_COLOR
            ordered.append(_NONE_LABEL)
        color_map[_QC_LABEL] = _QC_COLOR
        ordered.append(_QC_LABEL)
        title = "Acquisition order — group across the run"

    fig = go.Figure()
    _add_category_bars(fig, df, ordered, color_map)
    fig.update_layout(
        barmode="overlay",
        template="plotly_white",
        height=210,
        margin={"l": 40, "r": 15, "t": 40, "b": 85},
        title={"text": title, "x": 0.0, "font": {"size": 12}},
        # Anchor the legend's top well below the x-axis title so the two never overlap.
        legend={"orientation": "h", "yanchor": "top", "y": -0.5, "xanchor": "left", "x": 0.0},
        bargap=0,
    )
    fig.update_xaxes(title_text="acquisition order (injection 1..N)", tickformat="d")
    fig.update_yaxes(visible=False, range=[0, 1])
    return fig


def _gaps(positions: list[int]) -> list[int | None]:
    """Gap (in injections) from each position to the previous one; first is None.

    Same length as ``positions`` (``[]`` for empty, ``[None]`` for a single QC).
    """
    return [None if i == 0 else positions[i] - positions[i - 1] for i in range(len(positions))]
