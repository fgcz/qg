"""Balance scoring for generated queues (app-agnostic, no marimo dependency).

The **correlation ratio** (eta-squared, ``η²``) measures how much of the variance
in a *position* variable is explained by a categorical *group*. We use it to
report how well randomization decorrelated biological group from acquisition
order (group vs. queue position) and from physical layout (group vs. plate
position):

* ``0`` — group explains no positional variance: the groups are evenly
  interleaved across positions (ideal balance).
* ``1`` — positions are fully separated by group: each group occupies a
  contiguous run (worst case).

Unlike the normalized-spread statistic in
``docs/app_note/pipeline/scripts/make_randomization_figure.py`` (which divides by
the submission-order worst case), ``η²`` is **standalone** — it needs no baseline
run — so it can score a single generated queue live in the GUI.
"""

from __future__ import annotations

import statistics
from collections import defaultdict
from collections.abc import Sequence

import polars as pl


def correlation_ratio(positions: Sequence[float], groups: Sequence[str | None]) -> float | None:
    """Correlation ratio (``η²``) of ``positions`` explained by ``groups``.

    ``η² = SS_between / SS_total``: the fraction of the total variance in
    ``positions`` that lies *between* group means rather than within groups.

    Args:
        positions: Numeric position of each item (e.g. acquisition index).
        groups: Group label of each item, aligned with ``positions``. ``None``
            labels are dropped (ungrouped items carry no balance signal).

    Returns:
        ``η²`` in ``[0, 1]``, or ``None`` when fewer than two distinct non-null
        groups are present (the score is undefined). Returns ``0.0`` when there
        is no positional variance to explain.
    """
    by_group: dict[str, list[float]] = defaultdict(list)
    for pos, group in zip(positions, groups, strict=True):
        if group is not None:
            by_group[group].append(pos)
    if len(by_group) < 2:
        return None

    all_positions = [p for vals in by_group.values() for p in vals]
    grand_mean = statistics.fmean(all_positions)
    ss_total = sum((p - grand_mean) ** 2 for p in all_positions)
    if ss_total == 0:
        return 0.0
    ss_between = sum(len(vals) * (statistics.fmean(vals) - grand_mean) ** 2 for vals in by_group.values())
    return ss_between / ss_total


def queue_balance(df: pl.DataFrame) -> float | None:
    """``η²`` of acquisition position (``run_number``) explained by ``grouping_var``.

    Computed over user-sample rows (``slot_kind == "user"``); QC/blank injections
    carry no group and are excluded.
    """
    user = df.filter(pl.col("slot_kind") == "user")
    if user.is_empty():
        return None
    return correlation_ratio(user["run_number"].to_list(), user["grouping_var"].to_list())


def plate_balance(df: pl.DataFrame) -> float | None:
    """``η²`` of plate position explained by ``grouping_var``.

    Plate position is the 1-D rank of each occupied user well in reading order
    ``(tray, row, col)``. This linearization is lossy — it cannot see 2-D
    clustering — but it keeps the plate score mathematically parallel to the
    queue score and answers "is a group front/back-loaded on the plate?".
    """
    user = df.filter(pl.col("slot_kind") == "user")
    if user.is_empty():
        return None
    wells = (
        user.group_by(["tray", "row", "col"])
        .agg(pl.col("grouping_var").first())
        .with_columns(pl.col("tray").cast(pl.Utf8).alias("_tray"))
        .sort(["_tray", "row", "col"])
        .with_row_index("_plate_index")
    )
    return correlation_ratio(wells["_plate_index"].to_list(), wells["grouping_var"].to_list())
