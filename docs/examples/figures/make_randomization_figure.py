#!/usr/bin/env python3
"""Evidence figure: blocked randomization decorrelates group from run position.

Builds one synthetic order of 56 samples in 4 *unbalanced* biological groups
(A x 24, B x 14, C x 10, D x 8) using the real ``qg`` sample/queue models, then
applies each of the four randomization modes exposed by
``qg.randomize.randomize_plate_queue``:

* ``"no"``              -- submission order; groups land in contiguous blocks.
* ``"random"``          -- complete shuffle; decorrelates *on average* but leaves
                           visible run-to-run clustering for any single seed.
* ``"blocked"``         -- randomized complete block design (RCBD); one sample of
                           each group per block. For unbalanced groups the smaller
                           groups are exhausted first, leaving a majority-only tail.
* ``"blocked_uniform"`` -- group-uniform (fair-share) interleave; spreads every
                           group evenly across the whole run, removing the tail.

An unbalanced design is used deliberately: with balanced groups ``blocked`` and
``blocked_uniform`` are nearly identical, so the unbalanced case is what exposes
the difference the fourth mode was added to address.

Determinism: ``qg.randomize`` draws from the process-global ``random`` module when
no explicit RNG is passed (the same contract the unit tests rely on), so we call
``random.seed(SEED)`` immediately before each mode for a reproducible figure.

A per-mode eta-squared statistic quantifies the residual association between group
and run position; ``blocked_uniform`` is the most balanced for unbalanced groups.

Run with matplotlib available, e.g.::

    uv run --with matplotlib python scripts/make_randomization_figure.py

Output: figures/fig_randomization.png
"""

from __future__ import annotations

import random
import statistics
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.patches import Patch

from qg.params_models import ContainerBatch, Plate, PlateCell, PlateQueue, VialSample

ROOT = Path(__file__).resolve().parent
OUT = ROOT / "fig_randomization.png"

SEED = 20260112
GROUPS = ["A", "B", "C", "D"]
GROUP_SIZES = {"A": 24, "B": 14, "C": 10, "D": 8}  # unbalanced on purpose
N = sum(GROUP_SIZES.values())  # 56 samples
PLATE_ID = 1
CONTAINER_ID = 37180

MODES = ("no", "random", "blocked", "blocked_uniform")
MODE_TITLES = {
    "no": 'submission order ("no")',
    "random": 'complete randomization ("random")',
    "blocked": 'block randomization ("blocked", RCBD)',
    "blocked_uniform": 'group-uniform interleave ("blocked_uniform")',
}

# Colour-blind-safe palette, one colour per biological group.
GROUP_COLORS = {
    "A": "#4C78A8",
    "B": "#E45756",
    "C": "#59A14F",
    "D": "#B279A2",
}

plt.rcParams.update(
    {
        "font.family": "sans-serif",
        "font.sans-serif": ["Helvetica", "Arial", "DejaVu Sans"],
        "font.size": 8,
    }
)


def _submission_queue() -> PlateQueue:
    """An N-sample queue in submission order: all A, then all B, C, D."""
    cells: list[PlateCell] = []
    pos = 1
    for group in GROUPS:
        for i in range(GROUP_SIZES[group]):
            sample = VialSample(
                sample_name=f"{group}{i + 1}",
                sample_id=pos,
                container_id=CONTAINER_ID,
                grouping_var=group,
            )
            cells.append(PlateCell(sample=sample, grid_position=f"A{pos}", plate_id=PLATE_ID))
            pos += 1
    return PlateQueue(
        batches={CONTAINER_ID: ContainerBatch(container_id=CONTAINER_ID, container_name="Cohort")},
        plates={PLATE_ID: Plate(plate_id=PLATE_ID, tray="Y", nr_samples=N)},
        cells=cells,
    )


def _group_order(queue: PlateQueue) -> list[str]:
    """Biological group label at each acquisition position 1..N."""
    return [c.sample.grouping_var or "?" for c in queue.cells]


def _eta_squared(order: list[str]) -> float:
    """Share of acquisition-position variance explained by biological group.

    One-way ANOVA effect size, eta^2 = SS_between / SS_total, computed on the
    1-based acquisition positions grouped by biological group. eta^2 = 0 when
    group and run position are unrelated; eta^2 = 1 when the group is perfectly
    aliased with acquisition time. Bounded to [0, 1], so it needs no
    normalisation and matches the score the GUI reports.
    """
    by_group: dict[str, list[int]] = {g: [] for g in GROUPS}
    for idx, group in enumerate(order, start=1):
        by_group[group].append(idx)
    all_positions = list(range(1, len(order) + 1))
    grand_mean = statistics.fmean(all_positions)
    ss_total = sum((p - grand_mean) ** 2 for p in all_positions)
    ss_between = sum(
        len(positions) * (statistics.fmean(positions) - grand_mean) ** 2 for positions in by_group.values() if positions
    )
    return ss_between / ss_total if ss_total else 0.0


def main() -> int:
    base = _submission_queue()

    from qg.randomize import randomize_plate_queue

    orders: dict[str, list[str]] = {}
    for mode in MODES:
        random.seed(SEED)  # qg.randomize draws from the global RNG when none is passed
        orders[mode] = _group_order(randomize_plate_queue(base, mode))

    eta2 = {mode: _eta_squared(orders[mode]) for mode in MODES}

    fig, axes = plt.subplots(
        len(MODES),
        1,
        figsize=(7.0, 5.4),
        dpi=300,
        sharex=True,
    )

    for ax, mode in zip(axes, MODES, strict=True):
        order = orders[mode]
        positions = range(1, N + 1)
        colors = [GROUP_COLORS[g] for g in order]
        # One tall bar per acquisition position, coloured by group: reads as a
        # condition strip across the run.
        ax.bar(positions, [1] * N, width=1.0, color=colors, edgecolor="white", linewidth=0.3)
        ax.set_ylim(0, 1)
        ax.set_yticks([])
        ax.set_xlim(0.5, N + 0.5)
        ax.set_title(
            f"{MODE_TITLES[mode]}    |    $\\eta^2$ = {eta2[mode]:.2f}",
            fontsize=8.5,
            loc="left",
            pad=3,
        )
        for spine in ("top", "right", "left"):
            ax.spines[spine].set_visible(False)

    axes[-1].set_xlabel(f"acquisition order (injection 1 to {N})")
    axes[-1].set_xticks([1, 14, 28, 42, N])

    legend_handles = [
        Patch(facecolor=GROUP_COLORS[g], edgecolor="white", label=f"group {g} (n={GROUP_SIZES[g]})") for g in GROUPS
    ]
    fig.legend(
        handles=legend_handles,
        ncol=len(GROUPS),
        loc="lower center",
        bbox_to_anchor=(0.5, -0.02),
        frameon=False,
        fontsize=8,
    )
    fig.suptitle(
        "Blocked randomization decorrelates biological group from acquisition position",
        fontsize=10,
        fontweight="bold",
        y=0.99,
    )
    fig.tight_layout(rect=(0, 0.05, 1, 0.96))

    OUT.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(OUT, dpi=600, bbox_inches="tight")
    plt.close(fig)

    print(f"Wrote {OUT}")
    for mode in MODES:
        print(f"  {mode:>16}: eta^2 = {eta2[mode]:.3f}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
