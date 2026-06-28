#!/usr/bin/env python3
"""SI figure: how the RCBD-vs-shuffle ordering depends on group imbalance.

The main text shows, on one unbalanced design (A x 24, B x 14, C x 10, D x 8),
that ``blocked`` (RCBD) can be *worse* than a plain ``random`` shuffle: one member
of each group per block exhausts the small groups first and leaves a tail of the
dominant group. A reviewer asks how general that is. This script sweeps the
imbalance of the design and reports where the effect appears.

Design: three minor groups fixed at 8 samples each (B, C, D); one dominant group A
grows from 8 (ratio 1, perfectly balanced) to 40 (ratio 5). The imbalance ratio is
``size(A) / size(minor) = size(A) / 8``. For each ratio and mode we average two
quality metrics over many seeds:

* ``eta^2`` -- group<->acquisition-time aliasing (lower is better);
* ``max consecutive run length`` -- local clustering (lower is better).

Expected and reproduced:

* ``random`` stays low and roughly flat in eta^2.
* ``blocked`` (RCBD) starts equal to ``blocked_uniform`` at ratio 1 (balanced) but
  its eta^2 climbs with imbalance and crosses *above* ``random`` -- the script
  prints the crossover ratio, which feeds the main-text wording
  ("RCBD underperforms a plain shuffle once imbalance exceeds ~X").
* ``blocked_uniform`` stays ~0 at every ratio: the fair-share rule removes the
  tail regardless of imbalance.

Run (qg must be importable -- use the qg/ environment):

    cd qg && uv run --with matplotlib python \\
        ../qg_article/pipeline/scripts/make_imbalance_sweep_figure.py

Outputs:
    figures/fig_imbalance_sweep.png   eta^2 and max-run vs imbalance ratio (SI)
    figures/imbalance_sweep_stats.csv per-ratio/per-mode means (record)
"""

from __future__ import annotations

import csv
import random
import statistics
from itertools import groupby
from pathlib import Path

import matplotlib

matplotlib.use("Agg")
import matplotlib.pyplot as plt

from qg.params_models import ContainerBatch, Plate, PlateCell, PlateQueue, VialSample
from qg.randomize import randomize_plate_queue

ROOT = Path(__file__).resolve().parent
OUT_FIG = ROOT / "fig_imbalance_sweep.png"
OUT_CSV = ROOT / "imbalance_sweep_stats.csv"

N_SEEDS = 500
SEED_BASE = 2_000_000
MINOR = 8  # fixed size of the three minor groups B, C, D
RATIOS = [1.0, 1.5, 2.0, 2.5, 3.0, 3.5, 4.0, 4.5, 5.0]  # size(A) / MINOR

MODES = ("random", "blocked", "blocked_uniform")
MODE_LABELS = {"random": "random", "blocked": "blocked (RCBD)", "blocked_uniform": "blocked_uniform"}
MODE_COLORS = {"random": "#E45756", "blocked": "#F2A900", "blocked_uniform": "#4C78A8"}
PLATE_ID = 1
CONTAINER_ID = 37180

plt.rcParams.update(
    {"font.family": "sans-serif", "font.sans-serif": ["Helvetica", "Arial", "DejaVu Sans"], "font.size": 8}
)


def _queue_for_ratio(ratio: float) -> PlateQueue:
    """4 groups: A = round(MINOR*ratio), B=C=D=MINOR."""
    sizes = {"A": round(MINOR * ratio), "B": MINOR, "C": MINOR, "D": MINOR}
    cells: list[PlateCell] = []
    pos = 1
    for group, n in sizes.items():
        for _ in range(n):
            sample = VialSample(sample_name=f"{group}{pos}", sample_id=pos, container_id=CONTAINER_ID, grouping_var=group)
            cells.append(PlateCell(sample=sample, position=pos, grid_position=f"A{pos}", plate_id=PLATE_ID))
            pos += 1
    total = sum(sizes.values())
    return PlateQueue(
        batches={CONTAINER_ID: ContainerBatch(container_id=CONTAINER_ID, container_name="Cohort")},
        plates={PLATE_ID: Plate(plate_id=PLATE_ID, tray="Y", nr_samples=total)},
        cells=cells,
    )


def _group_order(queue: PlateQueue) -> list[str]:
    return [c.sample.grouping_var or "?" for c in queue.cells]


def _eta_squared(order: list[str]) -> float:
    by_group: dict[str, list[int]] = {}
    for idx, group in enumerate(order, start=1):
        by_group.setdefault(group, []).append(idx)
    positions = list(range(1, len(order) + 1))
    grand = statistics.fmean(positions)
    ss_total = sum((p - grand) ** 2 for p in positions)
    ss_between = sum(len(p) * (statistics.fmean(p) - grand) ** 2 for p in by_group.values() if p)
    return ss_between / ss_total if ss_total else 0.0


def _max_run_length(order: list[str]) -> int:
    return max(sum(1 for _ in grp) for _, grp in groupby(order))


def main() -> int:
    # results[metric][mode] = list aligned with RATIOS
    results = {"eta2": {m: [] for m in MODES}, "maxrun": {m: [] for m in MODES}}
    rows = []
    for ratio in RATIOS:
        base = _queue_for_ratio(ratio)
        for mode in MODES:
            etas, runs = [], []
            for i in range(N_SEEDS):
                rng = random.Random(SEED_BASE + i)
                order = _group_order(randomize_plate_queue(base, mode, rng))
                etas.append(_eta_squared(order))
                runs.append(_max_run_length(order))
            me, mr = statistics.fmean(etas), statistics.fmean(runs)
            results["eta2"][mode].append(me)
            results["maxrun"][mode].append(mr)
            rows.append({"ratio": ratio, "size_A": round(MINOR * ratio), "mode": mode,
                         "mean_eta2": round(me, 4), "mean_maxrun": round(mr, 3)})

    # Crossover: smallest ratio at which blocked eta^2 exceeds random eta^2.
    crossover = next(
        (RATIOS[i] for i in range(len(RATIOS)) if results["eta2"]["blocked"][i] > results["eta2"]["random"][i]),
        None,
    )

    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(7.0, 3.2), dpi=300)
    for mode in MODES:
        ax1.plot(RATIOS, results["eta2"][mode], marker="o", markersize=3.5, color=MODE_COLORS[mode], label=MODE_LABELS[mode])
        ax2.plot(RATIOS, results["maxrun"][mode], marker="o", markersize=3.5, color=MODE_COLORS[mode], label=MODE_LABELS[mode])
    if crossover is not None:
        ax1.axvline(crossover, color="#888888", linestyle=":", linewidth=1)
        ax1.annotate(f"RCBD > random\nfrom ratio ≈ {crossover:g}", xy=(crossover, 0.0),
                     xytext=(crossover + 0.15, max(results["eta2"]["blocked"]) * 0.45),
                     fontsize=7, color="#555555")
    for ax, ylab, title in (
        (ax1, r"mean $\eta^2$ (group $\leftrightarrow$ time)", "Global aliasing"),
        (ax2, "mean max consecutive run length", "Local clustering"),
    ):
        ax.set_xlabel("imbalance ratio  size(A) / size(minor)")
        ax.set_ylabel(ylab)
        ax.set_title(f"{title} (lower is better)", fontsize=8.5, loc="left")
        for spine in ("top", "right"):
            ax.spines[spine].set_visible(False)
    ax1.legend(frameon=False, fontsize=7.5, loc="upper left")
    fig.suptitle(f"Effect of group imbalance ({N_SEEDS} seeds/point; B=C=D={MINOR})",
                 fontsize=10, fontweight="bold", y=1.0)
    fig.tight_layout(rect=(0, 0, 1, 0.95))
    OUT_FIG.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(OUT_FIG, dpi=600, bbox_inches="tight")
    plt.close(fig)

    with OUT_CSV.open("w", newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=list(rows[0].keys()))
        w.writeheader()
        w.writerows(rows)

    print(f"\nImbalance sweep ({N_SEEDS} seeds/point), minor groups = {MINOR}\n")
    print(f"{'ratio':>6} {'A':>4} {'random':>9} {'blocked':>9} {'blk_unif':>9}")
    for i, ratio in enumerate(RATIOS):
        print(f"{ratio:>6} {round(MINOR*ratio):>4} "
              f"{results['eta2']['random'][i]:>9.4f} {results['eta2']['blocked'][i]:>9.4f} {results['eta2']['blocked_uniform'][i]:>9.4f}")
    print(f"\nCrossover (blocked eta^2 first exceeds random): ratio = {crossover}")
    print(f"Wrote {OUT_FIG}\nWrote {OUT_CSV}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
