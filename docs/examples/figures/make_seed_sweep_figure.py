#!/usr/bin/env python3
"""SI figure: randomization quality across many seeds, with a second metric.

The main-text randomization figure shows a single seed. A methods reviewer rightly
asks whether one number per mode is representative. This script re-runs each of
``qg``'s four randomization modes over many independent seeds on the *same*
unbalanced 56-sample design (A x 24, B x 14, C x 10, D x 8) and reports the
*distribution* of two complementary quality metrics:

* ``eta^2`` -- share of acquisition-position variance explained by biological
  group (global group<->time aliasing; lower is better). Blind to local
  clustering.
* ``max consecutive run length`` -- the longest stretch of identical consecutive
  group labels (local clustering; lower is better). ``eta^2`` is blind to this,
  so the two metrics together separate the modes cleanly.

Findings this reproduces:

* ``no`` is deterministic (a single point at eta^2 ~ 0.90).
* ``random`` decorrelates group from time *on average* (low mean eta^2) but has a
  wide seed-to-seed spread and a poor (high) max-run length: it leaves local
  clustering.
* ``blocked`` (RCBD) is *worse* than ``random`` on eta^2 for these unbalanced
  groups -- one member of each group per block exhausts the small groups first
  and leaves a majority-group tail -- and its max-run length is the worst.
* ``blocked_uniform`` is near-deterministic (only tie ordering varies) and
  dominates on *both* metrics: eta^2 ~ 0 and a max-run length of ~2.

The same RNG contract as the unit tests: an explicit ``random.Random(seed)`` is
passed per draw, so the sweep is fully reproducible.

Run (qg must be importable -- use the qg/ environment):

    cd qg && uv run --with matplotlib python \\
        ../qg_article/pipeline/scripts/make_seed_sweep_figure.py

Outputs:
    figures/fig_seed_sweep.png      two-panel distribution figure (SI)
    figures/seed_sweep_stats.csv    per-mode summary statistics (record)
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
from matplotlib.lines import Line2D

from qg.params_models import ContainerBatch, Plate, PlateCell, PlateQueue, VialSample
from qg.randomize import randomize_plate_queue

ROOT = Path(__file__).resolve().parent
OUT_FIG = ROOT / "fig_seed_sweep.png"
OUT_CSV = ROOT / "seed_sweep_stats.csv"

N_SEEDS = 2000
SEED_BASE = 1_000_000  # sweep uses SEED_BASE + i; independent of the display seed
DISPLAY_SEED = 20260112  # the single seed shown in the main-text figure

GROUPS = ["A", "B", "C", "D"]
GROUP_SIZES = {"A": 24, "B": 14, "C": 10, "D": 8}  # unbalanced on purpose
N = sum(GROUP_SIZES.values())  # 56
PLATE_ID = 1
CONTAINER_ID = 37180

MODES = ("no", "random", "blocked", "blocked_uniform")
MODE_LABELS = {
    "no": "no",
    "random": "random",
    "blocked": "blocked\n(RCBD)",
    "blocked_uniform": "blocked_\nuniform",
}
MODE_COLORS = {
    "no": "#9C9C9C",
    "random": "#E45756",
    "blocked": "#F2A900",
    "blocked_uniform": "#4C78A8",
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
            cells.append(PlateCell(sample=sample, position=pos, grid_position=f"A{pos}", plate_id=PLATE_ID))
            pos += 1
    return PlateQueue(
        batches={CONTAINER_ID: ContainerBatch(container_id=CONTAINER_ID, container_name="Cohort")},
        plates={PLATE_ID: Plate(plate_id=PLATE_ID, tray="Y", nr_samples=N)},
        cells=cells,
    )


def _group_order(queue: PlateQueue) -> list[str]:
    return [c.sample.grouping_var or "?" for c in queue.cells]


def _eta_squared(order: list[str]) -> float:
    """Share of acquisition-position variance explained by biological group."""
    by_group: dict[str, list[int]] = {}
    for idx, group in enumerate(order, start=1):
        by_group.setdefault(group, []).append(idx)
    all_positions = list(range(1, len(order) + 1))
    grand_mean = statistics.fmean(all_positions)
    ss_total = sum((p - grand_mean) ** 2 for p in all_positions)
    ss_between = sum(
        len(pos) * (statistics.fmean(pos) - grand_mean) ** 2 for pos in by_group.values() if pos
    )
    return ss_between / ss_total if ss_total else 0.0


def _max_run_length(order: list[str]) -> int:
    """Longest stretch of identical consecutive group labels (local clustering)."""
    return max(sum(1 for _ in grp) for _, grp in groupby(order))


def _sweep(base: PlateQueue) -> dict[str, dict[str, list[float]]]:
    """For each mode, eta^2 and max-run over N_SEEDS independent seeds."""
    out: dict[str, dict[str, list[float]]] = {m: {"eta2": [], "maxrun": []} for m in MODES}
    for mode in MODES:
        if mode == "no":  # deterministic: one representative draw
            order = _group_order(randomize_plate_queue(base, mode, random.Random(0)))
            out[mode]["eta2"].append(_eta_squared(order))
            out[mode]["maxrun"].append(float(_max_run_length(order)))
            continue
        for i in range(N_SEEDS):
            rng = random.Random(SEED_BASE + i)
            order = _group_order(randomize_plate_queue(base, mode, rng))
            out[mode]["eta2"].append(_eta_squared(order))
            out[mode]["maxrun"].append(float(_max_run_length(order)))
    return out


def _summary(values: list[float]) -> dict[str, float]:
    qs = statistics.quantiles(values, n=4) if len(values) >= 2 else [values[0]] * 3
    return {
        "mean": statistics.fmean(values),
        "median": statistics.median(values),
        "q1": qs[0],
        "q3": qs[2],
        "min": min(values),
        "max": max(values),
    }


def _box(ax, data_by_mode: dict[str, list[float]], display_by_mode: dict[str, float], ylabel: str) -> None:
    positions = range(len(MODES))
    bp = ax.boxplot(
        [data_by_mode[m] for m in MODES],
        positions=list(positions),
        widths=0.6,
        showfliers=False,
        patch_artist=True,
        medianprops={"color": "black", "linewidth": 1.1},
        whiskerprops={"color": "#555555"},
        capprops={"color": "#555555"},
    )
    for patch, mode in zip(bp["boxes"], MODES, strict=True):
        patch.set_facecolor(MODE_COLORS[mode])
        patch.set_alpha(0.55)
        patch.set_edgecolor("#555555")
    # Mark the single display seed used by the main-text figure.
    for x, mode in zip(positions, MODES, strict=True):
        ax.plot(
            x, display_by_mode[mode], marker="D", color="black", markersize=4.5,
            markeredgecolor="white", markeredgewidth=0.6, zorder=5,
        )
    ax.set_xticks(list(positions))
    ax.set_xticklabels([MODE_LABELS[m] for m in MODES])
    ax.set_ylabel(ylabel)
    for spine in ("top", "right"):
        ax.spines[spine].set_visible(False)


def main() -> int:
    base = _submission_queue()
    sweep = _sweep(base)

    # The single-seed values shown in the main-text figure (DISPLAY_SEED).
    display: dict[str, dict[str, float]] = {}
    for mode in MODES:
        rng = random.Random(0) if mode == "no" else random.Random(DISPLAY_SEED)
        order = _group_order(randomize_plate_queue(base, mode, rng))
        display[mode] = {"eta2": _eta_squared(order), "maxrun": float(_max_run_length(order))}

    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(7.0, 3.2), dpi=300)
    _box(ax1, {m: sweep[m]["eta2"] for m in MODES}, {m: display[m]["eta2"] for m in MODES},
         r"$\eta^2$ (group $\leftrightarrow$ time)")
    ax1.set_title("Global aliasing (lower is better)", fontsize=8.5, loc="left")
    _box(ax2, {m: sweep[m]["maxrun"] for m in MODES}, {m: display[m]["maxrun"] for m in MODES},
         "max consecutive run length")
    ax2.set_title("Local clustering (lower is better)", fontsize=8.5, loc="left")

    fig.legend(
        handles=[Line2D([0], [0], marker="D", linestyle="none", markerfacecolor="black",
                        markeredgecolor="white", markersize=6, label="single display seed (main-text figure)")],
        loc="lower center", bbox_to_anchor=(0.5, -0.04), frameon=False, fontsize=7.5,
    )
    fig.suptitle(
        f"Randomization quality over {N_SEEDS} seeds (unbalanced 56-sample design)",
        fontsize=10, fontweight="bold", y=1.0,
    )
    fig.tight_layout(rect=(0, 0.04, 1, 0.95))
    OUT_FIG.parent.mkdir(parents=True, exist_ok=True)
    fig.savefig(OUT_FIG, dpi=600, bbox_inches="tight")
    plt.close(fig)

    # Summary table -> CSV + stdout.
    rows = []
    print(f"\n{N_SEEDS} seeds, design A24/B14/C10/D8 (N={N})\n")
    print(f"{'mode':>16} {'metric':>8} {'mean':>8} {'median':>8} {'IQR':>16} {'display':>8}")
    for mode in MODES:
        for metric, fmt in (("eta2", "{:.3f}"), ("maxrun", "{:.2f}")):
            s = _summary(sweep[mode][metric])
            disp = display[mode][metric]
            iqr = f"[{fmt.format(s['q1'])}, {fmt.format(s['q3'])}]"
            print(f"{mode:>16} {metric:>8} {fmt.format(s['mean']):>8} {fmt.format(s['median']):>8} {iqr:>16} {fmt.format(disp):>8}")
            rows.append({
                "mode": mode, "metric": metric, "n_seeds": (1 if mode == "no" else N_SEEDS),
                "mean": round(s["mean"], 4), "median": round(s["median"], 4),
                "q1": round(s["q1"], 4), "q3": round(s["q3"], 4),
                "min": round(s["min"], 4), "max": round(s["max"], 4),
                "display_seed_value": round(disp, 4),
            })
    with OUT_CSV.open("w", newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=list(rows[0].keys()))
        w.writeheader()
        w.writerows(rows)
    print(f"\nWrote {OUT_FIG}\nWrote {OUT_CSV}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
