# Figure-reproduction scripts

These scripts regenerate the figures and GUI screenshots used in the *qg* JASMS
manuscript directly from the **public** `qg` engine, so every published figure is
reproducible from this repository alone (the manuscript repo is internal). Each
script writes its output PNG next to itself in this directory; the manuscript
build copies the PNGs it needs.

Dependencies live in the `docs` group (matplotlib, kaleido, pillow) and `dev`
group (playwright): `uv sync --group docs` (and `--group dev` for screenshots).

## Analytical figures (real `qg` engine)

| Script | Output | What it shows |
|--------|--------|---------------|
| `make_randomization_figure.py` | `fig_randomization.png` | Main-text randomization comparison (4 modes, one seed, η²). |
| `make_seed_sweep_figure.py` | `fig_seed_sweep.png` (+ `seed_sweep_stats.csv`) | Randomization quality over 2000 seeds (η² + max-run). |
| `make_imbalance_sweep_figure.py` | `fig_imbalance_sweep.png` (+ `imbalance_sweep_stats.csv`) | η² and max-run vs group-imbalance ratio (RCBD crossover). |
| `make_plate_figure.py` | `fig_plate.png` | Plate + acquisition-timeline panels via `qg.viz` (uses `../multi_container_samples.csv`). |

Run, e.g.:

```bash
uv run --group docs python docs/examples/figures/make_randomization_figure.py
```

## GUI screenshots (playwright against the local app)

`shot_editor.py`, `shot_local_tabs.py`, `shot_viz.py` drive the standalone local
app / config editor headlessly and capture the `*.png` screenshots used in the
Supporting Information.

| Script | Output | Notes |
|--------|--------|-------|
| `shot_viz.py` | `fig_lipidomics_queue.png`, `qg_gui_41786_visualizations_*.png` | **Self-contained** — spawns the local app itself. The SI "worked lipidomics queue" figure is the GUI's own *Acquisition Timeline* (QC-by-type colours + pos/neg polarity track), captured by loading the bundled `lipidomics_standard.json` in reproduce mode, so the figure and the live app cannot drift. |
| `shot_local_tabs.py`, `shot_editor.py` | `qg_gui_41786_*.png`, `editor_*.png` | Expect a **running** app launched with `QG_ALLOW_UNAUTHENTICATED=1`; use the bundled multi-container example (container `41786`). |

```bash
# Self-contained (no app needed):
uv run --group dev python docs/examples/figures/shot_viz.py

# Others: launch the app first, then run a shot script:
QG_ALLOW_UNAUTHENTICATED=1 uv run marimo run src/qg/apps/queue_app_local.py &
uv run --group dev python docs/examples/figures/shot_local_tabs.py
```
