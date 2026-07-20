#!/usr/bin/env python3
"""Generate the two-panel queue figure for the manuscript (Figure ``fig:plate``).

Builds a representative multi-container Proteomics queue with the real `qg`
engine from ``docs/examples/multi_container_samples.csv``, then renders two
panels with the same Plotly figures the GUI's "Visualizations" tab produces:

* (A) the **plate layout**, colored by sample type (Sample / QC / Blank), and
* (B) the **acquisition-order timeline** ("queue view"), colored by biological
  group (``grouping_var``).

The two panels are exported separately via kaleido and stacked vertically into a
single PNG.

Run with kaleido + pillow available, e.g.::

    uv run --with kaleido --with pillow python scripts/make_plate_figure.py

Output: figures/fig_plate.png
"""

from __future__ import annotations

import tempfile
from datetime import date
from pathlib import Path

import polars as pl
from PIL import Image

from qg.config_models.loader import qg_configuration
from qg.generator import QueueGenerator
from qg.params_models import (
    ContainerBatch,
    QueueParameters,
    VialQueue,
    VialQueueInput,
    VialSample,
    current_qg_version,
)
from qg.viz.plate import build_plate_figure, build_plate_wells
from qg.viz.timeline import build_timeline_figure

HERE = Path(__file__).resolve().parent
SAMPLES_CSV = HERE.parent / "multi_container_samples.csv"  # qg/docs/examples/
OUT = HERE / "fig_plate.png"


def _queue_input(config) -> VialQueueInput:
    """Build the queue input from the shipped multi-container example table."""
    df = pl.read_csv(SAMPLES_CSV)
    params = QueueParameters(
        tech_area="Proteomics",
        instrument="ASTRAL_1",
        sampler="Vanquish",
        output_format="xcalibur",
        queue_pattern="standard",
        queue_type="Vial",
        plate_layout="Vanquish_54",
        qc_layout_name="standard",
        polarity=["pos"],
        date=date(2026, 1, 12).strftime("%Y%m%d"),
        user="cpanse",
        method={},
        randomization="blocked_uniform",
        seed=2026,  # fixed so the figure is reproducible across rebuilds
        inj_vol_override=None,
    )
    samples = [
        VialSample(
            sample_name=r["sample_name"],
            sample_id=int(r["sample_id"]),
            tube_id=r["tube_id"],
            container_id=int(r["container_id"]),
            grouping_var=r["grouping_var"],
        )
        for r in df.iter_rows(named=True)
    ]
    container_ids = df["container_id"].unique().sort().to_list()
    queue = VialQueue(
        batches={cid: ContainerBatch(container_id=cid, container_name=f"Order {cid}") for cid in container_ids},
        samples=samples,
    )
    return VialQueueInput(
        parameters=params,
        queue=queue,
        qg_version=current_qg_version(),
        resolved_config=config.subset_for(params),
    )


def _panel_title(text: str) -> dict:
    return {"text": text, "x": 0.0, "xanchor": "left", "font": {"size": 16}}


def main() -> int:
    config = qg_configuration()
    queue_input = _queue_input(config)
    order_ids = sorted(queue_input.queue.batches)
    rows = QueueGenerator(queue_input.position_queue()).build_rows().to_table()
    layout = config.plate_layouts.get_layout("Vanquish_54")

    # (A) Plate layout, colored by sample type.
    wells = build_plate_wells(rows)
    plate = build_plate_figure(wells, layout, orders=order_ids, cell=34, color_by="category")
    plate.update_layout(
        title=_panel_title("<b>(A)</b>  Plate layout — colored by sample type"),
        margin={"l": 40, "r": 15, "t": 46, "b": 40},
    )
    width = int(plate.layout.width)

    # (B) Acquisition-order timeline ("queue view"), colored by group.
    timeline = build_timeline_figure(rows, color_by="grouping_var")
    timeline.update_layout(
        title=_panel_title("<b>(B)</b>  Acquisition order (queue view) — colored by group"),
        width=width,
        height=240,
    )

    OUT.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory() as tmp:
        a_png = Path(tmp) / "a.png"
        b_png = Path(tmp) / "b.png"
        plate.write_image(str(a_png), scale=3)
        timeline.write_image(str(b_png), width=width, height=240, scale=3)
        a = Image.open(a_png)
        b = Image.open(b_png)
        canvas = Image.new("RGB", (max(a.width, b.width), a.height + b.height), "white")
        canvas.paste(a, ((canvas.width - a.width) // 2, 0))
        canvas.paste(b, ((canvas.width - b.width) // 2, a.height))
        canvas.save(OUT)

    n_user = rows.filter(pl.col("slot_kind") == "user").height
    print(
        f"Wrote {OUT} | {len(order_ids)} containers, {n_user} user samples, "
        f"{rows.height} injections total, {wells.height} occupied wells"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
