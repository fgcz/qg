# Example sample tables

Ready-to-upload sample tables for the **standalone local queue app**
(`make app-local`, `qg-app-local`, or
`uv run marimo run src/qg/apps/queue_app_local.py`). Upload one on the app's
landing page, configure the queue, then preview/download — no B-Fabric needed.

You don't have to download these first: the local app's landing page has a
**bundled-example dropdown** that loads a vial or plate example straight into the
session, plus a button to **download** the selected table as an editable template
for your own data.

!!! note "Authoritative copy lives in the package"
    These files mirror `src/qg/examples/sample_tables/`, which is the
    authoritative copy shipped in the wheel (so `qg-app-local` finds them after
    `pip install qg`). A byte-equality test keeps the two locations identical —
    when adding or editing an example, change **both**.

Most files are in blocked submission order (groups in contiguous blocks), so they
are useful for exercising the randomization modes and the **η² balance score** in
the *Visualizations* tab (compare `no` vs `blocked`/`random`).

| File | Mode | Contents |
|------|------|----------|
| `vial_samples_5x5.csv` | vial | 5 groups (A–E) × 5 = 25 |
| `vial_samples_5x5.xlsx` | vial | same data, demonstrates `.xlsx` upload |
| `plate_samples_5x5.csv` | plate | 25 samples, 96-well positions A1…C1 |
| `vial_samples_80.csv` | vial | larger run: 80 samples, 8 groups (A–H) × 10, interleaved so every group appears on both generated trays |
| `plate_samples_80.csv` | plate | larger run: 80 samples split across two 96-well plates/trays, with each group on both plates |
| `multi_container_samples.csv` | vial | **3 containers/projects** in one upload — `50001`: 8 samples in two groups (case/control); `50002`: 6 ungrouped samples; `50003`: 16 samples in four groups (g1–g4) |

The multi-container file shows how a single upload spans several projects: the app
groups by `container_id`, shuffles only within each project, and inserts a
separation block between them. Container `50002` (no `grouping_var`) shows the
`N/A` balance score and sample-type-only colouring.

## Column schema

The parser normalizes common header spellings (e.g. `Sample Name` → `sample_name`,
`Sample ID` → `sample_id`, plus B-Fabric export names) and keeps only the columns
below; anything else is ignored. Mode is **plate** when both `plate_id` and
`grid_position` are present, otherwise **vial**.

**Vial** — required: `sample_name`, `sample_id`, `container_id`.
Optional: `tube_id`, `grouping_var`.

**Plate** — required: `sample_name`, `sample_id`, `container_id`, `plate_id`,
`grid_position`. Optional: `position`, `tray` (the app's *Start tray* supplies it
when blank), `grouping_var`.

Notes:
- `sample_id` must be unique whole numbers; `container_id` is a generic
  order/group id (samples are grouped and shuffled within each `container_id`).
- `grouping_var` is the blocking/colouring variable used by `blocked` /
  `blocked_uniform` randomization and the balance score.
- Plate mode needs **both** `plate_id` and `grid_position`. A table with only
  one of them (e.g. a B-Fabric export carrying `_position` but no
  `_gridposition`) is parsed as **vial** — no error is raised, so double-check
  the previewed mode if you expected a plate queue.

Regenerate or adapt these with any spreadsheet/CSV editor — they are plain tables.
