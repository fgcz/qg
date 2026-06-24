# Local app (no B-Fabric)

The **standalone** queue app lets you generate an instrument queue from a sample
table you upload — no FGCZ/B-Fabric account, no LIMS. It shares the full
generation pipeline, QC patterns, randomization, and visualizations with the
portal app; only the sample *source* (a file upload instead of a B-Fabric order)
and the *output* (a local download instead of a workunit upload) differ.

## Launch

```bash
make app-local
# or
qg-app-local
# or
uv run marimo run src/qg/apps/queue_app_local.py
```

No `QG_ALLOW_UNAUTHENTICATED` and no B-Fabric packages are needed — this works on
a core `pip install qg`.

## 1. Upload a sample table — or load a bundled example

Drop a **`.csv`** or **`.xlsx`** file on the landing page, **or** pick one from the
**bundled-example dropdown** (single-project vial/plate, an 80-sample run, and a
3-project multi-container set) to load it directly — no file hunting needed. A
**download** button next to the dropdown saves the selected example as an editable
template for your own data. An uploaded file takes precedence over a selected
example, and the page shows which source is active. The same examples also live in
[`docs/examples/`](https://gitlab.bfabric.org/metabolomics/queue-gen/-/tree/main/docs/examples)
for browsing.

The mode is inferred from the columns — **plate** when both `plate_id` and
`grid_position` are present, otherwise **vial**.

**Vial** — required `sample_name`, `sample_id`, `container_id`; optional
`tube_id`, `grouping_var`.

**Plate** — required `sample_name`, `sample_id`, `container_id`, `plate_id`,
`grid_position`; optional `position`, `tray`, `grouping_var`.

Header spellings such as `Sample Name`, `Sample ID`, `Tube ID`, `Container ID`
(and the B-Fabric export names) are accepted and normalized; unknown columns are
ignored. `sample_id` must be unique whole numbers. `container_id` is a generic
group id — samples are grouped and shuffled within each `container_id`, and a
`separation` QC block is inserted between containers, so a single upload can span
several projects.

Clear errors are shown for unreadable files, unknown extensions, missing required
columns, and duplicate `sample_id`s.

## 2. Configure and generate

Use the sidebar exactly as in the portal app: Tech Area → Instrument → Sampler →
Queue Type → Plate Layout → QC Layout → Pattern, plus polarity, methods,
randomization, date, user, injection volume, and QC frequency. The **Sample
Selection** tab lets you uncheck samples or reorder them.

- **Randomization** (`no` / `random` / `blocked` / `blocked_uniform`) shuffles
  user samples within each container. Set a `seed` (in a downloaded parameters
  JSON) to reproduce a run; otherwise one is drawn and recorded.
- The **Visualizations** tab shows the plate layout and the acquisition-order
  timeline, each annotated with an η² **balance score** (0 = group evenly
  interleaved, 1 = fully separated). Uploads with a `grouping_var` make this
  meaningful; ungrouped tables show `N/A`.

## 3. Download

On the **Queue Preview** tab the **Download Queue File** button is enabled as soon
as a queue is generated (no upload step). The **Parameters** tab offers
**Download Params JSON** — keep it for reproducibility; feeding it back through the
CLI (`qg params.json -o queue.csv`) regenerates the same queue.

## Differences from the portal app

| | Local app | Portal app |
|--|-----------|------------|
| Sample source | CSV/XLSX upload | B-Fabric order browser |
| Auth | none | B-Fabric session |
| Output | direct download | workunit upload (download gated behind it) |
| Install | core `qg` | `qg[bfabric]` |
