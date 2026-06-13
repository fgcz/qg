---
name: qg-config-authoring
description: Use when adding to or editing the qg queue-generation configuration under qg_configs/ — standing up a new technology, adding an instrument, adding a plate layout with its QC samples and a queue pattern, or adding/adjusting an output format. Triggers when the user mentions qg_configs, instruments.csv, queue_patterns.toml, plate_layouts.toml, qc_layouts, samples.csv, output_formats.toml, adding an instrument / technology / QC sample / plate layout, or keeping the config files consistent. Explains which files each change touches and the cross-file rules qg_configuration() enforces at load.
---

# Authoring qg configuration

`qg` is configuration-driven: technologies, instruments, samplers, plate layouts,
QC samples, patterns, and output formats are all rows in CSVs or tables in TOML
files under `qg_configs/`. Adding any of them is a config edit, not a code change.
This skill is the playbook for the recurring edits and — crucially — the cross-file
dependencies between them.

## How to make a change (and the one rule that matters)

The config files are **not** cross-validated incrementally as you edit them. They
are validated as a set, at load, by `qg_configuration()`
(`src/qg/config_models/loader.py:664`), which reads all twelve config files and then
runs the cross-reference validators (`loader.py:122-261`). A mismatch surfaces there
as a `ConfigValidationError` — **except** `ui/instrument_config.csv`, which is not
cross-validated and instead fails later as a runtime lookup in the app. So:

1. Make the edits across **all** the files a change touches (the stories below list them).
2. Run `uv run qg-validate` (add `-c <dir>` for a non-default config dir). This loads
   and cross-validates everything; a clean exit means the set is consistent.
3. Keep `ui/instrument_config.csv` in sync by hand — `qg-validate` will not catch a
   missing or mismatched row there.

**The consistency rule — the QC-sample → reserved-position cascade.** Every QC sample
id a queue pattern references (in its `start` / `middle` / `end` / `separation` /
`middle_extended` lists) must:

- exist in `core/structure/samples.csv` for the **same** `tech_area`, and
- have a **reserved position** in a QC layout
  (`core/position/qc_layouts_well.csv` for well samplers, `qc_layouts_tip.csv` for tip
  samplers) for the `(tech_area, qc_layout_name, plate_layout)` the queue uses.

This is why "add a QC sample to a pattern" is never a one-file edit: it cascades into
the QC layout, and a new layout into the plate-layout and sampler-mapping files. The
validators that enforce it are `_validate_pattern_sample_refs`,
`_validate_layout_sample_refs`, and `_validate_pattern_layout_compatibility`
(`loader.py:122-261`).

For the full field-by-field schema of each file, see
[docs/reference/config.md](../../../docs/reference/config.md). For the instrument-row
specifics, see "Adding a New Instrument" in [AGENTS.md](../../../AGENTS.md).

---

## Story 1 — Stand up a new technology end-to-end

This is the spine: every other story is a slice of it. The worked example below is a
complete, minimal technology `Testing_v3` (one instrument, the reused Vanquish well
sampler, the reused `xcalibur` output format, a new 12-well plate layout, three sample
types, one QC pattern). It is exactly what `tests/test_config_authoring.py` builds and
asserts is loadable, valid, and usable — copy it and adapt.

Author the files in this order (technology → plate layout → pattern):

**1. Declare the technology** — `qg_configs/ui/tech_area_defaults.toml`:

```toml
[Testing_v3]
default_user = ""
default_polarities = ["pos"]
bfabric_areas = []
```

**2. Define the sample types** — append to `qg_configs/core/structure/samples.csv`
(header: `tech_area,sample_id,sample_name,sample_type,level,description,inj_vol,file_name_template`):

```csv
Testing_v3,default,"",Unknown,,Default Testing_v3 user sample,2.0,{date}_{run}_C{container}_S{sample_id}_{sample_name}
Testing_v3,QCv3,QCv3,QC,,Testing_v3 QC sample,2.0,{date}_{run}_C{container}_{sample_name}
Testing_v3,cleanv3,cleanv3,Blank,,Testing_v3 column cleanup blank,2.0,{date}_{run}_C{container}_{sample_name}
```

Every technology needs a `default` row (the user-sample template). QC and blank rows
are the injections a pattern can reference.

**3. Add the instrument + its methods file.** Append to
`qg_configs/core/formatting/instruments.csv`
(header: `tech_area,instrument,methods_file,path_template`):

```csv
Testing_v3,TESTING_V3_1,methods/Testing_v3/TESTING_V3_1_methods.csv,D:\Data2San\p{container}\Testing_v3\TESTING_V3_1\{user}_{date}
```

Create the file the `methods_file` column points at,
`qg_configs/core/methods/Testing_v3/TESTING_V3_1_methods.csv`
(header: `sample_type,polarity,method_name,method_path`):

```csv
default,pos,DDA,C:\Xcalibur\methods\
QCv3,pos,DDA,C:\Xcalibur\methods\__autoQC\QCv3
```

A single-polarity technology (`pos` only) needs just `pos` rows; metabolomics /
lipidomics need both `pos` and `neg` because of polarity expansion. The `default` row
covers user samples; add a per-QC row for any QC sample placed in the queue.

**4. Add the plate layout + sampler mapping.** Append to
`qg_configs/core/position/plate_layouts.toml`:

```toml
[TestingV3_12]
rows = ["A", "B"]
cols = [1, 2, 3, 4, 5, 6]
```

Map it to a sampler + queue type in
`qg_configs/core/position/sampler_plate_layouts.csv`
(header: `sampler,plate_layout,queue_type`):

```csv
Vanquish,TestingV3_12,Vial
```

(Reusing the existing `Vanquish` sampler — no `sampler.toml` edit needed. Adding a new
sampler is a separate, larger change.)

**5. Reserve QC positions** for the layout — append to
`qg_configs/core/position/qc_layouts_well.csv`
(header: `tech_area,qc_layout_name,plate_layout,sample_id,tray,row,col`):

```csv
Testing_v3,v3std,TestingV3_12,QCv3,B,B,6
```

`row` must be one of the layout's `rows` and `col` one of its `cols`. This is the
reserved-position half of the cascade rule.

**6. Create the pattern** that uses those QC samples — append to
`qg_configs/core/structure/queue_patterns.toml`:

```toml
[Testing_v3.simple]
description = "Testing_v3 simple pattern: QC at start and end"
run_QC_after_n_samples = 4
start = ["QCv3"]
middle = []
end = ["QCv3"]
```

Reference **only** sample ids defined for `Testing_v3` in `samples.csv`, and make sure
each appears in the QC layout from step 5.

**7. Expose it in the UI** — append to `qg_configs/ui/instrument_config.csv`
(header: `tech_area,instrument,sampler,output_format,default_pattern`):

```csv
Testing_v3,TESTING_V3_1,Vanquish,xcalibur,simple
```

`sampler`, `output_format`, and `default_pattern` must all exist (in `sampler.toml`,
`output_formats.toml`, and `queue_patterns.toml` respectively). This file is **not**
cross-validated at load — double-check it manually.

Then validate: `uv run qg-validate`. A clean exit means the technology is consistent
and queue generation will resolve it.

---

## Story 2 — Add an instrument to an existing technology

Three files, and they must agree (this is step 3 of Story 1, plus the UI row):

- `qg_configs/core/formatting/instruments.csv` — a row whose `methods_file` points at…
- `qg_configs/core/methods/<Technology>/<Instrument>_methods.csv` — the methods file you create.
- `qg_configs/ui/instrument_config.csv` — one row per (instrument, sampler) pairing so
  the instrument appears in the app menu.

The `instruments.csv` `methods_file` path and the actual file must match exactly;
`MethodsConfig.load()` discovers methods files from these rows. See "Adding a New
Instrument" in [AGENTS.md](../../../AGENTS.md) for the full checklist.

---

## Story 3 — Add a plate layout, its QC samples, and a pattern

This is the cascade in concrete form. Touches:

- `qg_configs/core/position/plate_layouts.toml` — the geometry (`rows`, `cols`).
- `qg_configs/core/position/sampler_plate_layouts.csv` — map the layout to a sampler + `queue_type`.
- `qg_configs/core/structure/samples.csv` — any **new** QC / blank sample types.
- `qg_configs/core/position/qc_layouts_well.csv` (well samplers) **or** `qc_layouts_tip.csv`
  (tip samplers, e.g. Evosep) — a reserved position for **every** QC sample the pattern
  will reference, for this `(tech_area, qc_layout_name, plate_layout)`.
- `qg_configs/core/structure/queue_patterns.toml` — the pattern, referencing only those
  QC sample ids.

If you add a QC sample to a pattern but forget its reserved position,
`_validate_pattern_layout_compatibility` fails at load with a message naming the
required sample ids. Add the position; don't remove the reference.

---

## Story 4 — Add or adjust an output format

One file: `qg_configs/core/formatting/output_formats.toml`. Each format is a base table
plus a `columns` sub-table mapping output column name → internal field:

```toml
[xcalibur]
description = "Thermo XCalibur format"
file_extension = ".csv"
writer = "xcalibur_csv"
position_format = "{tray}:{grid_position}"
grid_position_format = "{row},{col}"
grid_position_conversion = "identity"

[xcalibur.columns]
"File Name" = "file_name"
Path = "data_path"
Position = "position"
"Inj Vol" = "inj_vol"
"Sample ID" = "sample_id"
"Sample Name" = "sample_name"
"Instrument Method" = "method"
```

`writer` must be a known writer (`xcalibur_csv`, `chronos_csv`, `hystar_xml`). To add a
column for **one technology only**, use a per-tech overlay sub-table — it merges over
the base `columns`:

```toml
# Metabolomics-only extra columns, inherited base columns otherwise
[xcalibur_sii.columns.Metabolomics]
"Sample Type" = "sample_type"
"Level" = "level"
```

A literal value uses the `literal:` prefix (e.g. `"L3 Laboratory" = "literal:FGCZ"`).

---

## Verify your change

```bash
uv run qg-validate                 # loads + cross-validates the committed config
uv run qg-validate -c <config_dir> # …or a custom config directory
```

Then generate a sample queue to confirm it is usable end-to-end — either through the
app, or programmatically as `tests/test_config_authoring.py` does
(`qg_configuration(<dir>)` → `QueueGenerator(cfg, queue_input).generate()`), or via the
CLI: `uv run qg <input.json> -o <out.csv> -c <config_dir>`.

If you add a real (committed) technology or instrument, also follow the Release Process
in [AGENTS.md](../../../AGENTS.md): a `CHANGELOG.md` bullet and a version bump.
