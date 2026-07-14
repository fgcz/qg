# Queue Generation Configuration

Static configuration files live in `qg_configs/`, grouped by concern under
`core/` (required for queue generation) and `ui/` (used by the GUIs for
filtering/validation only).

> **Access rule:** application code never reads these files directly. All access
> goes through `QGConfiguration`, returned by
> `qg.config_models.loader.qg_configuration()`. Only `config_models/loader.py`
> (read) and `apps/config_editor.py` (edit) touch the files.

## Directory Structure

```
qg_configs/
├── core/
│   ├── structure/
│   │   ├── samples.csv             # QC sample definitions (per tech_area)
│   │   └── queue_patterns.toml     # QC injection patterns
│   ├── position/
│   │   ├── sampler.toml            # Physical sampler layouts
│   │   ├── plate_layouts.toml      # Plate layouts (rows × cols)
│   │   ├── sampler_plate_layouts.csv  # Sampler → plate-layout mapping
│   │   ├── qc_layouts_well.csv     # QC positions for well-plate samplers
│   │   └── qc_layouts_tip.csv      # QC tip ranges for tip-plate samplers
│   ├── formatting/
│   │   ├── instruments.csv         # Instrument → methods_file, path_template
│   │   └── output_formats.toml     # Output column mappings
│   └── methods/
│       ├── Proteomics/<INSTR>_methods.csv
│       ├── Metabolomics/<INSTR>_methods.csv
│       └── Lipidomics/<INSTR>_methods.csv
└── ui/
    └── instrument_config.csv       # Instrument defaults for the GUI
```

## Files at a glance

| File | Location | Purpose |
|------|----------|---------|
| `samples.csv` | `core/structure/` | QC sample definitions (sample_id, type, inj_vol, file-name template) |
| `queue_patterns.toml` | `core/structure/` | Injection patterns: `<TechArea>.<pattern>` |
| `sampler.toml` | `core/position/` | Physical sampler layout (well vs tip, trays) |
| `plate_layouts.toml` | `core/position/` | Plate layouts: rows × cols |
| `sampler_plate_layouts.csv` | `core/position/` | Which plate layouts a sampler supports, and their queue type (Vial/Plate) |
| `qc_layouts_well.csv` | `core/position/` | QC positions on well plates (row/col) |
| `qc_layouts_tip.csv` | `core/position/` | QC tip ranges on tip plates (position_start/end) |
| `instruments.csv` | `core/formatting/` | Instrument → `methods_file`, `path_template` |
| `output_formats.toml` | `core/formatting/` | Output column mappings per format |
| `methods/<Tech>/<instr>_methods.csv` | `core/methods/` | Methods per instrument (with polarity) |
| `instrument_config.csv` | `ui/` | GUI defaults: sampler, output_format, default_pattern |

Technologies (`tech_area`): `Proteomics`, `Metabolomics`, `Lipidomics`.

---

## core/structure/

### samples.csv

QC and default-sample definitions, keyed by `(tech_area, sample_id)`.

```csv
tech_area,sample_id,sample_name,sample_type,qc_class,level,description,inj_vol,file_name_template
Proteomics,default,"",Unknown,,,Default settings for user samples,2.0,{date}_{run}_C{container}_S{sample_id}_{sample_name}
Lipidomics,pooledQCDil2,pooledQCDil2,QC,QC dilution series,,"Pooled QC dilution 2 (QC2, 1:1 dilution of QC1)",3.0,{date}_{run}_C{container}_{sample_name}_{polarity}
```

| Column | Description |
|--------|-------------|
| `tech_area` | `Proteomics` / `Metabolomics` / `Lipidomics` |
| `sample_id` | Unique id within the tech_area (e.g. `QC01`, `clean`, `default`). No spaces. |
| `sample_name` | Display/file name (empty for the `default` user-sample row) |
| `sample_type` | Output sample type (`Unknown`, `QC`, `Blank`, `Std Bracket`, …) |
| `qc_class` | **Optional.** Display category that groups several QC `sample_id`s under one legend entry/colour in the acquisition-timeline visualization (e.g. `Pooled QC`, `QC dilution series`, `EquiSPLASH (IS)`). Display-only — never written to the instrument queue. Blank → falls back to `sample_type`. |
| `level` | Optional dilution level (used by `level_concentrations`) |
| `description` | Free text |
| `inj_vol` | Injection volume (µL) |
| `file_name_template` | Raw-file name template (see [file-name templates](algorithm.md)) |

The `default` row defines the settings applied to user samples.

### queue_patterns.toml

Named QC patterns keyed `<TechArea>.<pattern>`. Sample ids reference
`samples.csv` rows of the same tech_area.

```toml
[Proteomics.standard]
description = "Standard proteomics: clean-qc pairs, clean-qc-qc ending"
run_QC_after_n_samples = 8
start = ["QC02", "QC01"]
middle = ["clean", "QC01"]
end = ["clean", "QC01", "QC02", "clean"]
separation = ["clean", "QC01", "clean"]   # injected between project groups
```

| Field | Description |
|-------|-------------|
| `run_QC_after_n_samples` | Inject the `middle` block after every N user samples |
| `start` / `middle` / `end` | QC sample-id sequences |
| `separation` | QC block inserted between containers/groups (defaults to `middle`) |
| `middle_extended` | Extended QC block used at intervals (Metabolomics dilution series) |
| `middle_extended_frequency_multiplier` | Every Mth middle block uses `middle_extended` |

Queue structure: `start` → samples → `middle` (every N) → … → `end`.

---

## core/position/

### sampler.toml

One table per physical sampler. There are **no** `.vial`/`.plate` sub-tables —
Vial vs Plate is a property of the chosen plate layout (see
`sampler_plate_layouts.csv`), not the sampler.

```toml
[Vanquish]
description = "Thermo Vanquish autosampler"
sampler_type = "well"
trays = ["Y", "R", "G", "B"]
position_fun = "string_concat"

[Evosep]
description = "Evosep One autosampler"
sampler_type = "tip"
trays = [1, 2, 3, 4, 5, 6]
position_fun = "string_concat"
```

| Field | Description |
|-------|-------------|
| `sampler_type` | `well` (row/col plates) or `tip` (Evosep tip ranges) |
| `trays` | Tray/plate identifiers |
| `position_fun` | How row + col combine into a position (`string_concat` → `"A1"`) |

### plate_layouts.toml

Plate geometries (rows × cols), referenced by samplers and QC layouts.

```toml
[Vanquish_54]
rows = ["A", "B", "C", "D", "E", "F"]
cols = [1, 2, 3, 4, 5, 6, 7, 8, 9]

[Plate_96]
rows = ["A", "B", "C", "D", "E", "F", "G", "H"]
cols = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
```

### sampler_plate_layouts.csv

Maps each sampler to the plate layouts it supports and the resulting queue type.

```csv
sampler,plate_layout,queue_type
Vanquish,Vanquish_54,Vial
Vanquish,Plate_96,Plate
```

| Column | Description |
|--------|-------------|
| `sampler` | Sampler name (matches `sampler.toml`) |
| `plate_layout` | Plate layout name (matches `plate_layouts.toml`) |
| `queue_type` | `Vial` (positions generated) or `Plate` (positions from input) |

### qc_layouts_well.csv

QC positions on well-plate samplers, keyed by
`(tech_area, qc_layout_name, plate_layout, sample_id)`.

```csv
tech_area,qc_layout_name,plate_layout,sample_id,tray,row,col
Proteomics,standard,Vanquish_54,QC01,B,F,9
Proteomics,standard,Vanquish_54,QC02,B,F,8
```

### qc_layouts_tip.csv

QC tip ranges on tip-plate samplers (Evosep). A range spans tips from
`position_start` to `position_end`.

```csv
tech_area,qc_layout_name,plate_layout,sample_id,tray,position_start,position_end
Proteomics,evosep_qc,Plate_96,QC02,6,A1,D12
Proteomics,evosep_qc,Plate_96,clean,6,E1,H12
```

The `qc_layout_name` is selected per instrument via `instrument_config.csv`
(see below); it usually matches the pattern name.

Besides the layouts defined here, the queue apps offer a synthetic
**`no_layout`** option in both **Vial** and **Plate** mode, for tech areas that
opt in via `tech_area_defaults.allow_no_layout` (Proteomics opts out). It is
recognised in code — not defined in any CSV — and means *use the samples as-is*:
no QC layout, no QC injected, no wells reserved (so a plate or vial set that is
full of biological samples still queues). Selecting it forces an empty pattern
and hides the Pattern picker.

---

## core/formatting/

### instruments.csv

```csv
tech_area,instrument,methods_file,path_template
Proteomics,ASTRAL_1,methods/Proteomics/ASTRAL_1_methods.csv,D:\Data2San\p{container}\Proteomics\ASTRAL_1\{user}_{date}
```

| Column | Description |
|--------|-------------|
| `methods_file` | Path (relative to `core/`) to the instrument's methods CSV |
| `path_template` | Data-path template; `{container}`, `{user}`, `{date}` substituted at build time |

### output_formats.toml

Output column structure per format. Top-level keys: `xcalibur`, `xcalibur_sii`,
`chronos`, `hystar`. Each has a `.columns` table mapping output column → internal
field; some formats add per-tech_area overrides (e.g. `[xcalibur_sii.columns.Metabolomics]`).

```toml
[xcalibur_sii]
description = "Thermo XCalibur SII sequence format"
file_extension = ".csv"

[xcalibur_sii.columns]
"File Name" = "file_name"
"Path" = "data_path"
"Instrument Method" = "method"
"Position" = "position"
"Inj Vol" = "inj_vol"
"Sample Type" = "sample_type"
"Sample Name" = "sample_name"
```

### methods/&lt;Tech&gt;/&lt;instr&gt;_methods.csv

```csv
sample_type,polarity,method_name,method_path
default,pos,Metabolomics,C:\Methods\Metabolomics\EXPLORIS_3\Metabolomics_Pos.meth
```

| Column | Description |
|--------|-------------|
| `sample_type` | `default` (user samples) or a QC sample id |
| `polarity` | `pos` / `neg` / empty (proteomics) |
| `method_name` | Method identifier referenced from the parameters JSON `method` map |
| `method_path` | Absolute instrument-method path written to the queue |

---

## ui/

### instrument_config.csv

GUI defaults: which sampler/output_format/pattern to preselect per instrument.
This replaces the old `combinations.csv` + `instrument_patterns.csv`.

```csv
tech_area,instrument,sampler,output_format,default_pattern
Proteomics,ASTRAL_1,Vanquish,xcalibur_sii,standard
Proteomics,ASTRAL_1,Evosep,chronos,evosep_qc
```

---

## Adding new configurations

- **New QC sample:** add a row to `core/structure/samples.csv`, then a position
  in `qc_layouts_well.csv` (or `qc_layouts_tip.csv`) for each plate layout it
  appears on.
- **New pattern:** add a `[<TechArea>.<name>]` table to
  `core/structure/queue_patterns.toml`; reference only sample ids that exist for
  that tech_area.
- **New instrument:** add a row to `core/formatting/instruments.csv`, create
  `core/methods/<Tech>/<INSTR>_methods.csv`, and add the GUI default(s) to
  `ui/instrument_config.csv`.

Use the [config editor](../users/editor_guide.md) GUI rather than hand-editing where
possible — it validates cross-references before saving.

---

## Queue Parameters (JSON input)

The runtime input to queue generation (distinct from the static config files
above), defined in `src/qg/params_models.py`. A `QueueInput` is one of two
shapes — `VialQueueInput` or `PlateQueueInput` — each `{parameters, queue}`.
`read_queue_input()` selects plate vs vial by whether `queue` contains `plates`.
See [Algorithm](algorithm.md) for how each field flows through the pipeline.

### Vial input

```json
{
  "parameters": {
    "tech_area": "Proteomics",
    "instrument": "ASTRAL_1",
    "sampler": "Vanquish",
    "output_format": "xcalibur_sii",
    "queue_pattern": "standard",
    "queue_type": "Vial",
    "plate_layout": "Vanquish_54",
    "qc_layout_name": "standard",
    "polarity": [],
    "date": "20260112",
    "user": "cpanse",
    "method": {},
    "randomization": "no",
    "seed": null,
    "inj_vol_override": null,
    "qc_frequency_override": null,
    "one_container_per_tray": false,
    "start_position": "A1",
    "start_tray": "",
    "level_concentrations": {},
    "mark_end_of_queue": true
  },
  "queue": {
    "batches": {"37180": {"container_id": 37180, "container_name": "Project A"}},
    "samples": [
      {"sample_name": "S1", "sample_id": 123456, "tube_id": "37180/1", "container_id": 37180, "grouping_var": null}
    ]
  }
}
```

### Plate input

Same `parameters` (with `"queue_type": "Plate"` and a plate `plate_layout` such
as `Plate_96`), but `queue` carries `plates` and `cells`:

```json
{
  "parameters": { "...": "as above, queue_type=Plate, plate_layout=Plate_96" },
  "queue": {
    "batches": {"37180": {"container_id": 37180, "container_name": null}},
    "plates": {"1": {"plate_id": 1, "tray": "B", "nr_samples": 1}},
    "cells": [
      {
        "sample": {"sample_name": "S1", "sample_id": 123456, "tube_id": null, "container_id": 37180, "grouping_var": null},
        "position": 1, "grid_position": "A1", "plate_id": 1, "row": "A", "col": 1
      }
    ]
  }
}
```

### parameters (`QueueParameters`)

| Field | Type | Description |
|-------|------|-------------|
| `tech_area` | string | `Proteomics` / `Metabolomics` / `Lipidomics` (title-case) |
| `instrument` | string | Instrument name |
| `sampler` | string | Bare sampler name (e.g. `Vanquish`) |
| `output_format` | string | `xcalibur` / `xcalibur_sii` / `chronos` / `hystar` |
| `queue_pattern` | string | Pattern name (e.g. `standard`) |
| `queue_type` | `Vial` \| `Plate` | Selects vial vs plate position handling |
| `plate_layout` | string | Plate layout for the sampler/queue_type (e.g. `Vanquish_54`) |
| `qc_layout_name` | string | QC layout to use (from `qc_layouts_well/tip.csv`), or `no_layout` (Vial/Plate, opt-in techs) for an as-is queue with no QC reserved or injected |
| `polarity` | list | `[]` for proteomics; `["pos", "neg"]` for metabolomics/lipidomics |
| `date` | string | `YYYYMMDD`; substituted into `path_template` and file names |
| `user` | string | Username; substituted into `path_template` |
| `method` | dict | Per-polarity method names: `{"pos": "...", "neg": "..."}` |
| `randomization` | string | `"no"` / `"random"` / `"blocked"` / `"blocked_uniform"` (see [Algorithm](algorithm.md)) |
| `seed` | int? | RNG seed for reproducible randomization. When null and a randomized mode is selected, a seed is drawn at generation and recorded back here. |
| `inj_vol_override` | float? | Override injection volume (null → use `samples.csv`) |
| `qc_frequency_override` | int? | Override pattern `run_QC_after_n_samples` (null → use pattern) |
| `one_container_per_tray` | bool | Place each container on its own tray (vial mode) |
| `start_position` | string | First grid position to assign (e.g. `A1`) |
| `start_tray` | string \| int | Tray to start from (`""` → first tray of the sampler) |
| `level_concentrations` | dict | Per-level concentrations for `standard`-type QC samples |
| `mark_end_of_queue` | bool | Append `_eoq` to the last file of each container subqueue |

`user` and `date` are **not** output columns — they are substituted into the
instrument's `path_template` (`…\{user}_{date}`) to form the per-row data path.

### queue (`VialQueue` / `PlateQueue`)

| Field | In | Description |
|-------|-----|-------------|
| `batches` | both | Map of `container_id` → `{container_id, container_name?}`. Multi-container support lives here, with a `separation` QC block inserted between containers. |
| `samples` | vial | List of `VialSample` (JSON alias `cells` also accepted) |
| `plates` | plate | Map of `plate_id` → `{plate_id, tray?, nr_samples}` |
| `cells` | plate | List of `PlateCell` (a `VialSample` plus `position`/`grid_position`/`plate_id`/`row`/`col`) |

### `VialSample` fields

| Field | Type | Description |
|-------|------|-------------|
| `sample_name` | string | Display name |
| `sample_id` | int | Source sample id (the B-Fabric sample id in portal mode; user-supplied in the local app) |
| `tube_id` | string? | Tube identifier |
| `container_id` | int | FK to the `batches` entry |
| `grouping_var` | string? | Grouping variable for `blocked` randomization |
