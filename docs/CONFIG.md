# Queue Generation Configuration

## Files

| File | Purpose |
|------|---------|
| **Data** | |
| `sampler.toml` | Physical sampler layout: positions, grids, output format |
| `qc_layouts.toml` | QC positions: `<technology>.<sampler_key>` |
| `output_formats.toml` | Queue file formats: column mappings |
| `samples.csv` | QC sample definitions (technology, sample_id, inj_vol, template) |
| `queue_patterns.toml` | Injection patterns: `<technology>.<pattern>` |
| `instruments.csv` | Instruments: technology, instrument, methods_file |
| `instrument_patterns.csv` | Available patterns per instrument |
| `combinations.csv` | Valid instrument+sampler+output_format+position_format |
| `methods/<tech>/<instr>_methods.csv` | Methods per instrument (with polarity column) |
| **Docs** | |
| `ALGORITHM.md` | Queue generation pseudocode |
| `*.puml` | PlantUML diagrams (ER, flow) |

## Configuration Structure

All config files use consistent `<technology>.<item>` hierarchical keys:

```
samples.toml:        [proteomics.QC01]
queue_patterns.toml: [proteomics.standard]
qc_layouts.toml:     [proteomics.MClass48]
instruments.toml:    [proteomics.ASTRAL_1]
```

Technologies: `proteomics`, `metabolomics`, `lipidomics`

## How It Works

1. User selects: Technology → Instrument → Sampler
2. `combinations.csv` validates the selection (e.g., `MClass48_XCaliburSII.vial`)
3. `instruments.toml[technology.instrument]` → `queue_patterns`, `methods_file`
4. `sampler.toml[sampler]` → physical layout, `output_format`
5. `qc_layouts.toml[technology.sampler_key]` → QC positions (tries `Parent.child`, falls back to `Parent`)
6. `queue_patterns.toml[technology.pattern]` → start/middle/end sequences
7. `samples.toml[technology.*]` → QC sample definitions (name, inj_vol)
8. Build queue: START → [samples + MIDDLE every N] → END
9. Format output per `output_formats.toml`

See `ALGORITHM.md` for detailed pseudocode.

## sampler.toml

Defines physical sampler layout. All samplers use nested `.vial`/`.plate` sub-tables.

```toml
# Vanquish: Vial and Plate have different physical layouts
[Vanquish_XCaliburSII]
description = "Thermo Vanquish autosampler, XCalibur SII control"
software = "XCaliburSII"
output_format = "xcalibur"
plates = ["Y", "R", "B", "G"]
qc_plate = "B"

[Vanquish_XCaliburSII.vial]
container_type = "Vial"
position_source = "generated"
fill_order = "row_major"
position_format = "{plate}:{row}{col}"
sample_rows = ["A", "B", "C", "D", "E"]
qc_row = "F"
cols = [1, 2, 3, 4, 5, 6, 7, 8, 9]
samples_per_plate = 45

[Vanquish_XCaliburSII.plate]
container_type = "Plate"
position_source = "input"
position_format = "{plate}:{grid_position}"
sample_rows = ["A", "B", "C", "D", "E", "F", "G"]
qc_row = "H"
cols = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
samples_per_plate = 84

# MClass48: Vial and Plate share same QC layout
[MClass48_XCaliburSII]
description = "Waters M-class 48-well, XCalibur SII control"
software = "XCaliburSII"
output_format = "xcalibur"
plates = ["1", "2"]
sample_rows = ["A", "B", "C", "D", "E"]
qc_row = "F"
cols = [1, 2, 3, 4, 5, 6, 7, 8]
samples_per_plate = 40
qc_plate = "1"

[MClass48_XCaliburSII.vial]
container_type = "Vial"
position_source = "generated"
fill_order = "row_major"
position_format = "{plate}:{row},{col}"

[MClass48_XCaliburSII.plate]
container_type = "Plate"
position_source = "input"
position_format = "{plate}:{grid_position}"
```

**Key fields:**
- `position_source` - "generated" (Vial) or "input" (Plate)
- `fill_order` - "row_major" (grid) or "sequential" (Evosep)

**QC layout lookup:** Code derives the key from the sampler path:
- Tries `qc_layouts[tech]["Parent.child"]` first (e.g., `"Vanquish.vial"`)
- Falls back to `qc_layouts[tech]["Parent"]` (e.g., `"MClass48"`)

## qc_layouts.toml

QC positions per technology + physical sampler base.

```toml
# Vanquish: separate Vial/Plate (different QC rows)
[proteomics.Vanquish.vial]
QC01 = "B:F9"
QC03dia = "B:F8"
clean = "B:F7"

[proteomics.Vanquish.plate]
QC01 = "B:H9"
QC03dia = "B:H10"
clean = "B:H1"

# MClass48: merged (same QC row for Vial and Plate)
[proteomics.MClass48]
QC01 = "1:F,8"
QC03dia = "1:F,7"
clean = "1:F,6"

[metabolomics.MClass48]
blank = "1:F,8"
pooledQC = "1:F,7"
108mix_AA = "1:F,6"
pooledQCDil1 = "1:F,5"
# ...

# Evosep: single entry (tray-based)
[proteomics.Evosep]
QC01 = { tray = 5, position_start = 1, position_end = 48 }
QC03dia = { tray = 5, position_start = 49, position_end = 96 }
clean = { tray = 6, position_start = 1, position_end = 96 }
```

## output_formats.toml

Defines queue file structure per software. Maps output column names to internal fields.

```toml
[xcalibur]
description = "Thermo XCalibur sequence format"
file_extension = ".csv"

[xcalibur.columns]
"File Name" = "file_name"
"Path" = "data_path"
"Instrument Method" = "method"
"Position" = "position"
"Inj Vol" = "inj_vol"
"Sample Type" = "sample_type"
"Sample Name" = "sample_name"

[chronos]
description = "Chronos autosampler format (Evosep)"
file_extension = ".csv"

[chronos.columns]
"Sample Name" = "sample_name"
"Source Vial" = "position"
"Source Tray" = "tray"
```

See ALGORITHM.md for internal field documentation.

## samples.toml

Defines QC samples per technology.

```toml
[proteomics.default]
description = "Default settings for user samples"
inj_vol = 2.0
file_name_template = "{date}_{run}_C{container}_{sample_name}"

[proteomics.QC01]
description = "autoQC01: Standard peptide mix"
sample_name = "autoQC01"
inj_vol = 2.0
file_name_template = "{date}_{run}_C{container}_autoQC01"

[proteomics.clean]
sample_name = "clean"
inj_vol = 2.0

[metabolomics.default]
inj_vol = 3.5
file_name_template = "{date}_{run}_C{container}_{sample_name}"

[metabolomics.pooledQC]
sample_name = "pooledQC"
inj_vol = 3.5
```

## queue_patterns.toml

Named patterns with start/middle/end sequences per technology.

```toml
[Proteomics.standard]
description = "Standard proteomics: clean-qc pairs"
run_QC_after_n_samples = 16
start = ["QC03dia", "QC01"]
middle = ["clean", "QC01"]
end = ["clean", "QC01", "QC03dia", "clean"]
separation = ["clean", "QC01", "clean"]  # Between project groups

[Metabolomics.standard]
description = "Standard metabolomics with dilution series"
run_QC_after_n_samples = 10
start = ["blank", "108mix_AA", "pooledQC", "blank", ...]
middle = ["blank", "108mix_AA", "pooledQC"]
separation = ["blank", "pooledQC"]  # Between project groups
middle_extended = ["pooledQCDil1", "pooledQCDil2", ...]
middle_extended_frequency_multiplier = 2
end = ["108mix_AA", "pooledQC", "blank"]
```

**Fields:**
- `run_QC_after_n_samples` - inject middle block after every N user samples
- `start`, `middle`, `end` - QC sample sequences
- `separation` - QC block inserted between project groups (defaults to `middle` if not set)
- `middle_extended` - extended QC block used at intervals
- `middle_extended_frequency_multiplier` - every Nth middle block uses extended

Queue structure: `start` → samples → `middle` (every N) → ... → `end`

Sample IDs in patterns reference samples from the same technology in `samples.csv`.

## instruments.toml

Instrument configs per technology.

```toml
[proteomics.ASTRAL_1]
methods_file = "methods/proteomics/ASTRAL_1_methods.csv"
queue_patterns = ["proteomics.standard", "proteomics.frequent", "proteomics.minimal"]

[metabolomics.EXPLORIS_3]
methods_file = "methods/metabolomics/EXPLORIS_3_methods.csv"
queue_patterns = ["metabolomics.standard", "metabolomics.simple"]

[lipidomics.EXPLORIS_3]
methods_file = "methods/lipidomics/EXPLORIS_3_methods.csv"
queue_patterns = ["lipidomics.standard"]
```

Same instrument can appear under multiple technologies with different configs.

**`combinations.csv`** - valid instrument+sampler+format combinations:
```csv
instrument,sampler,output_format,position_format
ASTRAL_1,Vanquish.vial,xcalibur,
ASTRAL_1,Vanquish.plate,xcalibur,
ASCEND_1,MClass48.vial,xcalibur,{plate}:{row}{col}
ASCEND_1,MClass48.plate,xcalibur,{plate}:{row}{col}
EXPLORIS_1,Evosep.vial,chronos,
```

**Fields:**
- `instrument` - instrument name (matches `instruments.csv`)
- `sampler` - sampler.container key (e.g., `Vanquish.vial`)
- `output_format` - output format (e.g., `xcalibur`, `chronos`, `hystar`)
- `position_format` - optional override for position formatting (empty = use sampler default)

## Adding New Configurations

**New technology:** Add sections to all config files under the new technology key.

**New sampler:**
- If Vial/Plate share same QC row: Add nested structure with `.vial`/`.plate` sub-tables
- If different: Add separate flat entries
- Add QC layouts to `qc_layouts.toml` for each technology

**New QC sample:** Add to `samples.csv` with the appropriate technology.

**New pattern:** Add to `queue_patterns.toml` under the appropriate technology.

**New instrument:**
1. Add row to `instruments.csv` for each technology it supports
2. Add rows to `instrument_patterns.csv` for available patterns
3. Add valid sampler rows to `combinations.csv`
4. Create `methods/<tech>/<INSTRUMENT>_methods.csv`

---

## Queue Parameters (JSON Input)

Queue generation takes JSON input with parameters and sample groups. This is the runtime input format (distinct from static config files).

### QueueInput Structure

```json
{
  "parameters": {
    "technology": "proteomics",
    "instrument": "ASTRAL_1",
    "sampler": "Vanquish.vial",
    "output_format": "xcalibur",
    "queue_pattern": "standard",
    "polarity": [],
    "date": "20260112",
    "user": "cpanse",
    "method": {},
    "randomization": false,
    "inj_vol_override": null,
    "qc_frequency_override": null
  },
  "sample_groups": [
    {
      "container_id": 37180,
      "group_name": null,
      "samples": [
        {"Sample Name": "S1", "Sample ID": 123456, "Tube ID": "37180/1"},
        {"Sample Name": "S2", "Sample ID": 123457, "Tube ID": "37180/2"}
      ]
    }
  ]
}
```

### QueueParameters Fields

| Field | Type | Description |
|-------|------|-------------|
| `technology` | string | Technology identifier (proteomics, metabolomics, lipidomics) |
| `instrument` | string | Instrument name |
| `sampler` | string | Sampler.container key (e.g., `Vanquish.vial`) |
| `output_format` | string | Output format (xcalibur, chronos, hystar) |
| `queue_pattern` | string | Pattern name (e.g., `standard`) |
| `polarity` | list | Empty for proteomics, `["pos", "neg"]` for metabolomics/lipidomics |
| `date` | string | Date in YYYYMMDD format |
| `user` | string | Username for output path |
| `method` | dict | Per-polarity methods: `{"pos": "Method_Pos", "neg": "Method_Neg"}` |
| `randomization` | bool | Whether to randomize sample order |
| `inj_vol_override` | float? | Override injection volume (null = use config) |
| `qc_frequency_override` | int? | Override run_QC_after_n_samples (null = use pattern) |

**Per-polarity method selection:**
For technologies requiring polarity (metabolomics, lipidomics), methods are specified per polarity:
```json
"method": {"pos": "Metabolomics_Pos", "neg": "Metabolomics_Neg"}
```
For proteomics (no polarity), use empty dict or single key.

### SampleGroup Fields

| Field | Type | Description |
|-------|------|-------------|
| `container_id` | int | B-Fabric container/order ID |
| `group_name` | string? | Optional display name (defaults to container_id) |
| `samples` | list | List of InputSample objects |

Multiple sample groups support multi-container queues with separation blocks between groups.

### InputSample Fields

| Field | Alias | Type | Description |
|-------|-------|------|-------------|
| `sample_name` | `Sample Name` | string | Sample display name |
| `sample_id` | `Sample ID` | int | B-Fabric sample ID |
| `tube_id` | `Tube ID` | string? | Tube identifier |
| `position` | `Position` | string? | Pre-assigned position (plate mode) |
| `grid_position` | `GridPosition` | string? | Grid position (plate mode) |
| `grouping_var` | `Grouping` | string? | Grouping variable for stratified randomization |
