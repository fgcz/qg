# Queue Generation Configuration

## Files

| File | Purpose |
|------|---------|
| **Data** | |
| `sampler.toml` | Physical sampler layout: positions, grids, output format |
| `qc_layouts.toml` | QC positions: `<technology>.<sampler_key>` |
| `output_formats.toml` | Queue file formats: column mappings |
| `samples.toml` | QC sample definitions: `<technology>.<sample_id>` |
| `queue_patterns.toml` | Injection patterns: `<technology>.<pattern>` |
| `instruments.toml` | Instrument configs: `<technology>.<instrument>` |
| `combinations.csv` | Valid instrument+sampler combinations |
| `methods/<technology>/*.csv` | Available methods per technology/instrument |
| **Docs** | |
| `ALGORITHM.md` | Queue generation pseudocode |
| `*.puml` | PlantUML diagrams (dependencies, flow) |

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
[proteomics.standard]
description = "Standard proteomics pattern"
run_QC_after_n_samples = 16
start = ["QC03dia", "QC01"]
middle = ["clean", "QC01"]
end = ["clean", "QC01", "QC03dia", "clean"]

[metabolomics.standard]
description = "Standard metabolomics with dilution series"
run_QC_after_n_samples = 10
start = ["blank", "108mix_AA", "pooledQC", "blank", ...]
middle = ["blank", "108mix_AA", "pooledQC"]
end = ["108mix_AA", "pooledQC", "blank"]
```

Queue structure: `start` → samples → `middle` (every N) → samples → `end`

Sample IDs in patterns reference samples from the same technology in `samples.toml`.

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

**`combinations.csv`** - valid instrument+sampler pairs:
```csv
instrument,sampler
ASTRAL_1,Vanquish_XCaliburSII.vial
ASTRAL_1,Vanquish_XCaliburSII.plate
ASTRAL_1,Evosep_Chronos.vial
ASTRAL_1,Evosep_Chronos.plate
ASCEND_1,MClass48_XCaliburSII.vial
ASCEND_1,MClass48_XCaliburSII.plate
```

Note: Nested samplers use dot notation (e.g., `MClass48_XCaliburSII.vial`).

## Adding New Configurations

**New technology:** Add sections to all config files under the new technology key.

**New sampler:**
- If Vial/Plate share same QC row: Add nested structure with `.vial`/`.plate` sub-tables
- If different: Add separate flat entries
- Add QC layouts to `qc_layouts.toml` for each technology

**New QC sample:** Add to `samples.toml` under the appropriate technology.

**New pattern:** Add to `queue_patterns.toml` under the appropriate technology.

**New instrument:**
1. Add section to `instruments.toml` under each technology it supports
2. Add valid sampler rows to `combinations.csv` (use dot notation for nested samplers)
3. Create `methods/<technology>/<INSTRUMENT>_methods.csv`
