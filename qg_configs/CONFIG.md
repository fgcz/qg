# Queue Generation Configuration

## Files

| File | Purpose |
|------|---------|
| **Data** | |
| `sampler.toml` | Physical sampler layout: positions, grids, output format |
| `qc_layouts.toml` | QC positions per sampler + area/workflow |
| `output_formats.toml` | Queue file formats: column mappings |
| `samples.toml` | QC sample definitions (name, injection volume) |
| `queue_patterns.toml` | Named patterns: start/middle/end sequences |
| `instruments.toml` | Per-instrument properties (area, methods, defaults) |
| `combinations.csv` | Valid instrument+sampler combinations |
| `methods/*.csv` | Available methods per instrument |
| **Docs** | |
| `ALGORITHM.md` | Queue generation pseudocode |
| `*.puml` | PlantUML diagrams (dependencies, flow) |

## How It Works

1. User selects: Area → Instrument → Sampler
2. `combinations.csv` validates the selection
3. `instruments.toml` lookup → `area`, `queue_pattern`, `methods_file`, defaults
4. `sampler.toml` lookup → physical layout, `sampler_base`, `output_format`
5. `qc_layouts.toml[sampler_base.area]` → QC positions for this workflow
6. `queue_patterns.toml` → start/middle/end sequences
7. `samples.toml` → QC sample definitions (name, inj_vol)
8. Build queue: START → [samples + MIDDLE every N] → END
9. Format output per `output_formats.toml`

See `ALGORITHM.md` for detailed pseudocode and `algorithm_flow.puml` for visual flow.

## sampler.toml

Defines physical sampler layout (QC positions are in `qc_layouts.toml`).

```toml
[Vanquish_Vial_XCaliburSII]
description = "Thermo Vanquish autosampler, vials, XCalibur SII control"
sampler_base = "Vanquish_Vial"    # links to qc_layouts.toml
container_type = "Vial"
software = "XCaliburSII"
output_format = "xcalibur"
position_source = "generated"
fill_order = "row_major"
position_format = "{plate}:{row}{col}"
plates = ["Y", "R", "B", "G"]
sample_rows = ["A", "B", "C", "D", "E"]
qc_row = "F"
cols = [1, 2, 3, 4, 5, 6, 7, 8, 9]
samples_per_plate = 45
qc_plate = "B"
```

**Key fields:**
- `sampler_base` - links to `qc_layouts.toml[sampler_base.area]`
- `position_source` - "generated" (Vial) or "input" (Plate)
- `fill_order` - "row_major", "col_major", or "sequential" (Evosep)

## qc_layouts.toml

QC positions per physical sampler + area/workflow.

```toml
[Vanquish_Vial.Proteomics]
QC01 = "B:F9"
QC03dia = "B:F8"
clean = "B:F7"

[Vanquish_Vial.Metabolomics]
blank = "B:F9"
pooledQC = "B:F8"
standard = "B:F7"
pooledQCDil1 = "B:F6"
# ...

[Evosep.Proteomics]
autoQC01 = { tray = 5, position_start = 1, position_end = 48 }
autoQC03dia = { tray = 5, position_start = 49, position_end = 96 }
clean = { tray = 6, position_start = 1, position_end = 96 }
```

**Key:** Same physical sampler, different QC layouts per workflow.

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

Defines QC samples referenced by patterns.

```toml
[samples.QC01]
sample_name = "autoQC01"
inj_vol = 2.0

[samples.clean]
sample_name = "clean"
inj_vol = 2.0

[samples.pooledQC]
sample_name = "pooledQC"
inj_vol = 3.5
```

## queue_patterns.toml

Named patterns with start/middle/end sequences.

```toml
[patterns.proteomics_standard]
description = "Standard proteomics pattern"
run_QC_after_n_samples = 16
randomization = "none"  # none | full | block | stratified
random_seed = null      # optional, for reproducibility
start = ["QC03dia", "QC01"]
middle = ["clean", "QC01"]
end = ["clean", "QC01", "QC03dia", "clean"]
```

Queue structure: `start` → samples → `middle` (every N) → samples → `end`

## Instrument Configuration

**`instruments.toml`** - static properties per instrument:
```toml
[ASTRAL_1]
area = "Proteomics"
methods_file = "methods/ASTRAL_1_methods.csv"
default_inj_vol = 2.0
queue_pattern = "proteomics_standard"
```

**`combinations.csv`** - valid instrument+sampler pairs:
```csv
instrument,sampler
ASTRAL_1,Vanquish_Vial_XCaliburSII
ASTRAL_1,Vanquish_Plate_XCaliburSII
ASTRAL_1,Evosep_Vial_Chronos
```

## Adding New Configurations

**New sampler:** Add section to `sampler.toml` with all required fields (container_type, software, output_format, position_format, plates, rows, cols, qc_positions).

**New QC sample:** Add to `samples.toml`.

**New pattern:** Add to `queue_patterns.toml`.

**New instrument:**
1. Add section to `instruments.toml` (area, defaults, queue_pattern)
2. Add valid sampler rows to `combinations.csv`
3. Create `methods/<INSTRUMENT>_methods.csv` if needed
