# Queue Generation System

Generate sample queues with QC injections for mass spectrometry instruments (XCalibur, Chronos, Hystar).

## Deployment information

The code is deployed on fgcz-r-039 in `/scratch/A401_queue_gen` as a test.

To deploy

```bash
ssh bfabric@localhost
cd /scratch/A401_queue_gen
make deploy-test
```

## Quick Start

### GUI (Marimo App)

```bash
uv run marimo run main.py
```

### CLI Tools

#### Generate Queue from Config JSON

```bash
# Output to stdout
uv run python qg_configs/queue_generator.py config.json

# Output to file
uv run python qg_configs/queue_generator.py config.json -o queue.csv
```

#### Batch Generate Config Files

```bash
# Generate configs for all orders in proteomics_projects.json
uv run python generate_all_configs.py --user cpanse

# Dry run
uv run python generate_all_configs.py --dry-run --limit 5

# Filter by technology
uv run python generate_all_configs.py --technology metabolomics --user cpanse
```

#### Validate Configs

```bash
uv run python qg_configs/validate_config_pydantic.py
```

## Supported Configurations

### Technologies
- Proteomics
- Metabolomics (with pos/neg polarity)
- Lipidomics (with pos/neg polarity)

### Samplers
| Sampler | Status | Instruments |
|---------|--------|-------------|
| Vanquish | ✅ Implemented | ASTRAL_1, EXPLORIS_3/4/5, QEXACTIVEHF_2, QUANTIVA_1 |
| MClass48 | ⚠️ Partial | ASCEND_1, EXPLORIS_1/2, LUMOS_2, QEXACTIVE_1 |
| Evosep | ⚠️ Partial | ASTRAL_1, EXPLORIS_1/2, TIMSTOF_1, TIMSTOFFLEX_1 |

### Output Formats
- XCalibur (`.csv`)
- Chronos (`.csv`)
- Hystar (`.csv`)

## Config JSON Format

```json
{
  "parameters": {
    "container_id": 37180,
    "technology": "proteomics",
    "instrument": "ASTRAL_1",
    "sampler": "Vanquish.vial",
    "software": "xcalibur",
    "pattern": "standard",
    "polarity": [],
    "date": "20260112",
    "user": "cpanse",
    "method": "",
    "randomization": false,
    "inj_vol_override": null
  },
  "samples": [
    {"Sample Name": "sample1", "Sample ID": 123456, "Tube ID": "37180/1"}
  ]
}
```

## Example Output

```csv
File Name,Path,Instrument Method,Position,Inj Vol,Sample Type,Sample Name
20260112_001_C37180_autoQC03dia,D:\Data2San\p37180\Proteomics\ASTRAL_1\cpanse_20260112,C:\Methods\Proteomics\ASTRAL_1\DIA_60min.meth,B:F8,1.0,QC,autoQC03dia
20260112_002_C37180_autoQC01,D:\Data2San\p37180\Proteomics\ASTRAL_1\cpanse_20260112,C:\Methods\Proteomics\ASTRAL_1\DIA_60min.meth,B:F9,2.0,QC,autoQC01
20260112_003_C37180_S852285_HeLa_10ng,D:\Data2San\p37180\Proteomics\ASTRAL_1\cpanse_20260112,C:\Methods\Proteomics\ASTRAL_1\DIA_60min.meth,Y:A1,2.0,Unknown,HeLa_10ng
```

## Configuration Files

Located in `qg_configs/`:

| File | Description |
|------|-------------|
| `instruments.csv` | Instruments with path templates and method files |
| `combinations.csv` | Valid instrument+sampler pairs |
| `instrument_patterns.csv` | QC patterns per instrument |
| `sampler.toml` | Physical sampler layouts |
| `samples.csv` | QC sample definitions |
| `queue_patterns.toml` | QC injection patterns |
| `qc_layouts.toml` | QC positions per sampler |
| `output_formats.toml` | Output column mappings |
| `methods/` | Method files per technology/instrument |
