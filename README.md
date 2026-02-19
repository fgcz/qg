# Queue Generation System

Generate sample queues with QC injections for mass spectrometry instruments (XCalibur, Chronos, Hystar).

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
| Sampler | Instruments |
|---------|-------------|
| Vanquish | ASTRAL_1, EXPLORIS_3/4/5, QEXACTIVEHF_2, QUANTIVA_1 |
| MClass | ASCEND_1, EXPLORIS_1/2, LUMOS_2, QEXACTIVE_1 |
| Evosep | ASTRAL_1, EXPLORIS_1/2, TIMSTOF_1, TIMSTOFFLEX_1 |

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

## Deployment

All apps are deployed on `fgcz-r-039` in `/scratch/A401_queue_gen`.

| App | Description | Compose file | Port | Make target |
|-----|-------------|-------------|------|-------------|
| Queue app | B-Fabric authenticated queue generation GUI | `docker-compose-test.yml` | 9505 | `deploy-app` |
| Config editor | Edit config files with GitLab review/merge-request workflow | `docker-compose-editor.yml` | 9506 | `deploy-editor` |

### How it works

Both apps are served by uvicorn (FastAPI + B-Fabric auth + marimo ASGI). They run continuously — when a user connects from B-Fabric, they hit the already-running server.

**Queue app:** Code and configs are baked into the Docker image. To pick up config changes merged on GitLab, rebuild the image: `make deploy-app`.

**Config editor:** Code and configs are baked into the image. The only external mount is `~/.qg_settings.toml` (GitLab API credentials for the review/merge-request workflow). The editor reads configs from the image, diffs changes in memory, and submits them to GitLab via the Commits API — it never writes to disk.

### First-time setup

```bash
ssh bfabric@localhost
mkdir -p /scratch/A401_queue_gen
cd /scratch/A401_queue_gen
git clone https://gitlab.bfabric.org/metabolomics/queue-gen.git .
```

**Config editor only** — create `~/.qg_settings.toml` with GitLab credentials:

```bash
cp .qg_settings.toml.example ~/.qg_settings.toml
# Edit ~/.qg_settings.toml and set your private_token
```

The file must contain:

```toml
[gitlab]
url = "https://gitlab.bfabric.org"
project = "metabolomics/queue-gen"
# Token needs scope: api (or write_repository + read_api)
private_token = "glpat-xxxxxxxxxxxxxxxxxxxx"
```

The token can also be overridden via the `QG_GITLAB_TOKEN` environment variable.

### Deploy / update

```bash
ssh bfabric@localhost
cd /scratch/A401_queue_gen
git pull
# Queue app (port 9505)
make deploy-app
# Config editor (port 9506)
make deploy-editor
```

### Verify

```bash
curl -k https://localhost:9505    # Queue app
curl -k https://localhost:9506    # Config editor
```
