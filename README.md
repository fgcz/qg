# Queue Generation System

Generate sample queues with QC injections for mass spectrometry instruments (XCalibur, Chronos, Hystar).

## Quick Start

### GUI (Marimo App)

```bash
QG_ALLOW_UNAUTHENTICATED=1 uv run marimo run src/qg/apps/queue_app.py
```

The app fails closed without a B-Fabric-authenticated request. `QG_ALLOW_UNAUTHENTICATED=1` bypasses auth for local dev and runs as an employee — **never set it in production**. See `docs/user_modes.md` for details.

### GUI with B-Fabric Auth

```bash
uv run python src/qg/apps/bfabric_app.py
```

Requires B-Fabric integration config (`WebappIntegrationSettings`): `VALIDATION_BFABRIC_INSTANCE`, `SUPPORTED_BFABRIC_INSTANCES`, `FEEDER_USER_CREDENTIALS`. On the deployment server these are already configured.

### CLI Tools

#### Generate Queue from Config JSON

```bash
# Output to stdout
uv run qg config.json

# Output to file
uv run qg config.json -o queue.csv
```

#### Validate Configs

```bash
uv run qg-validate
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

Both deployments run on `fgcz-r-039` as the `bfabric` user. See [`docs/deployment.md`](docs/deployment.md) for the full reference (architecture, mounts, first-time setup).

> **Security:** never set `QG_ALLOW_UNAUTHENTICATED=1` on a deployment host — it disables auth and runs every request as an employee.

### Production (queue app only)

Push a Git tag on `main` (locally, or via the GitLab UI under *Repository → Tags*). GitLab CI builds the OCI image. Then bump the pinned image tag in `portal/queue-gen/docker-compose.prod.yml` in the [web-apps repo](https://gitlab.bfabric.org/proteomics/web-apps), commit, and redeploy:

```bash
ssh bfabric@localhost
cd ~/web-apps/portal/queue-gen && git pull && make deploy
```

The config editor is not in the production deployment — it currently runs from the test deployment below.

### Test deployment

Built locally on the server from this repo, used to verify a branch before tagging.

| App | Compose file | Port | Make target |
|-----|--------------|------|-------------|
| Queue app | `docker-compose-test.yml` | 9505 | `deploy-app` |
| Config editor | `docker-compose-editor.yml` | 9506 | `deploy-editor` |

```bash
ssh bfabric@localhost
cd /scratch/A401_queue_gen && git pull
make deploy-app     # or deploy-editor
```
