# Queue Generation System

Generate sample queues with QC injections for mass spectrometry instruments (XCalibur, Chronos, Hystar).

## Quick Start

### GUI (Marimo App)

```bash
QG_ALLOW_UNAUTHENTICATED=1 uv run marimo run src/qg/apps/queue_app.py
```

The app fails closed without a B-Fabric-authenticated request. `QG_ALLOW_UNAUTHENTICATED=1` bypasses auth for local dev and runs as an employee — **never set it in production**. See `docs/users/user_modes.md` for details.

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

#### Seed B-Fabric Project Cache

- **Dev** (single instance, local `~/.bfabricpy.yml`): `uv run qg-find-projects`
- **Deployment** (all instances in `feeder_user_credentials`): `uv run qg-refresh-cache --all` — see [`docs/developers/deployment.md`](docs/developers/deployment.md)

Both write `bfabric_cache/<instance>/bfabric_container.csv`, which the GUI reads; its "Refresh Projects" button re-runs the dev-style write for the running instance.

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
- XCalibur (`.csv`, `xcalibur` / `xcalibur_sii`)
- Chronos (`.csv`)
- Hystar (`.xml`)

## Queue Parameters JSON

Queue generation takes a JSON file with a `parameters` object (instrument,
sampler, output format, pattern, …) and a nested `queue` object (`batches` plus
`samples`, or `plates`/`cells` for plate input). The canonical schema and
field-by-field reference live in one place:
**[docs/reference/config.md](docs/reference/config.md#queue-parameters-json-input)**.

## Example Output

```csv
File Name,Path,Instrument Method,Position,Inj Vol,Sample Type,Sample Name
20260112_001_C37180_autoQC03dia,D:\Data2San\p37180\Proteomics\ASTRAL_1\cpanse_20260112,C:\Methods\Proteomics\ASTRAL_1\DIA_60min.meth,B:F8,1.0,QC,autoQC03dia
20260112_002_C37180_autoQC01,D:\Data2San\p37180\Proteomics\ASTRAL_1\cpanse_20260112,C:\Methods\Proteomics\ASTRAL_1\DIA_60min.meth,B:F9,2.0,QC,autoQC01
20260112_003_C37180_S852285_HeLa_10ng,D:\Data2San\p37180\Proteomics\ASTRAL_1\cpanse_20260112,C:\Methods\Proteomics\ASTRAL_1\DIA_60min.meth,Y:A1,2.0,Unknown,HeLa_10ng
```

## Configuration Files

Static config lives in `qg_configs/`, grouped under
`core/{structure,position,formatting,methods}/` and `ui/`. The per-file
reference (purpose, columns, examples) is maintained in one place:
**[docs/reference/config.md](docs/reference/config.md)**.

## Deployment

The queue app and config editor both run on `fgcz-r-039` (as the `bfabric` user)
from a single Docker image, deployed via the web-apps repo. The full procedure —
tag → CI image build, bumping `IMAGE_TAG`, redeploy, rollback, and secrets — is
maintained in one place: **[docs/developers/deployment.md](docs/developers/deployment.md)**.

> **Security:** never set `QG_ALLOW_UNAUTHENTICATED=1` on a deployment host — it disables auth and runs every request as an employee.

