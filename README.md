# Queue Generation System

Generate sample queues with QC injections for mass spectrometry instruments (XCalibur, Chronos, Hystar).

`qg` runs in two modes:

- **Local / standalone** — upload a CSV/XLSX sample table in the GUI (or pass a parameters JSON to the CLI), configure the queue, preview, and download. No FGCZ/B-Fabric required.
- **B-Fabric portal** — the FGCZ deployment: browse LIMS orders, load samples, and upload the queue as a workunit. Requires the `qg[bfabric]` extra.

## Installation

```bash
# Core: local app + CLI, no B-Fabric (works in a clean environment)
pip install qg                 # as a dependency
uv sync --no-group portal      # for development in this repo, B-Fabric-free

# Full: add the B-Fabric portal (auth, LIMS sample loading, workunit upload, GitLab launcher)
pip install 'qg[bfabric]'
uv sync                        # for development: installs the portal extra by default
```

The core install has no `bfabric`, `fastapi`, `starlette`, or `python-gitlab`
dependency — `import qg`, the local app, and the `qg` / `qg-validate` CLIs all
work without them.

## Quick Start

### Local app — no B-Fabric

```bash
make app-local
# or: qg-app-local
# or: uv run marimo run src/qg/apps/queue_app_local.py
```

Upload a sample table — ready-made examples (vial/plate, single- and
multi-project) live in [`docs/examples/`](docs/examples/) — pick the
instrument / sampler / pattern, preview, and download the queue plus its
parameters JSON. See [`docs/users/local_app.md`](docs/users/local_app.md).

### CLI

```bash
uv run qg config.json -o queue.csv   # generate a queue from a parameters JSON (stdout if no -o)
uv run qg-validate                   # validate the config files
```

### B-Fabric portal app (requires `qg[bfabric]`)

```bash
QG_ALLOW_UNAUTHENTICATED=1 uv run marimo run src/qg/apps/queue_app.py
```

The portal app fails closed without a B-Fabric-authenticated request.
`QG_ALLOW_UNAUTHENTICATED=1` bypasses auth for local dev and runs as an employee —
**never set it in production**. The deployed entry point is
`uv run python src/qg/apps/bfabric_app.py` (needs `WebappIntegrationSettings`:
`VALIDATION_BFABRIC_INSTANCE`, `SUPPORTED_BFABRIC_INSTANCES`,
`FEEDER_USER_CREDENTIALS`). See [`docs/users/user_modes.md`](docs/users/user_modes.md).

#### Seed the B-Fabric project cache (portal only)

- **Dev** (single instance, local `~/.bfabricpy.yml`): `uv run qg-find-projects`
- **Deployment** (all instances in `feeder_user_credentials`): `uv run qg-refresh-cache --all` — see [`docs/developers/deployment.md`](docs/developers/deployment.md)

Both write `bfabric_cache/<instance>/bfabric_container.csv`, which the portal app reads; its "Refresh Projects" button re-runs the dev-style write for the running instance. These commands require the `qg[bfabric]` extra.

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

