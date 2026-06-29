# Queue Generation System

[![DOI](https://zenodo.org/badge/1280336047.svg)](https://doi.org/10.5281/zenodo.21040573)

Generate sample queues with QC injections for mass spectrometry instruments (XCalibur, Chronos, Hystar).

## ▶ Try the live demo

**[Launch the queue generator in your browser →](https://apps-dev.bfabric.org/queue-gen-local/)**

No install, no account. Upload a CSV/XLSX sample table (or load a bundled example),
pick the instrument / sampler / pattern, preview the queue, and download it.

---

`qg` is a standalone tool: upload a CSV/XLSX sample table in the GUI (or pass a
parameters JSON to the CLI), configure the queue, preview, and download — no
FGCZ/B-Fabric required. It also runs as the FGCZ **B-Fabric portal** app (LIMS order
browsing + workunit upload); see the [B-Fabric guide](docs/bfabric.md).

📖 **Documentation:** [**fgcz.github.io/qg**](https://fgcz.github.io/qg/) — local-app
and editor guides, the queue-generation algorithm, and the configuration reference.

## Installation

```bash
pip install qg                 # as a dependency
uv sync --no-group portal      # for development in this repo, B-Fabric-free
```

This installs the local app + the `qg` / `qg-validate` CLIs. The core install has no
`bfabric`, `fastapi`, `starlette`, or `python-gitlab` dependency — `import qg`, the
local app, and the CLIs all work without them. For the FGCZ B-Fabric portal,
install the `qg[bfabric]` extra — see the [B-Fabric guide](docs/bfabric.md).

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

> **Running at FGCZ?** The B-Fabric portal app, its `qg[bfabric]` install, project
> cache seeding, and deployment all live in the [B-Fabric guide](docs/bfabric.md).

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

