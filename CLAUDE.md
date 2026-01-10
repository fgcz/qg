# CLAUDE.md - Queue Generation System

## Project Overview

Queue generation system for mass spectrometry instruments. Generates sample queues with QC injections for XCalibur, Chronos, and Hystar software.

## Current Status

**Phase: Configuration Design**
- Defining configuration file structure
- Python implementation planned (marimo app in `main.py`)
- R code in `/Users/wolski/projects/queue/qg/` is **legacy reference only**

## Directory Structure

```
qg_dump/
├── main.py              # Marimo app (GUI for queue generation)
├── qg_configs/          # Configuration files
│   ├── sampler.toml     # Physical sampler layouts
│   ├── samples.toml     # QC sample definitions: <technology>.<sample_id>
│   ├── queue_patterns.toml  # Injection patterns: <technology>.<pattern>
│   ├── qc_layouts.toml  # QC positions: <technology>.<sampler>
│   ├── instruments.toml # Instruments: <technology>.<instrument>
│   ├── output_formats.toml  # Queue file formats
│   ├── combinations.csv # Valid instrument+sampler pairs
│   └── methods/         # Per-technology/instrument method CSVs
│       ├── proteomics/
│       ├── metabolomics/
│       └── lipidomics/
├── queue_files/         # Generated queue files (output)
└── NOTES.md             # Analysis notes
```

## Configuration Structure

All config files use consistent `<technology>.<item>` hierarchical keys:

| File | Key Pattern | Example |
|------|-------------|---------|
| samples.toml | `[proteomics.QC01]` | QC sample definitions |
| queue_patterns.toml | `[proteomics.standard]` | Injection patterns |
| qc_layouts.toml | `[proteomics.Vanquish_Vial]` | QC positions |
| instruments.toml | `[proteomics.ASTRAL_1]` | Instrument configs |

Technologies: `proteomics`, `metabolomics`, `lipidomics`

See `qg_configs/CONFIG.md` for configuration details.

## Commands

```bash
uv run marimo edit main.py   # Run the marimo app
```

## Related Projects

- `/Users/wolski/projects/queue/qg/` - Legacy R implementation (reference only)
- `/Users/wolski/projects/queue/bfabricShiny/` - B-Fabric Shiny apps
