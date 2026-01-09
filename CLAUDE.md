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
├── instrument.csv       # Master instrument registry (used by marimo app)
├── qg_configs/          # Configuration files
│   ├── lc.toml          # LC systems: positions, grids
│   ├── samples.toml     # QC sample definitions
│   ├── queue_patterns.toml  # Named injection patterns
│   ├── instrument.csv   # Instrument → pattern mapping
│   └── methods/         # Per-instrument method CSVs
├── queue_files/         # Generated queue files (output)
└── NOTES.md             # Analysis notes
```

## Configuration Principles

1. **Minimal** - 4 config files + method CSVs
2. **DRY** - No duplication; LC defines positions, patterns reference samples
3. **Flexible** - Add new instruments/patterns without code changes

See `qg_configs/CONFIG.md` for configuration details.

## Commands

```bash
uv run marimo edit main.py   # Run the marimo app
```

## Related Projects

- `/Users/wolski/projects/queue/qg/` - Legacy R implementation (reference only)
- `/Users/wolski/projects/queue/bfabricShiny/` - B-Fabric Shiny apps
