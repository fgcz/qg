# CLAUDE.md - Queue Generation System

## Project Overview

Queue generation system for mass spectrometry instruments. Generates sample queues with QC injections for XCalibur, Chronos, and Hystar software.

## Terminology

This project distinguishes between two types of data:

| Term | Description | Location |
|------|-------------|----------|
| **Configs** | Static, user-editable files defining system behavior (instruments, samplers, patterns, QC samples) | `qg_configs/*.csv`, `qg_configs/*.toml` |
| **Queue Parameters** | Generated JSON files specifying a particular queue run (instrument, sampler, pattern, date, samples) | `test_data/queue_params/`, GUI output |

**Configs** are reusable definitions that rarely change. **Queue parameters** are generated per-run and serve as input to `queue_generator.py`.

## Current Status

**Phase: Working Prototype**
- Marimo GUI app (`main.py`) for interactive queue generation
- CLI tools for batch generation and validation
- Vanquish sampler fully implemented (vial + plate modes)
- MClass48 and Evosep samplers defined but position generators not yet implemented

## Directory Structure

```
qg_dump/
├── main.py                    # Marimo app (GUI for queue generation)
├── proteomics_projects.json   # B-Fabric project/order data
│
├── src/qg/                    # Core library
│   ├── config.py              # Config loading (ConfigBundle)
│   └── generator.py           # Queue generation (QueueParameters)
│
├── qg_configs/                # Static configuration files
│   ├── sampler.toml           # Physical sampler layouts
│   ├── samples.csv            # QC sample definitions
│   ├── queue_patterns.toml    # Injection patterns
│   ├── qc_layouts.toml        # QC positions per sampler
│   ├── instruments.csv        # Instruments + path templates
│   ├── combinations.csv       # Valid instrument+sampler pairs
│   ├── instrument_patterns.csv # Patterns per instrument
│   ├── output_formats.toml    # Queue file formats (xcalibur, chronos, hystar)
│   └── methods/               # Per-technology/instrument method CSVs
│
└── test_data/                 # Test data and validation infrastructure
    ├── Snakefile              # Combined workflow orchestration
    ├── README.md              # Test documentation
    ├── reference/             # Ground truth data from instruments
    │   ├── sld/               # Raw Thermo SLD binaries by instrument
    │   ├── csv/               # Parsed reference CSVs by instrument
    │   └── merged.csv         # All instruments combined
    ├── queue_params/          # Test queue parameter JSONs by instrument
    ├── generated/             # Queue generator output
    ├── results/               # Comparison results and summary
    └── tools/                 # Processing scripts
        ├── generate_queue_params.py  # Batch queue params generator
        ├── sld_to_csv.py      # SLD → CSV converter
        ├── csv_to_params.py   # CSV → queue params JSON generator
        ├── compare_queues.py  # Compare generated vs reference
        ├── summarize.py       # Aggregate test results
        ├── queue_analysis_merge.py   # Merge CSVs for analysis
        └── queue_analysis_marimo.py  # Interactive analysis app
```

## CLI Tools

### 1. Queue Generator (`src/qg/` or direct script)
Generates queue CSV files from queue parameters JSON input.

```bash
# Generate queue to stdout
uv run python -m qg.generator input.json

# Generate queue to file
uv run python -m qg.generator input.json -o output.csv
```

### 2. Batch Queue Params Generator (`test_data/tools/generate_queue_params.py`)
Generates queue parameter JSON files for all valid combinations (for testing).

```bash
# Generate all queue params
uv run python test_data/tools/generate_queue_params.py --user cpanse

# Filter by technology
uv run python test_data/tools/generate_queue_params.py --technology proteomics

# Dry run (show what would be generated)
uv run python test_data/tools/generate_queue_params.py --dry-run

# Limit to N orders (for testing)
uv run python test_data/tools/generate_queue_params.py --limit 5
```

### 3. Config Validator
Validates all static configuration files.

```bash
uv run python -m qg.validate
```

### 4. Marimo Apps
```bash
# Main queue generation GUI
uv run marimo run main.py
```

## Queue Parameters JSON Structure

Input format for queue generation (produced by GUI or batch generator):

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
    {"Sample Name": "sample1", "Sample ID": 123456, "Tube ID": "37180/1"},
    ...
  ]
}
```

## Sampler Implementation Status

| Sampler | Vial Mode | Plate Mode | Output Format |
|---------|-----------|------------|---------------|
| Vanquish | Working | Working | xcalibur |
| MClass48 | Not implemented | Input positions | xcalibur |
| Evosep | Not implemented | Input positions | chronos/hystar |

## Technologies

- `proteomics` - No polarity
- `metabolomics` - Polarity expansion (pos/neg)
- `lipidomics` - Polarity expansion (pos/neg)

## Architectural Goal: Stateless Functional Pipeline

The queue generator should be refactored into a pipeline of **stateless pure functions**. Each function:

1. **Receives all required inputs as arguments** - no hidden dependencies or global state
2. **Returns a result** - no side effects, no mutation of inputs
3. **Does exactly one thing** - clear, single responsibility
4. **Takes only what it needs** - minimal interface, no excess data

### Target Pipeline Steps

```
QueueInput (JSON)
    |
    v
+-----------------------------------------------------------------+
| 1. validate_input(input, configs) -> ValidatedInput             |
|    - Validate parameters against configs                        |
|    - Resolve sampler/pattern/layout references                  |
+-----------------------------------------------------------------+
    |
    v
+-----------------------------------------------------------------+
| 2. build_queue_structure(num_samples, pattern) -> list[SlotType]|
|    - Pure logic: interleave QC slots with user slots            |
|    - No sample data, just structure                             |
+-----------------------------------------------------------------+
    |
    v
+-----------------------------------------------------------------+
| 3. assign_positions(structure, qc_layout, sampler) -> list[Pos] |
|    - Map each slot to a physical position                       |
|    - Stateless position generation                              |
+-----------------------------------------------------------------+
    |
    v
+-----------------------------------------------------------------+
| 4. expand_polarities(structure, polarities) -> list[SlotType]   |
|    - For metabolomics/lipidomics: duplicate rows per polarity   |
+-----------------------------------------------------------------+
    |
    v
+-----------------------------------------------------------------+
| 5. populate_rows(structure, samples, positions, params, ...)    |
|    -> list[QueueRow]                                            |
|    - Fill in sample names, methods, file names, etc.            |
+-----------------------------------------------------------------+
    |
    v
+-----------------------------------------------------------------+
| 6. format_output(rows, output_format) -> str                    |
|    - Convert to CSV with correct column mapping                 |
+-----------------------------------------------------------------+
    |
    v
CSV Output
```

### Implementation Status

| Aspect | Status | Implementation |
|--------|--------|----------------|
| Queue structure | Done | `build_queue_structure(n, pattern) -> list[str]` |
| Position generation | Done | `VanquishSampler.generate_positions(n) -> list[str]` (stateless) |
| Position assignment | Done | `assign_positions(structure, user_positions, qc_layout) -> list[str]` |
| Row population | Done | `_populate_queue()` - no position logic, just assembly |
| Config loading | TODO | Still in `QueueGenerator.__init__`, should be `ConfigBundle` |
| Method lookup | TODO | Still uses internal cache, should be pure function |

### Key Files

- `src/qg/structure.py` - Queue structure building: `build_queue_structure()`, `compute_queue_counts()`
- `src/qg/positions.py` - Sampler classes with `generate_positions(n)` method
- `src/qg/generator.py` - Pipeline orchestration: `assign_positions()`, `QueueGenerator`

### Design Principles

- **Configs as data**: Load once, pass explicitly to functions that need them
- **No hidden state**: Position counters, caches, etc. become explicit inputs
- **Composable steps**: Each step can be tested and reasoned about independently
- **Explicit dependencies**: Function signatures document what data flows where

## Related Projects

- `/Users/wolski/projects/queue/qg/` - Legacy R implementation (reference only)
- `/Users/wolski/projects/queue/bfabricShiny/` - B-Fabric Shiny apps
