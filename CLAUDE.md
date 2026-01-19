# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Queue generation system for mass spectrometry instruments. Generates sample queues with QC injections for XCalibur, Chronos, and Hystar software.

## Commands

```bash
# Run the GUI
uv run marimo run main.py

# Generate queue from JSON params
uv run qg input.json -o output.csv

# Validate config files
uv run qg-validate

# Run tests
uv run pytest

# Run single test
uv run pytest tests/test_file.py::test_name -v

# Validation pipeline (in test_data/)
cd test_data && snakemake -j4 all
```

## Terminology

| Term | Description | Location |
|------|-------------|----------|
| **Configs** | Static files defining system behavior (instruments, samplers, patterns, QC samples) | `qg_configs/*.csv`, `qg_configs/*.toml` |
| **Queue Parameters** | Runtime JSON specifying a queue run (instrument, sampler, samples) | GUI output, `test_data/params_json/` |

## Queue Parameters JSON Structure

```json
{
  "parameters": {
    "technology": "proteomics",
    "instrument": "ASTRAL_1",
    "sampler": "Vanquish.vial",
    "output_format": "xcalibur",
    "queue_pattern": "standard",
    "polarity": [],
    "date": "20260112",
    "user": "cpanse",
    "method": {},
    "randomization": false,
    "inj_vol_override": null,
    "qc_frequency_override": null
  },
  "sample_groups": [
    {
      "container_id": 37180,
      "samples": [
        {"Sample Name": "S1", "Sample ID": 123456, "Tube ID": "37180/1"}
      ]
    }
  ]
}
```

Key fields:
- `method`: `dict[str, str]` - per-polarity methods: `{"pos": "Method_Pos", "neg": "Method_Neg"}`
- `sample_groups`: multi-container support (container_id per group, not in parameters)
- `qc_frequency_override`: override pattern's `run_QC_after_n_samples`

## Architecture

### Pipeline (Stateless Functions)

```
QueueInput (JSON)
    ↓
1. validate_input(input, configs) → ValidatedInput
2. build_queue_structure(num_samples, pattern) → list[str]  # ["QC01", "default", ...]
3. generate_positions(num_samples, sampler) → list[str]     # ["Y:A1", "Y:A2", ...]
4. expand_polarities(structure, polarities) → list[tuple]   # metabolomics/lipidomics
5. populate_rows(...) → list[QueueRow]
6. format_output(rows, output_format) → CSV string
    ↓
CSV Output
```

### Key Modules

| Module | Purpose |
|--------|---------|
| `src/qg/generator.py` | Pipeline orchestration, `QueueGenerator` class |
| `src/qg/queue_structure.py` | `build_queue_structure()`, `compute_queue_counts()` |
| `src/qg/positions.py` | Sampler classes with `generate_positions(n)` |
| `src/qg/config.py` | `ConfigBundle` - loads all config files |
| `src/qg/params_models.py` | `QueueInput`, `QueueParameters`, `SampleGroup`, `InputSample` |
| `src/qg/config_models.py` | Pydantic models for config files |
| `src/qg/builder.py` | High-level queue building API |

### Config Files (`qg_configs/`)

| File | Purpose |
|------|---------|
| `sampler.toml` | Physical sampler layouts (plates, rows, cols) |
| `samples.csv` | QC sample definitions (inj_vol, file_name_template) |
| `queue_patterns.toml` | QC injection patterns (start/middle/end/separation) |
| `qc_layouts.toml` | QC positions per technology/sampler |
| `instruments.csv` | Instrument → methods_file mapping |
| `combinations.csv` | Valid (instrument, sampler, output_format, position_format) |
| `output_formats.toml` | Column mappings for xcalibur/chronos/hystar |
| `methods/<tech>/<instr>_methods.csv` | Methods with polarity column |

### Technologies

- `proteomics` - No polarity expansion
- `metabolomics` - Polarity expansion (pos/neg)
- `lipidomics` - Polarity expansion (pos/neg)

## Validation Testing (`test_data/`)

Snakemake workflow comparing generated queues against reference data from instruments:

```bash
cd test_data
snakemake -j4 all              # Full pipeline
snakemake marimo_orig_qgen     # View comparison results
snakemake clean_all            # Reset
```

Pipeline: SLD files → CSV → params JSON → generate queue → compare with original

## Related Projects

- `/Users/wolski/projects/queue/qg/` - Legacy R implementation (reference only)
