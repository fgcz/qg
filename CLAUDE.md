# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Queue generation system for mass spectrometry instruments. Generates sample queues with QC injections for XCalibur, Chronos, and Hystar software.

**Python version:** >=3.14

## Commands

```bash
# Run the GUI
uv run marimo run src/qg/apps/queue_app.py

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

### CLI Entry Points

| Command | Module | Purpose |
|---------|--------|---------|
| `qg` | `qg.cli.generate_queues` | Main queue generation from JSON params |
| `qg-validate` | `qg.cli.validate_config` | Validate config files |
| `qg-find-projects` | `qg.cli.find_projects` | Project discovery utility |
| `qg-params` | `qg.cli.generate_params` | Generate params JSON from CSV |
| `qg-tools` | `qg.tools.cli` | Tools CLI (SLD conversion, comparison) |

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
    |
1. validate_input(input, configs) -> ValidatedInput
2. build_queue_structure(num_samples, pattern) -> list[str]  # ["QC01", "default", ...]
3. generate_positions(num_samples, sampler) -> list[str]     # ["Y:A1", "Y:A2", ...]
4. expand_polarities(structure, polarities) -> list[tuple]   # metabolomics/lipidomics
5. populate_rows(...) -> list[QueueRow]
6. format_output(rows, output_format) -> CSV string
    |
CSV Output
```

### Key Modules (`src/qg/`)

| Module | Purpose |
|--------|---------|
| `generator.py` | Pipeline orchestration, `QueueGenerator` class, `QueueRow` dataclass |
| `builder.py` | `QueueGeneratorBuilder` - config resolution and generator creation |
| `queue_structure.py` | `build_queue_structure()`, `compute_queue_counts()`, `SlotEntry` |
| `positions.py` | `GridPositionGenerator`, `EvosepPositionGenerator`, factory function |
| `strategies.py` | `PositionAssigner` protocol, `GeneratedPositionAssigner`, `InputPositionAssigner` |
| `config.py` | `ConfigBundle` - loads all config files |
| `config_models.py` | Pydantic models for config files (Sample, Instrument, QueuePattern, etc.) |
| `config_models_samplers.py` | Sampler config models (GridSampler, EvosepSampler) |
| `params_models.py` | `QueueInput`, `QueueParameters`, `SampleGroup`, `InputSample` |
| `params_simulator.py` | Parameter simulation for testing |

### CLI Modules (`src/qg/cli/`)

| Module | Purpose |
|--------|---------|
| `generate_queues.py` | Main entry point for queue generation |
| `generate_params.py` | Generate parameter JSONs from CSV exports |
| `find_projects.py` | Project discovery utility |
| `validate_config.py` | Configuration validation |

### Tools (`src/qg/tools/`)

| Module | Purpose |
|--------|---------|
| `sld_to_csv.py` | Convert Thermo SLD files to CSV |
| `csv_to_params.py` | Convert queue CSV to params JSON |
| `compare.py` | Compare generated vs reference queues |
| `merge.py` | Merge queue files |
| `summarize.py` | Summarize queue data |
| `cli.py` | Tools CLI entry point |

### Apps (`src/qg/apps/`, `src/qg/tools_apps/`)

| App | Purpose |
|-----|---------|
| `apps/queue_app.py` | Main Marimo GUI for interactive queue generation |
| `apps/config_editor.py` | Configuration file editor |
| `tools_apps/queue_analysis_marimo.py` | Queue analysis in Marimo |
| `tools_apps/compare_existing_generated.py` | Compare existing vs generated queues |

### Config Files (`qg_configs/`)

| File | Purpose |
|------|---------|
| `sampler.toml` | Physical sampler layouts (Vanquish, MClass48, Evosep) |
| `samples.csv` | QC sample definitions (per technology, inj_vol, file_name_template) |
| `queue_patterns.toml` | QC injection patterns (start/middle/end/separation) |
| `qc_layouts.toml` | QC positions per technology/sampler |
| `instruments.csv` | Instrument -> methods_file, path_template mapping |
| `instrument_patterns.csv` | Valid patterns per instrument |
| `combinations.csv` | Valid (instrument, sampler, output_format, position_format) tuples |
| `output_formats.toml` | Column mappings for xcalibur/chronos/hystar |
| `methods/<tech>/<instr>_methods.csv` | Methods with polarity column |

**Method Files:**
- `methods/proteomics/`: 9 instruments (ASTRAL_1, ASCEND_1, EXPLORIS_1/2/5, LUMOS_2, QEXACTIVE_1, TIMSTOF_1, TIMSTOFFLEX_1)
- `methods/metabolomics/`: 3 instruments (EXPLORIS_3, QEXACTIVEHF_2, QUANTIVA_1)
- `methods/lipidomics/`: 2 instruments (EXPLORIS_3, EXPLORIS_4)

### Technologies

- `proteomics` - No polarity expansion
- `metabolomics` - Polarity expansion (pos/neg)
- `lipidomics` - Polarity expansion (pos/neg)

### Samplers

- `Vanquish` (vial/plate) - GridPositionGenerator
- `MClass48` (vial/plate) - GridPositionGenerator
- `Evosep` (vial/plate) - EvosepPositionGenerator

## Testing

### Unit Tests (`tests/`)

| File | Purpose |
|------|---------|
| `test_generator.py` | Generator pipeline tests |
| `test_queue_structure.py` | Queue structure building |
| `test_queue_structure_explicit.py` | Explicit queue structure tests |
| `test_position_assigner.py` | Position assignment strategies |
| `test_config_integration.py` | Configuration loading and validation |
| `test_cli.py` | CLI functionality |
| `test_params_simulator.py` | Parameter simulation |

### Validation Testing (`test_data/`)

Snakemake workflow comparing generated queues against reference data from instruments:

```bash
cd test_data
snakemake -j4 all              # Full pipeline
snakemake marimo_orig_qgen     # View comparison results
snakemake clean_all            # Reset
```

**Directory Structure:**
- `reference/sld/` - Raw Thermo Xcalibur SLD binaries (astral, ascend, lumos2, exploris2)
- `reference/csv/` - Parsed queue CSVs
- `params_json/` - Test input parameter files
- `generated/` - Generated queue CSVs
- `results/` - Comparison results with summary.csv

Pipeline: SLD files -> CSV -> params JSON -> generate queue -> compare with original

## Documentation (`docs/`)

| File | Purpose |
|------|---------|
| `ALGORITHM.md` | Queue generation algorithm details |
| `CONFIG.md` | Configuration file documentation |
| `Todo.md` | Development tasks |

## Related Projects

- `/Users/wolski/projects/queue/qg/` - Legacy R implementation (reference only)
