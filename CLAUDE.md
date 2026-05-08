# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## File Rules

**Never read `TODO/DONE_*.md` files unless explicitly asked by the user.** These are archived task documents and not relevant to current work.

## Project Overview

Queue generation system for mass spectrometry instruments. Generates sample queues with QC injections for XCalibur, Chronos, and Hystar software.

**Python version:** >=3.14

## Release Process

Every bugfix or feature MR adds a one-line bullet to `CHANGELOG.md` under the existing `## [Unreleased]` heading, in the appropriate `### Added/Changed/Fixed/Removed` subsection (Keep a Changelog format). Keep bullets terse — describe the user-visible change in one short sentence; mechanism and rationale belong in the commit/MR.

Bump the version in `pyproject.toml` only when the current version is already released (so the next change opens the next release line). Use patch for bugfixes and CI/build-only changes, minor for new features or dependency/API refreshes.

Only create a `## [x.y.z] - YYYY-MM-DD` section when explicitly asked to cut a release; otherwise leave entries under `## [Unreleased]`.

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
| `qg-tools` | `qg.tools.cli` | Tools CLI (SLD conversion, comparison) |
| `qg-app` | `qg.gitlab.launcher` | Launch queue app (GitLab deployment) |
| `qg-editor` | `qg.gitlab.launcher` | Launch config editor (GitLab deployment) |

## Terminology

| Term | Description | Location |
|------|-------------|----------|
| **Configs** | Static files defining system behavior (instruments, samplers, patterns, QC samples) | `qg_configs/core/` subdirectories |
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
- `user`, `date`: not output columns — substituted into the instrument's `path_template` (`{user}_{date}`) from `instruments.csv` to form the per-row data path.

## Architecture

### Pipeline (Stateless Functions)

Executed by `QueueGenerator.build_rows()` in `generator.py`:

```
QueueInput (JSON)
    |
1. randomize_plate_queue(plate_queue, randomization) -> PlateQueue
2. build_multi_container_queue_structure(groups, pattern) -> list[SlotEntry]
3. create_qc_position_provider(qc_layout, slot_entries) -> QCPositionProvider
4. _build_slots(slot_entries, plate_queue, qc_provider) -> list[SlotInfo]
5. _expand_polarities(slots, polarities) -> list[ExpandedSlot]   # metabolomics/lipidomics
6. _resolve_methods(slots, methods_config, ...) -> list[ExpandedSlot]
7. _format_file_names(slots, date) -> list[ExpandedSlot]
8. _build_queue_rows(slots, ...) -> QueueRowTable
9. format_table(queue_rows, output_format) -> pl.DataFrame
    |
CSV / XML Output
```

### Key Modules (`src/qg/`)

| Module | Purpose |
|--------|---------|
| `generator.py` | `QueueGenerator` class (config resolution + pipeline execution) |
| `queue_structure.py` | `build_multi_container_queue_structure()`, `SlotEntry` |
| `positionV2.py` | Position generation for well/tip samplers |
| `qc_layout.py` | `QCLayoutWell`, `QCLayoutTip` classes |
| `qc_positions.py` | `QCPositionProvider` |
| `queue_builder.py` | Queue building orchestration |
| `sample_rows.py` | Sample row generation |
| `writers.py` | Output format writers |
| `params_models.py` | `VialQueue`, `PlateQueue`, `QueueParameters`, `ContainerBatch` |
| `hystar_xml_writer.py` | Hystar XML output writer |
| `artifacts.py` | Build artifacts handling |
| `randomize.py` | Sample randomization |
| `logging_setup.py` | Loguru logging configuration |
| `config_models/` | Package: `loader.py`, `formatting.py`, `methods.py`, `positions.py`, `structure.py`, `ui.py` |
| `gitlab/` | Package: GitLab deployment, config sync (`launcher.py`, `service.py`, `config_bridge.py`, `settings.py`, `_git.py`) |
| `bfabric_utils.py` | B-Fabric LIMS integration utilities |

### Config Access Rules

**IMPORTANT:** The `config_models/loader.py` module has only ONE public function: `qg_configuration()`. All other functions are private (prefixed with `_`).

**Only these modules may read/write files in `qg_configs/`:**
- `config_models/loader.py` - loads configs via `qg_configuration()`
- `apps/config_editor.py` - edits config files directly

**All other modules MUST access configs through `QGConfiguration`** returned by `qg_configuration()`:
```python
from qg.config_models.loader import qg_configuration

config = qg_configuration()
config.instruments.to_table()  # Get DataFrame
config.samples.get_sample(tech, sample_id)  # Get specific sample
config.methods.to_table(tech, instrument)  # Get methods as DataFrame
```

**If something in `qg_configs/` is not accessible through QGConfiguration, add a new Pydantic model to `config_models/` and load it in `loader.py`.**

Never use `pl.read_csv()` or `Path().read_text()` to read config files directly in application code.

### CLI Modules (`src/qg/cli/`)

| Module | Purpose |
|--------|---------|
| `generate_queues.py` | Main entry point for queue generation |
| `find_projects.py` | Project discovery utility |
| `validate_config.py` | Configuration validation |

### Tools (`src/qg/tools/`)

| Module | Purpose |
|--------|---------|
| `sld_to_csv.py` | Convert Thermo SLD files to CSV |
| `compare.py` | Compare generated vs reference queues |
| `merge.py` | Merge queue files |
| `summarize.py` | Summarize queue data |
| `cli.py` | Tools CLI entry point |

### Apps (`src/qg/apps/`, `src/qg/tools_apps/`)

| App | Purpose |
|-----|---------|
| `apps/queue_app.py` | Main Marimo GUI for interactive queue generation |
| `apps/config_editor.py` | Configuration file editor |
| `apps/_bfabric_auth.py` | Shared B-Fabric auth helper (hasattr guard for SimpleUser) |
| `apps/bfabric_app.py` | B-Fabric integrated queue app |
| `apps/bfabric_app_editor.py` | B-Fabric integrated config editor |
| `tools_apps/queue_analysis_marimo.py` | Queue analysis in Marimo |

**B-Fabric session in the app:** `resolve_app_session()` in `bfabric_utils.py` returns an `AppSession` that bundles the per-request context (`client`, `is_employee`, `entity_id`, `instance_slug`, ...). The queue app's first cell unpacks it; downstream cells branch on `is_employee` (employees browse all containers; non-employees are pinned to `entity_id`).

### Config Files (`qg_configs/`)

```
qg_configs/
├── core/
│   ├── formatting/
│   │   ├── instruments.csv
│   │   └── output_formats.toml
│   ├── methods/
│   │   ├── Proteomics/
│   │   ├── Metabolomics/
│   │   └── Lipidomics/
│   ├── position/
│   │   ├── sampler.toml
│   │   ├── plate_layouts.toml
│   │   ├── sampler_plate_layouts.csv
│   │   ├── qc_layouts_well.csv
│   │   └── qc_layouts_tip.csv
│   └── structure/
│       ├── queue_patterns.toml
│       └── samples.csv
└── ui/
    └── instrument_config.csv
```

| File | Location | Purpose |
|------|----------|---------|
| `sampler.toml` | `core/position/` | Physical sampler layouts (Vanquish, MClass48, Evosep) |
| `plate_layouts.toml` | `core/position/` | Plate layout definitions |
| `sampler_plate_layouts.csv` | `core/position/` | Sampler-to-plate-layout mapping |
| `qc_layouts_well.csv` | `core/position/` | QC positions for well-plate samplers (Vanquish, MClass) |
| `qc_layouts_tip.csv` | `core/position/` | QC tip ranges for tip-plate samplers (consumable tips) |
| `samples.csv` | `core/structure/` | QC sample definitions (per technology, inj_vol, file_name_template) |
| `queue_patterns.toml` | `core/structure/` | QC injection patterns (start/middle/end/separation) |
| `instruments.csv` | `core/formatting/` | Instrument -> methods_file, path_template mapping |
| `output_formats.toml` | `core/formatting/` | Column mappings for xcalibur/chronos/hystar |
| `methods/<Tech>/<instr>_methods.csv` | `core/` | Methods with polarity column |
| `instrument_config.csv` | `ui/` | Instrument defaults (sampler, output_format, default_pattern) |

**Method Files:**
- `core/methods/Proteomics/`: 9 instruments (ASTRAL_1, ASCEND_1, EXPLORIS_1/2/5, LUMOS_2, QEXACTIVE_1, TIMSTOF_1, TIMSTOFFLEX_1)
- `core/methods/Metabolomics/`: 3 instruments (EXPLORIS_3, QEXACTIVEHF_2, QUANTIVA_1)
- `core/methods/Lipidomics/`: 2 instruments (EXPLORIS_3, EXPLORIS_4)

### Technologies

- `proteomics` - No polarity expansion
- `metabolomics` - Polarity expansion (pos/neg)
- `lipidomics` - Polarity expansion (pos/neg)

### Samplers

- `Vanquish` (vial/plate) - well-plate sampler
- `MClass` (vial/plate) - well-plate sampler
- `Evosep` (vial/plate) - tip-plate sampler

## Testing

### Unit Tests (`tests/`)

| File | Purpose |
|------|---------|
| `test_generator.py` | Generator pipeline tests |
| `test_queue_structure.py` | Queue structure building |
| `test_queue_structure_explicit.py` | Explicit queue structure tests |
| `test_config_integration.py` | Configuration loading and validation |
| `test_config_models_integration.py` | Config models integration tests |
| `test_positions.py` | Position generation tests |
| `test_queue_builder.py` | Queue builder tests |
| `test_cli.py` | CLI functionality |
| `test_randomize.py` | Randomization tests |
| `test_gitlab.py` | GitLab deployment tests |
| `test_hystar_xml_writer.py` | Hystar XML writer tests |
| `helpers.py` | Shared test helpers and fixtures |

### Validation Testing (`test_data/`)

Snakemake workflow comparing generated queues against reference data from instruments:

```bash
cd test_data
snakemake -j4 all              # Full pipeline
snakemake clean_all            # Reset
```

**Directory Structure:**
- `reference/sld/` - Raw Thermo Xcalibur SLD binaries (astral, ascend, lumos2, exploris2)
- `reference/csv/` - Parsed queue CSVs
- `params_json/` - Test input parameter files
- `generated/` - Generated queue CSVs
- `results/` - Comparison results with summary.csv

Pipeline: SLD files -> CSV -> params JSON -> generate queue -> compare with original

## Coding Standards

### Exception Handling

**NEVER** use broad exception handling that silently swallows errors:

```python
# BAD - catches everything, hides bugs
try:
    do_something()
except Exception:
    pass

# BAD - catches everything, no re-raise
try:
    do_something()
except Exception as e:
    logger.error(e)
    return None
```

**ALWAYS** catch specific exceptions, log with context, and re-raise:

```python
# GOOD - specific exception, logged, re-raised
try:
    data = json.load(f)
except json.JSONDecodeError as e:
    logger.exception("Failed to parse JSON from %s", filepath)
    raise

# GOOD - specific exceptions, converted to domain error
try:
    df = pl.read_csv(path)
except (FileNotFoundError, pl.exceptions.ComputeError) as e:
    logger.exception("Failed to load CSV from %s", path)
    raise ConfigurationError(f"Cannot load {path}") from e
```

If you must handle an error without re-raising (rare), document why:

```python
# Acceptable - fallback with clear rationale
try:
    text = data.decode("utf-16-le")
except UnicodeDecodeError:
    # SLD files sometimes use latin-1 encoding as fallback
    text = data.decode("latin-1", errors="replace")
```

### Prefer Declarative Style

**Use `itertools`, set operations, and comprehensions instead of imperative for loops.**

```python
# BAD - manual index tracking
plate_idx, row_idx, col_idx = 0, 0, 0
while len(positions) < n:
    # ... manual advancement logic

# GOOD - itertools.product
all_positions = [
    (plate, f"{row}{col}")
    for plate, row, col in product(plates, rows, cols)
]

# BAD - iterating to check membership
for sample in samples:
    if sample.position in reserved:
        raise ValueError(...)

# GOOD - set intersection
user_positions = {s.position for s in samples}
collisions = user_positions & reserved
if collisions:
    raise ValueError(...)

# BAD - building set with loop
reserved = set()
for qc_id in qc_positions:
    reserved.add(get_position(qc_id))

# GOOD - set comprehension
reserved = {get_position(qc_id) for qc_id in qc_positions}
```

### Testing

**Test public interfaces, not private implementations.** Classes and functions prefixed with `_` are private implementation details. Write tests against the public API (e.g., `SamplerStrategy`, not `_VanquishVialSampler_prototype`).

```python
# BAD - testing private class directly
def test_private_sampler():
    sampler = _VanquishVialSampler_prototype(...)
    sampler._generate_positions_default_samples(...)

# GOOD - testing through public interface
def test_sampler_strategy():
    strategy = SamplerStrategy("Vanquish.vial", config, qc_layout)
    strategy.assign_positions_user_samples(queue_input)
```

**Construct test patterns directly — do not load named patterns from TOML.** Pattern names in `queue_patterns.toml` can be added or removed; tests must not depend on specific names existing. Build `QueuePattern` objects inline or register them under a `_test_*` key:

```python
from qg.config_models.structure import QueuePattern

_NOQC = QueuePattern(description="test", run_QC_after_n_samples=1, start=[], middle=[], end=[])

# Or inject into config for integration tests:
cfg.queue_patterns.patterns.setdefault("Proteomics", {})["_test_noqc"] = _NOQC
```

**NEVER add methods to the public interface just to make tests pass.** If changing a class interface breaks tests, update the tests to use the new interface properly. Do not add factory methods, alternative constructors, or compatibility shims solely for test convenience.

```python
# BAD - adding a factory method just to keep old tests working
class SamplerStrategy:
    def __init__(self, name, config, tech_area, qc_layout_name):
        # new interface
        ...

    @classmethod
    def from_components(cls, sampler, plate_layout, qc_samples):
        # WRONG: added only because tests used old signature
        ...

# GOOD - update the tests to use the new interface
def test_sampler_strategy():
    config = qg_configuration()  # load real config
    strategy = SamplerStrategy("Vanquish", "vial", config, "Proteomics", "standard")
    ...
```

## Documentation (`docs/`)

| File | Purpose |
|------|---------|
| `ALGORITHM.md` | Queue generation algorithm details |
| `CONFIG.md` | Configuration file documentation |
| `HISTORY.md` | Development chronology (Jan–Feb 2026 rewrite) |
| `deployment.md` | Production and test deployment reference |
| `editor_guide.md` | Config editor usage guide |
| `user_modes.md` | User-facing modes and workflows |

Generated formats (.html, .pdf, .svg, .png) are also available.

## Related Projects

- `/Users/wolski/projects/queue/qg/` - Legacy R implementation (reference only)
