# AGENTS.md

Guidance for AI coding agents (Claude Code, Codex, Copilot, Gemini, etc.) working in this repository. This is the single source of truth; `CLAUDE.md` imports it via `@AGENTS.md`.

## Project Overview

Queue generation system for mass spectrometry instruments. Generates sample queues with QC injections for XCalibur, Chronos, and Hystar software.

**Python version:** >=3.14

## Release Process

### During development (every MR)

Every bugfix or feature MR adds a one-line bullet to `CHANGELOG.md` under the
existing `## [Unreleased]` heading, in the appropriate
`### Added/Changed/Fixed/Removed` subsection (Keep a Changelog format). Keep
bullets terse — describe the user-visible change in one short sentence;
mechanism and rationale belong in the commit/MR.

Bump the version in `pyproject.toml` **only when the current version is already
released** (so the first change after a release opens the next release line),
and run `uv lock` in the same MR so `uv.lock` records it. Use patch for bugfixes
and CI/build-only changes, minor for new features or dependency/API refreshes.
Because of this bump-ahead rule, by the time you cut a release the version in
`pyproject.toml`/`uv.lock` is **usually already the target** — see below.

### Cutting a release

Only do this when explicitly asked to cut a release. Otherwise leave entries
under `## [Unreleased]`. Steps:

1. **Verify the version — do not blindly re-bump.** Per the bump-ahead rule
   above, `pyproject.toml` and `uv.lock` usually already carry the target
   version from an earlier MR. Check (`grep '^version' pyproject.toml`,
   `grep -A1 'name = "qg"' uv.lock`); only bump and re-run `uv lock` if they
   still show the *last released* version. Most releases need neither.
2. **Consolidate `## [Unreleased]`.** A cycle of per-MR appends leaves scattered
   duplicate `### Fixed`/`### Changed` blocks and sometimes miscategorized
   entries; merge them into one `### Added/Changed/Fixed/Removed` each, and make
   sure marquee features sit under `### Added` (not buried in `### Fixed`).
3. **Date it.** Rename `## [Unreleased]` → `## [x.y.z] - YYYY-MM-DD` and leave a
   fresh empty `## [Unreleased]` heading on top for the next cycle.
4. **Commit** as `chore: cut release x.y.z`, staging whatever actually changed
   (`CHANGELOG.md`, plus `pyproject.toml` + `uv.lock` only if step 1 bumped).
5. **Land it on `main`** (direct push of the release commit is acceptable).
6. **Tag and push.** Create an annotated tag named `vx.y.z` with message
   `Release x.y.z` on the release commit, then `git push origin vx.y.z`. Do not
   force-push tags.

**What the tag does — and does not do.** Pushing the `vx.y.z` tag triggers the
CI `build` job, which cross-builds the arm64 OCI image and writes the archive to
NFS (`…/metabolomics/queue_gen/queue_gen-<tag>.oci.tar`). It does **not** roll
the new version out to the running host. The live rollout — bump `IMAGE_TAG` in
the [web-apps repo](https://gitlab.bfabric.org/proteomics/web-apps) and run
`make deploy` on the deploy host — is a separate manual step documented in
[`docs/developers/deployment.md`](docs/developers/deployment.md).

## Commands

```bash
# Run the standalone local app (CSV/XLSX upload, no B-Fabric)
make app-local                                  # or: uv run marimo run src/qg/apps/queue_app_local.py

# Run the B-Fabric portal app (needs the qg[bfabric] extra; auth-bypass for dev)
QG_ALLOW_UNAUTHENTICATED=1 uv run marimo run src/qg/apps/queue_app.py

# Generate queue from JSON params
uv run qg input.json -o output.csv

# Validate config files
uv run qg-validate

# Run tests (dev env includes the portal extra)
uv run pytest

# Run only the B-Fabric-free core tests (as on a core install)
uv run --no-group portal pytest -m "not bfabric"

# Run single test
uv run pytest tests/test_file.py::test_name -v
```

### Install profiles

- **Core** (`pip install qg`): the local app + `qg`/`qg-validate` CLIs. No
  `bfabric`, `fastapi`, `starlette`, or `python-gitlab`. `uv sync --no-group portal`
  reproduces this in-repo.
- **Portal** (`pip install 'qg[bfabric]'`): adds B-Fabric auth, LIMS loading,
  workunit upload, and the GitLab launcher. `uv sync` installs it by default in
  dev via the `portal` dependency-group (`[tool.uv] default-groups`).

### CLI Entry Points

| Command | Module | Purpose | Needs extra |
|---------|--------|---------|-------------|
| `qg` | `qg.cli.generate_queues` | Main queue generation from JSON params | — |
| `qg-validate` | `qg.cli.validate_config` | Validate config files | — |
| `qg-app-local` | `qg.apps.launcher_local` | Launch the standalone local upload app | — |
| `qg-find-projects` | `qg.cli.find_projects` | Project discovery utility | `qg[bfabric]` |
| `qg-refresh-cache` | `qg.cli.refresh_cache` | Refresh B-Fabric container caches | `qg[bfabric]` |
| `qg-app` | `qg.gitlab.launcher` | Launch portal queue app (GitLab deployment) | `qg[bfabric]` |
| `qg-editor` | `qg.gitlab.launcher` | Launch marimo config editor (GitLab deployment) | `qg[bfabric]` |
| `qg-config-viewer` | `qg.apps.dash_editor.launcher` | Launch the local, validation-only Dash config viewer | — |
| `qg-editor-dash` | `qg.apps.dash_editor.full_launcher` | Launch the full Dash config editor (dev; save + GitLab review) | `qg[bfabric]` |

## Terminology

| Term | Description | Location |
|------|-------------|----------|
| **Configs** | Static files defining system behavior (instruments, samplers, patterns, QC samples) | `qg_configs/core/` subdirectories |
| **Queue Parameters** | Runtime JSON specifying a queue run (instrument, sampler, samples) | GUI output |

## Queue Parameters JSON Structure

Defined in `src/qg/params_models.py`. `QueueInput` is `VialQueueInput | PlateQueueInput`,
each `{parameters, queue}`. `read_queue_input()` picks plate vs vial by whether
`queue` contains `plates`. (Vial example shown; the plate `queue` has `batches` +
`plates` + `cells`.)

```json
{
  "parameters": {
    "tech_area": "Proteomics",
    "instrument": "ASTRAL_1",
    "sampler": "Vanquish",
    "output_format": "xcalibur_sii",
    "queue_pattern": "standard",
    "queue_type": "Vial",
    "plate_layout": "Vanquish_54",
    "qc_layout_name": "standard",
    "polarity": [],
    "date": "20260112",
    "user": "cpanse",
    "method": {},
    "randomization": "no",
    "seed": null,
    "inj_vol_override": null,
    "qc_frequency_override": null,
    "one_container_per_tray": false,
    "start_position": "A1",
    "start_tray": "",
    "level_concentrations": {},
    "mark_end_of_queue": true
  },
  "queue": {
    "batches": {"37180": {"container_id": 37180, "container_name": "Project A"}},
    "samples": [
      {"sample_name": "S1", "sample_id": 123456, "tube_id": "37180/1", "container_id": 37180, "grouping_var": null}
    ]
  }
}
```

Key fields:
- `tech_area`: `Proteomics` / `Metabolomics` / `Lipidomics` (title-case).
- `sampler`: bare name (`Vanquish`). `queue_type` (`Vial`/`Plate`) + `plate_layout` select the layout; `qc_layout_name` selects the QC layout.
- `output_format`: `xcalibur` / `xcalibur_sii` / `chronos` / `hystar` (Hystar emits `.xml`).
- `randomization`: string `"no"` / `"random"` / `"blocked"` / `"blocked_uniform"` (not a bool).
- `seed`: optional `int | None` RNG seed. Used when set; otherwise a seed is drawn at generation (for randomized modes) and recorded back so the run is reproducible.
- `method`: `dict[str, str]` per-polarity methods: `{"pos": "Method_Pos", "neg": "Method_Neg"}`.
- `qc_frequency_override`: override pattern's `run_QC_after_n_samples`.
- `queue.batches`: `container_id` → `ContainerBatch`; multi-container support lives here, not in `parameters`. `queue.samples` (vial; alias `cells`) / `queue.cells` (plate) carry the samples.
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
| `utils.py` | Shared position types/helpers (used by `positionV2.py`, `qc_positions.py`) |
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
| `viz/` | Package: app-agnostic visualization helpers, no marimo dependency (`plate.py`) |
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
| `find_projects.py` | Project discovery utility (needs `qg[bfabric]`) |
| `refresh_cache.py` | Refresh B-Fabric container caches across instances (needs `qg[bfabric]`) |
| `validate_config.py` | Configuration validation |

### Apps (`src/qg/apps/`)

Both queue apps are thin marimo notebooks over a shared, B-Fabric-free pipeline
(`queue_app_shared.py`) plus swappable source/sink integrations
(`apps/integrations/`). The portal app imports B-Fabric; the local app does not.

| App | Purpose | Needs B-Fabric |
|-----|---------|----------------|
| `apps/queue_app.py` | Portal Marimo GUI (B-Fabric order browser + workunit upload) | yes |
| `apps/queue_app_local.py` | Standalone GUI: CSV/XLSX upload + local download | no |
| `apps/queue_app_shared.py` | Shared pipeline helpers (build/generate/filenames/downloads) — no marimo cells, no B-Fabric | no |
| `apps/launcher_local.py` | `qg-app-local` entry point (runs `queue_app_local.py`) | no |
| `apps/config_editor.py` | Marimo configuration editor (binds to `editor_core`) | yes |
| `apps/editor_core.py` | Framework-neutral config-editor core (TOML/table (de)serialization, `compact_toml`, payload→`QGConfiguration`); shared by the marimo editor and both Dash editors | no |
| `apps/dash_editor/app.py` | Local Dash config editor (`qg-config-viewer`): validate-only, no B-Fabric/GitLab | no |
| `apps/dash_editor/full_app.py` | Full Dash config editor (`qg-editor-dash`): validate + save + GitLab review, FGCZ-employee gated | yes |
| `apps/dash_editor/integrations.py` | Dash full-editor portal seam (session/save/review); only Dash file importing B-Fabric/GitLab | yes |
| `apps/_bfabric_auth.py` | Shared B-Fabric auth: `create_bfabric_fastapi_app` (marimo/ASGI) + `create_bfabric_wsgi_app` (Dash/WSGI) | yes |
| `apps/bfabric_app.py` | B-Fabric integrated queue app | yes |
| `apps/bfabric_app_editor.py` | B-Fabric integrated marimo config editor (ASGI) | yes |
| `apps/bfabric_dash_editor.py` | B-Fabric integrated full Dash config editor (ASGI, wraps the Dash WSGI app) | yes |

**Integrations (`src/qg/apps/integrations/`):** `local_samples.py` (pure CSV/XLSX
parser → normalized `sample_rows` schema), `bfabric_samples.py` (order/sample
loading), `bfabric_workunit.py` (workunit payload), `bfabric_context.py`
(session). The `local_*` modules import no B-Fabric; the `bfabric_*` modules need
the `qg[bfabric]` extra. Both notebooks honor a variable-name contract
(`full_samples_df`, `selected_orders`, `container_has_*`, `queue_input`,
`queue_output_str`, …) so the shared cells and the GUI tests are identical.

**B-Fabric session in the portal app:** `resolve_app_session()` in `bfabric_utils.py` returns an `AppSession` that bundles the per-request context (`client`, `is_employee`, `entity_id`, `instance_slug`, ...). The portal app's first cell unpacks it; downstream cells branch on `is_employee` (employees browse all containers; non-employees are pinned to `entity_id`). The local app has no session — its source cells synthesize `selected_orders` from the uploaded `container_id` column.

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
- `core/methods/Lipidomics/`: 3 instruments (EXPLORIS_3, EXPLORIS_4, QEXACTIVEHF_2)

### Adding a New Instrument

Three config files must be edited **together** — they are NOT cross-validated at
load time, so a mismatch surfaces only as a runtime lookup failure in the app,
not as a `qg-validate` error (see commit `bb33b32`: the UI offered
`Lipidomics/QEXACTIVEHF_2` but the `instruments.csv` row and methods file were
missing, so selecting it failed at instrument lookup):

1. **`core/formatting/instruments.csv`** — add a row
   `tech_area,instrument,methods_file,path_template`. `methods_file` points to
   `methods/<Tech>/<instr>_methods.csv`; `path_template` uses `{container}`,
   `{user}`, `{date}` placeholders. `MethodsConfig.load()` loads each methods
   file dynamically from these rows, so the row and the file must agree.
2. **`core/methods/<Tech>/<instr>_methods.csv`** — create the methods file with
   columns `sample_type,polarity,method_name,method_path`. Metabolomics and
   Lipidomics need both a `pos` and a `neg` row (polarity expansion);
   Proteomics needs only `pos`. Use `sample_type=default` plus optional per-QC
   overrides (e.g. a `QC03` row).
3. **`ui/instrument_config.csv`** — add a row
   `tech_area,instrument,sampler,output_format,default_pattern` so the
   instrument appears in the queue app menu. One row per (instrument, sampler)
   pairing; an instrument may appear several times with different
   sampler/output-format/pattern combinations.

`qg-validate` checks sample/pattern/layout cross-references but does **not**
verify that every `instrument_config.csv` row has a matching `instruments.csv`
row + methods file — keep the three in sync manually. Then follow the Release
Process (CHANGELOG bullet, version bump, `uv lock`).

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
| `test_bfabric_auth.py` | B-Fabric auth helper tests |
| `test_bfabric_utils.py` | B-Fabric utilities tests |
| `test_refresh_cache.py` | Container cache refresh tests |
| `test_viz_plate.py` | Plate visualization (`viz/plate.py`) tests |
| `helpers.py` | Shared test helpers and fixtures |

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

**Test public interfaces, not private implementations.** Classes and functions prefixed with `_` are private implementation details. Write tests against the public API (e.g., the `create_assembled_sampler()` factory in `positionV2.py`, not the private `_PlateValidatorWellConfig` / vial sampler classes it returns).

```python
# BAD - constructing a private sampler class directly
def test_private_sampler():
    sampler = _PlateValidatorWellConfig(...)
    sampler._generate_positions_default_samples(...)

# GOOD - go through the public factory + assign()
from qg.positionV2 import create_assembled_sampler

def test_sampler_assigns_positions(config):
    pattern = config.queue_patterns.get_pattern("Proteomics", "standard")
    sampler = create_assembled_sampler(
        "Vanquish", "vial", config, "Proteomics",
        pattern.get_all_sample_ids(), "Vanquish_54", "standard",
    )
    result = sampler.assign(create_vial_queue(3))
    assert len(result.cells) == 3
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
# BAD - adding an alternative constructor just to keep old tests working
def create_assembled_sampler(...):
    ...

def create_assembled_sampler_from_components(sampler, plate_layout, qc_samples):
    # WRONG: added only because tests used an old signature
    ...

# GOOD - update the tests to call the real factory with the current signature
def test_sampler_assigns_positions(config):
    pattern = config.queue_patterns.get_pattern("Proteomics", "standard")
    sampler = create_assembled_sampler(
        "Vanquish", "vial", config, "Proteomics",
        pattern.get_all_sample_ids(), "Vanquish_54", "standard",
    )
    ...
```

## Documentation (`docs/`)

Built into an MkDocs Material site (`mkdocs.yml`) and published to GitLab Pages
by the CI `pages` job. Organized by audience:

| File | Purpose |
|------|---------|
| `index.md` | Landing page / router |
| `users/editor_guide.md` | Config editor usage guide |
| `reference/algorithm.md` | Queue generation pipeline details |
| `reference/config.md` | Configuration file + queue-parameters reference |
| `developers/user_modes.md` | Auth/security requirements spec for the portal app (`queue_app.py`) |
| `developers/deployment.md` | Production/test deployment reference |
| `developers/history.md` | Development chronology (Jan–Feb 2026 rewrite) |

The site HTML in `public/` is build output and is not committed.

## Related Projects

- `/Users/wolski/projects/queue/qg/` - Legacy R implementation (reference only)
