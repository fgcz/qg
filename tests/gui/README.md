# GUI tests for `queue_app.py`

Headless, BDD-driven end-to-end tests of the marimo queue app. Stack:
`pytest-bdd` for the Gherkin layer, `pytest-playwright` for browser automation.

## Two tiers

| Tier | Backend | When it runs |
|------|---------|--------------|
| **Hermetic (A)** | Fake `Bfabric` client via `_TEST_SESSION_FACTORY`; no network. | Default — picked up by every plain `pytest` invocation. |
| **Real test instance (B)** | `Bfabric.connect()` against `fgcz-bfabric-test.uzh.ch` (creds in `~/.bfabricpy.yml`). | Opt-in via `pytest --gui-tier=bfabric-test -m bfabric_test_instance tests/gui/`. |

Tier B is auto-marked `bfabric_test_instance` by `pytest_collection_modifyitems`,
and the global `addopts` filter `-m "not bfabric_test_instance"` excludes it from
default runs.

## One-time setup

```bash
uv sync
uv run playwright install chromium
```

## Run

```bash
# Hermetic (default)
uv run pytest tests/gui/

# Against fgcz-bfabric-test.uzh.ch (requires ~/.bfabricpy.yml)
uv run pytest tests/gui/ --gui-tier=bfabric-test -m bfabric_test_instance
```

## Layout

```
tests/gui/
├── AGENTS.md                  # contract for parallel-fanout agents adding scenarios
├── conftest.py                # uvicorn fixture, --gui-tier option, set_session fixture
├── _test_app.py               # uvicorn entry; installs _TEST_SESSION_FACTORY + /_test/session
├── _fake_bfabric.py           # FakeBfabric + build_test_session()
├── _helpers.py                # Playwright helpers (sidebar scope, select_order, downloads, ...)
├── features/                  # one .feature per scenario
├── fixtures/
│   ├── bfabric/               # canned `samples_<id>.json`, `plates_<id>.json`
│   ├── expected/              # golden reference outputs (CSV)
│   └── projects.csv           # seeded into the per-instance ContainerCache dir
└── test_*.py                  # one `scenarios("features/<x>.feature")` + step defs per scenario
```

## Scenario inventory

| Feature file | Test file | Covers |
|---|---|---|
| `happy_path.feature` | `test_happy_path.py` | End-to-end Proteomics queue download, byte-exact vs golden CSV (date frozen via Gherkin) |
| `metabolomics_polarity.feature` | `test_metabolomics_polarity.py` | Per-polarity method dropdown visibility |
| `metabolomics_download.feature` | `test_metabolomics_download.py` | Downloaded CSV carries pos/neg rows for each Metabolomics sample |
| `lipidomics.feature` | `test_lipidomics.py` | Downloaded CSV carries pos/neg rows for each Lipidomics sample |
| `randomization.feature` | `test_randomization.py` | `Randomization=random` reshuffles samples; full set is preserved |
| `overrides.feature` | `test_overrides.py` | Inj-vol and qc_frequency overrides reach the generated CSV |
| `sample_exclusion.feature` | `test_sample_exclusion.py` | Unticking a sample in the selection table excludes it from the queue |
| `generation_error.feature` | `test_generation_error.py` | Generator failure surfaces a danger callout in the Queue Preview |
| `qc_visits.feature` | `test_qc_visits.py` | QC Positions sidebar shows per-well visit counts after generation |
| `valid_combinations_tab.feature` | `test_valid_combinations_tab.py` | Switching to the Valid Combinations tab shows the master table |
| `concentrations.feature` | `test_concentrations.py` | `cal_series` reveals the 7-level concentration grid |
| `tray_capacity.feature` | `test_tray_capacity.py` | Plate count vs sampler trays — both negative and positive paths |
| `sampler_incompat.feature` | `test_sampler_incompat.py` | Incompatible-sampler warn callout |
| `non_employee.feature` | `test_non_employee.py` | Pinned-container workflow, no project table |
| `non_employee_empty.feature` | `test_non_employee_empty.py` | "No samples found" danger callout |
| `noqc.feature` | `test_noqc.py` | `noqc` QC layout hides the Pattern dropdown |
| `vial_mode.feature` | `test_vial_mode.py` | Start Tray + Start Position visibility |
| `upload_workunit.feature` | `test_upload_workunit.py` | Mock workunit message + bundled resource count |
| `params_json.feature` | `test_params_json.py` | Download Params JSON round-trip (full schema) |
| `hystar_output.feature` | `test_hystar_output.py` | TIMSTOF → `.xml` Hystar output, parsed and validated |
| `multi_container.feature` | `test_multi_container.py` | Two-order selection — CSV body verified for both containers |
| `validation_required.feature` | `test_validation_required.py` | "Please select an order" warning |

## Adding a scenario

1. Drop a new `features/<slug>.feature` and a sibling `test_<slug>.py` calling `scenarios("features/<slug>.feature")`.
2. Step impls should be one-liners that delegate to `_helpers.py` — see existing tests for the pattern.
3. If you need new B-Fabric data, add it under `fixtures/bfabric/`.
4. Run `uv run pytest tests/gui/test_<slug>.py -v`, then `uv run pytest -v` for regression coverage.

See `AGENTS.md` for the contract followed by the parallel-fanout build that produced the current scenarios — including which shared files are off-limits to scenario-level edits.

## Architecture notes

- The FastAPI app is constructed with `test_mode=True` (see
  `qg.apps._bfabric_auth.create_bfabric_fastapi_app`), which skips the
  `BfabricAuthMiddleware` — neither tier requires a browser JWT round-trip.
- Tier-A scenarios install `qg.bfabric_utils._TEST_SESSION_FACTORY` from the
  uvicorn subprocess (set before the first request). Tier B leaves that hook
  unset so `resolve_app_session` follows the unauthenticated path
  (`Bfabric.connect()`).
- `QG_CACHE_DIR` is set to a `tmp_path` so the container-cache CSV the app
  loads on startup is hermetic; the hermetic tier pre-seeds it from
  `fixtures/projects.csv`.

## Tier-B housekeeping

**Status:** the Tier-B test instance has no scenarios yet — the `--gui-tier=bfabric-test`
switch and the `bfabric_test_instance` marker are plumbed and inert. Promoting
a hermetic scenario to Tier B requires (1) a stable test-instance container with
plates and samples that mirror the fixture shape, (2) credentials in
`~/.bfabricpy.yml`, and (3) duplication of the relevant `test_*.py` (or marker
parameterisation) so the Tier-A run stays hermetic.

TODO(tier-b): file a tracking issue and fill in the table below with the
stable IDs once the first real-instance scenario is promoted.

| Entity | ID | Notes |
|--------|-----|-------|
| _TBD_ | — | Add stable test containers/plates here as scenarios grow. |
