# Agent contract for `tests/gui/`

If you are an agent implementing one BDD scenario for this suite, read this first.

## Files you MAY edit

- `tests/gui/features/<your-scenario>.feature` — your Gherkin
- `tests/gui/test_<your-scenario>.py` — `scenarios(...)` call + step impls
- `tests/gui/fixtures/bfabric/scenario_<your-scenario>/*.json` — scenario-specific canned B-Fabric data, **only** if the default fixtures don't already cover your needs

## Files you MUST NOT edit

These are shared infrastructure — if you change them, parallel agents conflict.

- `tests/gui/_fake_bfabric.py`
- `tests/gui/_test_app.py`
- `tests/gui/_helpers.py` (extending the helper set is fine; changing existing signatures requires coordination)
- `tests/gui/conftest.py`
- `tests/gui/AGENTS.md` (this file)
- `tests/gui/fixtures/bfabric/samples_*.json` (the default set)
- `tests/gui/fixtures/bfabric/plates_*.json` (the default set)
- `tests/gui/fixtures/bfabric/order_*.json` / `project_*.json` (the default set — single-container fetch-by-id records)
- `tests/gui/fixtures/projects.csv`
- `tests/gui/fixtures/expected/proteomics_astral1_37180.csv`

If you need to change anything in this list, stop and ask the coordinator instead.

## Available fixtures (Tier-A hermetic)

| Container ID | Composition | Samples | Area | Notes |
|---|---|---|---|---|
| 37180 | Plate (50001) + 12 samples on `A1`–`A12` | 12 | Proteomics | Default happy-path container |
| 37181 | Plate (50002) + 4 samples on `B1`–`B4` | 4 | Proteomics | Second container for multi-container scenarios |
| 37182 | Vials only | 6 | Proteomics | Vial-only container for Vial-mode scenarios |
| 37183 | Plate (50003) on `A1`–`A4` + 4 off-plate samples | 8 | Metabolomics/Biophysics | Mixed plate+vial container (`has_plates` and `has_vials` both true) for Queue-Type availability scenarios; Metabolomics so it doesn't crowd the Proteomics project-table page |
| 37190 | Plate (50090) + 8 samples | 8 | Metabolomics/Biophysics | Lipidomics/Metabolomics container; launching-order scenarios |
| 37195 | Plate (50095) + 6 samples | 6 | Metabolomics/Biophysics | Metabolomics container |
| 37200 | 5 plates (50201–50205) + 10 samples | 10 | Proteomics | Many-plates container for tray-capacity scenarios |
| 99999 | Empty | 0 | Proteomics | For "no samples" danger-callout scenario |

`projects.csv` exposes all of the above to the employee project table. A few also have a
raw container fixture (`container_<id>.json`) so the app can fetch them by id for the
launching-order pre-load (see below); `container_37190.json` is the Metabolomics one.

## Available session config

The default session is `is_employee=True, entity_id=None`. To vary per-scenario:

```python
def test_x(page, queue_app_url, set_session):
    set_session(is_employee=False, entity_id=37180)
    page.goto(queue_app_url)
    ...
```

`set_session` POSTs to `/_test/session` and auto-resets at teardown.

**Launching from an order.** Pass `entity_class` to simulate the app being opened
*from* a B-Fabric order (the launching-order pre-load: Tech-Area auto-default and,
for employees, order pre-selection). When omitted, the server derives it (`None`
for employees, `"Container"` for non-employees), so existing callers are unchanged.

```python
set_session(is_employee=True, entity_id=37190, entity_class="Order")  # employee launched from an order
set_session(is_employee=False, entity_id=37190)                       # non-employee pinned (entity_class="Container")
```

The pre-load calls `read("order"|"project", {"id": X})`; `FakeBfabric` serves these
from `order_<id>.json` / `project_<id>.json` (raw B-Fabric shape — `technology` is a
list), returning empty when no fixture exists (a graceful "not found", so a pinned
container without an order fixture simply gets no Tech-Area default).

## Helpers you SHOULD use

Import from `tests/gui/_helpers.py`:

- `open_app(page, queue_app_url)` — navigates + waits for the sidebar Tech Area dropdown
- `sidebar(page)` — scope a locator to the sidebar (form widgets)
- `set_dropdown(page, "Tech Area", "Proteomics")` — set a sidebar dropdown
- `expect_dropdown_options(page, "Sampler", "Vanquish", "Evosep")` — assert each value is an option
- `expect_dropdown_value(page, "Tech Area", "Metabolomics")` — assert the *currently-selected* option
- `expect_dropdown_visible(page, label)` / `expect_dropdown_hidden(page, label)` — conditional UI
- `click_button(page, "Upload to B-Fabric")` — click any button by accessible name
- `select_order(page, 37180)` — tick a row in the employee project table
- `upload_to_bfabric(page)` — required before `download_queue` (the download button is disabled until upload runs)
- `download_queue(page) -> Path` — clicks "Download Queue File" and returns the downloaded path
- `download_params_json(page) -> dict` — clicks "Download Params JSON" and returns parsed dict
- `get_workunit_message(page) -> str` — text of the "Would create workunit ..." mock-upload message
- `assert_warn(page, text)` / `assert_danger(page, text)` — visible callout containing `text`

Do NOT use naked `page.get_by_label(...)` — marimo renders each form widget twice (sidebar + main output) so it's ambiguous. Always scope via `sidebar(page)` or the helpers above.

## Definition of done

1. `uv run pytest tests/gui/test_<your-scenario>.py -v` passes locally.
2. `uv run pytest -v` passes (no regressions in the full suite).
3. Step impls are one-liners that delegate to `_helpers.py` — no Playwright boilerplate duplicated.
4. Feature file uses `Given`, `When`, `Then` correctly; no "And" without a preceding step.
5. Locators scoped via `sidebar(page)` / `_helpers.py` — never `page.get_by_label` directly.

## Filenames

- Feature: `tests/gui/features/<scenario_slug>.feature`
- Test: `tests/gui/test_<scenario_slug>.py` (must start with `test_`)
- Scenario-specific data (only if needed): `tests/gui/fixtures/bfabric/scenario_<scenario_slug>/*.json`

`<scenario_slug>` is lowercase snake_case, matches the task ID's purpose (e.g. `happy_path`, `metabolomics_polarity`).
