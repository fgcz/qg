# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

### Added
- Randomization mode `blocked_uniform`: spreads each `grouping_var` group evenly across the whole queue (fair-share interleave) instead of front-loading complete blocks like `blocked`, avoiding majority-only tail injections for unbalanced group counts.
- Optional `seed` queue parameter for reproducible randomization: used when set in the params JSON, otherwise a seed is drawn at generation and recorded back into the exported params JSON / B-Fabric workunit so any `random`/`blocked`/`blocked_uniform` run can be reproduced.
- GUI test suite runs a fast `marimo check` sanity gate before spawning the app, so a notebook graph error (e.g. a variable defined in multiple cells) fails in <1s instead of after the full browser suite.
- Queue app: renamed the **Show Plate** tab to **Visualizations** with two sub-views — a *Plate Layout* map that can color wells by `grouping_var` as well as sample type, and an *Acquisition Timeline* strip colored by group or QC cadence — each annotated with a correlation-ratio (η²) balance score reporting how well randomization decorrelated group from plate and queue position.
- Unit tests for `qg.utils`, `qg.writers`, `qg.artifacts`, and `read_queue_input` error paths.
- Standalone, B-Fabric-free local queue app (`qg-app-local`, `apps/queue_app_local.py`): upload a CSV/XLSX sample table, configure the queue with the same controls, preview it, and download the queue + params JSON locally — no FGCZ portal required.
- `qg[bfabric]` optional extra: the B-Fabric/portal packages (`bfabric`, `bfabric-asgi-auth`, `bfabric-rest-proxy`, `fastapi`, `starlette`, `python-gitlab`) are no longer core dependencies, so `pip install qg` (and `import qg`) works without them; install `qg[bfabric]` for the portal app, workunit upload, and the GitLab launcher.

### Changed
- Visualizations: the η² balance scores (group vs. queue position and group vs. plate position) are now computed within each container and averaged across containers, since randomization never crosses container boundaries; a single global score over a multi-container queue was inflated by the by-design separation between projects.
- Acquisition-timeline visualization: the color legend no longer overlaps the x-axis title (moved below it with extra bottom margin).
- Local queue app: the load status now names the uploaded file (e.g. _Loaded 30 samples from `samples.csv`_) instead of marimo's generic "Uploaded 1 file".
- JASMS article workflow now sources the abstract from `jasms_article.Rmd` and builds both ACS PDF and DOCX drafts from the same markdown pipeline.
- Refactored `test_queue_structure.py` to exercise the public `build_multi_container_queue_structure` API instead of private helpers.
- Both queue apps now share a B-Fabric-free pipeline core (`apps/queue_app_shared.py`) and swappable source/sink integrations under `apps/integrations/` (`local_samples`, `bfabric_samples`, `bfabric_workunit`, `bfabric_context`); the portal app is unchanged for users.

## [0.6.1] - 2026-06-09

### Fixed
- Config: added the missing `Lipidomics`/`QEXACTIVEHF_2` instrument (methods file + `instruments.csv` row) so the instrument offered in the queue app's Lipidomics menu can actually generate queues instead of failing at lookup.

## [0.6.0] - 2026-06-08

### Added
- Headless GUI test suite for the queue app under `tests/gui/` (pytest-bdd + Playwright), hermetic by default with an optional Tier-B suite against `fgcz-bfabric-test.uzh.ch`.
- Queue app: **Show Plate** tab visualizing the plate layout and generated queue positions, colored by sample category and (for multi-order queues) shaped by order, with per-well hover details.
- Documentation site (MkDocs Material) published to GitLab Pages via a CI `pages` job.
- `LICENSE` (Apache-2.0), `CITATION.cff`, and `CONTRIBUTING.md` at the repo root.
- Per-tech-area UI defaults (User field, pre-checked polarities, and the B-Fabric order-browser Area filter) now live in `qg_configs/ui/tech_area_defaults.toml` and are editable from the config editor's new **Tech Area Defaults** tab.

### Changed
- Documentation overhaul: `README`, `docs/reference/config.md`, `docs/reference/algorithm.md`, `qg_configs/README.md`, and the config ER diagram now reflect the current `core/{structure,position,formatting,methods}` + `ui/` layout and pipeline.
- README de-duplicated against the docs: the parameters JSON schema, config-files table, and deployment procedure now live solely in `docs/` and are linked from the README rather than copied.
- Docs site: dropped the PlantUML diagrams (and their CI render step); the pipeline is described in `docs/reference/algorithm.md`. Site emits plain `.html` (browsable from disk).
- Queue app: session banner now shares the top row with the Refresh Projects button, the "Order Selection" heading is removed, and the order table shows 5 rows per page — saving vertical space.
- Queue app: the User field now defaults to the logged-in user for every technology except Proteomics (which keeps `analytic`); employees can still edit it.
- Queue app: opening the app from a B-Fabric order now aligns the Tech Area to that order and, for employees, pre-selects it in the order table — fetching it on the fly (no full refresh) if it isn't cached yet — so the launching order's samples load immediately.
- CI: split tests into parallel `test:unit` / `test:gui` jobs on the Playwright image with uv download caching — ~35% faster pipelines.

### Fixed
- Docs: corrected the queue-parameters JSON schema (README, AGENTS.md, `docs/reference/`) to match `params_models.py` — `{parameters, queue}` with `tech_area`, `queue_type`/`plate_layout`/`qc_layout_name`, string `randomization`, and nested `batches`/`samples`/`cells`; Hystar output is `.xml`.
- Tests: the `cal_series` concentration-grid GUI scenario no longer flakes under CI load — its QC Layout option wait now uses the suite's 15s marimo round-trip budget instead of Playwright's 5s default.
- Queue app: the selected-orders banner now renders again — its `mo.md` output was previously discarded inside `if/elif/else` branches.
- Queue app no longer crashes building queues where a grouping variable (or tray) first appears after the 100th row; polars now scans the full row set for schema inference.

### Removed
- Removed the obsolete `test_data/` validation corpus and the `qg-tools` CLI (`sld_to_csv`/`compare`/`summarize`/`merge`) and `tools_apps` marimo app that only served it.

## [0.5.5] - 2026-06-04

### Fixed
- `Sample Type` column now emits Xcalibur-accepted values (`Unknown`/`Blank`/`QC`/`Std Bracket`) instead of lowercase internal names, so Metabolomics sequences import without sample-type errors.
- Queue app: the Vial/Plate dropdown is now labelled "Queue Type" (was mislabelled "Sample Type").

## [0.5.4] - 2026-06-02

### Added
- **End-of-queue marker**: the last file of each container subqueue gets an `_eoq` suffix (so downstream QC can detect end-of-queue from filenames). Toggle via `mark_end_of_queue`, default on.

## [0.5.3] - 2026-05-22

Consolidated re-release of the 0.5.x line; 0.5.0–0.5.2 were never properly deployed.

### Added
- `QG_GITLAB_URL` / `QG_GITLAB_PROJECT` env vars supply the config editor's GitLab settings, so no `.qg_settings.toml` is required.
- **Start tray** dropdown also available in plate mode, so QC-layout collisions on the default tray can be resolved from the GUI.

### Changed
- Config editor refuses non-employees; MRs are attributed to the requesting employee's B-Fabric login (the free-text author input is gone).
- Marimo bumped to `>=0.23.7` to fix `Invalid session id` errors (marimo PR #9364).

### Fixed
- Authenticated B-Fabric users are no longer rejected with "Authentication required" — workaround for marimo 0.23.4 coercing `scope["user"]` into a plain dict before cells see it.
- Config editor no longer rejects authenticated users: its session cookie path now matches the `/queue-gen-editor` deployment prefix (was scoped to `/qg-editor`, so the browser never sent it back).
- Config editor no longer drops per-technology overlays (e.g. `[xcalibur_sii.columns.Metabolomics]`) when saving `output_formats.toml`. `OutputFormatsConfig.to_dict` now re-nests `columns_by_tech` under `columns`, matching the on-disk shape that `from_dict` expects.

## [0.5.2] - 2026-05-22 [YANKED]

Never deployed — tag was placed on the `0.5.1` commit by mistake (`pyproject.toml` at the tag still reads `0.5.1`). Superseded by `0.5.3`.

## [0.5.1] - 2026-05-22 [YANKED]

Never properly deployed — shipped a broken authentication flow (marimo 0.23.4 `scope["user"]` coercion). Superseded by `0.5.3`.

## [0.5.0] - 2026-05-21 [YANKED]

Never properly deployed. Superseded by `0.5.3`.

## [0.4.4] - 2026-05-21

### Added
- `noqc` QC layout for Metabolomics and Lipidomics — pick it to skip all QC injections. The Queue Pattern dropdown is hidden when this layout is selected.
- Metabolomics calibration series: `cal1..cal7` samples (`sample_id` unique, shared `sample_name=cal`, `sample_type=standard`, `levels=1..7`), a new `cal_series` QC layout (Vanquish_54 / Plate_96, tray Y row D), and a `Metabolomics.cal_series` queue pattern that bookends the user samples with the cal dilution series.
- `{level}` and `{concentration}` placeholders in `file_name_template`. The Metabolomics cal templates render as `..._cal_<level>_<concentration>_<polarity>`; runs of consecutive underscores in the rendered filename collapse to one so an empty `{concentration}` does not produce `__`.
- Per-level concentration GUI in the queue app: shown when the selected QC layout contains `standard`-type samples (currently only Metabolomics `cal_series`). Numeric value 1..999 + unit dropdown (`pmol`/`nmol`/`umol`/`pgml`/`ngml`/`ugml`); persisted on `QueueParameters.level_concentrations` (round-trips through `params.json`).
- QC Positions sidebar table gains a `visits` column showing how many times each well is injected in the generated queue (well-plate samplers). Wells defined by the layout but unused by the chosen pattern surface as `0`. Tip-plate samplers still show only the range — per-range counts deferred.
- `samples.csv` gains `sample_type` (`unknown`/`blank`/`qc`/`standard`) and `level` columns for explicit downstream-analysis categorisation. Loaded onto `Sample.sample_type` / `Sample.level` and carried through `QueueRow.sample_type` / `QueueRow.level`.
- Per-technology output-column overrides in `output_formats.toml` via nested `[<format>.columns.<TechArea>]` sub-tables, merged over the base column map at write time.
- Metabolomics xcalibur_sii output now includes `Sample Type` and `Level` columns; other technologies are unchanged.

### Changed
- `samples.csv`: replace literal sample IDs in `file_name_template` with `{sample_name}` to remove copy/paste duplication. Rendered filenames are unchanged.
- Internal: `QueueRow.sample_type` (formerly `"user"|"qc"`, used during position generation) renamed to `QueueRow.slot_kind`. The `sample_type` field now carries Alaa's downstream-analysis classification.
- `samples.csv` column and pydantic field `levels` → `level` (singular, since the value is per-row); placeholder `{levels}` → `{level}`. Output column header was already singular.

### Fixed
- File names no longer render with `__` when a placeholder (typically `{concentration}` for a `standard` sample) is unset; consecutive underscores in the rendered filename collapse to one (e.g. `cal_3__pos` → `cal_3_pos`).
- `QCSampleWell` now rejects layout rows with partial position fields outside the `noqc` placeholder, so typos in `qc_layouts_well.csv` raise at config-load time instead of being silently dropped.
- Cross-config validation now fails on `[<format>.columns.<TechArea>]` overlays whose tech_area is not present in `samples.csv`, preventing silently-unused overlays from spelling mistakes.
- QC Positions sidebar preview now shows only the QC samples actually referenced by the selected pattern (effective layout = QC layout ∩ pattern). Previously it surfaced every layout-declared position regardless of pattern; under `noqc` pattern the join against `raw_queue_df` produced misleading `visits` counts driven by user samples sharing the same wells.

## [0.4.3] -- 2026-05-14

### Added
- `BfabricHelper.get_container_composition()` returns a three-state classification (`has_plates`, `has_vials`) for a B-Fabric container.

### Changed
- Drop the stale "at bottom of the table" hint from the Order Selection heading in the queue app.

### Fixed
- Mixed plate+vial orders no longer block vial-only samplers in the queue app — Sample Type now reflects what the order actually contains, not just whether any plate exists.

## [0.4.2] -- 2026-05-11

### Changed
- Metabolomics config updated.

## [0.4.1] -- 2026-05-08

### Fixed
- Lock the `User` field to the B-Fabric login for non-employees.
- Silence marimo "Permission denied" warnings on read-only HOME by redirecting `XDG_CONFIG_HOME` / `XDG_STATE_HOME` to `/tmp` before marimo imports.

## [0.4.0] - 2026-05-08

Multi-instance B-Fabric support: PROD and TEST no longer overwrite each other.

### Added
- `qg-refresh-cache` CLI: refreshes container caches per configured B-Fabric instance using `feeder_user_credentials`. Bare invocation lists instances; `--all` or explicit URLs runs them.
- `QueueParameters.bfabric_instance` stamped on saved `params.json` for provenance.

### Changed
- Container cache is per-instance: `<root>/<instance-host>/bfabric_container*.csv`. Root defaults to `<repo>/bfabric_cache`, override with `$QG_CACHE_DIR`. Re-run `qg-find-projects` once per instance.
- `qg-find-projects` uses bfabricpy's `@use_client` instead of hardcoded `PRODUCTION`.
- Authenticated banner shows the instance host alongside the login.

### Fixed
- Non-employee users can load plate-typed orders; shared-plate samples are filtered to the user's container.

## [0.3.2] - 2026-04-22

### Fixed
- Bumped `bfabric-asgi-auth` (and sibling `bfabric` / `bfabric-rest-proxy` packages from the same `bfabricPy` repo) to pick up the fix that respects `scope["root_path"]`, so the app works correctly when bound behind a reverse proxy at a non-root path.

## [0.3.1] - 2026-04-21

### Changed
- CI `build` job now cross-builds an `arm64` OCI image on the `amd64` GitLab runner via `buildah bud --platform linux/arm64`, matching the ARM64 deploy target. Requires `qemu-user-static` + registered `binfmt_misc` handlers on the runner host (one-time admin setup, kernel-global).
- `qg-find-projects` now includes orders in the `processed` state when listing active projects.

## [0.3.0] - 2026-04-20

### Changed
- Modernized B-Fabric authentication to use upstream `BfabricUser` from `bfabric-asgi-auth`.
- Bumped `bfabric` to 1.18.0 and refreshed related auth dependencies.

## [0.2.0]

Initial tracked release.
