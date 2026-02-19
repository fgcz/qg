# Development History

Chronology of the queue generator rewrite (Jan–Feb 2026), reconstructed from
development task documents before archival.

## Phase 1: Foundation & Data Modeling (late Jan 2026)

**Config singleton & access patterns**
- Established `qg_config()` as exclusive config entry point with `ConfigBundle`
  typed accessors (`DONE_access_configurations`, `DONE_for_config_access`)
- Config editor planned as 5-stage migration to read/write API
  (`DONE_Editor_qg_config`)

**Queue data model**
- Introduced explicit Plate/Vial queue types with `VialSample`, `PlateCell`,
  `Plate` classes; randomization boundaries defined within plates/containers
  (`DONE_model_plates_20260125`)
- Designed parameter inference from 400+ legacy queue CSVs
  (`DONE_param_inference`)

**Sampler refactoring (round 1)**
- Cleaned sampler.toml: removed dead fields (qc_row, samples_per_plate,
  fill_order); proposed factory pattern for 6 sampler instances
  (`DONE_refactor_samplers`)
- Position formatting unified: row/col → grid_position for all samplers
  (`DONE_position_formatting`)

**GUI first steps** (2026-01-27)
- Tech area filtering implemented; hit marimo table limitations (no multi-column
  sort, no resizable columns) (`DONE_GUI_20260127`)
- Identified dead code: 6 unused config model methods (`DONE_dead_code`)
- API surface cleanup: 19 functions marked private (`DONE_make_PRIVATE`)
- Samples.csv consolidation proposed (`DONE_update_samples`)

## Phase 2: Config Model Redesign (early Feb 2026)

**New config model from scratch**
- Parallel config model designed: unified plate layouts, sampler TOML,
  sampler_plate_layouts.csv, separate QC layout CSVs for grid vs Evosep
  (`DONE_config_model_new_doc_3`)
- Config directory reorganized into structure/ (WHEN), position/ (WHERE),
  formatting/ (HOW), methods/ (WHAT); UI simplified by eliminating
  instrument_patterns.csv (`DONE_config_sub_modules`)

**Sampler refactoring (round 2)**
- Unified PlateLayout model, Sampler class with position_fun callable,
  sampler_plate_layouts.csv mapping (`DONE_sampler_refactoring_doc_1`)
- Clarified Evosep IS a grid (rows=position_starts, cols=offsets);
  position_fun registry approach; QC layout separation rationale
  (`DONE_sampler_refactoring_review_doc_2`)

**Position & tray allocation**
- Confirmed user samples CAN share trays with QC but avoid exact QC positions;
  documented vial vs plate collision handling
  (`DONE_are_trays_with_qc_allocated_to_defaults`)
- HyStar XML output support analyzed: needs XML writer with attributes
  (Position, SampleID, Volume, DataPath, SuperMethod) (`DONE_hystar_xml_support`)

**Terminology & data fixes** (2026-01-25 content, committed Feb 4)
- Tray/plate consistency fixed; randomization plate ordering preserves B-Fabric
  input order; one_container_per_tray parameter added (`DONE_martina`)

**Faceted GUI filtering**
- Single denormalized table defines valid configuration space; user selections
  filter rows; remaining unique values populate dropdowns
  (`DONE_Helping_GUI_Selection`)

## Phase 3: Code Review & Quality (Feb 4–5, 2026)

**Comprehensive code review** (Feb 4)
- Full 4,000+ line review: rated 4/5 with modern Python practices; identified
  mixed responsibilities in QueueGenerator, type system bypasses, monolithic
  Marimo apps, scattered validation (`DONE_CODE_REVIEW_REPORT`, `DONE_review`)

**Config access review** (Feb 5)
- Added `get_qc_samples()` convenience method; removed implicit plate layout
  fallback; concluded no need for separate QueueConfigResolver
  (`DONE_config_model_access_review`)
- Identified 4 redundant validation checks; proposed custom exception hierarchy
  (ConfigurationError, PositionError, CapacityError, CollisionError)
  (`DONE_excessive_exception_handling`)

**Quick wins identified** (Feb 5)
- Priority 1: DEFAULT_SAMPLE_ID constant, importlib.resources for paths,
  QueueConfigResolver extraction (`DONE_5_points`)

## Phase 4: User Testing & Output Fixes (Feb 6–12, 2026)

**First round — Claudia Fortes** (2026-02-06)
- Tested Exploris_1 Evosep on Chronos: randomized export format mismatch,
  Evosep/Plate position format wrong (alpha vs numeric), QC positions shift on
  CSV export (`DONE_Claudia_first_round`)

**Second round — Claudia** (2026-02-12)
- All 8 issues resolved: position format (xcalibur vs xcalibur_sii), preamble
  line, column order, sample type removal, sample ID addition, L3 Laboratory
  literal, position quoting, inj_vol formatting
  (`DONE_Second_round_review_Claudia_12_02_ENG`)

## Phase 5: Final Refactoring & Cleanup (Feb 16–18, 2026)

**Plate layout unification**
- Both Evosep (tip) and Vanquish/MClass (well) use 8×12 grids but different
  internal representations; proposed unified alpha (A1-H12) with late conversion
  to numeric at output time (`DONE_c_h_`, `DONE_chronos_hystar_evosep`,
  `DONE_tip_vs_well_plates_V2`)

**Code review actions completed** (Feb 16)
- Eliminated duplicate alpha_to_flat, added sampler_type to model, added runtime
  bounds checks, removed layout_mode from QueueGenerator
  (`DONE_code_review_Feb_16`, `DONE_review_addressed`)

**Magic strings audit**
- Identified sentinels ("default"), polarities, queue types, pattern names;
  recommended constants/enums (`DONE_string_variables`)

**GUI cleanup**
- Queue app UI improvements (`DONE_queue_app_gui_cleanup`)
- Writer factory for output formats (`DONE_writer_factory`)

**Chronos output format** (Feb 18)
- Chronos requires: row counter, exact column order, "EvoSlot N" tray format;
  dedicated Chronos writer with tray_format field
  (`DONE_chronos_formatting`)
