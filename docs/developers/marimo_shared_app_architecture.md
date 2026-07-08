# Shared queue-app architecture

How the portal (`queue_app.py`) and local (`queue_app_local.py`) marimo
notebooks avoid code duplication by sharing a single, B-Fabric-free helper
module (`queue_app_shared.py`). Read this before editing either notebook.

## Motivation

The two notebooks solve the same problem (build a sample queue from a set of
inputs) through the same widgets, filters, and views. They differ only at the
edges: the portal loads samples from B-Fabric and uploads a workunit; the local
app uploads a CSV/XLSX and offers a download.

Before the refactoring (commit `105b93b`, Jul 2026) that difference was drowned
out by duplication: about 50 cell bodies were byte-identical across the two
files, roughly half of each. `queue_app.py` was 1573 lines and
`queue_app_local.py` was 1445. Every change to a shared widget or view had to be
made twice and kept in sync by hand, and the two copies drifted.

## The pattern: shared bodies, thin binding cells

marimo requires **one cell per reactive symbol**: a global such as
`instrument_field` or `samples_table` must be assigned in its own `@app.cell`
function so the reactive graph can track it. That constraint is why the two
notebooks cannot simply `import` each other's cells.

The refactoring splits each shared cell into two parts:

- **The substance** moves into a plain function in `queue_app_shared.py`
  (no marimo, no B-Fabric), defined **once**.
- **A thin binding cell** stays in each notebook. It only wires the notebook's
  reactive inputs into the shared function and returns the output symbol.

A binding cell is now a one-liner over `shared`:

```python
@app.cell
def _(config, tech_area_field):
    name_suffix = shared.make_name_suffix(config, tech_area_field.value)
    return (name_suffix,)
```

The wrapper is unavoidable (marimo needs the cell), but the logic behind it lives
in exactly one place.

The shared module is imported once per notebook, in the `with app.setup:` cell
(`from qg.apps import queue_app_shared as shared`), next to the other common
imports and `configure_logging()`. `app.setup` is marimo's sanctioned home for
imports and constants every cell may use, so binding cells never re-import
`shared`.

## What moved

The extracted helpers fall into three categories.

- **Views (`render_*`)** return the marimo layout for a panel: plate/timeline/
  visualizations, valid-combinations, the concentration block, validation
  status, sample-selection content, the sidebar body, and the tabbed layout.
- **Transforms** are pure functions over data: `filter_by_column` (which
  collapses the seven-cell faceted-filter chain into one reusable call),
  `validate_selection`, `resolve_qc_layout_preview`, `available_method_names`,
  `load_methods_table`, `resolve_default_qc_frequency`,
  `resolve_level_concentrations`, and `build_queue_parameters`.
- **Widget factories (`make_*`)** build `mo.ui` controls: the menu dropdowns,
  method fields, the polarity group, the queue-type field (plus its warning),
  the samples table and editor, the visualization controls, and the tab
  selector.

As of this writing `queue_app_shared.py` is about 1010 lines exposing ~45 public
helpers, and the notebooks are 1037 (`queue_app.py`) and 950
(`queue_app_local.py`) lines, almost all of it binding cells and the divergent
seam below.

## The variable-name contract

Both notebooks (and the GUI tests) agree on a fixed set of global names, so a
shared cell body reads and writes the same symbols regardless of which notebook
it is bound in. The contract includes `full_samples_df`, `selected_orders`,
`container_has_*`, `queue_input`, `queue_output_str`, and the widget fields
(`tech_area_field`, `instrument_field`, `samples_table`, ...).

Two consequences:

- A shared helper depends on names, not on which app is running. `filter_by_column`
  is called identically in both notebooks.
- The GUI tests drive the notebooks by these names, so one test body can cover
  both apps. When a test previously ran only on the portal, moving the logic to
  a shared helper made the local equivalent testable too (for example the local
  Visualizations tab, added as `tests/gui_local/test_local_viz.py`).

Renaming one of these globals in a notebook without updating the contract (and
the shared helpers that reference it) will silently break the other app and the
tests. Keep the names identical across both notebooks.

## What stays divergent (the integration seam)

The refactoring deliberately did **not** try to share the parts that genuinely
differ between the two apps. These cells remain per-notebook:

- **Sample source** (portal: B-Fabric order/sample loading; local: CSV/XLSX
  upload and synthesized `selected_orders`).
- **Tech-area / user fields** and the **sidebar reproduce summary**, which the
  portal derives from the B-Fabric session and the local app synthesizes.
- **The `queue_parameters` reproduce override** (the local app can replay an
  uploaded params JSON verbatim, bypassing `sample_df`).
- **Generation, preview, and the sink** (portal: workunit upload; local:
  file download).

The portal app imports B-Fabric; the local app must not. Keeping the seam in
each notebook is what lets `queue_app_shared.py` stay B-Fabric-free and
core-installable, so the local app and the `test:core-no-bfabric` CI job never
pull in the `qg[bfabric]` extra. Swappable source/sink integrations live under
`apps/integrations/` (see `AGENTS.md`).

## Alignment with marimo's modularization model

marimo offers three ways to share code, and this design uses them as intended.

1. **Import a plain Python module** (what `queue_app_shared.py` is). Because a
   marimo notebook is an ordinary `.py` file, a notebook can import normal
   functions and call them from cells. This is the primary reuse and testability
   path, and it is where all shared substance lives.
2. **The setup cell** (`with app.setup:`) holds imports and constants shared by
   every cell. Both notebooks use it for the `shared` import and common
   dependencies.
3. **`@app.function` / `@app.class_definition`** export a single top-level,
   *pure* function or class defined inside a notebook so other scripts or
   notebooks can import it directly. We deliberately do **not** use this to share
   between the two apps (see below).

A fair reading of the marimo docs is: marimo is **good at modularizing logic and
poor at modularizing the reactive layer**, and that split is by design, not an
accident.

- **Logic modularizes cleanly.** Pushing pure functions into a module (or into
  `@app.function`) is explicitly encouraged; it makes the code importable,
  unit-testable, and editable in a normal editor. This is a real improvement over
  Jupyter, whose JSON cell format resists extraction. Our `render_*` / transform
  / `make_*` helpers are exactly this.
- **The reactive graph does not modularize across notebooks.** Every global must
  be defined by exactly one cell (variable uniqueness), execution order comes
  from the dependency DAG, and there is no "import these cells" or reusable
  reactive sub-component that spans notebooks. So whenever two notebooks share a
  reactive symbol, each must carry its own binding cell. The residual
  boilerplate (the one-line wrappers, the variable-name contract) is a property
  of the reactive model, not of this design.

**Why a shared module rather than `@app.function` cross-imports.** `@app.function`
could expose a helper from one notebook to the other, but that would make the
local app import the portal notebook, which imports B-Fabric, breaking the
core-installable boundary. A separate module keeps the helpers B-Fabric-free,
testable without spinning up a notebook, and equally reachable from both apps and
the test suite. For this codebase the module is the correct choice; `@app.function`
is the right tool only for single-notebook reuse.

**What the constraint costs, and what it does not.** The pattern does not
eliminate every duplicated line; each shared reactive symbol still needs a
one-line binding cell in both notebooks, and the two must agree on global names.
What it does eliminate is duplicated *substance*: the body that actually does the
work exists once. The binding cells are mechanical and cheap to keep in sync
(`marimo check --strict` and the GUI parity tests catch drift); the pre-refactor
duplication was ~50 full cell bodies that silently diverged.

**Possible refinements** (not yet done, not urgent):

- Split `queue_app_shared.py` (~1010 lines) into `views` / `transforms` /
  `widgets` submodules if it keeps growing; the three categories are already
  clear seams.
- The variable-name contract is enforced only by convention and the GUI tests. A
  typed container passed to the helpers would make it explicit, but it fights
  marimo's model (reactive globals *are* the interface), so the convention plus
  test coverage is the pragmatic choice for now.
- Audit underscore-prefixed cell-locals: the marimo guidance warns against
  over-prefixing, so prefer a fresh descriptive name over `_`-prefixing a whole
  cell's variables where it does not prevent a genuine name clash.

## Working on the apps

- **A change to a shared widget, view, or transform** goes in
  `queue_app_shared.py`, once. Both notebooks pick it up through their binding
  cells; no second edit.
- **A change to how an app loads samples or emits its output** goes in that
  notebook's seam cell (or the relevant `apps/integrations/` module), not in the
  shared module.
- **Never import B-Fabric from `queue_app_shared.py`.** If a helper needs
  data that only B-Fabric can provide, pass it in as an argument from the portal
  notebook's seam.
- After editing either notebook, run `marimo check --strict` on both and the
  `test_queue_app_shared.py` unit tests plus the GUI suites; the refactoring is
  guarded by unit tests for the pure helpers and by GUI parity tests that run the
  same body against both apps.
