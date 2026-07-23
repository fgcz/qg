# Code Review — `plate_assignment_refactor`

**Date:** 2026-07-23
**Repo:** `qg`
**Scope:** full delta of branch `plate_assignment_refactor` vs `main` (10 commits) **plus** the uncommitted working-tree changes (`git diff main`). 59 files, ~2188 insertions / ~1316 deletions.
**Method:** six review dimensions (correctness, design/abstraction, DRY,
consistency, tests, build/CI) run against the project's own `AGENTS.md` rules.
The raw, overlapping reports were consolidated into 13 findings. A subsequent
independent verification reproduced every behavioral claim and checked each
recommendation against project policy: 10 findings were confirmed (one at lower
severity) and 3 were rejected as actionable defects. Two additional candidates
had already been checked and dismissed.

---

## Overall assessment

The refactor is **largely sound**. The core direction is good:

- concrete plate/sampler types get the methods they need instead of orchestration layers;
- the well-path validator now raises a clean `ValueError` for an orphan plate foreign key instead of silently propagating `None`;
- production dispatch is narrowed through typed `@overload`s.

There is **no correctness defect in the happy path**. The refactor's logic is
correct for in-system producers; the crashes flagged below are reachable through
previously archived JSON or externally supplied plate data. Most findings are
hygiene (stale docstrings, dead helpers, duplicated defaulting).

**Two issues should block merge**, and a **third cluster** (the cell→plate resolution) is worth fixing as one unit.

---

## Findings summary

| # | Sev | Category | Location | Verdict | Issue |
|---|-----|----------|----------|---------|-------|
| 1 | HIGH | build | `scripts/` | CONFIRMED | New CI/pre-commit scripts untracked; clean checkout can't run the jobs |
| 2 | HIGH | correctness | `params_models.py:101` | CONFIRMED | `seed` change rejects explicit `null`, breaks reading old params JSON |
| 3 | MED | correctness | `generator.py:133` + `positionV2.py:236` | CONFIRMED | cell→plate resolution: tip/well inconsistency + duplication + untested branch |
| 4 | LOW | duplication | `positionV2.py:145` / `:189` | CONFIRMED | start-position filter block duplicated verbatim across both pools |
| 5 | MED | build | `.gitlab-ci.yml:30` | CONFIRMED | release build lost its test gate but stays tag-triggered |
| 6 | — | build | `security.yml:26` | REJECTED | known-red scheduled audit is intentional and explicitly must not suppress advisories |
| 7 | LOW | duplication | `positionV2.py:505` | CONFIRMED | `start_tray` `""` default applied twice + redundant sampler fetch |
| 8 | LOW | consistency | `positionV2.py:216` / `:261` | CONFIRMED | stale / diff-narrating validator docstrings |
| 9 | — | design | `hystar_xml_writer.py:14` | REJECTED | `str` is a valid conventional filename input and predates the refactor |
| 10 | LOW | design | `params_models.py:279` | CONFIRMED | `parse/read_positioned_queue_input` unused, untested |
| 11 | LOW | tests | `positionV2.py:289` | CONFIRMED | Evosep tip out-of-layout rejection untested |
| 12 | LOW | build | `pyproject.toml:16` | CONFIRMED | dead deps `vegafusion` + `vl-convert-python` |
| 13 | — | consistency | `positionV2.py:250` | REJECTED | common assigner signature is deliberate and explicitly documented |

---

## Answers to the two questions that prompted this review

### Q1 — Why does `write_hystar_xml` take `path: Path | str | BinaryIO`?

`path` is forwarded verbatim to `xml.etree.ElementTree.ElementTree.write()` ([`hystar_xml_writer.py:50`](src/qg/hystar_xml_writer.py#L50)), whose first argument accepts either a filename or an open writable binary stream. So the union mirrors "whatever `ET.write` accepts." But of the three arms, only two are used by any caller:

| Type | Caller |
|------|--------|
| `BinaryIO` | [`writers.py:36`](src/qg/writers.py#L36) — the only production caller, writing into a `BytesIO` buffer so `_write_hystar_xml` can return a `str` for the unified `WriterFn` |
| `Path` | [`test_hystar_xml_writer.py:35`,`:64`](tests/test_hystar_xml_writer.py#L35) |
| `str` | **nobody** |

The refactor *added* `BinaryIO` (was `Path | str`), which is justified by the
new buffer path. Keeping `str` is also correct: it is a conventional filename
input accepted by `ET.write`, and narrowing it would remove valid existing API
surface without fixing a defect. Keep `Path | str | BinaryIO`; update the
docstring to mention the binary-stream case when the file is next touched.

> The sibling edit on that hunk, `ET.SubElement(root, "Sample", **attrs)` → `..., attrs)`, is a genuine correctness fix: `SubElement` takes the attrib dict positionally, and `**attrs` breaks the moment an attribute name is not a valid Python identifier.

### Q2 — Should the `plate = queue.plates.get(cell.plate_id)` lookup + `None` guard live in `_check_collisions`?

No. Your instinct is right on two counts, and it connects to finding #3.

**The change itself is a good direction.** The pre-refactor code silently degraded a missing plate/tray into `Position(tray=None, ...)`, a latent wrong-result. The new explicit `raise` at [`positionV2.py:238-241`](src/qg/positionV2.py#L238-L241) fails loud instead.

**But the lookup is misplaced.** `PlateCell.plate_id → PlateQueue.plates[id] → Plate.tray` is a foreign-key resolution: a domain operation that belongs on the model, not hand-rolled inside a validator. The tell is that the identical join + guard + error string is **duplicated** in [`generator.py:133-135`](src/qg/generator.py#L133-L135). Per `AGENTS.md` ("keep behavior beside the subsystem that owns the domain operation"), give `PlateQueue` the operation once (e.g. `cell_position(cell, plate_layout) -> Position`) and call it from both sites.

**Reachability nuance (from adversarial verification):**
- The `plate is None` (unknown `plate_id`) raise **is** reachable — nothing cross-validates that every `cell.plate_id` exists in `plates`. This is the check with real value; it belongs at the model boundary.
- The `plate.tray is None` raise **never fires at runtime** (`_assign_trays_if_missing` + the `PositionedQueueInput.plates_have_trays` validator both guarantee a tray upstream). It is *not*, however, freely deletable: it also type-narrows `tray` from `str | int | None` to non-`None` for the `Position(...)` constructor. Fold it into the shared accessor rather than dropping it outright.

---

## Detailed findings

### 1. HIGH · build · New CI/pre-commit scripts are untracked

[`scripts/`](scripts/) is `?? untracked` (empty in `git ls-files scripts/`), yet the branch adds jobs and hooks that invoke it:
- [`ci.yml:97`](.github/workflows/ci.yml#L97) → `python scripts/package_smoke.py`
- [`ci.yml:157`](.github/workflows/ci.yml#L157) → `python scripts/core_profile_check.py`
- [`.pre-commit-config.yaml:71`,`:81`](.pre-commit-config.yaml#L71) → the same two scripts

Only committed files reach a CI runner or a fresh clone, so on a pushed branch/PR the package-smoke and core-no-bfabric jobs (and the matching pre-commit hooks) fail immediately with `FileNotFoundError`. The new CI coverage this branch advertises never actually runs.

**Fix:** `git add scripts/package_smoke.py scripts/core_profile_check.py` and commit them on this branch.

### 2. HIGH · correctness · `seed` change breaks reading pre-refactor params JSON

[`params_models.py:101`](src/qg/params_models.py#L101) changed `seed: int | None = None` → `seed: int = Field(default_factory=draw_seed)`. A pydantic `default_factory` fires only when the key is **absent**; an explicit `"seed": null` present in the JSON is validated against `int` and raises `ValidationError` (confirmed at runtime).

Every params file the pre-refactor tool exported with `randomization="no"` (the default, and the exact shape in the `AGENTS.md` canonical example) serialized `seed` as `null`. Those files still carry valid `qg_version` and `resolved_config`, so `seed: null` becomes the single field that makes an archived, previously-reproducible file unreadable. `read_queue_input(old.json)`, `qg old.json`, `qg-assign-positions`, and the app's file-upload path all now crash at parse time on files that worked before.

**Fix:** keep the field typed as `int` and add a `mode="before"` field
validator that substitutes `draw_seed()` for the legacy `null` input. This
preserves the concrete downstream invariant and backward compatibility without
leaking `None` into the runtime type.

### 3. MED · correctness · Cell→plate resolution: inconsistency + duplication + untested branch

Three overlapping facets (see Q2 above):
- **(a) tip/well inconsistency.** The well path raises a clean `ValueError("... references unknown plate ...")` ([`positionV2.py:239`](src/qg/positionV2.py#L239)), but the tip path ([`positionV2.py:281`](src/qg/positionV2.py#L281)) never validates the FK and `plates_have_trays` only checks trays, so the same orphan `plate_id` surfaces downstream as a raw `KeyError` at [`generator.py:133`](src/qg/generator.py#L133).
- **(b) duplication + drift.** `_build_slots` ([`generator.py:132-141`](src/qg/generator.py#L132-L141)) and `_check_collisions` ([`positionV2.py:236-243`](src/qg/positionV2.py#L236-L243)) both do lookup + `split_alpha` + `Position` build with the identical `"is missing a tray."` string, and have already drifted (bare subscript vs `.get()` guard).
- **(c) untested.** The new unknown-plate branch has no test.

**Fix:** add `PlateQueue.cell_position(cell, plate_layout) -> Position` holding the lookup, the single unknown-plate guard, `split_alpha`, and the one canonical error string; call it from both sites (covers the tip path uniformly). Add a test asserting `position_queue()` raises `ValueError(match="unknown plate")` for a cell whose `plate_id` is absent from `plates`.

### 4. LOW · duplication · Verbatim start-position filter block across both pools

The `tray_strs` / `start_tray_idx` / `start_flat` setup, the on-or-after comprehension, and the `self.by_tray = group_by_key(...)` assignment are byte-identical in `_PositionPoolWell` ([`145-159`](src/qg/positionV2.py#L145-L159)) and `_PositionPoolTip` ([`189-203`](src/qg/positionV2.py#L189-L203)). The only real difference is one line above (Well seeds `self.available` from the reserved-filtered list, Tip from `all_positions`). The refactor already had to edit both copies in lockstep.

**Fix:** extract `_filter_from_start(positions, trays, start_tray, start_position, plate_layout) -> list[Position]`; Well passes the reserved-filtered list, Tip passes `all_positions`. Two real call sites, so this is the sanctioned "abstract when multiple real sites need it" case.

### 5. MED · build · GitLab release build lost its test gate

[`.gitlab-ci.yml:7-9`](.gitlab-ci.yml#L7) deletes the entire `test` stage (`stages` is now `[build, deploy]`) but keeps the tag-triggered [build job](.gitlab-ci.yml#L30) that cross-builds the arm64 OCI archive to NFS. Previously the classic stage ordering made `build` wait for `test`, and the test jobs (no `rules:`) ran on tag pipelines, so a release tag was test-gated. That gate is gone: pushing `vX.Y.Z` now builds and writes `queue_gen-<tag>.oci.tar` with no tests in the pipeline. `AGENTS.md` still documents this GitLab job as the live release-to-NFS path.

**Fix:** pick one release path. If `publish.yml` (pre-existing on `main`) truly replaces it, delete the GitLab build/deploy jobs; if GitLab still builds the deployed artifact, restore a test gate the build depends on before it can run on a tag. Contingent: the GitLab remote is pre-wired but, per the workspace `AGENTS.md`, not pushed.

### 6. REJECTED · build · Scheduled pip-audit has no allow-list

[`security.yml:26`](.github/workflows/security.yml#L26) runs `pip-audit`
blocking with no `--ignore-vuln`, and installs the bfabric-pinned `starlette
0.52.1` (via `qg[bfabric]`). Runtime verification reports seven advisories, all
requiring Starlette 1.x.

This is intentional project policy, not a defect:
`TODO_automatic_code_review.md` explicitly says the advisories must remain
unsuppressed until the upstream constraint is fixed. The workflow is scheduled
and manual, not a pull-request merge gate. Adding `--ignore-vuln` would
contradict both the root-cause rule and the recorded security decision.

### 7. LOW · duplication · `start_tray` default applied twice

`_create_input_sampler` ([`positionV2.py:504-505`](src/qg/positionV2.py#L504-L505)) fetches the sampler and computes `start_tray = parameters.start_tray if parameters.start_tray != "" else sampler.trays[0]` purely to hand a concrete tray to `create_assembled_sampler`, which re-fetches the same sampler and re-applies the byte-identical default at [line 442](src/qg/positionV2.py#L442). `parameters.start_tray` is `str | int = ""`, the same type and sentinel the factory defaults on. Flagged independently by three dimensions.

**Fix:** delete lines 504-505 and pass `start_tray=parameters.start_tray` straight through, letting the factory apply the single canonical default.

### 8. LOW · consistency · Stale / diff-narrating validator docstrings

Both plate validators still claim they "split alpha grid_position into row/col components" after that behavior and the `PlateCell` row/col fields were removed: [`positionV2.py:211-217`](src/qg/positionV2.py#L211-L217) and [`:258-264`](src/qg/positionV2.py#L258-L264). The tip docstring's "are now the internal representation, no numeric conversion needed" is change-narration `AGENTS.md` forbids.

**Fix:** state the current contract (validate wells against the layout; well path also rejects QC-position conflicts; tip path has no QC pass) and drop the row/col-split and "now" wording.

### 9. REJECTED · design · `write_hystar_xml` accepts `str`

See **Q1**. Keep `str`; only the docstring needs to describe both path and
binary-stream inputs.

### 10. LOW · tests · Dead positioned-queue read helpers

`parse_positioned_queue_input` / `read_positioned_queue_input` ([`params_models.py:279`,`:284`](src/qg/params_models.py#L279)), added this refactor, have zero callers in `src/` and no tests (both CLIs read the unpositioned `QueueInput` then `.position_queue()`; `test_cli.py` reads positioned JSON via `PositionedQueueInput.model_validate_json` directly). The sibling `write_positioned_queue_input` **is** used.

**Fix:** remove them, or wire the read helper into the CLI / a round-trip test.

### 11. LOW · tests · Evosep tip out-of-layout rejection untested

The Evosep tip validator's out-of-layout rejection (`assign` → `split_alpha`, [`positionV2.py:289`](src/qg/positionV2.py#L289)) is never exercised: `TestEvosepPlateAlphaPassthrough` feeds only valid wells despite its docstring, and the parametrized out-of-layout tests cover only Vanquish/MClass. Low impact (`split_alpha` is itself unit-tested).

**Fix:** add one Evosep `"A99"` case asserting `pytest.raises(ValueError)`.

### 12. LOW · build · Dead dependencies `vegafusion` + `vl-convert-python`

[`pyproject.toml:16-17`](pyproject.toml#L16) are altair's Vega backends, left over after altair was removed; the repo now uses only plotly and has zero vega/altair/vl_convert references. The `DEP002` deptry ignore ([line 175](pyproject.toml#L175)) masks them.

**Fix:** drop both from `[project.dependencies]` and from the ignore list (keep `pyarrow`, which is legitimate); re-run deptry and the apps.

### 13. REJECTED · consistency · Unused `one_container_per_tray` on the plate path

`PlateValidator.assign` and the protocol ([`positionV2.py:250`,`:281`,`:389`](src/qg/positionV2.py#L250)) declare `one_container_per_tray`, but both plate implementations ignore it (`# noqa: ARG002`) and no plate call site supplies it. It only mirrors the vial assigner's signature.

**Assessment:** keep it. Uniform signatures across the two-member
`AssembledSampler` union are deliberate, and the implementations explicitly
mark the argument with `# noqa: ARG002`. Removing it would create asymmetric
dispatch for no behavioral gain.

---

## Refuted findings (checked and dismissed)

Recorded for completeness so they are not re-raised:

- **`__init__.py:5` — eager `version("qg")` crashes from an uninstalled tree.** Refuted: the project uses a `src` layout (`packages = ["src/qg"]`), so `import qg` is impossible from a plain checkout; it works only after an install, and every supported install (`pip`/wheel or `uv sync` editable) registers the `.dist-info` metadata `version()` needs.
- **`generator.py:134` — tray-missing guard is removable dead code.** Partially refuted: the raise indeed never fires at runtime (`plates_have_trays` guarantees a tray), **but** the guard also type-narrows `tray` for the `Position(...)` constructor, so it is not freely deletable. Correct action is to fold it into the shared accessor from finding #3, not to remove it.

---

## Implementation scope

1. **Merge-blockers:** #1 (commit `scripts/`), #2 (backward-compatible `seed`).
2. **One cohesive change:** #3 (+ the `tray is None` narrowing from the refuted
   note): add `PlateQueue.cell_position(...)`, reuse it, and test both sampler
   families through the public positioning API.
3. **Release assurance:** #5.
4. **Hygiene:** #4, #7, #8, #10, #11, #12.
5. **Deliberately unchanged:** #6, #9, #13.

## Implementation update

The confirmed code and CI findings have been addressed:

- #2 accepts legacy `seed: null` while retaining an integer runtime field.
- #3 resolves positions through `PlateQueue.cell_position()` for both sampler
  families and rejects unknown plates during positioning.
- #4 and #7 remove the duplicated position filter and start-tray defaulting.
- #5 restores a GitLab tag-release test stage before the OCI build.
- #8 updates the validator docstrings.
- #10 removes the unused positioned-input readers.
- #11 adds Evosep out-of-layout coverage.
- #12 removes the dead Vega dependencies and their deptry exemptions.

#1 is a source-control completion requirement: the two files under `scripts/`
must be included in the eventual commit. Findings #6, #9, and #13 remain
unchanged by design.
