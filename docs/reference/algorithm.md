# Queue Generation Algorithm

## Architectural Goal

The queue generator follows a **stateless functional pipeline** design:

- Each stage receives all required inputs as arguments and returns a result
  without mutating shared state.
- Stages are composable and independently testable.

Physical positioning is orchestrated by `QueueInput.position_queue()`. Its
implementation in [`positionV2.py`](https://github.com/fgcz/qg/blob/main/src/qg/positionV2.py) converts vial
inputs or validates plate inputs and returns a `PositionedQueueInput`. The
remaining pipeline is orchestrated by `QueueGenerator.build_rows()` in
[`generator.py`](https://github.com/fgcz/qg/blob/main/src/qg/generator.py).

## Inputs

### Queue Parameters (JSON)

A `QueueInput` contains `{parameters, queue, qg_version, resolved_config}`
(vial shown; plate's `queue` carries `plates` + `cells`). Both provenance fields
are required:

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
    "inj_vol_override": null,
    "qc_frequency_override": null
  },
  "queue": {
    "batches": {"37180": {"container_id": 37180, "container_name": "Project A"}},
    "samples": [
      {"sample_name": "S1", "sample_id": 123456, "tube_id": "37180/1", "container_id": 37180}
    ]
  },
  "qg_version": "0.10.0",
  "resolved_config": {"...": "embedded configuration snapshot"}
}
```

The full field reference lives in [Configuration](config.md#queue-parameters-json-input).

### Config files

Loaded once via `qg_configuration()` and passed explicitly to `QueueGenerator`.
Interactive callers inject the configuration owned by the UI. The CLI and local
reproduce mode explicitly reconstruct the embedded `resolved_config` at their
composition boundary, then inject that configuration. `QueueGenerator` does not
select a configuration source internally.

See [Configuration](config.md) for the per-file reference. The constructor selects:

- the **pattern** (`queue_patterns.toml`),
- the **sampler** + **plate layout** (`sampler.toml`, `plate_layouts.toml`,
  `sampler_plate_layouts.csv`),
- the **QC layout** (`qc_layouts_well.csv` / `qc_layouts_tip.csv`; the special
  `no_layout` value applies no layout — queue as-is, nothing reserved),
- the **methods** table (`methods/<Tech>/<instr>_methods.csv`),
- the **path template** (`instruments.csv`).

---

## Positioning and generation pipeline

```
VialQueueInput / PlateQueueInput
    │
0.  queue_input.position_queue()                                 -> PositionedQueueInput
    │
1.  randomize_plate_queue(plate_queue, randomization)            -> PlateQueue
2.  build_multi_container_queue_structure(groups, pattern, …)    -> list[SlotEntry]
3.  create_qc_position_provider(qc_layout, slot_entries, …)      -> QCPositionProvider
4.  _build_slots(slot_entries, plate_queue, qc_provider, …)      -> list[SlotInfo]
5.  _expand_polarities(slots, polarities)                        -> list[ExpandedSlot]
6.  _resolve_methods(slots, methods_config, …)                   -> list[ExpandedSlot]
7.  _format_file_names(slots, date, …)                           -> list[ExpandedSlot]
8.  _build_queue_rows(slots, path_template, user, date, …)       -> QueueRowTable
9.  format_table(rows, output_format, plate_layout, tech_area)   -> pl.DataFrame
    │
    CSV / XML output
```

### 0. `queue_input.position_queue()`

Vial inputs are assigned deterministic physical plate/tray positions using the
configured sampler, plate layout, start position, and QC reservations. Plate
inputs retain their submitted positions after tray assignment and collision
validation. Both paths return the same `PositionedQueueInput`, which is the only
input accepted by `QueueGenerator`.

### 1. `randomize_plate_queue(plate_queue, randomization, rng)`

Reorders user samples *within* container/plate boundaries before structure is
built. Modes (`randomize.py`):

| Mode | Behavior |
|------|----------|
| `no` | Keep original order |
| `random` | Shuffle samples |
| `blocked` | Randomized complete block design — group by `grouping_var`, shuffle within blocks (front-loads complete blocks; majority-only tail) |
| `blocked_uniform` | Group-uniform interleave — spread each `grouping_var` group evenly across the whole run via fair-share selection; identity shuffled within groups. Reduces to `blocked` for equal group sizes |

Returns a new `PlateQueue`.

**Reproducibility.** Randomized modes draw from a `random.Random(seed)` instance.
Randomized inputs always carry a concrete `seed`. `QueueBuilder` draws it during
input construction when the caller did not supply one, before positioning or
generation. The exported input and positioned assignment are therefore already
self-contained and reproducible. `no` mode is deterministic and uses no seed.

**Balance score.** The queue app's **Visualizations** tab reports how well a run
is balanced with the *correlation ratio* (η², `qg.viz.balance`): the fraction of
variance in position explained by `grouping_var`, where `0` means the groups are
evenly interleaved (ideal) and `1` means they are fully separated. The *Plate
Layout* sub-view scores group ↔ plate position; the *Acquisition Timeline*
sub-view scores group ↔ queue position. Unlike the normalized statistic in the
JASMS figure script, η² is standalone and needs no baseline run, so it scores a
single generated queue. The score is **reporting only** — it is never used to
optimize over candidate orderings.

### 2. `build_multi_container_queue_structure(groups, pattern, default_sample_id, qc_frequency_override)`

Builds the abstract slot sequence for **all** containers. `groups` is a list of
`(container_id, num_samples)`. For each container it emits the pattern's `start`,
the user-sample placeholders interleaved with `middle` (every
`run_QC_after_n_samples`, or `qc_frequency_override` if given), and `end`;
between containers it inserts the pattern's `separation` block.

Returns a `list[SlotEntry]` — each entry is a QC sample id or the
`default_sample_id` placeholder, tagged with its container.

### 3. `create_qc_position_provider(qc_layout, slot_entries, default_sample_id, plate_layout)`

Builds a `QCPositionProvider` that hands out physical positions for QC slots
from the resolved QC layout (well row/col or Evosep tip range), reserving them
so user samples never collide with QC positions.

### 4. `_build_slots(slot_entries, plate_queue, qc_provider, samples_config, tech_area, default_sample_id)`

Turns each `SlotEntry` into a concrete `SlotInfo`: assigns the user sample (for
`default` slots) or QC sample definition, the position (generated for user
samples / from `qc_provider` for QC), injection volume, and sample type from
`samples.csv`.

### 5. `_expand_polarities(slots, polarities)`

For metabolomics/lipidomics (`polarities = ["pos", "neg"]`) each slot is
duplicated per polarity; for proteomics (`[]`) slots pass through with no
polarity. Returns `list[ExpandedSlot]`.

```
Input:  [QC01, default, default]                  (metabolomics)
Output: [(QC01, pos), (QC01, neg),
         (default, pos), (default, neg),
         (default, pos), (default, neg)]
```

### 6. `_resolve_methods(slots, methods_config, tech_area, instrument, method, default_sample_id)`

Resolves each slot's instrument-method path from
`methods/<Tech>/<instrument>_methods.csv`, keyed by `(sample_type, polarity)`.
The per-polarity `method` map from the parameters
(`{"pos": "...", "neg": "..."}`) selects which method name to use.

### 7. `_format_file_names(slots, date, level_concentrations, mark_end_of_queue)`

Fills each slot's `file_name` from the sample's `file_name_template` and runtime
values. When `mark_end_of_queue` is set, the last file of each container
subqueue gets an `_eoq` suffix.

### 8. `_build_queue_rows(slots, path_template, user, date, inj_vol_override, default_sample_id)`

Produces the final `QueueRowTable`: assigns sequential run numbers, fills the
`data_path` from `path_template` (`{container}`/`{user}`/`{date}`), and applies
`inj_vol_override` if provided.

### 9. `format_table(rows, output_format, plate_layout, tech_area)`

Maps internal fields to the output columns defined in `output_formats.toml` for
the requested format (`xcalibur` / `xcalibur_sii` / `chronos` / `hystar`),
returning a `polars.DataFrame` that the writers serialize to CSV/XML.

---

## QC sample definitions

Defined in `core/structure/samples.csv` (subset shown — see the file for the
authoritative list):

### Proteomics
| sample_id | sample_name | type | inj_vol |
|-----------|-------------|------|---------|
| QC01 | autoQC01 | QC | 2.0 |
| QC02 | autoQC02 | QC | 1.0 |
| clean | clean | Blank | 2.0 |
| default | (user sample) | Unknown | 2.0 |

### Metabolomics / Lipidomics
| sample_id | sample_name | type | inj_vol (Metab / Lipid) |
|-----------|-------------|------|--------------------------|
| blank | blank | Blank | 6.0 / 3.5 |
| pooledQC | pooledQC | QC | 6.0 / 3.5 |
| pooledQCDil2–7 | pooledQCDil2–7 | QC | 6.0 / 3.5 |
| 108mix_AA / 108mix_OAP | … | QC | 6.0 (Metabolomics) |
| cal1–7 | cal | Std Bracket | 6.0 (Metabolomics) |
| default | (user sample) | Unknown | 6.0 / 3.5 |

---

## File-name templates

From the `file_name_template` column of `samples.csv`, e.g.:

```
{date}_{run}_C{container}_S{sample_id}_{sample_name}   # default user sample
{date}_{run}_C{container}_{sample_name}                # QC sample
```

| Variable | Source |
|----------|--------|
| `{date}` | parameters `date` (YYYYMMDD) |
| `{run}` | sequential run number (zero-padded) |
| `{container}` | `queue.batches[].container_id` |
| `{sample_id}` | B-Fabric sample id (user) or QC sample id |
| `{sample_name}` | sample name |

Polarity, when present, is appended via the per-tech template.
