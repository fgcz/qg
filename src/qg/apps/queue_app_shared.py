"""B-Fabric-free pipeline helpers shared by the portal and local queue apps.

Both ``queue_app.py`` (FGCZ portal) and ``queue_app_local.py`` (standalone upload)
are thin marimo notebooks that declare their own cells; the reusable logic lives
here so it is written once and is unit-testable. Nothing in this module imports
B-Fabric — only ``marimo`` (for the download controls) and the source-neutral core
(``QueueBuilder`` / ``QueueGenerator``).

The notebooks agree on a variable-name contract so the shared cells and the GUI
tests are identical across both: ``sample_df`` -> ``queue_input`` ->
(``generated_queue_df``, ``raw_queue_df``, ``queue_output_str``) ->
``queue_output_filename``.
"""

from __future__ import annotations

import datetime
import json
from dataclasses import dataclass
from typing import TYPE_CHECKING

import marimo as mo
import polars as pl
import pydantic
from loguru import logger

from qg.artifacts import save_generation_artifact, save_positioning_artifacts
from qg.config_models.structure import NO_LAYOUT, SamplesConfig
from qg.generator import QueueGenerator, format_table, write_queue
from qg.params_models import QueueParameters
from qg.queue_builder import QueueBuilder
from qg.viz.balance import plate_balance, queue_balance
from qg.viz.plate import build_plate_figure, build_plate_wells
from qg.viz.timeline import build_timeline_figure

if TYPE_CHECKING:
    from qg.config_models.loader import QGConfiguration
    from qg.params_models import PositionedQueueInput, QueueInput


def synthesize_local_orders(full_samples_df: pl.DataFrame) -> list[tuple[int, None]]:
    """Derive the ``selected_orders`` contract from an uploaded sample table.

    Replaces the B-Fabric order browser in the local app: sorts the unique
    ``container_id`` values from the uploaded DataFrame into ascending order and
    returns them as ``(container_id, None)`` tuples — the same shape
    ``selected_orders`` takes in the portal (where the second element is the
    B-Fabric tech-area object).

    Args:
        full_samples_df: Normalized sample DataFrame with a ``container_id`` column.
            An empty frame (no upload yet) produces an empty list.

    Returns:
        Sorted list of ``(container_id, None)`` tuples, one per distinct container.
    """
    if full_samples_df.is_empty():
        return []
    return [(int(c), None) for c in full_samples_df["container_id"].unique().sort().to_list()]


def resolve_output_format(filtered_table: pl.DataFrame, sampler_selected: bool) -> str:
    """Output format implied by the instrument+sampler row (fallback ``xcalibur``)."""
    if sampler_selected and not filtered_table.is_empty():
        formats = filtered_table["output_format"].unique().to_list()
        return formats[0] if formats else "xcalibur"
    return "xcalibur"


# =============================================================================
# Config-derived transforms — pure functions that turn the current selections
# into the dataframes/values the notebook cells expose. No marimo widgets here;
# each notebook cell passes in the relevant ``.value`` and binds the result.
# =============================================================================


def filter_by_column(table: pl.DataFrame, column: str, value: object) -> pl.DataFrame:
    """Filter ``table`` to rows where ``column == value``; pass through when ``value`` is falsy.

    The app narrows the overview table one selection at a time (tech area, instrument,
    sampler, queue type, plate layout, QC layout, pattern) — each step is this filter.
    """
    return table.filter(pl.col(column) == value) if value else table


def resolve_level_concentrations(concentration_inputs: dict | None) -> dict[int, str]:
    """Map each level to ``"<value><unit>"`` from the per-level concentration widgets."""
    if concentration_inputs is None:
        return {}
    return {
        level: f"{widgets['value'].value}{widgets['unit'].value}"
        for level, widgets in concentration_inputs.items()
        if widgets["value"].value is not None
    }


def resolve_default_qc_frequency(config: QGConfiguration, tech_area: str | None, pattern: str | None) -> int:
    """Selected pattern's ``run_QC_after_n_samples`` (fallback 16 when unresolved)."""
    if tech_area and pattern:
        queue_pattern = config.queue_patterns.get_pattern(tech_area, pattern)
        return queue_pattern.run_QC_after_n_samples if queue_pattern else 16
    return 16


def load_methods_table(config: QGConfiguration, tech_area: str | None, instrument: str | None) -> pl.DataFrame:
    """Methods table for the tech-area/instrument (empty frame when unresolved)."""
    if tech_area and instrument:
        methods = config.methods.get_methods(tech_area, instrument)
        return methods.to_table() if methods else pl.DataFrame()
    return pl.DataFrame()


def available_method_names(
    config: QGConfiguration,
    methods_df: pl.DataFrame,
    tech_area: str | None,
    instrument: str | None,
    pattern: str | None,
) -> tuple[list[str], list[str]]:
    """Method names available for ``(pos, neg)``, intersected across every sample type.

    A queue mixes user samples (the ``default`` sample type) with the QC samples the
    pattern injects, so an offered method must exist for all of them. Sample types with
    no rows in the methods file are unconstrained and skipped from the intersection.
    """

    def intersection(polarity: str) -> list[str]:
        if methods_df.is_empty() or not tech_area or not instrument:
            return []
        methods_for_instr = config.methods.get_methods(tech_area, instrument)
        sample_ids: set[str] = {SamplesConfig.DEFAULT_SAMPLE_ID}
        if pattern:
            queue_pattern = config.queue_patterns.get_pattern(tech_area, pattern)
            sample_ids |= queue_pattern.get_all_sample_ids()
        sets = [methods_for_instr.get_method_names(sid, polarity) for sid in sample_ids]
        non_empty = [s for s in sets if s]
        if not non_empty:
            return []
        return sorted(non_empty[0].intersection(*non_empty[1:]), reverse=True)

    has_polarity = not methods_df.is_empty() and "polarity" in methods_df.columns
    return intersection("pos" if has_polarity else ""), intersection("neg" if has_polarity else "")


def validate_selection(
    config: QGConfiguration,
    *,
    selected_orders: list,
    tech_area: str | None,
    instrument: str | None,
    sampler: str | None,
    queue_type: str | None,
    plate_layout: str | None,
    qc_layout: str | None,
    pattern: str | None,
    filtered_table: pl.DataFrame,
    no_source_message: str,
) -> tuple[bool, list[str]]:
    """Validate every selection is set and a QC layout exists for the combination.

    ``no_source_message`` is the single hint shown before any sample source is chosen
    (portal: ``"Please select an order"``; local: ``"Upload a sample table"``).

    Returns ``(is_valid, errors)``.
    """
    errors: list[str] = []
    if not selected_orders:
        # Until a source is picked nothing downstream can resolve; show one actionable
        # hint instead of a cascade of "X not selected" entries.
        errors.append(no_source_message)
    else:
        if not tech_area:
            errors.append("Tech area not selected")
        if not instrument:
            errors.append("Instrument not selected")
        if not sampler:
            errors.append("Sampler not selected")
        if not queue_type:
            errors.append("Queue type not selected")
        if not plate_layout:
            errors.append("Plate layout not selected")
        if not qc_layout:
            errors.append("QC layout not selected")
        if not pattern:
            errors.append("Pattern not selected")

    if not errors and filtered_table.is_empty():
        errors.append("No valid combination found")

    if not errors and plate_layout and qc_layout:
        queue_pattern = config.queue_patterns.get_pattern(tech_area, pattern)
        if queue_pattern and queue_pattern.get_all_sample_ids():
            qc_samples = config.get_qc_samples(tech_area, qc_layout, plate_layout, config.samplers.get_sampler(sampler))
            if not qc_samples:
                errors.append(f"No QC samples for {tech_area}/{qc_layout}/{plate_layout}")

    return len(errors) == 0, errors


def resolve_qc_layout_preview(
    config: QGConfiguration,
    *,
    tech_area: str | None,
    sampler: str | None,
    plate_layout: str | None,
    qc_layout: str | None,
    pattern: str | None,
    raw_queue_df: pl.DataFrame | None,
) -> pl.DataFrame | None:
    """Effective QC layout: the QC layout intersected with the sample_ids the pattern injects.

    Returns None for patterns that inject no QC (the intersection is empty). Once a queue
    has been generated, well-plate samplers gain a ``visits`` column counting touches per
    well; tip samplers expose ranges (per-range visit counts are not computed yet).
    """
    if not (tech_area and sampler and plate_layout and qc_layout and pattern):
        return None
    sampler_obj = config.samplers.get_sampler(sampler)
    samples = config.get_qc_samples(tech_area, qc_layout, plate_layout, sampler_obj)
    # Keep only the layout rows the selected pattern actually injects. For the
    # `no_layout` option the pattern is empty, so this yields nothing (no preview).
    queue_pattern = config.queue_patterns.get_pattern(tech_area, pattern)
    used_sample_ids = queue_pattern.get_all_sample_ids()
    samples = [s for s in samples if s.sample_id in used_sample_ids]
    if not samples:
        return None
    if sampler_obj.is_tip:
        # TODO: per-range visit counts for tip samplers (Evosep) — needs interval intersection.
        rows = [
            {"sample_id": s.sample_id, "tray": s.tray, "range": f"{s.position_start}-{s.position_end}"} for s in samples
        ]
        return pl.DataFrame(rows)
    rows = [{"sample_id": s.sample_id, "tray": s.tray, "pos": f"{s.row}{s.col}"} for s in samples]
    preview = pl.DataFrame(rows)
    if raw_queue_df is not None and not raw_queue_df.is_empty():
        well_counts = (
            raw_queue_df.group_by(["tray", "row", "col"])
            .agg(pl.len().alias("visits"))
            .with_columns(pl.concat_str(["row", pl.col("col").cast(pl.Utf8)]).alias("pos"))
            .select(["tray", "pos", "visits"])
        )
        # Align dtypes — preview's tray is Utf8 from the dict; queue's tray may be int.
        well_counts = well_counts.with_columns(pl.col("tray").cast(preview["tray"].dtype))
        preview = preview.join(well_counts, on=["tray", "pos"], how="left").with_columns(pl.col("visits").fill_null(0))
    return preview


def build_queue_parameters(
    *,
    tech_area: str | None,
    instrument: str | None,
    sampler: str | None,
    output_format: str,
    queue_pattern: str | None,
    queue_type: str | None,
    plate_layout: str | None,
    qc_layout_name: str | None,
    polarity_flags: dict[str, bool],
    date: datetime.date,
    user: str,
    method_pos: str | None,
    method_neg: str | None,
    randomization: str,
    inj_vol_text: str,
    qc_frequency_text: str,
    start_position: str | None,
    start_tray: str | None,
    level_concentrations: dict,
) -> tuple[QueueParameters | None, pydantic.ValidationError | None]:
    """Assemble ``QueueParameters`` from the sidebar control values.

    Takes the raw control values (widget ``.value``) and applies the light
    conversions the form needs: parse the inj-vol / QC-frequency text, collapse
    the polarity checkboxes and the per-polarity method selections, format the
    date, and default the optional start position/tray. Returns ``(params,
    error)`` — the ``pydantic.ValidationError`` is returned, not raised, so the
    caller can surface it in the UI. Non-numeric inj-vol / QC-frequency text
    still raises ``ValueError`` (unchanged from the original inline cell).
    """
    try:
        inj_vol = float(inj_vol_text) if inj_vol_text.strip() else None
        qc_freq = int(qc_frequency_text) if qc_frequency_text.strip() else None
        polarity = [p for p in ("pos", "neg") if polarity_flags.get(p)]
        method = {pol: val for pol, val in (("pos", method_pos), ("neg", method_neg)) if val}
        params = QueueParameters.model_validate(
            {
                "tech_area": tech_area,
                "instrument": instrument,
                "sampler": sampler,
                "output_format": output_format,
                "queue_pattern": queue_pattern,
                "queue_type": queue_type,
                "plate_layout": plate_layout,
                "qc_layout_name": qc_layout_name,
                "polarity": polarity,
                "date": date.strftime("%Y%m%d"),
                "user": user.strip(),
                "method": method,
                "randomization": randomization,
                "inj_vol_override": inj_vol,
                "qc_frequency_override": qc_freq,
                "start_position": start_position if start_position is not None else "A1",
                "start_tray": start_tray if start_tray is not None else "",
                "level_concentrations": level_concentrations,
            }
        )
        return params, None
    except pydantic.ValidationError as exc:
        return None, exc


def build_queue_input(
    config: QGConfiguration,
    queue_parameters: QueueParameters | None,
    sample_df: pl.DataFrame | None,
    *,
    has_samples_source: bool,
    provenance_instance: str | None = None,
) -> tuple[QueueInput | None, str | None]:
    """Build a ``QueueInput`` once, shared by the Parameters tab and generation.

    Resolves the randomization seed up front for randomized modes so the Parameters
    tab, the params-JSON download, and (portal) the workunit all record the same
    reproducible seed. ``provenance_instance`` is stamped onto
    ``QueueParameters.bfabric_instance`` when given (portal mode); local mode leaves
    it ``None``.

    Args:
        has_samples_source: whether a sample source is present at all (portal:
            ``selected_orders`` truthy; local: a file was uploaded). Guards building
            before the user has provided any input.

    Returns:
        ``(queue_input, error_message)`` — exactly one is non-None on a real attempt.
    """
    if not (queue_parameters and has_samples_source and sample_df is not None):
        return None, None
    try:
        builder = QueueBuilder(config).with_parameters(queue_parameters)
        if provenance_instance is not None:
            builder = builder.with_bfabric_instance(provenance_instance)
        if not sample_df.is_empty():
            builder = builder.add_samples_from_dataframe(sample_df)
        return builder.build(), None
    except ValueError as exc:
        return None, str(exc)


@dataclass
class GenerationResult:
    """Outcome of a single ``build_rows()`` pass (called exactly once)."""

    generated_df: pl.DataFrame | None = None
    raw_df: pl.DataFrame | None = None
    output_str: str | None = None
    positioned_input: PositionedQueueInput | None = None
    file_extension: str = ".csv"
    error: str | None = None


def generate_queue(
    queue_input: QueueInput | None,
    queue_parameters: QueueParameters | None,
) -> GenerationResult:
    """Run generation once so preview and downloaded file are identical.

    Randomization is non-deterministic per build, so ``build_rows()`` must be
    called a single time and its result reused for both the preview table and the
    downloaded content.
    """
    result = GenerationResult()
    if queue_input is None:
        return result
    try:
        result.positioned_input = queue_input.position_queue()
        generator = QueueGenerator(result.positioned_input)
        queue_rows = generator.build_rows()
        result.raw_df = queue_rows.to_table()
        result.generated_df = format_table(
            queue_rows, generator.output_format, generator.plate_layout, generator.tech_area
        )
        result.output_str = write_queue(result.generated_df, generator.output_format)
        result.file_extension = generator.file_extension
    except ValueError as exc:
        logger.exception("Queue generation failed")
        err = str(exc)
        if "Not enough trays" in err:
            sampler = queue_parameters.sampler if queue_parameters else "sampler"
            result.error = (
                f"{err}\n\n"
                f"The {sampler} sampler cannot hold this many plates simultaneously. "
                f"Either select fewer plates, or use a sampler with more tray positions "
                f"(e.g., Vanquish has 4 trays)."
            )
        else:
            result.error = err
    return result


def queue_output_filename(
    queue_parameters: QueueParameters | None, container_ids: list[int], file_extension: str
) -> str | None:
    """`<date>_<instrument>_<container ids>.<ext>` — the downloaded queue file name."""
    if queue_parameters is None:
        return None
    ids_str = "_".join(str(c) for c in container_ids) if container_ids else "queue"
    return f"{queue_parameters.date}_{queue_parameters.instrument}_{ids_str}{file_extension}"


def params_json_filename(queue_parameters: QueueParameters, sample_df: pl.DataFrame) -> str:
    """`<tech>_<sampler>_c<container ids>_n<count>.json` — the params download name."""
    ids_str = "_".join(str(c) for c in sample_df["container_id"].unique().sort().to_list())
    sampler = queue_parameters.sampler.replace(".", "_")
    return f"{queue_parameters.tech_area}_{sampler}_c{ids_str}_n{len(sample_df)}.json"


def params_download_button(queue_input: QueueInput, filename: str) -> mo.Html:
    """A `Download Params JSON` button for the resolved ``queue_input``."""
    data = json.dumps(queue_input.model_dump(mode="json"), indent=2)
    return mo.download(data=data.encode("utf-8"), filename=filename, label="Download Params JSON")


def commit_run_artifacts(
    source_input: QueueInput,
    positioned_input: PositionedQueueInput,
    raw_queue: pl.DataFrame,
) -> None:
    """Persist audit artifacts at an explicit download or upload commit point."""
    stem = save_positioning_artifacts(source_input, positioned_input)
    save_generation_artifact(positioned_input, raw_queue, stem=stem)


def queue_download_button(
    queue_output_str: str,
    filename: str,
    *,
    source_input: QueueInput,
    positioned_input: PositionedQueueInput,
    raw_queue: pl.DataFrame,
    disabled: bool = False,
) -> mo.Html:
    """Build a queue download that persists audit artifacts when clicked."""

    def committed_content() -> bytes:
        commit_run_artifacts(source_input, positioned_input, raw_queue)
        return queue_output_str.encode("utf-8")

    return mo.download(
        data=committed_content,
        filename=filename,
        label="Download Queue File",
        disabled=disabled,
    )


# =============================================================================
# View builders — marimo UI assembled here so both notebooks render identically.
# Each notebook keeps only a thin binding cell that calls one of these; the
# marimo-free plotting/data helpers live in ``qg.viz`` (see imports above).
# =============================================================================


def render_concentration_block(concentration_inputs: dict | None) -> mo.Html:
    """Compact three-column grid: Level | Value | Unit. Empty when nothing to show."""
    if concentration_inputs is None:
        return mo.md("")
    widths = [1, 2, 2]
    header = mo.hstack(
        [mo.md("**Level**"), mo.md("**Value**"), mo.md("**Unit**")],
        widths=widths,
        justify="start",
    )
    rows = [
        mo.hstack(
            [mo.md(str(level)), widgets["value"], widgets["unit"]],
            widths=widths,
            justify="start",
            align="center",
        )
        for level, widgets in concentration_inputs.items()
    ]
    return mo.vstack([mo.md("**Concentration per level**"), header, *rows], gap=0.25)


def render_validation_status(
    config: QGConfiguration,
    *,
    config_valid: bool,
    validation_errors: list[str],
    queue_type: str | None,
    sampler: str | None,
    sample_df: pl.DataFrame | None,
) -> mo.Html | None:
    """Config validation plus a dynamic plate-capacity check, as a warn callout (None if clean)."""
    all_errors = list(validation_errors)
    if (
        config_valid
        and queue_type == "Plate"
        and sampler
        and sample_df is not None
        and not sample_df.is_empty()
        and "plate_id" in sample_df.columns
    ):
        sampler_obj = config.samplers.get_sampler(sampler)
        if sampler_obj:
            num_trays = len(sampler_obj.trays)
            num_plates = sample_df["plate_id"].n_unique()
            if num_plates > num_trays:
                all_errors.append(f"{sampler} has {num_trays} trays but {num_plates} plates selected")
    if not all_errors:
        return None
    errors_md = "\n".join(f"• {e}" for e in all_errors)
    return mo.callout(mo.md(f"**Issues:**\n{errors_md}"), kind="warn")


def render_sample_selection_content(
    *,
    sample_df: pl.DataFrame | None,
    selected_orders: list,
    name_suffix: mo.ui.dropdown,
    sample_mode_selector: mo.ui.radio,
    samples_table: mo.ui.table | None,
    samples_editor: mo.ui.data_editor | None,
    subject_label: str,
) -> mo.Html:
    """Edit-Samples tab: sample summary + Selection/Editor sub-panels (CSS-toggled).

    ``subject_label`` names the sample source in the summary line — ``"order(s)"``
    in the portal, ``"group(s)"`` in the local app.
    """
    order_count = len(selected_orders) if selected_orders else 0
    if sample_df is not None and not sample_df.is_empty():
        summary = mo.md(f"**{len(sample_df)} samples from {order_count} {subject_label}**")
    else:
        summary = mo.md("**No samples loaded**")
    if samples_editor is not None:
        editor_panel = mo.vstack([name_suffix, samples_editor])
    else:
        editor_panel = mo.md("_No samples loaded_")
    panels = {
        "Sample Selection": samples_table if samples_table is not None else mo.md("_No samples loaded_"),
        "Sample Editor": editor_panel,
    }
    panel_stack = [
        mo.md(f'<div style="display: {"block" if name == sample_mode_selector.value else "none"}">{widget}</div>')
        for name, widget in panels.items()
    ]
    return mo.vstack([summary, sample_mode_selector, *panel_stack])


def render_plate_layout_view(
    config: QGConfiguration,
    raw_queue_df: pl.DataFrame | None,
    queue_parameters: QueueParameters | None,
    plate_color_by: mo.ui.dropdown,
    plate_well_size: mo.ui.slider,
) -> mo.Html:
    """Plate map colored by sample type or covariate, with a group<->plate-position balance score."""
    if raw_queue_df is None or raw_queue_df.is_empty():
        return mo.md("_Generate a queue to see the plate layout._")
    geom = raw_queue_df.filter((pl.col("row") != "") & (pl.col("col") != 0))
    if geom.is_empty():
        return mo.callout(
            mo.md("**Plate view not available** for this layout — positions have no row/column geometry."),
            kind="info",
        )
    has_group = geom["grouping_var"].drop_nulls().len() > 0
    want_group = plate_color_by.value.startswith("Group") and has_group
    color_by = "grouping_var" if want_group else "category"
    legend = "grouping_var" if want_group else "sample type"
    score = plate_balance(raw_queue_df)
    score_md = (
        f"Group ↔ plate position (η²): **{score:.2f}** — 0 = balanced, 1 = separated"
        if score is not None
        else "Group ↔ plate position (η²): **N/A** (no grouping variable)"
    )
    wells = build_plate_wells(geom)
    orders = sorted(geom["container_id"].unique().to_list())
    layout = config.plate_layouts.get_layout(queue_parameters.plate_layout)
    return mo.vstack(
        [
            mo.md(f"**Plate layout** — color = {legend}, shape = order. Hover a well for details."),
            mo.md(score_md),
            mo.hstack([plate_color_by, plate_well_size], justify="start", gap=2),
            mo.ui.plotly(build_plate_figure(wells, layout, orders, cell=plate_well_size.value, color_by=color_by)),
        ]
    )


def render_timeline_view(raw_queue_df: pl.DataFrame | None, timeline_color_by: mo.ui.dropdown) -> mo.Html:
    """Per-injection timeline recolored by group or QC cadence, with a queue-position balance score."""
    if raw_queue_df is None or raw_queue_df.is_empty():
        return mo.md("_Generate a queue to see the acquisition timeline._")
    color_by = "qc_cadence" if timeline_color_by.value == "QC cadence" else "grouping_var"
    score = queue_balance(raw_queue_df)
    score_md = (
        f"Group ↔ queue position (η²): **{score:.2f}** — 0 = balanced, 1 = separated"
        if score is not None
        else "Group ↔ queue position (η²): **N/A** (no grouping variable)"
    )
    return mo.vstack(
        [
            mo.md("**Acquisition timeline** — one tile per injection along run order. Hover for details."),
            mo.md(score_md),
            timeline_color_by,
            mo.ui.plotly(build_timeline_figure(raw_queue_df, color_by=color_by)),
        ]
    )


def render_visualizations_content(
    viz_subtab: mo.ui.radio, plate_layout_view: mo.Html, timeline_view: mo.Html
) -> mo.Html:
    """Visualizations tab: a sub-tab selector over the plate and timeline views."""
    views = {"Plate Layout": plate_layout_view, "Acquisition Timeline": timeline_view}
    return mo.vstack([viz_subtab, views[viz_subtab.value]])


def render_valid_combinations_content(
    master_table: pl.DataFrame,
    *,
    tech_area: str | None,
    instrument: str | None,
    sampler: str | None,
    qc_layout: str | None,
    pattern: str | None,
    queue_type: str | None,
    plate_layout: str | None,
) -> mo.Html:
    """Valid-Combinations tab: the full master table with matching rows flagged and sorted to top."""
    # (column, human label, selected value), in display order.
    filters = [
        ("tech_area", "tech_area", tech_area),
        ("instrument", "instrument", instrument),
        ("sampler", "sampler", sampler),
        ("qc_layout_name", "qc_layout", qc_layout),
        ("pattern_name", "pattern_name", pattern),
        ("queue_type", "queue_type", queue_type),
        ("plate_layout", "plate_layout", plate_layout),
    ]
    match_conditions = [pl.col(column) == value for column, _label, value in filters if value]
    active_filters = [f"**{label}** = {value}" for _column, label, value in filters if value]

    display_cols = [c for c in master_table.columns if c != "default_pattern"]
    if match_conditions:
        combined_match = pl.all_horizontal(*match_conditions)
        display_table = (
            master_table.with_columns(pl.when(combined_match).then(pl.lit("✓")).otherwise(pl.lit("")).alias("✓"))
            .select(["✓"] + display_cols)
            .sort("✓", descending=True)
        )
        match_count = master_table.filter(combined_match).height
    else:
        display_table = master_table.with_columns(pl.lit("").alias("✓")).select(["✓"] + display_cols)
        match_count = master_table.height

    filters_md = " | ".join(active_filters) if active_filters else "None"
    return mo.vstack(
        [
            mo.md(f"**{match_count}/{len(master_table)} combinations** match | Filters: {filters_md}"),
            display_table,
        ]
    )


def render_sidebar_body(
    *,
    tech_area_field: mo.ui.dropdown,
    instrument_field: mo.ui.dropdown,
    sampler_field: mo.ui.dropdown,
    queue_type_field: mo.ui.dropdown,
    queue_type_warning: mo.Html | None,
    plate_layout_field: mo.ui.dropdown,
    start_tray_field: mo.ui.dropdown | None,
    start_position_field: mo.ui.dropdown | None,
    output_format_value: str,
    qc_layout_field: mo.ui.dropdown,
    pattern_field: mo.ui.dropdown,
    concentration_inputs: dict | None,
    concentration_block: mo.Html,
    polarity_group: mo.ui.batch,
    method_field_pos: mo.ui.dropdown | None,
    method_field_neg: mo.ui.dropdown | None,
    randomization_field: mo.ui.dropdown,
    date_field: mo.ui.date,
    user_field: mo.ui.text,
    inj_vol_field: mo.ui.text,
    qc_frequency_field: mo.ui.text,
    validation_status: mo.Html | None,
    qc_layout_preview: pl.DataFrame | None,
) -> mo.Html:
    """Assemble the configuration sidebar body (both apps' normal, non-reproduce mode).

    Groups the Queue controls (hiding Pattern under the ``no_layout`` as-is
    option, showing the concentration grid and method dropdowns only when present),
    then the Options, then appends validation issues and the QC-position preview.
    """
    queue_items = [qc_layout_field]
    if qc_layout_field.value != NO_LAYOUT:
        queue_items.append(pattern_field)
    if concentration_inputs is not None:
        queue_items.append(concentration_block)
    queue_items.append(polarity_group)
    if method_field_pos is not None:
        queue_items.append(method_field_pos)
    if method_field_neg is not None:
        queue_items.append(method_field_neg)

    items = [
        mo.md("## Queue Generator"),
        tech_area_field,
        instrument_field,
        sampler_field,
        queue_type_field,
        *([] if queue_type_warning is None else [queue_type_warning]),
        plate_layout_field,
        *(
            []
            if start_tray_field is None
            else (
                [mo.hstack([start_tray_field, start_position_field], justify="start")]
                if start_position_field is not None
                else [start_tray_field]
            )
        ),
        mo.md(f"**Output:** {output_format_value}"),
        mo.md("### Queue"),
        *queue_items,
        mo.md("### Options"),
        randomization_field,
        date_field,
        user_field,
        inj_vol_field,
        qc_frequency_field,
    ]
    if validation_status is not None:
        items.append(validation_status)
    if qc_layout_preview is not None:
        items.append(mo.md(f"**QC Positions** _{qc_layout_field.value}_"))
        items.append(qc_layout_preview)
    return mo.vstack(items)


def render_tabbed_layout(
    tab_selector: mo.ui.radio,
    *,
    edit_samples_content: mo.Html,
    queue_preview_content: mo.Html,
    visualizations_content: mo.Html,
    parameters_content: mo.Html,
    valid_combinations_content: mo.Html,
) -> mo.Html:
    """Top-level tabbed layout. All panels stay in the DOM via CSS ``display`` toggling."""
    sections = {
        "Queue Preview": queue_preview_content,
        "Visualizations": visualizations_content,
        "✎ Edit Samples": edit_samples_content,
        "Parameters": parameters_content,
        "Valid Combinations": valid_combinations_content,
    }
    # display:none keeps every widget in the DOM (data_editor preserves visual state).
    panels = [
        mo.md(f'<div style="display: {"block" if name == tab_selector.value else "none"}">{content}</div>')
        for name, content in sections.items()
    ]
    return mo.vstack([tab_selector, *panels])


# =============================================================================
# Widget factories — construct the ``mo.ui.*`` controls both notebooks share.
# Each notebook keeps a one-line binding cell (``x = shared.make_x(...); return
# (x,)``) so marimo tracks ``x`` and its ``.value`` reads; the construction (and
# any option/label logic) lives here once. Trivial single-line static widgets
# with no domain literals (inj vol, date, formatted-preview toggle) stay inline.
# =============================================================================


def make_column_dropdown(table: pl.DataFrame, column: str, *, enabled: bool, label: str) -> mo.ui.dropdown:
    """Dropdown over the sorted unique ``column`` values (empty until ``enabled``); defaults to first."""
    options = sorted(table[column].unique().to_list()) if enabled else []
    return mo.ui.dropdown(options=options, value=options[0] if options else None, label=label)


def make_pattern_field(table_by_qc_layout: pl.DataFrame, *, enabled: bool) -> mo.ui.dropdown:
    """Pattern dropdown, with each combination's default pattern(s) listed first."""
    if enabled:
        df = table_by_qc_layout.select(["pattern_name", "default_pattern"]).unique()
        default_patterns = df.filter(pl.col("pattern_name") == pl.col("default_pattern"))["pattern_name"].to_list()
        others = (
            df.filter(pl.col("pattern_name") != pl.col("default_pattern"))["pattern_name"].unique().sort().to_list()
        )
        options = default_patterns + others
    else:
        options = []
    return mo.ui.dropdown(options=options, value=options[0] if options else None, label="Pattern")


def make_qc_layout_field(table_by_plate_layout: pl.DataFrame, *, enabled: bool) -> mo.ui.dropdown:
    """QC Layout dropdown; auto-selects the default pattern's QC layout when present."""
    if enabled and "qc_layout_name" in table_by_plate_layout.columns:
        all_layouts = sorted(table_by_plate_layout["qc_layout_name"].unique().to_list())
        default_rows = table_by_plate_layout.filter(pl.col("pattern_name") == pl.col("default_pattern"))
        if not default_rows.is_empty():
            default_qc = default_rows["qc_layout_name"].to_list()[0]
        else:
            default_qc = all_layouts[0] if all_layouts else None
        options = all_layouts
        default = default_qc if default_qc in all_layouts else (all_layouts[0] if all_layouts else None)
    else:
        options = []
        default = None
    return mo.ui.dropdown(options=options, value=default, label="QC Layout")


def make_queue_type_field(
    table_by_sampler: pl.DataFrame,
    *,
    sampler: str | None,
    has_plates: bool,
    has_vials: bool,
    incompatible_subject: str,
) -> tuple[mo.ui.dropdown, mo.Html | None]:
    """Queue Type dropdown (Vial/Plate) plus an incompatibility warning callout (or None).

    Offers the intersection of what the order contains and what the sampler supports,
    Vial first. ``incompatible_subject`` names the samples in the warning (portal:
    ``"this order's samples"``; local: ``"the uploaded samples"``).
    """
    warning = None
    if sampler:
        supports = set(table_by_sampler["queue_type"].unique().to_list())
        order_has: set[str] = set()
        if has_plates:
            order_has.add("Plate")
        if has_vials:
            order_has.add("Vial")
        usable = supports & order_has
        options = [t for t in ("Vial", "Plate") if t in usable]
        default = options[0] if options else None
        if order_has and not usable:
            warning = mo.callout(
                mo.md(f"Sampler **{sampler}** is incompatible with {incompatible_subject}."),
                kind="warn",
            )
    else:
        options = []
        default = None
    return mo.ui.dropdown(options=options, value=default, label="Queue Type"), warning


def make_mixed_order_note(*, has_plates: bool, has_vials: bool) -> mo.Html | None:
    """Neutral note when an order holds both plate-resident and standalone (vial) samples.

    Purely informational — it states the composition, not its consequences (queue-type
    guidance is intentionally left to a later change). Returns ``None`` unless both are present.
    """
    if has_plates and has_vials:
        return mo.callout(
            mo.md("This order contains both plate-resident and standalone (vial) samples."),
            kind="info",
        )
    return None


def make_start_position_field(
    config: QGConfiguration, *, queue_type: str | None, plate_layout: str | None
) -> mo.ui.dropdown | None:
    """Vial-mode start-position dropdown over the layout's wells (None outside Vial mode)."""
    if queue_type == "Vial" and plate_layout:
        layout = config.plate_layouts.get_layout(plate_layout)
        positions = [f"{row}{col}" for row in layout.rows for col in layout.cols]
        return mo.ui.dropdown(options=positions, value="A1", label="& position")
    return None


def make_start_tray_field(config: QGConfiguration, *, sampler: str | None) -> mo.ui.dropdown | None:
    """Start-tray dropdown over the sampler's trays (None until a sampler is chosen)."""
    if sampler:
        trays = [str(t) for t in config.samplers.get_sampler(sampler).trays]
        return mo.ui.dropdown(options=trays, value=trays[0], label="Start: tray")
    return None


def make_concentration_inputs(
    config: QGConfiguration, *, tech_area: str | None, qc_layout: str | None, plate_layout: str | None
) -> dict | None:
    """Per-level number+unit widgets for a Std-Bracket QC layout (None when none apply).

    Presets a halving dilution (100, 50, ... 1 umol) across up to seven levels; extra
    levels fall back to the lowest value.
    """
    units = ["pmol", "nmol", "umol", "pgml", "ngml", "ugml"]
    preset_values = [100, 50, 25, 12, 6, 3, 1]
    preset_unit = "umol"
    if not (qc_layout and plate_layout and tech_area):
        return None
    qc_sample_ids = config.qc_layouts_well.get_sample_ids(tech_area, qc_layout, plate_layout)
    level_map: dict[int, str] = {}
    for sid in qc_sample_ids:
        s = config.samples.get_sample(tech_area, sid)
        if s.sample_type == "Std Bracket" and s.level is not None:
            level_map[s.level] = sid
    if not level_map:
        return None
    return {
        level: {
            "value": mo.ui.number(
                start=1,
                stop=999,
                step=1,
                value=preset_values[idx] if idx < len(preset_values) else preset_values[-1],
            ),
            "unit": mo.ui.dropdown(options=units, value=preset_unit),
        }
        for idx, level in enumerate(sorted(level_map))
    }


def make_qc_frequency_field(default_qc_frequency: int) -> mo.ui.text:
    """QC-frequency text input, showing the pattern default as placeholder."""
    return mo.ui.text(value="", label="QC frequency", placeholder=str(default_qc_frequency))


def make_method_field(available_methods: list[str], *, show: bool, label: str) -> mo.ui.dropdown | None:
    """Method-name dropdown (blank = no method) for one polarity, or None when hidden."""
    if not show:
        return None
    return mo.ui.dropdown(
        options=[""] + available_methods,
        value=available_methods[0] if available_methods else "",
        label=label,
    )


def make_polarity_group(default_polarities: list[str]) -> mo.ui.batch:
    """Pos/neg polarity checkboxes, pre-checked from the tech-area defaults."""
    return mo.ui.batch(
        mo.md("**Polarity:** {pos} pos {neg} neg"),
        {
            "pos": mo.ui.checkbox(value="pos" in default_polarities),
            "neg": mo.ui.checkbox(value="neg" in default_polarities),
        },
    )


def make_randomization_field() -> mo.ui.dropdown:
    """Randomization-mode dropdown."""
    return mo.ui.dropdown(options=["no", "random", "blocked", "blocked_uniform"], value="no", label="Randomization")


def suffix_options_for_tech(config: QGConfiguration, tech_area: str) -> list[str]:
    """Sample-name suffix options for a tech area.

    The prep-type vocabulary is per-tech and lives in ``ui/tech_area_defaults.toml``
    (``sample_name_suffixes``); e.g. Proteomics offers ``enriched`` / ``total`` /
    ``lip``. The ``none`` no-op is prepended here as a UI concern, so every tech
    area — including those with no vocabulary yet — offers at least ``none``.
    """
    return ["none", *config.tech_area_defaults.get_sample_name_suffixes(tech_area)]


def make_name_suffix(config: QGConfiguration, tech_area: str) -> mo.ui.dropdown:
    """Dropdown appending a controlled prep-type suffix to every sample name.

    The option list is tech-area specific (see :func:`suffix_options_for_tech`);
    tech areas with no configured vocabulary show only ``none``. The dropdown stays
    visible either way.
    """
    return mo.ui.dropdown(
        options=suffix_options_for_tech(config, tech_area),
        value="none",
        label="Append suffix to every sample name",
    )


def make_sample_mode_selector() -> mo.ui.radio:
    """Selection/Editor sub-tab radio for the Edit-Samples tab."""
    return mo.ui.radio(options=["Sample Selection", "Sample Editor"], value="Sample Selection", inline=True)


def make_samples_table(full_samples_df: pl.DataFrame, name_suffix: str) -> mo.ui.table | None:
    """Multi-select samples table with every row pre-selected (None when no samples).

    ``name_suffix`` (other than ``"none"``) is appended to ``sample_name`` at the source,
    so it shows in the table, the editor, the preview, and the output. Rebuilt from the
    pristine frame each run, so switching suffixes never accumulates.
    """
    if full_samples_df.is_empty():
        return None
    display_df = full_samples_df
    if name_suffix != "none":
        display_df = full_samples_df.with_columns((pl.col("sample_name") + "_" + name_suffix).alias("sample_name"))
    n = len(display_df)
    return mo.ui.table(
        data=display_df,
        selection="multi",
        initial_selection=list(range(n)),
        label="Uncheck to exclude samples",
        show_download=False,
        pagination=False,
    )


def make_samples_editor(samples_table: mo.ui.table | None) -> mo.ui.data_editor | None:
    """Data editor (reorder via the ``order`` column) over the kept samples (None when none)."""
    if samples_table is None:
        return None
    with_order = samples_table.value.with_row_index("order", offset=1).cast({"order": pl.Float64})
    return mo.ui.data_editor(with_order, label="Samples (edit order to reorder)", pagination=False)


def make_plate_well_size() -> mo.ui.slider:
    """Well-size slider for the Plate Layout view."""
    return mo.ui.slider(start=14, stop=52, step=2, value=30, label="Well size", show_value=True)


def make_viz_subtab() -> mo.ui.radio:
    """Sub-view selector for the Visualizations tab."""
    return mo.ui.radio(options=["Plate Layout", "Acquisition Timeline"], value="Plate Layout", inline=True)


def make_viz_color_selectors() -> tuple[mo.ui.dropdown, mo.ui.dropdown]:
    """The ``(plate_color_by, timeline_color_by)`` 'Color by' dropdowns for the Visualizations tab."""
    plate_color_by = mo.ui.dropdown(
        options=["Sample type", "Group (grouping_var)"], value="Sample type", label="Color by"
    )
    timeline_color_by = mo.ui.dropdown(
        options=["Injection class", "QC cadence"], value="Injection class", label="Color by"
    )
    return plate_color_by, timeline_color_by


def make_tab_selector() -> mo.ui.radio:
    """Top-level tab selector. Opens on Queue Preview; Edit Samples is flagged with a pencil."""
    return mo.ui.radio(
        options=["✎ Edit Samples", "Queue Preview", "Visualizations", "Parameters", "Valid Combinations"],
        value="Queue Preview",
        inline=True,
    )
