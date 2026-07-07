import marimo

__generated_with = "0.19.4"
app = marimo.App(width="full", sql_output="polars")

with app.setup:
    import importlib.metadata
    import os
    from datetime import date
    from pathlib import Path

    import marimo as mo
    import polars as pl
    import pydantic
    from loguru import logger

    from qg.logging_setup import configure_logging

    configure_logging()

    from qg.apps import queue_app_shared as shared
    from qg.apps.integrations.example_params import (
        list_example_params,
        read_example_params,
    )
    from qg.apps.integrations.example_samples import (
        list_example_sample_tables,
        read_example_sample_table,
    )
    from qg.apps.integrations.local_samples import parse_sample_table
    from qg.config_models.loader import qg_configuration
    from qg.config_models.structure import SamplesConfig
    from qg.params_models import QueueParameters, parse_queue_input
    from qg.viz.balance import plate_balance, queue_balance
    from qg.viz.plate import build_plate_figure, build_plate_wells
    from qg.viz.timeline import build_timeline_figure


@app.cell
def _():
    app_version = importlib.metadata.version("qg")
    return (app_version,)


@app.cell
def _():
    # Load configs via qg_configuration() — from CLI arg or default path
    _args = mo.cli_args()
    _config_dir = Path(_args["config-dir"]) if "config-dir" in _args else None
    config = qg_configuration(_config_dir)
    logger.info("Local queue app started | config_dir={}", _config_dir or "default")
    return (config,)


@app.cell
def _(config):
    # Master table with all valid combinations - this is the "carbon copy"
    master_table = config.to_overview_table()
    return (master_table,)


# ---------------------------------------------------------------------------
# Sample source: local CSV/XLSX upload (replaces the B-Fabric order browser).
# ---------------------------------------------------------------------------
@app.cell
def _():
    file_upload = mo.ui.file(filetypes=[".csv", ".xlsx"], label="Upload sample table (CSV/XLSX)")
    return (file_upload,)


@app.cell
def _():
    # Bundled example tables (reactive — selecting one loads it; no button needed).
    example_selector = mo.ui.dropdown(
        options={e.label: e.id for e in list_example_sample_tables()},
        value=None,
        label="…or load a bundled example",
    )
    return (example_selector,)


@app.cell
def _(example_selector):
    # Download link for the selected example (an editable template for user data).
    if example_selector.value:
        _entry, _data = read_example_sample_table(example_selector.value)
        example_download = mo.download(
            data=_data,
            filename=_entry.filename,
            label=f"Download {_entry.filename}",
        )
    else:
        example_download = None
    return (example_download,)


@app.cell
def _():
    # Reproduce mode: load a previously-exported params.json and regenerate its
    # queue from the file's embedded configuration alone (no sidebar config needed).
    params_upload = mo.ui.file(filetypes=[".json"], label="…or load a params.json (reproduce a run)")
    return (params_upload,)


@app.cell
def _():
    # Bundled *self-contained* example runs (full params + embedded config), distinct
    # from the sample-table examples above which still need the sidebar configured.
    params_example_selector = mo.ui.dropdown(
        options={e.label: e.id for e in list_example_params()},
        value=None,
        label="…or a bundled self-contained run",
    )
    return (params_example_selector,)


@app.cell
def _(params_example_selector, params_upload):
    # Parse the active params source into a QueueInput. An uploaded file wins over a
    # selected example; when set, the app reproduces it instead of building from the sidebar.
    loaded_queue_input = None
    loaded_params_source = None
    loaded_params_error = None
    if params_upload.value:
        _f = params_upload.value[0]
        loaded_params_source = _f.name
        try:
            loaded_queue_input = parse_queue_input(_f.contents)
        except (ValueError, pydantic.ValidationError) as exc:
            loaded_params_error = str(exc)
    elif params_example_selector.value:
        _entry, loaded_queue_input = read_example_params(params_example_selector.value)
        loaded_params_source = _entry.filename
    return loaded_params_error, loaded_params_source, loaded_queue_input


@app.cell
def _(example_selector, file_upload):
    # Resolve the active sample source, then parse it into the normalized schema
    # (qg.sample_rows). An uploaded file takes precedence over a selected example;
    # both go through the same parse_sample_table path.
    full_samples_df = pl.DataFrame()
    parsed_mode = None
    sample_source_kind = None
    sample_source_filename = None
    sample_source_description = None
    sample_source_recommended = None
    sample_source_error = None

    _bytes = None
    if file_upload.value:
        _f = file_upload.value[0]
        sample_source_kind = "upload"
        sample_source_filename = _f.name
        _bytes = _f.contents
    elif example_selector.value:
        _entry, _data = read_example_sample_table(example_selector.value)
        sample_source_kind = "example"
        sample_source_filename = _entry.filename
        sample_source_description = _entry.description
        _rs = _entry.recommended_settings
        sample_source_recommended = " / ".join(
            _rs[k] for k in ("tech_area", "instrument", "sampler", "queue_type") if k in _rs
        )
        _bytes = _data

    if _bytes is not None:
        try:
            _parsed = parse_sample_table(_bytes, sample_source_filename)
            full_samples_df = _parsed.df
            parsed_mode = _parsed.mode
        except ValueError as exc:
            sample_source_error = str(exc)
    return (
        full_samples_df,
        parsed_mode,
        sample_source_description,
        sample_source_error,
        sample_source_filename,
        sample_source_kind,
        sample_source_recommended,
    )


@app.cell
def _(full_samples_df, shared):
    # Synthetic "orders" derived from the uploaded container_id column — keeps the
    # shared cells' selected_orders contract (list of (container_id, area)).
    selected_orders = shared.synthesize_local_orders(full_samples_df)
    return (selected_orders,)


@app.cell
def _(parsed_mode):
    # Queue-type capability inferred from the uploaded table shape.
    container_has_plates = parsed_mode == "plate"
    container_has_vials = parsed_mode == "vial"
    return container_has_plates, container_has_vials


# ---------------------------------------------------------------------------
# Configuration controls (shared, B-Fabric-free).
# ---------------------------------------------------------------------------
@app.cell
def _(master_table, tech_area_field):
    # Filter by tech_area for instrument options
    if tech_area_field.value:
        table_by_tech = master_table.filter(pl.col("tech_area") == tech_area_field.value)
    else:
        table_by_tech = master_table
    return (table_by_tech,)


@app.cell
def _(instrument_field, table_by_tech):
    # Filter by instrument for sampler options
    if instrument_field.value:
        table_by_instrument = table_by_tech.filter(pl.col("instrument") == instrument_field.value)
    else:
        table_by_instrument = table_by_tech
    return (table_by_instrument,)


@app.cell
def _(sampler_field, table_by_instrument):
    # Filter by sampler for pattern options
    if sampler_field.value:
        table_by_sampler = table_by_instrument.filter(pl.col("sampler") == sampler_field.value)
    else:
        table_by_sampler = table_by_instrument
    return (table_by_sampler,)


@app.cell
def _(pattern_field, table_by_qc_layout):
    # Final filtered table with all selections
    if pattern_field.value:
        filtered_table = table_by_qc_layout.filter(pl.col("pattern_name") == pattern_field.value)
    else:
        filtered_table = table_by_qc_layout
    return (filtered_table,)


@app.cell
def _(master_table):
    # Tech area dropdown - options from master table (defaults to first, e.g. Proteomics).
    _options = sorted(master_table["tech_area"].unique().to_list())
    tech_area_field = mo.ui.dropdown(
        options=_options,
        value=_options[0] if _options else None,
        label="Tech Area",
    )
    return (tech_area_field,)


@app.cell
def _(table_by_tech, tech_area_field):
    # Instrument dropdown - options based on selected tech_area
    _options = sorted(table_by_tech["instrument"].unique().to_list()) if tech_area_field.value else []
    _default = _options[0] if _options else None

    instrument_field = mo.ui.dropdown(
        options=_options,
        value=_default,
        label="Instrument",
    )
    return (instrument_field,)


@app.cell
def _(instrument_field, table_by_instrument):
    # Sampler dropdown - options based on selected instrument
    _options = sorted(table_by_instrument["sampler"].unique().to_list()) if instrument_field.value else []
    _default = _options[0] if _options else None

    sampler_field = mo.ui.dropdown(
        options=_options,
        value=_default,
        label="Sampler",
    )
    return (sampler_field,)


@app.cell
def _(qc_layout_field, table_by_qc_layout):
    # Pattern dropdown - filtered by QC layout compatibility, with default first
    if qc_layout_field.value:
        _df = table_by_qc_layout.select(["pattern_name", "default_pattern"]).unique()
        _default_patterns = _df.filter(pl.col("pattern_name") == pl.col("default_pattern"))["pattern_name"].to_list()
        _others = (
            _df.filter(pl.col("pattern_name") != pl.col("default_pattern"))["pattern_name"].unique().sort().to_list()
        )
        _options = _default_patterns + _others
        _default = _options[0] if _options else None
    else:
        _options = []
        _default = None

    pattern_field = mo.ui.dropdown(
        options=_options,
        value=_default,
        label="Pattern",
    )
    return (pattern_field,)


@app.cell
def _(container_has_plates, container_has_vials, sampler_field, table_by_sampler):
    # Queue Type dropdown: intersection of what the upload has and what the sampler supports.
    queue_type_warning = None
    if sampler_field.value:
        _sampler_supports = set(table_by_sampler["queue_type"].unique().to_list())
        _order_has: set[str] = set()
        if container_has_plates:
            _order_has.add("Plate")
        if container_has_vials:
            _order_has.add("Vial")
        _usable = _sampler_supports & _order_has
        # Vial-first display keeps Vial as the default when both are present.
        _options = [t for t in ("Vial", "Plate") if t in _usable]
        _default = _options[0] if _options else None

        if _order_has and not _usable:
            queue_type_warning = mo.callout(
                mo.md(f"Sampler **{sampler_field.value}** is incompatible with the uploaded samples."),
                kind="warn",
            )
    else:
        _options = []
        _default = None

    queue_type_field = mo.ui.dropdown(
        options=_options,
        value=_default,
        label="Queue Type",
    )
    return queue_type_field, queue_type_warning


@app.cell
def _(queue_type_field, table_by_sampler):
    # Filter by queue_type for plate_layout options
    if queue_type_field.value:
        table_by_queue_type = table_by_sampler.filter(pl.col("queue_type") == queue_type_field.value)
    else:
        table_by_queue_type = table_by_sampler
    return (table_by_queue_type,)


@app.cell
def _(queue_type_field, table_by_queue_type):
    # Plate layout dropdown - options based on selected sampler + queue_type
    if queue_type_field.value:
        _options = sorted(table_by_queue_type["plate_layout"].unique().to_list())
        _default = _options[0] if _options else None
    else:
        _options = []
        _default = None

    plate_layout_field = mo.ui.dropdown(
        options=_options,
        value=_default,
        label="Plate Layout",
    )
    return (plate_layout_field,)


@app.cell
def _(config, plate_layout_field, queue_type_field):
    # Start position dropdown — only for Vial mode (skip already-used wells)
    if queue_type_field.value == "Vial" and plate_layout_field.value:
        _layout = config.plate_layouts.get_layout(plate_layout_field.value)
        _positions = [f"{row}{col}" for row in _layout.rows for col in _layout.cols]
        start_position_field = mo.ui.dropdown(
            options=_positions,
            value="A1",
            label="& position",
        )
    else:
        start_position_field = None
    return (start_position_field,)


@app.cell
def _(config, sampler_field):
    # Start tray dropdown — shown for both Vial and Plate modes.
    if sampler_field.value:
        _sampler = config.samplers.get_sampler(sampler_field.value)
        _trays = [str(t) for t in _sampler.trays]
        start_tray_field = mo.ui.dropdown(
            options=_trays,
            value=_trays[0],
            label="Start: tray",
        )
    else:
        start_tray_field = None
    return (start_tray_field,)


@app.cell
def _(plate_layout_field, table_by_queue_type):
    # Filter by plate_layout for QC layout options
    if plate_layout_field.value:
        table_by_plate_layout = table_by_queue_type.filter(pl.col("plate_layout") == plate_layout_field.value)
    else:
        table_by_plate_layout = table_by_queue_type
    return (table_by_plate_layout,)


@app.cell
def _(plate_layout_field, table_by_plate_layout):
    # QC layout dropdown - options based on (tech_area, sampler, plate_layout)
    if plate_layout_field.value and "qc_layout_name" in table_by_plate_layout.columns:
        _all_layouts = sorted(table_by_plate_layout["qc_layout_name"].unique().to_list())
        # Auto-select: prefer the default_pattern's qc_layout_name, else first
        _default_rows = table_by_plate_layout.filter(pl.col("pattern_name") == pl.col("default_pattern"))
        if not _default_rows.is_empty():
            _default_qc = _default_rows["qc_layout_name"].to_list()[0]
        else:
            _default_qc = _all_layouts[0] if _all_layouts else None
        _options = _all_layouts
        _default = _default_qc if _default_qc in _all_layouts else (_all_layouts[0] if _all_layouts else None)
    else:
        _options = []
        _default = None

    qc_layout_field = mo.ui.dropdown(
        options=_options,
        value=_default,
        label="QC Layout",
    )
    return (qc_layout_field,)


@app.cell
def _(qc_layout_field, table_by_plate_layout):
    # Filter by qc_layout for pattern options
    if qc_layout_field.value:
        table_by_qc_layout = table_by_plate_layout.filter(pl.col("qc_layout_name") == qc_layout_field.value)
    else:
        table_by_qc_layout = table_by_plate_layout
    return (table_by_qc_layout,)


@app.cell
def _(config, plate_layout_field, qc_layout_field, tech_area_field):
    # Per-level concentration inputs: shown when the selected QC layout contains
    # at least one `standard`-type sample (e.g. Metabolomics `cal_series`).
    _units = ["pmol", "nmol", "umol", "pgml", "ngml", "ugml"]
    _preset_values = [100, 50, 25, 12, 6, 3, 1]
    _preset_unit = "umol"
    concentration_inputs = None
    if qc_layout_field.value and plate_layout_field.value and tech_area_field.value:
        _qc_sample_ids = config.qc_layouts_well.get_sample_ids(
            tech_area_field.value, qc_layout_field.value, plate_layout_field.value
        )
        _level_map: dict[int, str] = {}
        for _sid in _qc_sample_ids:
            _s = config.samples.get_sample(tech_area_field.value, _sid)
            if _s.sample_type == "Std Bracket" and _s.level is not None:
                _level_map[_s.level] = _sid

        if _level_map:
            _sorted_levels = sorted(_level_map)
            concentration_inputs = {
                _level: {
                    "value": mo.ui.number(
                        start=1,
                        stop=999,
                        step=1,
                        value=_preset_values[_idx] if _idx < len(_preset_values) else _preset_values[-1],
                    ),
                    "unit": mo.ui.dropdown(options=_units, value=_preset_unit),
                }
                for _idx, _level in enumerate(_sorted_levels)
            }
    return (concentration_inputs,)


@app.cell
def _(concentration_inputs):
    # Resolved level -> "<value><unit>" mapping, threaded into QueueParameters.
    if concentration_inputs is None:
        level_concentrations: dict[int, str] = {}
    else:
        level_concentrations = {
            _level: f"{_widgets['value'].value}{_widgets['unit'].value}"
            for _level, _widgets in concentration_inputs.items()
            if _widgets["value"].value is not None
        }
    return (level_concentrations,)


@app.cell
def _(concentration_inputs):
    # Compact three-column grid: Level | Value | Unit. Empty when nothing to show.
    if concentration_inputs is None:
        _concentration_block = mo.md("")
    else:
        _widths = [1, 2, 2]
        _header = mo.hstack(
            [mo.md("**Level**"), mo.md("**Value**"), mo.md("**Unit**")],
            widths=_widths,
            justify="start",
        )
        _rows = [
            mo.hstack(
                [mo.md(str(_level)), _widgets["value"], _widgets["unit"]],
                widths=_widths,
                justify="start",
                align="center",
            )
            for _level, _widgets in concentration_inputs.items()
        ]
        _concentration_block = mo.vstack(
            [mo.md("**Concentration per level**"), _header, *_rows],
            gap=0.25,
        )
    concentration_block = _concentration_block
    return (concentration_block,)


@app.cell
def _(filtered_table, sampler_field):
    # Output format is derived (determined by instrument+sampler combination)
    output_format_value = shared.resolve_output_format(filtered_table, bool(sampler_field.value))
    return (output_format_value,)


@app.cell
def _(
    config,
    filtered_table,
    instrument_field,
    pattern_field,
    plate_layout_field,
    qc_layout_field,
    queue_type_field,
    sampler_field,
    selected_orders,
    tech_area_field,
):
    # Validate that all parameters are set and a valid QC layout exists
    validation_errors = []

    if not selected_orders:
        validation_errors.append("Upload a sample table")
    else:
        if not tech_area_field.value:
            validation_errors.append("Tech area not selected")
        if not instrument_field.value:
            validation_errors.append("Instrument not selected")
        if not sampler_field.value:
            validation_errors.append("Sampler not selected")
        if not queue_type_field.value:
            validation_errors.append("Queue type not selected")
        if not plate_layout_field.value:
            validation_errors.append("Plate layout not selected")
        if not qc_layout_field.value:
            validation_errors.append("QC layout not selected")
        if not pattern_field.value:
            validation_errors.append("Pattern not selected")

    # Check combination exists in filtered table
    if not validation_errors and filtered_table.is_empty():
        validation_errors.append("No valid combination found")

    # Check QC layout has samples for this combination (skip for patterns with no QC references)
    if not validation_errors and plate_layout_field.value and qc_layout_field.value:
        _pattern = config.queue_patterns.get_pattern(tech_area_field.value, pattern_field.value)
        if _pattern and _pattern.get_all_sample_ids():
            _qc_samples = config.get_qc_samples(
                tech_area_field.value,
                qc_layout_field.value,
                plate_layout_field.value,
                config.samplers.get_sampler(sampler_field.value),
            )
            if not _qc_samples:
                validation_errors.append(
                    f"No QC samples for {tech_area_field.value}/{qc_layout_field.value}/{plate_layout_field.value}"
                )

    config_valid = len(validation_errors) == 0
    return config_valid, validation_errors


@app.cell
def _(
    config,
    config_valid,
    queue_type_field,
    sample_df,
    sampler_field,
    validation_errors,
):
    # Combine config validation with dynamic plate capacity check
    _all_errors = list(validation_errors)

    if (
        config_valid
        and queue_type_field.value == "Plate"
        and sampler_field.value
        and sample_df is not None
        and not sample_df.is_empty()
        and "plate_id" in sample_df.columns
    ):
        _sampler = config.samplers.get_sampler(sampler_field.value)
        if _sampler:
            _num_trays = len(_sampler.trays)
            _num_plates = sample_df["plate_id"].n_unique()
            if _num_plates > _num_trays:
                _all_errors.append(f"{sampler_field.value} has {_num_trays} trays but {_num_plates} plates selected")

    if not _all_errors:
        validation_status = None
    else:
        _errors_md = "\n".join(f"• {e}" for e in _all_errors)
        validation_status = mo.callout(mo.md(f"**Issues:**\n{_errors_md}"), kind="warn")
    return (validation_status,)


@app.cell
def _(config, pattern_field, tech_area_field):
    # Get default QC frequency from selected pattern (fallback to 16 if not yet selected)
    if tech_area_field.value and pattern_field.value:
        _pattern = config.queue_patterns.get_pattern(tech_area_field.value, pattern_field.value)
        default_qc_frequency = _pattern.run_QC_after_n_samples if _pattern else 16
    else:
        default_qc_frequency = 16
    return (default_qc_frequency,)


@app.cell
def _(default_qc_frequency):
    qc_frequency_field = mo.ui.text(
        value="",
        label="QC frequency",
        placeholder=str(default_qc_frequency),
    )
    return (qc_frequency_field,)


@app.cell
def _(config, instrument_field, tech_area_field):
    # Load available methods from config (empty DataFrame if not yet selected)
    if tech_area_field.value and instrument_field.value:
        _methods = config.methods.get_methods(tech_area_field.value, instrument_field.value)
        methods_df = _methods.to_table() if _methods else pl.DataFrame()
    else:
        methods_df = pl.DataFrame()
    return (methods_df,)


@app.cell
def _(config, instrument_field, methods_df, pattern_field, tech_area_field):
    # Compute available method names as the intersection across all sample types
    # in the selected pattern (QC samples + default for user samples).
    def _get_methods_intersection(polarity: str) -> list[str]:
        if methods_df.is_empty() or not tech_area_field.value or not instrument_field.value:
            return []
        methods_for_instr = config.methods.get_methods(tech_area_field.value, instrument_field.value)

        sample_ids: set[str] = {SamplesConfig.DEFAULT_SAMPLE_ID}
        if pattern_field.value:
            _pattern = config.queue_patterns.get_pattern(tech_area_field.value, pattern_field.value)
            sample_ids |= _pattern.get_all_sample_ids()

        sets = [methods_for_instr.get_method_names(sid, polarity) for sid in sample_ids]
        non_empty = [s for s in sets if s]
        if not non_empty:
            return []
        return sorted(non_empty[0].intersection(*non_empty[1:]), reverse=True)

    _has_polarity = not methods_df.is_empty() and "polarity" in methods_df.columns
    available_methods_pos = _get_methods_intersection("pos" if _has_polarity else "")
    available_methods_neg = _get_methods_intersection("neg" if _has_polarity else "")
    return available_methods_neg, available_methods_pos


@app.cell
def _(available_methods_pos, polarity_group):
    # Method dropdown for positive polarity (with None option)
    _show_pos = polarity_group.value.get("pos", False)
    if _show_pos:
        _options = [""] + available_methods_pos  # Empty string = no method
        _label_pos = "Method Name (pos)" if polarity_group.value.get("neg", False) else "Method Name"
        method_field_pos = mo.ui.dropdown(
            options=_options,
            value=available_methods_pos[0] if available_methods_pos else "",
            label=_label_pos,
        )
    else:
        method_field_pos = None
    return (method_field_pos,)


@app.cell
def _(available_methods_neg, polarity_group):
    # Method dropdown for negative polarity (with None option)
    _show_neg = polarity_group.value.get("neg", False)
    if _show_neg:
        _options = [""] + available_methods_neg  # Empty string = no method
        method_field_neg = mo.ui.dropdown(
            options=_options,
            value=available_methods_neg[0] if available_methods_neg else "",
            label="Method Name (neg)",
        )
    else:
        method_field_neg = None
    return (method_field_neg,)


@app.cell
def _(config, tech_area_field):
    _defaults = config.tech_area_defaults.get_default_polarities(tech_area_field.value)
    polarity_group = mo.ui.batch(
        mo.md("**Polarity:** {pos} pos {neg} neg"),
        {
            "pos": mo.ui.checkbox(value="pos" in _defaults),
            "neg": mo.ui.checkbox(value="neg" in _defaults),
        },
    )
    return (polarity_group,)


@app.cell
def _():
    randomization_field = mo.ui.dropdown(
        options=["no", "random", "blocked", "blocked_uniform"], value="no", label="Randomization"
    )
    return (randomization_field,)


@app.cell
def _():
    inj_vol_field = mo.ui.text(value="1", label="Inj Vol (µl)")
    return (inj_vol_field,)


@app.cell
def _():
    # Local mode: user is free text, defaulting to the OS user. No B-Fabric login.
    user_field = mo.ui.text(value=os.environ.get("USER", ""), label="User")
    return (user_field,)


@app.cell
def _():
    date_field = mo.ui.date(value=date.today(), label="Date")
    return (date_field,)


@app.cell
def _(
    config,
    pattern_field,
    plate_layout_field,
    qc_layout_field,
    raw_queue_df,
    sampler_field,
    tech_area_field,
):
    # Show the *effective* QC layout for this run — the intersection of what
    # the QC layout declares and what the selected pattern actually injects.
    qc_layout_preview = None
    if (
        tech_area_field.value
        and sampler_field.value
        and plate_layout_field.value
        and qc_layout_field.value
        and pattern_field.value
    ):
        _sampler = config.samplers.get_sampler(sampler_field.value)
        _samples = config.get_qc_samples(
            tech_area_field.value,
            qc_layout_field.value,
            plate_layout_field.value,
            _sampler,
        )
        _pattern = config.queue_patterns.get_pattern(tech_area_field.value, pattern_field.value)
        _used_sample_ids = _pattern.get_all_sample_ids()
        _samples = [s for s in _samples if s.sample_id is not None and s.sample_id in _used_sample_ids]
        if _samples:
            _rows = []
            if _sampler.is_tip:
                for s in _samples:
                    _rows.append(
                        {"sample_id": s.sample_id, "tray": s.tray, "range": f"{s.position_start}-{s.position_end}"}
                    )
                qc_layout_preview = pl.DataFrame(_rows)
            else:
                for s in _samples:
                    _rows.append({"sample_id": s.sample_id, "tray": s.tray, "pos": f"{s.row}{s.col}"})
                _preview = pl.DataFrame(_rows)
                if raw_queue_df is not None and not raw_queue_df.is_empty():
                    _well_counts = (
                        raw_queue_df.group_by(["tray", "row", "col"])
                        .agg(pl.len().alias("visits"))
                        .with_columns(pl.concat_str(["row", pl.col("col").cast(pl.Utf8)]).alias("pos"))
                        .select(["tray", "pos", "visits"])
                    )
                    _well_counts = _well_counts.with_columns(pl.col("tray").cast(_preview["tray"].dtype))
                    _preview = _preview.join(_well_counts, on=["tray", "pos"], how="left").with_columns(
                        pl.col("visits").fill_null(0)
                    )
                qc_layout_preview = _preview
    return (qc_layout_preview,)


@app.cell
def _(
    app_version,
    concentration_block,
    concentration_inputs,
    date_field,
    inj_vol_field,
    instrument_field,
    method_field_neg,
    method_field_pos,
    output_format_value,
    pattern_field,
    plate_layout_field,
    polarity_group,
    qc_frequency_field,
    qc_layout_field,
    qc_layout_preview,
    queue_type_field,
    queue_type_warning,
    randomization_field,
    loaded_params_source,
    loaded_queue_input,
    sampler_field,
    start_position_field,
    start_tray_field,
    tech_area_field,
    user_field,
    validation_status,
):
    # Hide the Pattern dropdown when the user picks the "noqc" QC layout.
    _queue_items = [qc_layout_field]
    if qc_layout_field.value != "noqc":
        _queue_items.append(pattern_field)
    if concentration_inputs is not None:
        _queue_items.append(concentration_block)
    _queue_items.append(polarity_group)
    if method_field_pos is not None:
        _queue_items.append(method_field_pos)
    if method_field_neg is not None:
        _queue_items.append(method_field_neg)

    _sidebar_items = [
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
        *_queue_items,
        mo.md("### Options"),
        randomization_field,
        date_field,
        user_field,
        inj_vol_field,
        qc_frequency_field,
    ]
    if validation_status is not None:
        _sidebar_items.append(validation_status)

    if qc_layout_preview is not None:
        _sidebar_items.append(mo.md(f"**QC Positions** _{qc_layout_field.value}_"))
        _sidebar_items.append(qc_layout_preview)

    if loaded_queue_input is not None:
        # Reproduce mode: the run is fully determined by the loaded file, so the
        # config controls are replaced by a read-only summary (panel disabled).
        _p = loaded_queue_input.parameters
        _seed = f", seed `{_p.seed}`" if _p.seed is not None else ""
        _sidebar_content = mo.vstack(
            [
                mo.md("## Reproducing a saved run"),
                mo.callout(
                    mo.md(
                        f"**Loaded from `{loaded_params_source}`.**\n\n"
                        f"qg version `{loaded_queue_input.qg_version or 'n/a'}`\n\n"
                        f"{_p.tech_area} / {_p.instrument} / {_p.sampler}\n\n"
                        f"pattern `{_p.queue_pattern}`, layout `{_p.plate_layout}`, "
                        f"QC `{_p.qc_layout_name}`\n\n"
                        f"randomization `{_p.randomization}`{_seed}\n\n"
                        "The queue is regenerated from this file's embedded configuration; "
                        "the controls are disabled. Clear the params file above to start over."
                    ),
                    kind="info",
                ),
            ]
        )
    else:
        _sidebar_content = mo.vstack(_sidebar_items)
    mo.sidebar(_sidebar_content, footer=mo.md(f"Version: {app_version}"), width="28rem")
    return


# ---------------------------------------------------------------------------
# Upload header + sample selection (shared selection/editor cells).
# ---------------------------------------------------------------------------
@app.cell
def _(
    example_download,
    example_selector,
    file_upload,
    full_samples_df,
    loaded_params_error,
    loaded_params_source,
    loaded_queue_input,
    params_example_selector,
    params_upload,
    sample_source_description,
    sample_source_error,
    sample_source_filename,
    sample_source_kind,
    sample_source_recommended,
):
    _items = [mo.md("# Local Queue Generator"), file_upload, example_selector]
    if example_download is not None:
        _items.append(example_download)
    _items += [mo.md("---"), params_upload, params_example_selector]

    if loaded_params_error:
        _items.append(mo.callout(mo.md(f"**Could not load params JSON:** {loaded_params_error}"), kind="danger"))
    elif loaded_queue_input is not None:
        _items.append(
            mo.callout(
                mo.md(
                    f"**Reproducing `{loaded_params_source}`** — the queue is regenerated from the file's "
                    "embedded configuration and the sidebar controls are disabled. See the **Queue Preview** tab."
                ),
                kind="success",
            )
        )
    elif sample_source_error:
        _label = sample_source_filename or "sample table"
        _items.append(mo.callout(mo.md(f"**Could not parse `{_label}`:** {sample_source_error}"), kind="danger"))
    elif not full_samples_df.is_empty():
        _n = len(full_samples_df)
        if sample_source_kind == "example":
            _msg = f"**Loaded {_n} samples** from example `{sample_source_filename}`."
            if sample_source_recommended:
                _msg += f" Recommended: {sample_source_recommended}."
            if sample_source_description:
                _msg += f" {sample_source_description}"
        else:
            _msg = f"**Loaded {_n} samples** from `{sample_source_filename}` (uploaded)."
        _items.append(mo.md(_msg))
    elif sample_source_kind is not None:
        _items.append(mo.callout(mo.md("**File parsed but no samples found.**"), kind="warn"))
    else:
        _items.append(mo.md("_Upload a CSV/XLSX sample table or pick an example to begin._"))
    mo.vstack(_items)
    return


@app.cell
def _():
    sample_mode_selector = mo.ui.radio(
        options=["Sample Selection", "Sample Editor"],
        value="Sample Selection",
        inline=True,
    )
    return (sample_mode_selector,)


@app.cell
def _():
    name_suffix = mo.ui.dropdown(
        options=["none", "enriched", "total", "lip"],
        value="none",
        label="Append suffix to every sample name",
    )
    return (name_suffix,)


@app.cell
def _(full_samples_df, name_suffix):
    # Apply the name suffix at the source so it shows in the selection table, the
    # editor grid, the preview, and the output. Derived from the pristine
    # full_samples_df each run, so switching suffixes never accumulates.
    if not full_samples_df.is_empty():
        _display_df = full_samples_df
        if name_suffix.value != "none":
            _display_df = full_samples_df.with_columns(
                (pl.col("sample_name") + "_" + name_suffix.value).alias("sample_name")
            )
        _n = len(_display_df)
        samples_table = mo.ui.table(
            data=_display_df,
            selection="multi",
            initial_selection=list(range(_n)),
            label="Uncheck to exclude samples",
            show_download=False,
            pagination=False,
        )
    else:
        samples_table = None
    return (samples_table,)


@app.cell
def _(full_samples_df, samples_table):
    if samples_table is not None:
        _kept = samples_table.value
        _with_order = _kept.with_row_index("order", offset=1).cast({"order": pl.Float64})
        samples_editor = mo.ui.data_editor(_with_order, label="Samples (edit order to reorder)", pagination=False)
    else:
        samples_editor = None
    return (samples_editor,)


@app.cell
def _(samples_editor):
    if samples_editor is not None:
        sample_df = samples_editor.value.sort("order").drop("order")
    else:
        sample_df = pl.DataFrame()
    return (sample_df,)


@app.cell
def _(
    date_field,
    inj_vol_field,
    instrument_field,
    level_concentrations,
    method_field_neg,
    method_field_pos,
    output_format_value,
    pattern_field,
    plate_layout_field,
    polarity_group,
    qc_frequency_field,
    qc_layout_field,
    queue_type_field,
    randomization_field,
    sampler_field,
    start_position_field,
    start_tray_field,
    tech_area_field,
    user_field,
    loaded_queue_input,
):
    queue_parameters_err = None
    try:
        _inj_vol = float(inj_vol_field.value) if inj_vol_field.value.strip() else None
        _qc_freq = int(qc_frequency_field.value) if qc_frequency_field.value.strip() else None
        _polarity = [p for p in ("pos", "neg") if polarity_group.value.get(p)]

        _method_fields = {"pos": method_field_pos, "neg": method_field_neg}
        method_dict = {pol: field.value for pol, field in _method_fields.items() if field is not None and field.value}

        queue_parameters = QueueParameters.model_validate(
            {
                "tech_area": tech_area_field.value,
                "instrument": instrument_field.value,
                "sampler": sampler_field.value,
                "output_format": output_format_value,
                "queue_pattern": pattern_field.value,
                "queue_type": queue_type_field.value,
                "plate_layout": plate_layout_field.value,
                "qc_layout_name": qc_layout_field.value,
                "polarity": _polarity,
                "date": date_field.value.strftime("%Y%m%d"),
                "user": user_field.value.strip(),
                "method": method_dict,
                "randomization": randomization_field.value,
                "inj_vol_override": _inj_vol,
                "qc_frequency_override": _qc_freq,
                "start_position": start_position_field.value if start_position_field is not None else "A1",
                "start_tray": start_tray_field.value if start_tray_field is not None else "",
                "level_concentrations": level_concentrations,
            }
        )
    except pydantic.ValidationError as e:
        queue_parameters_err = e
        queue_parameters = None
    if loaded_queue_input is not None:
        # Reproduce mode: override with the loaded file's parameters. The sidebar is
        # disabled and may be incompletely populated (no samples -> no queue_type),
        # so its build above is discarded here.
        queue_parameters = loaded_queue_input.parameters
        queue_parameters_err = None
    return queue_parameters, queue_parameters_err


@app.cell
def _(name_suffix, sample_df, sample_mode_selector, samples_editor, samples_table, selected_orders):
    # Sample Selection tab content
    _order_count = len(selected_orders) if selected_orders else 0
    if sample_df is not None and not sample_df.is_empty():
        _summary = mo.md(f"**{len(sample_df)} samples from {_order_count} group(s)**")
    else:
        _summary = mo.md("**No samples loaded**")
    if samples_editor is not None:
        _editor_panel = mo.vstack([name_suffix, samples_editor])
    else:
        _editor_panel = mo.md("_No samples loaded_")
    _panels = {
        "Sample Selection": samples_table if samples_table is not None else mo.md("_No samples loaded_"),
        "Sample Editor": _editor_panel,
    }
    _panel_stack = [
        mo.md(f'<div style="display: {"block" if _name == sample_mode_selector.value else "none"}">{_widget}</div>')
        for _name, _widget in _panels.items()
    ]
    sample_selection_content = mo.vstack([_summary, sample_mode_selector, *_panel_stack])
    return (sample_selection_content,)


# ---------------------------------------------------------------------------
# Pipeline: build -> generate -> preview/download (shared helpers, no B-Fabric).
# ---------------------------------------------------------------------------
@app.cell
def _(config, loaded_queue_input, queue_parameters, sample_df, selected_orders):
    # Build QueueInput once — shared by the Parameters tab and queue generation.
    # In reproduce mode the loaded params JSON *is* the queue input; otherwise build
    # it from the sidebar. No provenance instance in local mode (bfabric_instance None).
    if loaded_queue_input is not None:
        queue_input, queue_input_err = loaded_queue_input, None
    else:
        queue_input, queue_input_err = shared.build_queue_input(
            config,
            queue_parameters,
            sample_df,
            has_samples_source=bool(selected_orders),
            provenance_instance=None,
        )
    return queue_input, queue_input_err


@app.cell
def _(
    loaded_params_source,
    loaded_queue_input,
    queue_input,
    queue_input_err,
    queue_parameters,
    queue_parameters_err,
    sample_df,
):
    # Parameters tab content — uses the shared queue_input.
    _output = None
    _download_button = None
    if queue_input is not None:
        _output = queue_input.model_dump(mode="json")
        # In reproduce mode there is no uploaded sample_df to name the file from;
        # reuse the loaded file's name instead.
        if loaded_queue_input is not None:
            _fname = loaded_params_source or "params.json"
        else:
            _fname = shared.params_json_filename(queue_parameters, sample_df)
        _download_button = shared.params_download_button(queue_input, _fname)

    _err_msg = queue_input_err or (str(queue_parameters_err) if queue_parameters_err else "Upload a sample table")
    parameters_content = mo.vstack(
        [
            _download_button,
            mo.callout(_output, kind="info") if _output else mo.callout(_err_msg, kind="danger"),
        ]
    )
    return (parameters_content,)


@app.cell
def _(config, loaded_queue_input, queue_input, queue_parameters):
    # Generate the queue exactly once so preview and download are identical. When
    # reproducing a loaded run, regenerate from its embedded resolved_config so the
    # result is independent of the local qg_configs tree.
    if loaded_queue_input is not None and loaded_queue_input.resolved_config is not None:
        _gen_config = loaded_queue_input.resolved_config.to_configuration()
        _gen_params = loaded_queue_input.parameters
    else:
        _gen_config = config
        _gen_params = queue_parameters
    _result = shared.generate_queue(_gen_config, queue_input, _gen_params)
    generated_queue_df = _result.generated_df
    raw_queue_df = _result.raw_df
    queue_output_str = _result.output_str
    generation_error = _result.error
    output_file_extension = _result.file_extension
    return (
        generated_queue_df,
        generation_error,
        output_file_extension,
        queue_output_str,
        raw_queue_df,
    )


@app.cell
def _():
    formatted_ticket_toggle = mo.ui.checkbox(True, label="Formatted Preview")
    return (formatted_ticket_toggle,)


@app.cell
def _(output_file_extension, queue_parameters, selected_orders):
    _ids = [o[0] for o in selected_orders] if selected_orders else []
    queue_output_filename = shared.queue_output_filename(queue_parameters, _ids, output_file_extension)
    return (queue_output_filename,)


@app.cell
def _(
    formatted_ticket_toggle,
    generated_queue_df,
    generation_error,
    queue_output_filename,
    queue_output_str,
    queue_parameters,
    raw_queue_df,
):
    # Queue Preview tab content — local mode enables download immediately (no upload gate).
    if generation_error:
        queue_preview_content = mo.callout(mo.md(f"**Generation Error:** {generation_error}"), kind="danger")
    elif generated_queue_df is not None and queue_output_str is not None:
        _display_df = generated_queue_df if formatted_ticket_toggle.value else raw_queue_df
        _download_button = shared.queue_download_button(queue_output_str, queue_output_filename)
        _rand_label = (
            f" | randomization: {queue_parameters.randomization}" if queue_parameters.randomization != "no" else ""
        )
        queue_preview_content = mo.vstack(
            [
                _download_button,
                mo.md(f"**{len(_display_df)} rows{_rand_label}** {formatted_ticket_toggle}"),
                mo.ui.table(_display_df, show_column_summaries=False, show_download=False, selection=None),
            ]
        )
    else:
        queue_preview_content = mo.md("_Upload a sample table and configure parameters to preview the queue._")
    return (queue_preview_content,)


# ---------------------------------------------------------------------------
# Visualizations (shared, B-Fabric-free).
# ---------------------------------------------------------------------------
@app.cell
def _():
    plate_well_size = mo.ui.slider(start=14, stop=52, step=2, value=30, label="Well size", show_value=True)
    return (plate_well_size,)


@app.cell
def _():
    viz_subtab = mo.ui.radio(
        options=["Plate Layout", "Acquisition Timeline"],
        value="Plate Layout",
        inline=True,
    )
    return (viz_subtab,)


@app.cell
def _():
    plate_color_by = mo.ui.dropdown(
        options=["Sample type", "Group (grouping_var)"], value="Sample type", label="Color by"
    )
    timeline_color_by = mo.ui.dropdown(
        options=["Injection class", "QC cadence"], value="Injection class", label="Color by"
    )
    return plate_color_by, timeline_color_by


@app.cell
def _(config, plate_color_by, plate_well_size, queue_parameters, raw_queue_df):
    if raw_queue_df is None or raw_queue_df.is_empty():
        plate_layout_view = mo.md("_Generate a queue to see the plate layout._")
    else:
        _geom = raw_queue_df.filter((pl.col("row") != "") & (pl.col("col") != 0))
        if _geom.is_empty():
            plate_layout_view = mo.callout(
                mo.md("**Plate view not available** for this layout — positions have no row/column geometry."),
                kind="info",
            )
        else:
            _has_group = _geom["grouping_var"].drop_nulls().len() > 0
            _want_group = plate_color_by.value.startswith("Group") and _has_group
            _color_by = "grouping_var" if _want_group else "category"
            _legend = "grouping_var" if _want_group else "sample type"
            _score = plate_balance(raw_queue_df)
            _score_md = (
                f"Group ↔ plate position (η²): **{_score:.2f}** — 0 = balanced, 1 = separated"
                if _score is not None
                else "Group ↔ plate position (η²): **N/A** (no grouping variable)"
            )
            _wells = build_plate_wells(_geom)
            _orders = sorted(_geom["container_id"].unique().to_list())
            _layout = config.plate_layouts.get_layout(queue_parameters.plate_layout)
            plate_layout_view = mo.vstack(
                [
                    mo.md(f"**Plate layout** — color = {_legend}, shape = order. Hover a well for details."),
                    mo.md(_score_md),
                    mo.hstack([plate_color_by, plate_well_size], justify="start", gap=2),
                    mo.ui.plotly(
                        build_plate_figure(_wells, _layout, _orders, cell=plate_well_size.value, color_by=_color_by)
                    ),
                ]
            )
    return (plate_layout_view,)


@app.cell
def _(raw_queue_df, timeline_color_by):
    if raw_queue_df is None or raw_queue_df.is_empty():
        timeline_view = mo.md("_Generate a queue to see the acquisition timeline._")
    else:
        _color_by = "qc_cadence" if timeline_color_by.value == "QC cadence" else "grouping_var"
        _score = queue_balance(raw_queue_df)
        _score_md = (
            f"Group ↔ queue position (η²): **{_score:.2f}** — 0 = balanced, 1 = separated"
            if _score is not None
            else "Group ↔ queue position (η²): **N/A** (no grouping variable)"
        )
        timeline_view = mo.vstack(
            [
                mo.md("**Acquisition timeline** — one tile per injection along run order. Hover for details."),
                mo.md(_score_md),
                timeline_color_by,
                mo.ui.plotly(build_timeline_figure(raw_queue_df, color_by=_color_by)),
            ]
        )
    return (timeline_view,)


@app.cell
def _(plate_layout_view, timeline_view, viz_subtab):
    _views = {"Plate Layout": plate_layout_view, "Acquisition Timeline": timeline_view}
    visualizations_content = mo.vstack([viz_subtab, _views[viz_subtab.value]])
    return (visualizations_content,)


@app.cell
def _(
    instrument_field,
    master_table,
    pattern_field,
    plate_layout_field,
    qc_layout_field,
    queue_type_field,
    sampler_field,
    tech_area_field,
):
    # Valid Combinations tab content - shows ALL combinations with matching rows highlighted
    _match_conditions = []
    _active_filters = []

    if tech_area_field.value:
        _match_conditions.append(pl.col("tech_area") == tech_area_field.value)
        _active_filters.append(f"**tech_area** = {tech_area_field.value}")
    if instrument_field.value:
        _match_conditions.append(pl.col("instrument") == instrument_field.value)
        _active_filters.append(f"**instrument** = {instrument_field.value}")
    if sampler_field.value:
        _match_conditions.append(pl.col("sampler") == sampler_field.value)
        _active_filters.append(f"**sampler** = {sampler_field.value}")
    if qc_layout_field.value:
        _match_conditions.append(pl.col("qc_layout_name") == qc_layout_field.value)
        _active_filters.append(f"**qc_layout** = {qc_layout_field.value}")
    if pattern_field.value:
        _match_conditions.append(pl.col("pattern_name") == pattern_field.value)
        _active_filters.append(f"**pattern_name** = {pattern_field.value}")
    if queue_type_field.value:
        _match_conditions.append(pl.col("queue_type") == queue_type_field.value)
        _active_filters.append(f"**queue_type** = {queue_type_field.value}")
    if plate_layout_field.value:
        _match_conditions.append(pl.col("plate_layout") == plate_layout_field.value)
        _active_filters.append(f"**plate_layout** = {plate_layout_field.value}")

    _display_cols = [c for c in master_table.columns if c != "default_pattern"]
    if _match_conditions:
        _combined_match = pl.all_horizontal(*_match_conditions)
        _display_table = (
            master_table.with_columns(pl.when(_combined_match).then(pl.lit("✓")).otherwise(pl.lit("")).alias("✓"))
            .select(["✓"] + _display_cols)
            .sort("✓", descending=True)
        )
        _match_count = master_table.filter(_combined_match).height
    else:
        _display_table = master_table.with_columns(pl.lit("").alias("✓")).select(["✓"] + _display_cols)
        _match_count = master_table.height

    _filters_md = " | ".join(_active_filters) if _active_filters else "None"
    valid_combinations_content = mo.vstack(
        [
            mo.md(f"**{_match_count}/{len(master_table)} combinations** match | Filters: {_filters_md}"),
            _display_table,
        ]
    )
    return (valid_combinations_content,)


@app.cell
def _():
    tab_selector = mo.ui.radio(
        options=["✎ Edit Samples", "Queue Preview", "Visualizations", "Parameters", "Valid Combinations"],
        value="Queue Preview",
        inline=True,
    )
    return (tab_selector,)


@app.cell
def _(
    parameters_content,
    queue_preview_content,
    sample_selection_content,
    tab_selector,
    valid_combinations_content,
    visualizations_content,
):
    _sections = {
        "Queue Preview": queue_preview_content,
        "Visualizations": visualizations_content,
        "✎ Edit Samples": sample_selection_content,
        "Parameters": parameters_content,
        "Valid Combinations": valid_combinations_content,
    }
    _panels = [
        mo.md(f'<div style="display: {"block" if _name == tab_selector.value else "none"}">{_content}</div>')
        for _name, _content in _sections.items()
    ]
    mo.vstack([tab_selector, *_panels])
    return


if __name__ == "__main__":
    app.run()
