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
    from qg.params_models import parse_queue_input


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
    table_by_tech = shared.filter_by_column(master_table, "tech_area", tech_area_field.value)
    return (table_by_tech,)


@app.cell
def _(instrument_field, table_by_tech):
    table_by_instrument = shared.filter_by_column(table_by_tech, "instrument", instrument_field.value)
    return (table_by_instrument,)


@app.cell
def _(sampler_field, table_by_instrument):
    table_by_sampler = shared.filter_by_column(table_by_instrument, "sampler", sampler_field.value)
    return (table_by_sampler,)


@app.cell
def _(pattern_field, table_by_qc_layout):
    filtered_table = shared.filter_by_column(table_by_qc_layout, "pattern_name", pattern_field.value)
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
    instrument_field = shared.make_column_dropdown(
        table_by_tech, "instrument", enabled=bool(tech_area_field.value), label="Instrument"
    )
    return (instrument_field,)


@app.cell
def _(instrument_field, table_by_instrument):
    sampler_field = shared.make_column_dropdown(
        table_by_instrument, "sampler", enabled=bool(instrument_field.value), label="Sampler"
    )
    return (sampler_field,)


@app.cell
def _(qc_layout_field, table_by_qc_layout):
    pattern_field = shared.make_pattern_field(table_by_qc_layout, enabled=bool(qc_layout_field.value))
    return (pattern_field,)


@app.cell
def _(container_has_plates, container_has_vials, sampler_field, table_by_sampler):
    queue_type_field, queue_type_warning = shared.make_queue_type_field(
        table_by_sampler,
        sampler=sampler_field.value,
        has_plates=container_has_plates,
        has_vials=container_has_vials,
        incompatible_subject="the uploaded samples",
    )
    return queue_type_field, queue_type_warning


@app.cell
def _(queue_type_field, table_by_sampler):
    table_by_queue_type = shared.filter_by_column(table_by_sampler, "queue_type", queue_type_field.value)
    return (table_by_queue_type,)


@app.cell
def _(queue_type_field, table_by_queue_type):
    plate_layout_field = shared.make_column_dropdown(
        table_by_queue_type, "plate_layout", enabled=bool(queue_type_field.value), label="Plate Layout"
    )
    return (plate_layout_field,)


@app.cell
def _(config, plate_layout_field, queue_type_field):
    start_position_field = shared.make_start_position_field(
        config, queue_type=queue_type_field.value, plate_layout=plate_layout_field.value
    )
    return (start_position_field,)


@app.cell
def _(config, sampler_field):
    start_tray_field = shared.make_start_tray_field(config, sampler=sampler_field.value)
    return (start_tray_field,)


@app.cell
def _(plate_layout_field, table_by_queue_type):
    table_by_plate_layout = shared.filter_by_column(table_by_queue_type, "plate_layout", plate_layout_field.value)
    return (table_by_plate_layout,)


@app.cell
def _(plate_layout_field, table_by_plate_layout):
    qc_layout_field = shared.make_qc_layout_field(table_by_plate_layout, enabled=bool(plate_layout_field.value))
    return (qc_layout_field,)


@app.cell
def _(qc_layout_field, table_by_plate_layout):
    table_by_qc_layout = shared.filter_by_column(table_by_plate_layout, "qc_layout_name", qc_layout_field.value)
    return (table_by_qc_layout,)


@app.cell
def _(config, plate_layout_field, qc_layout_field, tech_area_field):
    concentration_inputs = shared.make_concentration_inputs(
        config,
        tech_area=tech_area_field.value,
        qc_layout=qc_layout_field.value,
        plate_layout=plate_layout_field.value,
    )
    return (concentration_inputs,)


@app.cell
def _(concentration_inputs):
    level_concentrations = shared.resolve_level_concentrations(concentration_inputs)
    return (level_concentrations,)


@app.cell
def _(concentration_inputs):
    concentration_block = shared.render_concentration_block(concentration_inputs)
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
    config_valid, validation_errors = shared.validate_selection(
        config,
        selected_orders=selected_orders,
        tech_area=tech_area_field.value,
        instrument=instrument_field.value,
        sampler=sampler_field.value,
        queue_type=queue_type_field.value,
        plate_layout=plate_layout_field.value,
        qc_layout=qc_layout_field.value,
        pattern=pattern_field.value,
        filtered_table=filtered_table,
        no_source_message="Upload a sample table",
    )
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
    validation_status = shared.render_validation_status(
        config,
        config_valid=config_valid,
        validation_errors=validation_errors,
        queue_type=queue_type_field.value,
        sampler=sampler_field.value,
        sample_df=sample_df,
    )
    return (validation_status,)


@app.cell
def _(config, pattern_field, tech_area_field):
    default_qc_frequency = shared.resolve_default_qc_frequency(config, tech_area_field.value, pattern_field.value)
    return (default_qc_frequency,)


@app.cell
def _(default_qc_frequency):
    qc_frequency_field = shared.make_qc_frequency_field(default_qc_frequency)
    return (qc_frequency_field,)


@app.cell
def _(config, instrument_field, tech_area_field):
    methods_df = shared.load_methods_table(config, tech_area_field.value, instrument_field.value)
    return (methods_df,)


@app.cell
def _(config, instrument_field, methods_df, pattern_field, tech_area_field):
    available_methods_pos, available_methods_neg = shared.available_method_names(
        config, methods_df, tech_area_field.value, instrument_field.value, pattern_field.value
    )
    return available_methods_neg, available_methods_pos


@app.cell
def _(available_methods_pos, polarity_group):
    _label = "Method Name (pos)" if polarity_group.value.get("neg", False) else "Method Name"
    method_field_pos = shared.make_method_field(
        available_methods_pos, show=polarity_group.value.get("pos", False), label=_label
    )
    return (method_field_pos,)


@app.cell
def _(available_methods_neg, polarity_group):
    method_field_neg = shared.make_method_field(
        available_methods_neg, show=polarity_group.value.get("neg", False), label="Method Name (neg)"
    )
    return (method_field_neg,)


@app.cell
def _(config, tech_area_field):
    polarity_group = shared.make_polarity_group(
        config.tech_area_defaults.get_default_polarities(tech_area_field.value)
    )
    return (polarity_group,)


@app.cell
def _():
    randomization_field = shared.make_randomization_field()
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
    qc_layout_preview = shared.resolve_qc_layout_preview(
        config,
        tech_area=tech_area_field.value,
        sampler=sampler_field.value,
        plate_layout=plate_layout_field.value,
        qc_layout=qc_layout_field.value,
        pattern=pattern_field.value,
        raw_queue_df=raw_queue_df,
    )
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
        _sidebar_content = shared.render_sidebar_body(
            tech_area_field=tech_area_field,
            instrument_field=instrument_field,
            sampler_field=sampler_field,
            queue_type_field=queue_type_field,
            queue_type_warning=queue_type_warning,
            plate_layout_field=plate_layout_field,
            start_tray_field=start_tray_field,
            start_position_field=start_position_field,
            output_format_value=output_format_value,
            qc_layout_field=qc_layout_field,
            pattern_field=pattern_field,
            concentration_inputs=concentration_inputs,
            concentration_block=concentration_block,
            polarity_group=polarity_group,
            method_field_pos=method_field_pos,
            method_field_neg=method_field_neg,
            randomization_field=randomization_field,
            date_field=date_field,
            user_field=user_field,
            inj_vol_field=inj_vol_field,
            qc_frequency_field=qc_frequency_field,
            validation_status=validation_status,
            qc_layout_preview=qc_layout_preview,
        )
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
    sample_mode_selector = shared.make_sample_mode_selector()
    return (sample_mode_selector,)


@app.cell
def _(config, tech_area_field):
    name_suffix = shared.make_name_suffix(config, tech_area_field.value)
    return (name_suffix,)


@app.cell
def _(full_samples_df, name_suffix):
    samples_table = shared.make_samples_table(full_samples_df, name_suffix.value)
    return (samples_table,)


@app.cell
def _(samples_table):
    samples_editor = shared.make_samples_editor(samples_table)
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
    queue_parameters, queue_parameters_err = shared.build_queue_parameters(
        tech_area=tech_area_field.value,
        instrument=instrument_field.value,
        sampler=sampler_field.value,
        output_format=output_format_value,
        queue_pattern=pattern_field.value,
        queue_type=queue_type_field.value,
        plate_layout=plate_layout_field.value,
        qc_layout_name=qc_layout_field.value,
        polarity_flags=polarity_group.value,
        date=date_field.value,
        user=user_field.value,
        method_pos=method_field_pos.value if method_field_pos is not None else None,
        method_neg=method_field_neg.value if method_field_neg is not None else None,
        randomization=randomization_field.value,
        inj_vol_text=inj_vol_field.value,
        qc_frequency_text=qc_frequency_field.value,
        start_position=start_position_field.value if start_position_field is not None else None,
        start_tray=start_tray_field.value if start_tray_field is not None else None,
        level_concentrations=level_concentrations,
    )
    if loaded_queue_input is not None:
        # Reproduce mode: override with the loaded file's parameters. The sidebar is
        # disabled and may be incompletely populated (no samples -> no queue_type),
        # so its build above is discarded here.
        queue_parameters = loaded_queue_input.parameters
        queue_parameters_err = None
    return queue_parameters, queue_parameters_err


@app.cell
def _(name_suffix, sample_df, sample_mode_selector, samples_editor, samples_table, selected_orders):
    sample_selection_content = shared.render_sample_selection_content(
        sample_df=sample_df,
        selected_orders=selected_orders,
        name_suffix=name_suffix,
        sample_mode_selector=sample_mode_selector,
        samples_table=samples_table,
        samples_editor=samples_editor,
        subject_label="group(s)",
    )
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
    plate_well_size = shared.make_plate_well_size()
    return (plate_well_size,)


@app.cell
def _():
    viz_subtab = shared.make_viz_subtab()
    return (viz_subtab,)


@app.cell
def _():
    plate_color_by, timeline_color_by = shared.make_viz_color_selectors()
    return plate_color_by, timeline_color_by


@app.cell
def _(config, plate_color_by, plate_well_size, queue_parameters, raw_queue_df):
    plate_layout_view = shared.render_plate_layout_view(
        config, raw_queue_df, queue_parameters, plate_color_by, plate_well_size
    )
    return (plate_layout_view,)


@app.cell
def _(raw_queue_df, timeline_color_by):
    timeline_view = shared.render_timeline_view(raw_queue_df, timeline_color_by)
    return (timeline_view,)


@app.cell
def _(plate_layout_view, timeline_view, viz_subtab):
    visualizations_content = shared.render_visualizations_content(viz_subtab, plate_layout_view, timeline_view)
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
    valid_combinations_content = shared.render_valid_combinations_content(
        master_table,
        tech_area=tech_area_field.value,
        instrument=instrument_field.value,
        sampler=sampler_field.value,
        qc_layout=qc_layout_field.value,
        pattern=pattern_field.value,
        queue_type=queue_type_field.value,
        plate_layout=plate_layout_field.value,
    )
    return (valid_combinations_content,)


@app.cell
def _():
    tab_selector = shared.make_tab_selector()
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
    shared.render_tabbed_layout(
        tab_selector,
        edit_samples_content=sample_selection_content,
        queue_preview_content=queue_preview_content,
        visualizations_content=visualizations_content,
        parameters_content=parameters_content,
        valid_combinations_content=valid_combinations_content,
    )
    return


if __name__ == "__main__":
    app.run()
