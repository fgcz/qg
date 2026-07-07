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
    from loguru import logger

    from qg.logging_setup import configure_logging

    configure_logging()

    from qg.apps import queue_app_shared as shared
    from qg.apps.integrations import bfabric_samples, bfabric_workunit
    from qg.apps.integrations.bfabric_context import SessionError, resolve_app_session
    from qg.cli.find_projects import ContainerCache
    from qg.config_models.loader import qg_configuration


@app.cell
def _():
    try:
        _session = resolve_app_session(
            mo.app_meta().request,
            allow_unauthenticated=os.environ.get("QG_ALLOW_UNAUTHENTICATED") == "1",
        )
    except SessionError as exc:
        mo.stop(True, mo.callout(mo.md(f"**{exc.message}**"), kind="danger"))

    bfabric = _session.bfabric
    client = _session.client
    feeder_uploader = _session.feeder_uploader
    bfabric_application_id = _session.application_id
    is_employee = _session.is_employee
    entity_class = _session.entity_class
    entity_id = _session.entity_id
    bfabric_base_url = _session.base_url
    banner_message = _session.banner_message
    return (
        banner_message,
        bfabric,
        bfabric_application_id,
        bfabric_base_url,
        client,
        entity_class,
        entity_id,
        feeder_uploader,
        is_employee,
    )


@app.cell
def _():
    app_version = importlib.metadata.version("qg")
    return (app_version,)


@app.cell
def _():
    # Debug/audit dump directory (shared with artifacts and loguru logs)
    DEBUG_DUMP_DIR = Path.home() / ".qg" / "logs"
    # Use --all-projects flag to load all containers (no status filter)
    _args = mo.cli_args()
    USE_ALL_PROJECTS = "all-projects" in _args
    USE_CONTAINER_TYPE = "container-type" in _args
    return DEBUG_DUMP_DIR, USE_ALL_PROJECTS, USE_CONTAINER_TYPE


@app.cell
def _(USE_CONTAINER_TYPE, client, entity_class, entity_id):
    # Fetch the B-Fabric order the app was launched from so users get it pre-loaded even if
    # it predates the cached container list. One query by ID, no full refresh; empty frame if N/A.
    # Status-agnostic by design (fetch_container_row ignores active_only), so no active-status flag here.
    launching_order_row = pl.DataFrame()
    if entity_id is not None and entity_class in {"Container", "Order", "Project"}:
        launching_order_row = ContainerCache(client).fetch_container_row(entity_id, with_type=USE_CONTAINER_TYPE)
    return (launching_order_row,)


@app.cell
def _():
    # Load configs via qg_configuration() — from CLI arg or default path
    _args = mo.cli_args()
    _config_dir = Path(_args["config-dir"]) if "config-dir" in _args else None
    config = qg_configuration(_config_dir)
    logger.info("Queue app started | config_dir={}", _config_dir or "default")
    return (config,)


@app.cell
def _(config):
    # Master table with all valid combinations - this is the "carbon copy"
    master_table = config.to_overview_table()
    return (master_table,)


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
def _(launching_order_row, master_table):
    # Tech area dropdown - options from master table
    # Default to Proteomics, or to the launching order's technology when launched from one.
    _options = sorted(master_table["tech_area"].unique().to_list())
    _default = _options[0] if _options else None

    _area = launching_order_row["Area"][0] if not launching_order_row.is_empty() else None
    # B-Fabric "Metabolomics/Biophysics" is ambiguous (Metabolomics vs Lipidomics); default to Metabolomics.
    _launch_tech_area = {"Proteomics": "Proteomics", "Metabolomics/Biophysics": "Metabolomics"}.get(_area)
    if _launch_tech_area in _options:
        _default = _launch_tech_area

    tech_area_field = mo.ui.dropdown(
        options=_options,
        value=_default,
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
        incompatible_subject="this order's samples",
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
        no_source_message="Please select an order",
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
def _(client, config, is_employee, tech_area_field):
    if is_employee:
        _service = config.tech_area_defaults.get_default_user(tech_area_field.value).strip()
        _initial = _service or client.auth.login
    else:
        _initial = client.auth.login
    user_field = mo.ui.text(
        value=_initial,
        label="User",
        disabled=not is_employee,
    )
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
    sampler_field,
    start_position_field,
    start_tray_field,
    tech_area_field,
    user_field,
    validation_status,
):
    mo.sidebar(
        shared.render_sidebar_body(
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
        ),
        footer=mo.md(f"Version: {app_version}"),
        width="28rem",
    )
    return


@app.cell
def _(is_employee):
    # Refresh button + state exist only for employees; non-employees have no container list.
    if is_employee:
        get_refresh, set_refresh = mo.state(0)
        refresh_projects_button = mo.ui.run_button(label="Refresh Projects")
    else:
        get_refresh = None
        set_refresh = None
        refresh_projects_button = None
    return get_refresh, refresh_projects_button, set_refresh


@app.cell
def _(USE_ALL_PROJECTS, client, get_refresh, refresh_projects_button, set_refresh):
    if refresh_projects_button is not None and refresh_projects_button.value:
        ContainerCache(client, active_only=not USE_ALL_PROJECTS).write_containers()
        set_refresh(get_refresh() + 1)
    return


@app.cell
def _(USE_ALL_PROJECTS, USE_CONTAINER_TYPE, client, get_refresh, is_employee, launching_order_row):
    # Load merged container cache for employees only; non-employees never browse containers.
    if not is_employee:
        projects_df = pl.DataFrame()
    else:
        _ = get_refresh()  # re-read CSV when refresh button is clicked
        # Empty (typed) frame when no cache exists yet — the "Refresh Projects" button populates it.
        projects_df = ContainerCache(client, active_only=not USE_ALL_PROJECTS).read_containers(
            with_type=USE_CONTAINER_TYPE
        )
        # Surface the launching order even if it predates the cache (in-memory only).
        if (
            not launching_order_row.is_empty()
            and launching_order_row["Container ID"][0] not in projects_df["Container ID"]
        ):
            projects_df = pl.concat([launching_order_row, projects_df], how="diagonal_relaxed").sort(
                "Container ID", descending=True
            )
    return (projects_df,)


@app.cell
def _(config, entity_id, is_employee, projects_df, tech_area_field):
    # Employees browse the container cache; non-employees get only a heading from the fixed container.
    if is_employee:
        _allowed_areas = config.tech_area_defaults.get_bfabric_areas(tech_area_field.value)
        if _allowed_areas:
            _expr = pl.col("Area").is_in(_allowed_areas)
            if entity_id is not None:
                _expr = _expr | (pl.col("Container ID") == entity_id)  # never filter out the launching order
            _filtered = projects_df.filter(_expr)
        else:
            _filtered = projects_df
        # Pre-select the launching order so its samples load immediately.
        _initial = None
        if entity_id is not None:
            _hit = _filtered.with_row_index().filter(pl.col("Container ID") == entity_id)
            if not _hit.is_empty():
                _initial = [int(_hit["index"][0])]
        project_table = mo.ui.table(
            data=_filtered,
            selection="multi",
            initial_selection=_initial,
            label="Select orders (multi-select)",
            show_download=False,
            page_size=5,
        )
    else:
        project_table = None
    return (project_table,)


@app.cell
def _(entity_id, is_employee, project_table):
    if is_employee:
        if project_table.value.is_empty():
            selected_orders = []
        else:
            selected_orders = [(int(row["Container ID"]), row["Area"]) for row in project_table.value.to_dicts()]
    else:
        selected_orders = [(entity_id, None)]
    return (selected_orders,)


@app.cell
def _(bfabric, selected_orders):
    # For each selected container: keep its plate entities (needed by plates_select)
    # and classify its sample composition (plates / vials / both).
    all_plates, container_has_plates, container_has_vials = bfabric_samples.container_composition(
        bfabric, selected_orders
    )
    return all_plates, container_has_plates, container_has_vials


@app.cell
def _(queue_type_field):
    # Derive container_type for BfabricHelper.get_samples() from queue type dropdown
    _type_map = {"Vial": "Vials", "Plate": "Plates"}
    container_type = _type_map.get(queue_type_field.value, "Vials")
    return (container_type,)


@app.cell
def _(all_plates, selected_orders):
    # For now, plates_select only works with single order (first one)
    # TODO: Support per-order plate selection for multi-order queues
    if selected_orders and all_plates:
        _first_container = selected_orders[0][0]
        _first_plates = all_plates.get(_first_container, {})
        _plate_ids = sorted(_plate.id for _plate in _first_plates.values())
        plates_select = mo.ui.multiselect(_plate_ids, label="Plates (first order)")
    else:
        plates_select = mo.ui.multiselect([], label="Plates")
    return (plates_select,)


@app.cell
def _(
    DEBUG_DUMP_DIR,
    bfabric,
    container_type,
    plates_select,
    selected_orders,
):
    # Load samples from all selected orders (empty DataFrame if no orders selected)
    full_samples_df = bfabric_samples.load_samples(
        bfabric, selected_orders, container_type, plates_select.value or None, DEBUG_DUMP_DIR
    )
    return (full_samples_df,)


@app.cell
def _(full_samples_df, is_employee):
    # Non-employees have a fixed container: if it has no samples, halt with no partial data.
    if not is_employee and full_samples_df.is_empty():
        mo.stop(
            True,
            mo.callout(mo.md("**No samples found in this container.**"), kind="danger"),
        )
    return


@app.cell
def _():
    sample_mode_selector = shared.make_sample_mode_selector()
    return (sample_mode_selector,)


@app.cell
def _(tech_area_field):
    name_suffix = shared.make_name_suffix(tech_area_field.value)
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
    return queue_parameters, queue_parameters_err


@app.cell
def _(banner_message, entity_id, is_employee, project_table, refresh_projects_button):
    # Bind to a name and leave it as the cell's final expression so marimo displays the vstack;
    # a bare `mo.vstack(...)` inside if/else is not the last top-level expression of the cell.
    # The banner shares the top row with the refresh button to avoid wasted vertical space.
    _banner = mo.md(banner_message)
    if is_employee:
        _order_section = mo.vstack(
            [
                mo.hstack([_banner, refresh_projects_button], justify="space-between", align="center"),
                project_table,
            ]
        )
    else:
        _order_section = mo.vstack([mo.hstack([_banner], justify="start"), mo.md(f"## Order {entity_id}")])
    _order_section
    return


@app.cell
def _(container_type, is_employee, selected_orders):
    # Assign each branch to a cell-local and display it as the last unnested
    # expression — a bare ``mo.md(...)`` inside a branch is computed then discarded.
    if not is_employee:
        _banner = mo.md("")
    elif not selected_orders:
        _banner = mo.md("**Select orders from the table above**")
    else:
        _container_ids = [o[0] for o in selected_orders]
        _ids_str = ", ".join(str(c) for c in _container_ids)
        _banner = mo.md(f"**Selected:** {len(selected_orders)} order(s): {_ids_str} ({container_type})")
    _banner
    return


@app.cell
def _(full_samples_df, is_employee, selected_orders):
    # Employees pick orders from the project table; warn (don't halt) when the
    # selected order(s) contain no samples so they can choose a different order.
    # The ``selected_orders`` truthiness guard prevents firing on the initial
    # no-selection load. Non-employees use the danger/halt path at line ~970.
    if is_employee and selected_orders and full_samples_df.is_empty():
        _empty_order_warning = mo.callout(mo.md("**No samples found in the selected order(s).**"), kind="warn")
    else:
        _empty_order_warning = mo.md("")
    _empty_order_warning
    return


@app.cell
def _(all_plates, plates_select, selected_orders):
    _has_plates = any(all_plates.get(o[0]) for o in selected_orders) if selected_orders else False
    plates_select if _has_plates else mo.md("")
    return


@app.cell
def _(name_suffix, sample_df, sample_mode_selector, samples_editor, samples_table, selected_orders):
    sample_selection_content = shared.render_sample_selection_content(
        sample_df=sample_df,
        selected_orders=selected_orders,
        name_suffix=name_suffix,
        sample_mode_selector=sample_mode_selector,
        samples_table=samples_table,
        samples_editor=samples_editor,
        subject_label="order(s)",
    )
    return (sample_selection_content,)


@app.cell
def _(bfabric_base_url, config, queue_parameters, sample_df, selected_orders):
    # Build QueueInput once — shared by Parameters tab and queue generation.
    # The B-Fabric instance URL is stamped as provenance (QueueParameters.bfabric_instance).
    queue_input, queue_input_err = shared.build_queue_input(
        config,
        queue_parameters,
        sample_df,
        has_samples_source=bool(selected_orders),
        provenance_instance=bfabric_base_url,
    )
    return queue_input, queue_input_err


@app.cell
def _(
    queue_input,
    queue_input_err,
    queue_parameters,
    queue_parameters_err,
    sample_df,
):
    # Parameters tab content — uses shared queue_input
    _output = None
    _download_button = None
    if queue_input is not None:
        _output = queue_input.model_dump(mode="json")
        _download_button = shared.params_download_button(
            queue_input, shared.params_json_filename(queue_parameters, sample_df)
        )

    _err_msg = queue_input_err or (str(queue_parameters_err) if queue_parameters_err else "Select orders")
    parameters_content = mo.vstack(
        [
            _download_button,
            mo.callout(_output, kind="info") if _output else mo.callout(_err_msg, kind="danger"),
        ]
    )
    return (parameters_content,)


@app.cell
def _(
    app_version,
    bfabric_application_id,
    queue_input,
    queue_output_filename,
    queue_output_str,
    target_container_id_field,
):
    def gather_workunit_parameters() -> bfabric_workunit.CreateWorkunitParams:
        # Upload is only reachable once a queue is generated, which implies a
        # selected order (hence a non-None container field). See the upload cell.
        assert queue_input is not None
        assert target_container_id_field is not None
        return bfabric_workunit.gather_workunit_parameters(
            queue_input,
            app_version=app_version,
            application_id=bfabric_application_id,
            target_container_id=target_container_id_field.value,
            queue_output_filename=queue_output_filename,
            queue_output_str=queue_output_str,
        )

    return (gather_workunit_parameters,)


@app.cell
def _(feeder_uploader, gather_workunit_parameters):
    def upload_workunit() -> str:
        params = gather_workunit_parameters()
        return feeder_uploader.upload(params)

    return (upload_workunit,)


@app.cell
def _(config, queue_input, queue_parameters):
    # Generate the queue exactly once so preview and download are identical
    # (randomization is non-deterministic per build).
    _result = shared.generate_queue(config, queue_input, queue_parameters)
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
def _(selected_orders):
    _container_ids = sorted((str(o[0]) for o in selected_orders), reverse=True) if selected_orders else []
    if _container_ids:
        target_container_id_field = mo.ui.dropdown(
            options=_container_ids,
            value=_container_ids[0],
            allow_select_none=False,
        )
    else:
        target_container_id_field = None
    return (target_container_id_field,)


@app.cell
def _(output_file_extension, queue_parameters, selected_orders):
    _ids = [o[0] for o in selected_orders] if selected_orders else []
    queue_output_filename = shared.queue_output_filename(queue_parameters, _ids, output_file_extension)
    return (queue_output_filename,)


@app.cell
def _():
    upload_run_button = mo.ui.run_button(label="Upload to B-Fabric")
    return (upload_run_button,)


@app.cell
def _(upload_run_button, upload_workunit):
    if upload_run_button.value:
        upload_result = upload_workunit()
    else:
        upload_result = None
    return (upload_result,)


@app.cell
def _(
    formatted_ticket_toggle,
    generated_queue_df,
    generation_error,
    queue_output_filename,
    queue_output_str,
    queue_parameters,
    raw_queue_df,
    target_container_id_field,
    upload_result,
    upload_run_button,
):
    # Queue Preview tab content
    if generation_error:
        queue_preview_content = mo.callout(mo.md(f"**Generation Error:** {generation_error}"), kind="danger")
    elif generated_queue_df is not None and queue_output_str is not None:
        _display_df = generated_queue_df if formatted_ticket_toggle.value else raw_queue_df

        _download_button = shared.queue_download_button(
            queue_output_str, queue_output_filename, disabled=upload_result is None
        )

        _rand_label = (
            f" | randomization: {queue_parameters.randomization}" if queue_parameters.randomization != "no" else ""
        )
        _upload_items = [upload_run_button]
        if target_container_id_field is not None:
            _upload_items += [mo.md("to"), target_container_id_field]
        _upload_items.append(_download_button)
        _upload_result_msg = mo.md(upload_result) if upload_result is not None else mo.md("")

        queue_preview_content = mo.vstack(
            [
                mo.hstack(_upload_items, justify="start", gap=1, align="center"),
                _upload_result_msg,
                mo.md(f"**{len(_display_df)} rows{_rand_label}** {formatted_ticket_toggle}"),
                mo.ui.table(_display_df, show_column_summaries=False, show_download=False, selection=None),
            ]
        )
    else:
        queue_preview_content = mo.md("_Select a project and configure parameters to preview queue_")
    return (queue_preview_content,)


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
