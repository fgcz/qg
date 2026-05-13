import marimo

__generated_with = "0.19.4"
app = marimo.App(width="full", sql_output="polars")

with app.setup:
    import base64
    import importlib.metadata
    import json
    import os
    from datetime import date
    from pathlib import Path

    import marimo as mo
    import polars as pl
    import pydantic
    import yaml
    from bfabric_rest_proxy.feeder_operations.create_workunit import CreateWorkunitParams
    from loguru import logger

    from qg.artifacts import build_timestamp
    from qg.logging_setup import configure_logging

    configure_logging()

    from qg.bfabric_utils import SessionError, resolve_app_session
    from qg.cli.find_projects import ContainerCache, get_cache_dir
    from qg.config_models.loader import qg_configuration
    from qg.config_models.structure import SamplesConfig
    from qg.generator import QueueGenerator, format_table, write_queue
    from qg.params_models import QueueParameters
    from qg.queue_builder import QueueBuilder


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
    mo.md(_session.banner_message)
    return (
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
def _(client):
    # Per-instance B-Fabric cache directory for cached project data
    BFABRIC_CACHE_DIR = get_cache_dir(client)
    # Debug/audit dump directory (shared with artifacts and loguru logs)
    DEBUG_DUMP_DIR = Path.home() / ".qg" / "logs"
    # Use --all-projects flag to load all containers (no status filter)
    _args = mo.cli_args()
    USE_ALL_PROJECTS = "all-projects" in _args
    USE_CONTAINER_TYPE = "container-type" in _args
    return BFABRIC_CACHE_DIR, DEBUG_DUMP_DIR, USE_ALL_PROJECTS, USE_CONTAINER_TYPE


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
    # Tech area dropdown - options from master table
    # Default to Proteomics, user can change
    _options = sorted(master_table["tech_area"].unique().to_list())
    _default = "Proteomics" if "Proteomics" in _options else (_options[0] if _options else None)

    tech_area_field = mo.ui.dropdown(
        options=_options,
        value=_default,
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
    # Sample Type dropdown: intersection of what the order has and what the sampler supports.
    sample_type_warning = None
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
            sample_type_warning = mo.callout(
                mo.md(f"Sampler **{sampler_field.value}** is incompatible with this order's samples."),
                kind="warn",
            )
    else:
        _options = []
        _default = None

    queue_type_field = mo.ui.dropdown(
        options=_options,
        value=_default,
        label="Sample Type",
    )
    return queue_type_field, sample_type_warning


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
def _(config, queue_type_field, sampler_field):
    # Start tray dropdown — only for Vial mode (select which tray to start on)
    if queue_type_field.value == "Vial" and sampler_field.value:
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
def _(filtered_table, sampler_field):
    # Output format is derived (determined by instrument+sampler combination)
    if sampler_field.value and not filtered_table.is_empty():
        _formats = filtered_table["output_format"].unique().to_list()
        output_format_value = _formats[0] if _formats else "xcalibur"
    else:
        output_format_value = "xcalibur"
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
        # Until an order is picked nothing downstream can resolve; show a single
        # actionable hint instead of a cascade of "X not selected" entries.
        validation_errors.append("Please select an order")
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

    # Check plate count vs sampler tray capacity (only for plate mode)
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

    # Only show validation status when there are errors
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

        # Collect sample_ids from pattern + "default" for user samples
        sample_ids: set[str] = {SamplesConfig.DEFAULT_SAMPLE_ID}
        if pattern_field.value:
            _pattern = config.queue_patterns.get_pattern(tech_area_field.value, pattern_field.value)
            sample_ids |= _pattern.get_all_sample_ids()

        # Intersect method names across all sample types
        sets = [methods_for_instr.get_method_names(sid, polarity) for sid in sample_ids]
        # Only include sample types that have at least one method row;
        # sample types with no rows in the method file are not constrained
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
def _(tech_area_field):
    # Polarity selection - proteomics uses single polarity (pos), metabolomics/lipidomics use both
    _needs_both_polarities = tech_area_field.value in ("Metabolomics", "Lipidomics")
    polarity_group = mo.ui.batch(
        mo.md("**Polarity:** {pos} pos {neg} neg"),
        {"pos": mo.ui.checkbox(value=True), "neg": mo.ui.checkbox(value=_needs_both_polarities)},
    )
    return (polarity_group,)


@app.cell
def _():
    randomization_field = mo.ui.dropdown(options=["no", "random", "blocked"], value="no", label="Randomization")
    return (randomization_field,)


@app.cell
def _():
    inj_vol_field = mo.ui.text(value="1", label="Inj Vol (µl)")
    return (inj_vol_field,)


@app.cell
def _(client, is_employee):
    if is_employee:
        user_field = mo.ui.text(value="analytic", label="User")
    else:
        user_field = mo.ui.text(value=client.auth.login, label="User", disabled=True)
    return (user_field,)


@app.cell
def _():
    date_field = mo.ui.date(value=date.today(), label="Date")
    return (date_field,)


@app.cell
def _(
    config,
    plate_layout_field,
    qc_layout_field,
    sampler_field,
    tech_area_field,
):
    # Build a small summary table of the selected QC layout's sample positions
    qc_layout_preview = None
    if tech_area_field.value and sampler_field.value and plate_layout_field.value and qc_layout_field.value:
        _sampler = config.samplers.get_sampler(sampler_field.value)
        _samples = config.get_qc_samples(
            tech_area_field.value,
            qc_layout_field.value,
            plate_layout_field.value,
            _sampler,
        )
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
                qc_layout_preview = pl.DataFrame(_rows)
    return (qc_layout_preview,)


@app.cell
def _(
    app_version,
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
    randomization_field,
    sample_type_warning,
    sampler_field,
    start_position_field,
    start_tray_field,
    tech_area_field,
    user_field,
    validation_status,
):
    _queue_items = [qc_layout_field, pattern_field, polarity_group]
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
        *([] if sample_type_warning is None else [sample_type_warning]),
        plate_layout_field,
        *([] if start_tray_field is None else [mo.hstack([start_tray_field, start_position_field], justify="start")]),
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
    # Add validation errors at bottom only if present
    if validation_status is not None:
        _sidebar_items.append(validation_status)

    # Add QC layout preview table at the bottom
    if qc_layout_preview is not None:
        _sidebar_items.append(mo.md(f"**QC Positions** _{qc_layout_field.value}_"))
        _sidebar_items.append(qc_layout_preview)

    mo.sidebar(mo.vstack(_sidebar_items), footer=mo.md(f"Version: {app_version}"), width="22rem")
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
def _(BFABRIC_CACHE_DIR, USE_ALL_PROJECTS, USE_CONTAINER_TYPE, get_refresh, is_employee):
    # Load merged container cache for employees only; non-employees never browse containers.
    if not is_employee:
        projects_df = pl.DataFrame()
    else:
        _ = get_refresh()  # re-read CSV when refresh button is clicked
        _suffix = "_all" if USE_ALL_PROJECTS else ""
        _name = "bfabric_container_type" if USE_CONTAINER_TYPE else "bfabric_container"
        projects_df = pl.read_csv(BFABRIC_CACHE_DIR / f"{_name}{_suffix}.csv").sort("Container ID", descending=True)
    return (projects_df,)


@app.cell
def _(is_employee, projects_df, tech_area_field):
    # Employees browse the container cache; non-employees get only a heading from the fixed container.
    if is_employee:
        _area_map = {
            "Proteomics": ["Proteomics"],
            "Metabolomics": ["Metabolomics/Biophysics"],
            "Lipidomics": ["Metabolomics/Biophysics"],
        }
        _allowed_areas = _area_map.get(tech_area_field.value)
        _filtered = projects_df.filter(pl.col("Area").is_in(_allowed_areas)) if _allowed_areas else projects_df
        project_table = mo.ui.table(
            data=_filtered,
            selection="multi",
            label="Select orders (multi-select)",
            show_download=False,
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
    all_plates = {}
    container_has_plates: bool | None = None
    container_has_vials: bool | None = None
    if selected_orders:
        container_has_plates = False
        container_has_vials = False
        for _container_id, *_ in selected_orders:
            all_plates[_container_id] = bfabric.get_plates(_container_id)
            _comp = bfabric.get_container_composition(_container_id)
            container_has_plates = container_has_plates or _comp.has_plates
            container_has_vials = container_has_vials or _comp.has_vials
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
    all_samples_dfs = []

    if selected_orders:
        _ts = build_timestamp()
        for _container_id, *_ in selected_orders:
            # Only apply plate filter to first order (for now)
            _selected_plate_ids = (
                plates_select.value if _container_id == selected_orders[0][0] and plates_select.value else None
            )
            _df = bfabric.get_samples(
                _container_id, container_type, _selected_plate_ids, dump_dir=DEBUG_DUMP_DIR, filename_prefix=_ts
            )
            if not _df.is_empty():
                _df = _df.with_columns(pl.lit(_container_id).alias("container_id"))
                all_samples_dfs.append(_df)

    if all_samples_dfs:
        full_samples_df = pl.concat(all_samples_dfs, how="vertical_relaxed").sort(["container_id", "sample_id"])
    else:
        full_samples_df = pl.DataFrame()
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
    sample_mode_selector = mo.ui.radio(
        options=["Sample Selection", "Sample Editor"],
        value="Sample Selection",
        inline=True,
    )
    return (sample_mode_selector,)


@app.cell
def _(full_samples_df):
    if not full_samples_df.is_empty():
        _n = len(full_samples_df)
        samples_table = mo.ui.table(
            data=full_samples_df,
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
    queue_parameters_err = None
    try:
        _inj_vol = float(inj_vol_field.value) if inj_vol_field.value.strip() else None
        _qc_freq = int(qc_frequency_field.value) if qc_frequency_field.value.strip() else None
        _polarity = [p for p in ("pos", "neg") if polarity_group.value.get(p)]

        # Build method dict from per-polarity selections
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
            }
        )
    except pydantic.ValidationError as e:
        queue_parameters_err = e
        queue_parameters = None
    return queue_parameters, queue_parameters_err


@app.cell
def _(entity_id, is_employee, project_table, refresh_projects_button):
    # Bind to a name and leave it as the cell's final expression so marimo displays the vstack;
    # a bare `mo.vstack(...)` inside if/else is not the last top-level expression of the cell.
    if is_employee:
        _order_section = mo.vstack(
            [
                mo.hstack(
                    [mo.md("## Order Selection _(🔍 to search)_"), refresh_projects_button],
                    justify="space-between",
                    align="center",
                ),
                project_table,
            ]
        )
    else:
        _order_section = mo.md(f"## Order {entity_id}")
    _order_section
    return


@app.cell
def _(container_type, is_employee, selected_orders):
    if not is_employee:
        mo.md("")
    elif not selected_orders:
        mo.md("**Select orders from the table above**")
    else:
        _container_ids = [o[0] for o in selected_orders]
        _ids_str = ", ".join(str(c) for c in _container_ids)
        mo.md(f"**Selected:** {len(selected_orders)} order(s): {_ids_str} ({container_type})")
    return


@app.cell
def _(all_plates, plates_select, selected_orders):
    _has_plates = any(all_plates.get(o[0]) for o in selected_orders) if selected_orders else False
    plates_select if _has_plates else mo.md("")
    return


@app.cell
def _(sample_df, sample_mode_selector, samples_editor, samples_table, selected_orders):
    # Sample Selection tab content
    _order_count = len(selected_orders) if selected_orders else 0
    if sample_df is not None and not sample_df.is_empty():
        _summary = mo.md(f"**{len(sample_df)} samples from {_order_count} order(s)**")
    else:
        _summary = mo.md("**No samples loaded**")
    _panels = {
        "Sample Selection": samples_table if samples_table is not None else mo.md("_No samples loaded_"),
        "Sample Editor": samples_editor if samples_editor is not None else mo.md("_No samples loaded_"),
    }
    _panel_stack = [
        mo.md(f'<div style="display: {"block" if _name == sample_mode_selector.value else "none"}">{_widget}</div>')
        for _name, _widget in _panels.items()
    ]
    sample_selection_content = mo.vstack([_summary, sample_mode_selector, *_panel_stack])
    return (sample_selection_content,)


@app.cell
def _(bfabric_base_url, config, queue_parameters, sample_df, selected_orders):
    # Build QueueInput once — shared by Parameters tab and queue generation
    queue_input = None
    queue_input_err = None
    if queue_parameters and selected_orders and sample_df is not None:
        try:
            _builder = QueueBuilder(config).with_parameters(queue_parameters).with_bfabric_instance(bfabric_base_url)
            if not sample_df.is_empty():
                _builder = _builder.add_samples_from_dataframe(sample_df)
            queue_input = _builder.build()
        except ValueError as e:
            queue_input_err = str(e)
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

        # Create download button for JSON
        _json_data = json.dumps(_output, indent=2)
        _ids_str = "_".join(str(c) for c in sample_df["container_id"].unique().sort().to_list())
        _filename = f"{queue_parameters.tech_area}_{queue_parameters.sampler.replace('.', '_')}_c{_ids_str}_n{len(sample_df)}.json"
        _download_button = mo.download(
            data=_json_data.encode("utf-8"),
            filename=_filename,
            label="Download Params JSON",
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
    def gather_workunit_parameters() -> CreateWorkunitParams | None:
        if queue_input is None:
            return None

        parameters = queue_input.parameters.model_dump()
        for key in parameters:
            if isinstance(parameters[key], list | dict):
                parameters[key] = yaml.safe_dump(parameters[key], default_flow_style=True).strip()
            else:
                parameters[key] = str(parameters[key])

        _queue_params_filename = "parameters.json"
        _description = f"Queue configuration generated with qg version {app_version}."
        return CreateWorkunitParams(
            container_id=target_container_id_field.value,
            application_id=bfabric_application_id,
            workunit_name=queue_output_filename.split(".")[0],
            parameters=parameters,
            resources={
                queue_output_filename: base64.b64encode(queue_output_str.encode("utf8")),
                _queue_params_filename: base64.b64encode(queue_input.model_dump_json(indent=2).encode("utf8")),
            },
            links={},
            input_resource_ids=[],
            description=_description,
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
    # Generate queue from shared queue_input
    # IMPORTANT: build_rows() is called exactly ONCE to ensure the displayed queue
    # and the downloaded file are identical (randomization is non-deterministic).
    generated_queue_df = None
    raw_queue_df = None
    queue_output_str = None
    generation_error = None
    output_file_extension = ".csv"

    if queue_input is not None:
        try:
            _generator = QueueGenerator(config, queue_input)
            _queue_rows = _generator.build_rows()
            raw_queue_df = _queue_rows.to_table()
            generated_queue_df = format_table(_queue_rows, _generator.output_format, _generator._plate_layout)
            queue_output_str = write_queue(generated_queue_df, _generator.output_format)
            output_file_extension = _generator.file_extension
        except ValueError as e:
            # Config errors - provide user-friendly message
            logger.exception("Queue generation failed")
            _err_str = str(e)
            if "Not enough tray positions" in _err_str:
                _sampler = queue_parameters.sampler if queue_parameters else "sampler"
                generation_error = (
                    f"{_err_str}\n\n"
                    f"The {_sampler} sampler cannot hold this many plates simultaneously. "
                    f"Either select fewer plates, or use a sampler with more tray positions (e.g., Vanquish has 4 trays)."
                )
            else:
                generation_error = _err_str
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
    if queue_parameters is None:
        queue_output_filename = None
    else:
        _container_ids = [o[0] for o in selected_orders] if selected_orders else []
        _ids_str = "_".join(str(c) for c in _container_ids) if _container_ids else "queue"
        queue_output_filename = (
            f"{queue_parameters.date}_{queue_parameters.instrument}_{_ids_str}{output_file_extension}"
        )
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
def _(queue_output_str):
    def dl_button_callback() -> bytes:
        return queue_output_str.encode("utf-8")

    return (dl_button_callback,)


@app.cell
def _(
    dl_button_callback,
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

        _download_button = mo.download(
            data=dl_button_callback,
            filename=queue_output_filename,
            label="Download Queue File",
            disabled=upload_result is None,
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
    # Build match condition based on current selections
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

    # Create combined match expression
    _display_cols = [c for c in master_table.columns if c != "default_pattern"]
    if _match_conditions:
        _combined_match = pl.all_horizontal(*_match_conditions)
        # Add "✓" column for matching rows, sort matches to top
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
        options=["Queue Preview", "Sample Selection", "Parameters", "Valid Combinations"],
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
):
    _sections = {
        "Queue Preview": queue_preview_content,
        "Sample Selection": sample_selection_content,
        "Parameters": parameters_content,
        "Valid Combinations": valid_combinations_content,
    }
    # CSS display:none keeps all widgets in the DOM (data_editor preserves visual state)
    _panels = [
        mo.md(f'<div style="display: {"block" if _name == tab_selector.value else "none"}">{_content}</div>')
        for _name, _content in _sections.items()
    ]
    mo.vstack([tab_selector, *_panels])
    return


if __name__ == "__main__":
    app.run()
