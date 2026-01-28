import marimo

__generated_with = "0.19.4"
app = marimo.App(width="full", sql_output="polars")

with app.setup:
    import logging
    from datetime import date
    from pathlib import Path

    import marimo as mo
    import polars as pl
    import pydantic
    from bfabric import Bfabric

    logger = logging.getLogger(__name__)

    from qg.bfabric_utils import BfabricHelper
    from qg.config import qg_config
    from qg.generator import QueueGenerator
    from qg.params_models import QueueParameters
    from qg.queue_builder import QueueBuilder


@app.cell
def _():
    _request = mo.app_meta().request
    if _request.user and _request.user.is_authenticated and hasattr(_request.user, "get_bfabric_client"):
        client = _request.user.get_bfabric_client()
        _content = f"Authenticated user: {client.auth.login}"
    else:
        _content = "No user information in request. Using default configuration."
        client = Bfabric.connect()

    mo.md(_content)
    bfabric = BfabricHelper(client)
    return (bfabric,)


@app.cell
def _():
    # B-Fabric cache directory for cached project data
    BFABRIC_CACHE_DIR = Path(__file__).parent.parent.parent.parent / "bfabric_cache"
    return (BFABRIC_CACHE_DIR,)


@app.cell
def _():
    # Load configs via qg_config() - uses default path
    configs = qg_config()
    return (configs,)


@app.cell
def _(configs):
    # Get DataFrames for UI filtering from the loaded config bundle
    instruments_df = configs.instruments.to_table()
    instrument_patterns_df = configs.instrument_patterns.to_table()
    # Valid combinations filtered by QC layout availability
    valid_combinations_df = configs.get_valid_instruments_samplers()
    return instrument_patterns_df, instruments_df, valid_combinations_df


@app.cell
def _(BFABRIC_CACHE_DIR):
    # Load orders and projects data
    _orders_data = pl.read_csv(BFABRIC_CACHE_DIR / "bfabric_order.csv")
    _projects_data = pl.read_csv(BFABRIC_CACHE_DIR / "bfabric_project.csv")

    projects_df = pl.concat((_orders_data, _projects_data), how="diagonal_relaxed").sort(
        "Container ID", descending=True
    )
    return (projects_df,)


@app.cell
def _(projects_df, tech_area_field):
    _area_map = {
        "Proteomics": ["Proteomics"],
        "Metabolomics": ["Metabolomics/Biophysics"],
        "Lipidomics": ["Metabolomics/Biophysics"],
    }
    _allowed_areas = _area_map.get(tech_area_field.value, [])
    _filtered = projects_df.filter(pl.col("Area").is_in(_allowed_areas))
    project_table = mo.ui.table(
        data=_filtered,
        selection="multi",
        label="Select orders (multi-select)",
    )
    return (project_table,)


@app.cell
def _(project_table):
    if project_table.value.is_empty():
        selected_orders = []
        container_type = "Vials"
    else:
        selected_orders = [
            (int(row["Container ID"]), row["Area"], row["Type"]) for row in project_table.value.to_dicts()
        ]
        container_type = selected_orders[0][2]
    return container_type, selected_orders


# =============================================================================
# Sidebar: Instrument Configuration Fields
# =============================================================================


@app.cell
def _(instruments_df):
    _options = sorted(instruments_df["tech_area"].unique().to_list())
    tech_area_field = mo.ui.dropdown(
        options=_options, value="Proteomics" if "Proteomics" in _options else _options[0], label="tech_area"
    )
    return (tech_area_field,)


@app.cell
def _(instruments_df, tech_area_field):
    mo.stop(not tech_area_field.value)
    _options = (
        instruments_df.filter(pl.col("tech_area") == tech_area_field.value)["instrument"].unique().sort().to_list()
    )
    instrument_field = mo.ui.dropdown(
        options=_options,
        value=_options[0] if _options else None,
        label="Instrument",
    )
    return (instrument_field,)


@app.cell
def _(
    instrument_field,
    tech_area_field,
    valid_combinations_df,
):
    # Filter samplers by tech_area and instrument (simplified names from combinations.csv)
    mo.stop(not instrument_field.value or not tech_area_field.value)
    _options = (
        valid_combinations_df.filter(pl.col("tech_area") == tech_area_field.value)
        .filter(pl.col("instrument") == instrument_field.value)["sampler"]
        .unique()
        .sort()
        .to_list()
    )
    sampler_field = mo.ui.dropdown(
        options=_options,
        value=_options[0] if _options else None,
        label="Sampler",
    )
    return (sampler_field,)


@app.cell
def _(instrument_field, sampler_field, tech_area_field, valid_combinations_df):
    # Software is derived from valid_combinations_df
    mo.stop(not instrument_field.value or not sampler_field.value or not tech_area_field.value)
    _row = valid_combinations_df.filter(
        (pl.col("tech_area") == tech_area_field.value)
        & (pl.col("instrument") == instrument_field.value)
        & (pl.col("sampler") == sampler_field.value)
    )
    output_format_value = _row["output_format"][0] if len(_row) > 0 else "xcalibur"
    return (output_format_value,)


@app.cell
def _(instrument_field, instrument_patterns_df, tech_area_field):
    # Pattern dropdown filtered by tech_area + instrument
    mo.stop(not tech_area_field.value or not instrument_field.value)
    _patterns_df = instrument_patterns_df.filter(
        (pl.col("tech_area") == tech_area_field.value) & (pl.col("instrument") == instrument_field.value)
    ).sort("is_default", descending=True)
    _options = _patterns_df["queue_pattern"].to_list()
    pattern_field = mo.ui.dropdown(
        options=_options,
        value=_options[0] if _options else None,
        label="Pattern",
    )
    return (pattern_field,)


@app.cell
def _(configs, pattern_field, tech_area_field):
    # Get default QC frequency from selected pattern
    mo.stop(not tech_area_field.value or not pattern_field.value)
    _pattern = configs.queue_patterns.get_pattern(tech_area_field.value, pattern_field.value)
    default_qc_frequency = _pattern.run_QC_after_n_samples if _pattern else 16
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
def _(configs, instrument_field, tech_area_field):
    # Load available methods from config
    mo.stop(not tech_area_field.value or not instrument_field.value)
    methods_df = configs.methods.to_table(tech_area_field.value, instrument_field.value)
    return (methods_df,)


@app.cell
def _(methods_df):
    # Get unique method names for each polarity from default sample_type
    def _get_methods(df: pl.DataFrame, polarity: str | None) -> list[str]:
        if df.is_empty():
            return []
        filtered = df.filter(pl.col("sample_type") == "default")
        if polarity and "polarity" in filtered.columns:
            filtered = filtered.filter(pl.col("polarity") == polarity)
        return sorted(filtered["method_name"].unique().to_list())

    _has_polarity = not methods_df.is_empty() and "polarity" in methods_df.columns
    available_methods_pos = _get_methods(methods_df, "pos" if _has_polarity else None)
    available_methods_neg = _get_methods(methods_df, "neg" if _has_polarity else None)
    return available_methods_neg, available_methods_pos


@app.cell
def _(available_methods_pos, polarity_group):
    # Method dropdown for positive polarity (with None option)
    _show_pos = polarity_group.value.get("pos", False)
    if _show_pos:
        _options = [""] + available_methods_pos  # Empty string = no method
        method_field_pos = mo.ui.dropdown(
            options=_options,
            value=available_methods_pos[0] if available_methods_pos else "",
            label="Method (pos)",
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
            label="Method (neg)",
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
def _():
    user_field = mo.ui.text(value="analytic", label="User")
    return (user_field,)


@app.cell
def _():
    date_field = mo.ui.date(value=date.today(), label="Date")
    return (date_field,)


# =============================================================================
# Sidebar Layout
# =============================================================================


@app.cell
def _(
    date_field,
    inj_vol_field,
    instrument_field,
    method_field_neg,
    method_field_pos,
    output_format_value,
    pattern_field,
    polarity_group,
    qc_frequency_field,
    randomization_field,
    sampler_field,
    tech_area_field,
    user_field,
):
    _queue_items = [pattern_field, polarity_group]
    if method_field_pos is not None:
        _queue_items.append(method_field_pos)
    if method_field_neg is not None:
        _queue_items.append(method_field_neg)

    _sidebar_items = [
        mo.md("# Queue Generator"),
        mo.md("### Instrument"),
        tech_area_field,
        instrument_field,
        sampler_field,
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

    mo.sidebar(mo.vstack(_sidebar_items))
    return


# =============================================================================
# Sample Loading
# =============================================================================


@app.cell
def _(bfabric, selected_orders):
    # Load plates for all selected orders (only for plate-type containers)
    all_plates = {}
    for _container_id, *_ in selected_orders:
        all_plates[_container_id] = bfabric.get_plates(_container_id)
    return (all_plates,)


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
def _(BFABRIC_CACHE_DIR, bfabric, container_type, plates_select, selected_orders):
    # Load samples from all selected orders
    mo.stop(not selected_orders)
    all_samples_dfs = []

    for _container_id, *_ in selected_orders:
        # Only apply plate filter to first order (for now)
        _selected_plate_ids = (
            plates_select.value if _container_id == selected_orders[0][0] and plates_select.value else None
        )
        _df = bfabric.get_samples(
            _container_id, container_type, _selected_plate_ids, dump_dir=BFABRIC_CACHE_DIR / "debug"
        )
        if not _df.is_empty():
            _df = _df.with_columns(pl.lit(_container_id).alias("container_id"))
            all_samples_dfs.append(_df)

    if all_samples_dfs:
        full_samples_df = pl.concat(all_samples_dfs, how="vertical_relaxed").sort(["container_id", "sample_id"])
    else:
        full_samples_df = pl.DataFrame()
    mo.stop(full_samples_df.is_empty(), mo.md("**No samples found**"))
    return (full_samples_df,)


@app.cell
def _():
    subset_samples_toggle = mo.ui.checkbox(False, label="Subset samples")
    return (subset_samples_toggle,)


@app.cell
def _(full_samples_df, subset_samples_toggle):
    if subset_samples_toggle.value:
        subset_samples_select = mo.ui.table(data=full_samples_df, freeze_columns_left=["sample_name"])
    else:
        subset_samples_select = None
    return (subset_samples_select,)


@app.cell
def _(full_samples_df, subset_samples_select):
    if subset_samples_select is None:
        sample_df = full_samples_df
    else:
        sample_df = subset_samples_select.value
    return (sample_df,)


# =============================================================================
# Queue Parameters
# =============================================================================


@app.cell
def _(
    container_type,
    date_field,
    inj_vol_field,
    instrument_field,
    method_field_neg,
    method_field_pos,
    output_format_value,
    pattern_field,
    polarity_group,
    qc_frequency_field,
    randomization_field,
    sampler_field,
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

        # Derive layout_mode from container_type (used by QueueBuilder, not QueueParameters)
        layout_mode = "vial" if container_type == "Vials" else "plate"

        queue_parameters = QueueParameters.model_validate(
            {
                "tech_area": tech_area_field.value,
                "instrument": instrument_field.value,
                "sampler": sampler_field.value,
                "output_format": output_format_value,
                "queue_pattern": pattern_field.value,
                "polarity": _polarity,
                "date": date_field.value.strftime("%Y%m%d"),
                "user": user_field.value.strip(),
                "method": method_dict,
                "randomization": randomization_field.value,
                "inj_vol_override": _inj_vol,
                "qc_frequency_override": _qc_freq,
            }
        )
    except pydantic.ValidationError as e:
        queue_parameters_err = e
        queue_parameters = None
        layout_mode = None
    return layout_mode, queue_parameters, queue_parameters_err


# =============================================================================
# Main Content Layout
# =============================================================================


@app.cell
def _(project_table):
    mo.vstack(
        [
            mo.md("## Order Selection"),
            project_table,
        ]
    )
    return


@app.cell
def _(container_type, selected_orders):
    if not selected_orders:
        mo.md("**Select orders from the table above**")
    else:
        _container_ids = [o[0] for o in selected_orders]
        _ids_str = ", ".join(str(c) for c in _container_ids)
        mo.md(f"**Selected:** {len(selected_orders)} order(s): {_ids_str} ({container_type})")
    return


@app.cell
def _(all_plates, plates_select, selected_orders):
    _has_plates = any(all_plates.get(o[0]) for o in selected_orders) if selected_orders else False
    mo.output.replace(plates_select if _has_plates else mo.md(""))
    return


@app.cell
def _(
    sample_df,
    selected_orders,
    subset_samples_select,
    subset_samples_toggle,
):
    # Sample Selection tab content
    _order_count = len(selected_orders) if selected_orders else 0
    sample_selection_content = mo.vstack(
        [
            mo.hstack(
                [
                    subset_samples_toggle,
                    mo.md(f"**{len(sample_df)} samples from {_order_count} order(s)**"),
                ],
                justify="start",
            ),
            subset_samples_select if subset_samples_select is not None else sample_df,
        ]
    )
    return (sample_selection_content,)


@app.cell
def _(
    configs,
    layout_mode,
    queue_parameters,
    queue_parameters_err,
    sample_df,
    selected_orders,
):
    # Build full QueueInput for display (Parameters tab content)
    _output = None
    _download_button = None
    if (
        queue_parameters
        and layout_mode
        and selected_orders
        and sample_df is not None
        and "container_id" in sample_df.columns
    ):
        try:
            _queue_input = (
                QueueBuilder(configs)
                .with_parameters(queue_parameters, layout_mode)
                .add_samples_from_dataframe(sample_df)
                .build()
            )
            _output = _queue_input.model_dump(mode="json")

            # Create download button for JSON
            import json

            _json_data = json.dumps(_output, indent=2)
            _ids_str = "_".join(str(c) for c in sample_df["container_id"].unique().sort().to_list())
            _filename = f"{queue_parameters.tech_area}_{queue_parameters.sampler.replace('.', '_')}_c{_ids_str}_n{len(sample_df)}.json"
            _download_button = mo.download(
                data=_json_data.encode("utf-8"),
                filename=_filename,
                label="Download Params JSON",
            )
        except ValueError:
            pass  # Builder validation failed, show error state

    parameters_content = mo.vstack(
        [
            _download_button if _download_button else None,
            mo.callout(_output, kind="info")
            if _output
            else mo.callout(str(queue_parameters_err) if queue_parameters_err else "Select orders", kind="danger"),
        ]
    )
    return (parameters_content,)


@app.cell
def _(configs, layout_mode, queue_parameters, sample_df, selected_orders):
    # Generate queue from current parameters (multi-order support)
    generated_queue_df = None
    raw_queue_df = None
    generation_error = None

    if queue_parameters and layout_mode and selected_orders and sample_df is not None and not sample_df.is_empty():
        try:
            _queue_input = (
                QueueBuilder(configs)
                .with_parameters(queue_parameters, layout_mode)
                .add_samples_from_dataframe(sample_df)
                .build()
            )
            _generator = QueueGenerator(configs, _queue_input, layout_mode)
            generated_queue_df = _generator.generate()
            raw_queue_df = _generator.build_rows().to_table()
        except ValueError as e:
            # Config errors (missing QC layout, etc.) - log and display to user
            logger.exception("Queue generation failed")
            generation_error = str(e)
    return generated_queue_df, generation_error, raw_queue_df


@app.cell
def _():
    formatted_ticket_toggle = mo.ui.checkbox(True, label="Formatted")
    return (formatted_ticket_toggle,)


@app.cell
def _(
    formatted_ticket_toggle,
    generated_queue_df,
    generation_error,
    queue_parameters,
    raw_queue_df,
    selected_orders,
):
    # Queue Preview tab content
    if generation_error:
        queue_preview_content = mo.callout(mo.md(f"**Generation Error:** {generation_error}"), kind="danger")
    elif generated_queue_df is not None:
        _display_df = generated_queue_df if formatted_ticket_toggle.value else raw_queue_df

        # Create download button with CSV data
        _csv_data = generated_queue_df.write_csv()
        _container_ids = [o[0] for o in selected_orders] if selected_orders else []
        _ids_str = "_".join(str(c) for c in _container_ids) if _container_ids else "queue"
        _filename = f"{queue_parameters.date}_{queue_parameters.instrument}_{_ids_str}.csv"
        _download_button = mo.download(
            data=_csv_data.encode("utf-8"),
            filename=_filename,
            label="Download CSV",
        )

        queue_preview_content = mo.vstack(
            [
                mo.hstack([formatted_ticket_toggle, _download_button], justify="start", gap=1),
                mo.md(f"**{len(_display_df)} rows**"),
                _display_df,
            ]
        )
    else:
        queue_preview_content = mo.md("_Select a project and configure parameters to preview queue_")
    return (queue_preview_content,)


@app.cell
def _(valid_combinations_df):
    # Valid Combinations tab content - shows all valid (tech_area, instrument, sampler) combos
    valid_combinations_content = mo.vstack(
        [
            mo.md(f"**{len(valid_combinations_df)} valid combinations** (filtered by QC layout availability)"),
            valid_combinations_df,
        ]
    )
    return (valid_combinations_content,)


@app.cell
def _(
    parameters_content,
    queue_preview_content,
    sample_selection_content,
    valid_combinations_content,
):
    # Tabbed interface for the right panel
    right_panel_tabs = mo.ui.tabs(
        {
            "Queue Preview": queue_preview_content,
            "Sample Selection": sample_selection_content,
            "Parameters": parameters_content,
            "Valid Combinations": valid_combinations_content,
        }
    )
    right_panel_tabs
    return


if __name__ == "__main__":
    app.run()
