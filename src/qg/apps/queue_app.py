import marimo

__generated_with = "0.18.4"
app = marimo.App(width="full", sql_output="polars")

with app.setup:
    import marimo as mo
    import polars as pl
    import pydantic
    from bfabric import Bfabric
    import json
    from pathlib import Path
    from datetime import date

    from qg.bfabric_utils import load_orders_from_cache, load_plates_for_container, load_samples_for_container, normalize_samples_df
    from qg.config import load_all_configs
    from qg.config_models import requires_polarity
    from qg.params_models import QueueInput, QueueParameters, SampleGroup, samples_from_dataframe
    from qg.params_simulator import write_params
    from qg.builder import QueueGeneratorBuilder


@app.cell
def _():
    client = Bfabric.connect(config_file_env="TEST")
    return (client,)


@app.cell
def _():
    # Config directory is qg_configs/ relative to the project root
    CONFIG_DIR = Path(__file__).parent.parent.parent.parent / "qg_configs"
    return (CONFIG_DIR,)


@app.cell
def _():
    # B-Fabric cache directory for cached project data
    BFABRIC_CACHE_DIR = Path(__file__).parent.parent.parent.parent / "bfabric_cache"
    return (BFABRIC_CACHE_DIR,)


@app.cell
def _(CONFIG_DIR):
    # Load all configs using the qg package
    configs = load_all_configs(CONFIG_DIR)
    return (configs,)


@app.cell
def _(CONFIG_DIR):
    # Also load raw DataFrames for UI filtering (instruments, combinations, patterns)
    instruments_df = pl.read_csv(CONFIG_DIR / "instruments.csv")
    combinations_df = pl.read_csv(CONFIG_DIR / "combinations.csv")
    instrument_patterns_df = pl.read_csv(CONFIG_DIR / "instrument_patterns.csv")
    return combinations_df, instrument_patterns_df, instruments_df


# =============================================================================
# Project Data Loading
# =============================================================================


@app.cell
def _(BFABRIC_CACHE_DIR):
    projects_df = load_orders_from_cache(BFABRIC_CACHE_DIR)
    return (projects_df,)


@app.cell
def _(projects_df):
    project_table = mo.ui.table(
        data=projects_df,
        selection="single",
        label="Select a project",
    )
    return (project_table,)


@app.cell
def _(project_table):
    # Provide defaults so sidebar shows immediately, update when project selected
    if project_table.value.is_empty():
        container_id = None
        selected_area = "Proteomics"  # Default technology
        container_type = "Vials"  # Default container type
    else:
        container_id = int(project_table.value["Container ID"][0])
        selected_area = project_table.value["Area"][0]
        container_type = project_table.value["Type"][0]  # "Vials" or "Plates"
    return container_id, container_type, selected_area


# =============================================================================
# Sidebar: Instrument Configuration Fields
# =============================================================================


@app.cell
def _(instruments_df, selected_area):
    _options = sorted(instruments_df["technology"].unique().to_list())
    technology_field = mo.ui.dropdown(
        options=_options,
        value=selected_area if selected_area in _options else _options[0],
        label="Technology"
    )
    return (technology_field,)


@app.cell
def _(instruments_df, technology_field):
    mo.stop(not technology_field.value)
    _options = (
        instruments_df
        .filter(pl.col("technology") == technology_field.value)
        ["instrument"]
        .unique()
        .sort()
        .to_list()
    )
    instrument_field = mo.ui.dropdown(
        options=_options,
        value=_options[0] if _options else None,
        label="Instrument",
    )
    return (instrument_field,)


@app.cell
def _(combinations_df, container_type, instrument_field):
    # Filter samplers by instrument and container type (vial vs plate)
    mo.stop(not instrument_field.value)
    _container_suffix = ".vial" if container_type == "Vials" else ".plate"
    _options = (
        combinations_df
        .filter(pl.col("instrument") == instrument_field.value)
        .filter(pl.col("sampler").str.ends_with(_container_suffix))
        ["sampler"]
        .unique()
        .sort()
        .to_list()
    )
    # If no matching container type, show all
    if not _options:
        _options = (
            combinations_df
            .filter(pl.col("instrument") == instrument_field.value)
            ["sampler"]
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
def _(combinations_df, instrument_field, sampler_field):
    # Software is derived from combinations.csv
    mo.stop(not instrument_field.value or not sampler_field.value)
    _row = combinations_df.filter(
        (pl.col("instrument") == instrument_field.value) &
        (pl.col("sampler") == sampler_field.value)
    )
    output_format_value = _row["output_format"][0] if len(_row) > 0 else "xcalibur"
    return (output_format_value,)


@app.cell
def _(instrument_field, instrument_patterns_df, technology_field):
    # Pattern dropdown filtered by technology + instrument
    mo.stop(not technology_field.value or not instrument_field.value)
    _patterns_df = (
        instrument_patterns_df
        .filter(
            (pl.col("technology") == technology_field.value) &
            (pl.col("instrument") == instrument_field.value)
        )
        .sort("is_default", descending=True)
    )
    _options = _patterns_df["queue_pattern"].to_list()
    pattern_field = mo.ui.dropdown(
        options=_options,
        value=_options[0] if _options else None,
        label="Pattern",
    )
    return (pattern_field,)


@app.cell
def _(configs, pattern_field, technology_field):
    # Get default QC frequency from selected pattern
    mo.stop(not technology_field.value or not pattern_field.value)
    _pattern = configs.queue_patterns.get_pattern(technology_field.value, pattern_field.value)
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
def _(CONFIG_DIR, instrument_field, technology_field):
    # Load available methods from CSV
    mo.stop(not technology_field.value or not instrument_field.value)
    _methods_file = CONFIG_DIR / "methods" / technology_field.value / f"{instrument_field.value}_methods.csv"
    if _methods_file.exists():
        methods_df = pl.read_csv(_methods_file)
    else:
        methods_df = pl.DataFrame()
    return (methods_df,)


@app.cell
def _(methods_df):
    # Get unique method names for each polarity from default sample_type
    if methods_df.is_empty():
        available_methods_pos = []
        available_methods_neg = []
    else:
        _default = methods_df.filter(pl.col("sample_type") == "default")
        # Check if polarity column exists
        if "polarity" in _default.columns:
            available_methods_pos = sorted(
                _default.filter(pl.col("polarity") == "pos")["method_name"].unique().to_list()
            )
            available_methods_neg = sorted(
                _default.filter(pl.col("polarity") == "neg")["method_name"].unique().to_list()
            )
        else:
            # Backward compat: no polarity column, all methods available for both
            available_methods_pos = sorted(_default["method_name"].unique().to_list())
            available_methods_neg = available_methods_pos
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
def _(technology_field):
    # Polarity selection for all technologies
    # Default: proteomics=pos only, metabolomics/lipidomics=pos+neg
    _default_neg = requires_polarity(technology_field.value)  # True for metabolomics/lipidomics
    polarity_group = mo.ui.batch(
        mo.md("**Polarity:** {pos} pos {neg} neg"),
        {"pos": mo.ui.checkbox(value=True), "neg": mo.ui.checkbox(value=_default_neg)},
    )
    return (polarity_group,)


@app.cell
def _():
    randomization_field = mo.ui.dropdown(
        options=["no", "yes"],
        value="no",
        label="Randomization"
    )
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
    container_id,
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
    technology_field,
    user_field,
):
    # Build sidebar content - show title always, inputs only after order selected
    _sidebar_items = [mo.md("# Queue Generator")]

    if container_id is not None:
        # Build queue section items: pattern, polarity, then method per polarity
        _queue_items = [pattern_field, polarity_group]
        if method_field_pos:
            _queue_items.append(method_field_pos)
        if method_field_neg:
            _queue_items.append(method_field_neg)

        _sidebar_items.extend([
            mo.md("### Instrument"),
            technology_field,
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
        ])

    mo.sidebar(mo.vstack(_sidebar_items))
    return


# =============================================================================
# Sample Loading
# =============================================================================


@app.cell
def _(client, container_id):
    mo.stop(container_id is None)
    plates = load_plates_for_container(client, container_id)
    return (plates,)


@app.cell
def _(plates):
    _plate_ids = sorted(_plate.id for _plate in plates.values())
    plates_select = mo.ui.multiselect(_plate_ids, label="Plates")
    return (plates_select,)


@app.cell
def _(client, container_id, plates, plates_select):
    selected_plate_ids = plates_select.value if plates_select.value else None
    full_samples_df = load_samples_for_container(client, container_id, plates, selected_plate_ids)
    mo.stop(full_samples_df.is_empty(), mo.md("**No samples found**"))
    full_samples_df = full_samples_df.sort("id")
    return (full_samples_df,)


@app.cell
def _():
    subset_samples_toggle = mo.ui.checkbox(False, label="Subset samples")
    return (subset_samples_toggle,)


@app.cell
def _(full_samples_df, subset_samples_toggle):
    if subset_samples_toggle.value:
        subset_samples_select = mo.ui.table(data=full_samples_df, freeze_columns_left=["name"])
    else:
        subset_samples_select = None
    return (subset_samples_select,)


@app.cell
def _(full_samples_df, subset_samples_select):
    if subset_samples_select is None:
        selected_samples_df = full_samples_df
    else:
        selected_samples_df = subset_samples_select.value
    return (selected_samples_df,)


@app.cell
def _(selected_samples_df):
    sample_df = normalize_samples_df(selected_samples_df)
    return (sample_df,)


# =============================================================================
# Queue Parameters
# =============================================================================


@app.cell
def _(
    date_field,
    inj_vol_field,
    instrument_field,
    method_field_neg,
    method_field_pos,
    pattern_field,
    polarity_group,
    qc_frequency_field,
    randomization_field,
    sampler_field,
    output_format_value,
    technology_field,
    user_field,
):
    queue_parameters_err = None
    try:
        _inj_vol = float(inj_vol_field.value) if inj_vol_field.value.strip() else None
        _qc_freq = int(qc_frequency_field.value) if qc_frequency_field.value.strip() else None
        # Build polarity list from checkbox group
        _polarity = []
        if polarity_group.value.get("pos"):
            _polarity.append("pos")
        if polarity_group.value.get("neg"):
            _polarity.append("neg")
        _randomization = randomization_field.value == "yes"

        # Build method dict from per-polarity selections
        method_dict = {}
        if method_field_pos is not None and method_field_pos.value:
            method_dict["pos"] = method_field_pos.value
        if method_field_neg is not None and method_field_neg.value:
            method_dict["neg"] = method_field_neg.value

        queue_parameters = QueueParameters.model_validate(
            {
                "technology": technology_field.value,
                "instrument": instrument_field.value,
                "sampler": sampler_field.value,
                "output_format": output_format_value,
                "queue_pattern": pattern_field.value,
                "polarity": _polarity,
                "date": date_field.value.strftime("%Y%m%d"),
                "user": user_field.value.strip(),
                "method": method_dict,
                "randomization": _randomization,
                "inj_vol_override": _inj_vol,
                "qc_frequency_override": _qc_freq,
            }
        )
    except pydantic.ValidationError as e:
        queue_parameters_err = e
        queue_parameters = None
    return queue_parameters, queue_parameters_err


@app.cell
def _(queue_parameters):
    save_button = mo.ui.run_button(label="Save Config JSON")
    # Button is displayed in Parameters tab, not here
    return (save_button,)


@app.cell
def _(CONFIG_DIR, container_id, queue_parameters, sample_df, save_button):
    mo.stop(not save_button.value or queue_parameters is None)
    _samples = samples_from_dataframe(sample_df)
    _sample_group = SampleGroup(container_id=container_id, samples=_samples)
    _queue_input = QueueInput(parameters=queue_parameters, sample_groups=[_sample_group])
    _examples_dir = CONFIG_DIR / "examples"
    _examples_dir.mkdir(exist_ok=True)
    _n_samples = len(sample_df)
    _tech = queue_parameters.technology
    _sampler = queue_parameters.sampler.replace(".", "_")
    _filepath = _examples_dir / f"{_tech}_{_sampler}_c{container_id}_n{_n_samples}.json"
    write_params(_queue_input, _filepath)
    mo.md(f"**Saved configuration to `{_filepath}`**")
    return


# =============================================================================
# Main Content Layout
# =============================================================================


@app.cell
def _(project_table):
    mo.vstack([
        mo.md("## Project Selection"),
        project_table,
    ])
    return


@app.cell
def _(container_id, container_type):
    if container_id is None:
        mo.md("**Select a project from the table above**")
    else:
        mo.md(f"**Selected:** Container {container_id} ({container_type})")
    return


@app.cell
def _(plates, plates_select, sample_df, subset_samples_select, subset_samples_toggle):
    # Sample Selection tab content
    sample_selection_content = mo.vstack([
        plates_select if plates else None,
        mo.hstack([subset_samples_toggle, mo.md(f"**{len(sample_df)} samples**")], justify="start"),
        subset_samples_select if subset_samples_select else sample_df,
    ])
    return (sample_selection_content,)


@app.cell
def _(container_id, queue_parameters, queue_parameters_err, sample_df, save_button):
    # Build full QueueInput for display (Parameters tab content)
    if queue_parameters and container_id is not None and sample_df is not None:
        _samples = samples_from_dataframe(sample_df)
        _sample_group = SampleGroup(container_id=container_id, samples=_samples)
        _queue_input = QueueInput(parameters=queue_parameters, sample_groups=[_sample_group])
        _output = _queue_input.model_dump(mode="json")
    else:
        _output = None

    parameters_content = mo.vstack([
        mo.callout(_output, kind="info") if _output else mo.callout(str(queue_parameters_err) if queue_parameters_err else "Select a project", kind="danger"),
        save_button if queue_parameters else None,
    ])
    return (parameters_content,)


@app.cell
def _(configs, container_id, queue_parameters, sample_df):
    # Generate queue from current parameters
    generated_queue_df = None
    generation_error = None

    if queue_parameters and container_id and sample_df is not None and not sample_df.is_empty():
        try:
            _samples = samples_from_dataframe(sample_df)
            _sample_group = SampleGroup(container_id=container_id, samples=_samples)
            _queue_input = QueueInput(parameters=queue_parameters, sample_groups=[_sample_group])
            _builder = QueueGeneratorBuilder(configs)
            _generator = _builder.build(_queue_input)
            generated_queue_df = _generator.generate(_queue_input.get_all_samples())
        except Exception as e:
            generation_error = str(e)

    return generated_queue_df, generation_error


@app.cell
def _(generated_queue_df, generation_error):
    # Queue Preview tab content
    if generation_error:
        queue_preview_content = mo.callout(f"Generation error: {generation_error}", kind="danger")
    elif generated_queue_df is not None:
        queue_preview_content = mo.vstack([
            mo.md(f"**{len(generated_queue_df)} rows**"),
            generated_queue_df,
        ])
    else:
        queue_preview_content = mo.md("_Select a project and configure parameters to preview queue_")
    return (queue_preview_content,)


@app.cell
def _(parameters_content, queue_preview_content, sample_selection_content):
    # Tabbed interface for the right panel
    right_panel_tabs = mo.ui.tabs({
        "Queue Preview": queue_preview_content,
        "Sample Selection": sample_selection_content,
        "Parameters": parameters_content,
    })
    right_panel_tabs
    return (right_panel_tabs,)


if __name__ == "__main__":
    app.run()
