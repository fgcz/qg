import marimo

__generated_with = "0.18.4"
app = marimo.App(width="medium", sql_output="polars")

with app.setup:
    import marimo as mo
    import polars as pl
    import pydantic
    from bfabric import Bfabric
    from pydantic import BaseModel
    import functools
    import json
    import operator
    from pathlib import Path
    from datetime import date

    from qg.config import load_all_configs
    from qg.config_models import requires_polarity


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


@app.cell
def _():
    mo.md(r"""
    ## Project Selection
    """)
    return


@app.cell
def _(BFABRIC_CACHE_DIR):
    with open(BFABRIC_CACHE_DIR / "proteomics_projects.json") as f:
        _projects_data = json.load(f)
    # Extract orders from projects - order ID is the actual container ID
    _orders = []
    for _p in _projects_data:
        for _order in _p.get("order", []):
            _plate_count = _order.get("plate_count", 0)
            _sample_count = _order.get("sample_count", 0)
            _orders.append({
                "Container ID": _order["id"],
                "Project ID": _p["id"],
                "Project Name": _p.get("name", ""),
                "PI": _p.get("billingcustomer", ""),
                "Samples": _sample_count,
                "Type": "Plates" if _plate_count > 0 else "Vials",
                "Plates": _plate_count,
                "Status": _p.get("status", ""),
                "Area": _p.get("technology", [""])[0] if _p.get("technology") else "",
            })
    projects_df = pl.DataFrame(_orders).filter(
        (pl.col("Samples") > 0) & ~pl.col("Area").str.to_lowercase().is_in(["genomics", "administration"])
    ).sort("Container ID", descending=True)
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
    project_table
    return


@app.cell
def _(project_table):
    mo.stop(
        project_table.value.is_empty(),
        mo.md("**Select a project from the table above**"),
    )
    container_id = int(project_table.value["Container ID"][0])
    selected_area = project_table.value["Area"][0]
    container_type = project_table.value["Type"][0]  # "Vials" or "Plates"
    return container_id, container_type, selected_area


@app.cell
def _(client, container_id):
    container = client.reader.read_id("container", container_id)
    return


@app.cell
def _(container_id, container_type):
    mo.md(f"""
    **Selected Container ID:** {container_id} ({container_type})
    """)
    return


@app.cell
def _():
    mo.md(r"""
    ## Instrument Configuration
    """)
    return


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
def _(instrument_field, sampler_field, technology_field):
    mo.hstack([technology_field, instrument_field, sampler_field], justify="start")
    return


@app.cell
def _(CONFIG_DIR, instrument_field, technology_field):
    # Method dropdown - load available methods from methods CSV
    mo.stop(not technology_field.value or not instrument_field.value)
    _methods_file = CONFIG_DIR / "methods" / technology_field.value / f"{instrument_field.value}_methods.csv"
    if _methods_file.exists():
        _methods_df = pl.read_csv(_methods_file)
        _default_methods = _methods_df.filter(pl.col("sample_type") == "default")["method_name"].unique().to_list()
    else:
        _default_methods = []
    method_field = mo.ui.dropdown(
        options=[""] + sorted(_default_methods),
        value="",
        label="Method (for user samples)",
    ) if _default_methods else None
    return (method_field,)


@app.cell
def _(method_field, pattern_field, output_format_value):
    mo.hstack([
        pattern_field,
        method_field if method_field else None,
        mo.md(f"**Output Format:** {output_format_value}"),
    ], justify="start")
    return


@app.cell
def _(technology_field):
    # Polarity selection (only for technologies requiring it)
    _show_polarity = requires_polarity(technology_field.value)
    polarity_field = mo.ui.multiselect(
        options=["pos", "neg"],
        value=["pos", "neg"],
        label="Polarity",
    ) if _show_polarity else None
    return (polarity_field,)


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
    inj_vol_field = mo.ui.text(value="", label="Injection Volume Override (optional)")
    return (inj_vol_field,)


@app.cell
def _():
    user_field = mo.ui.text(value="", label="User (e.g., cpanse)")
    return (user_field,)


@app.cell
def _():
    date_field = mo.ui.date(value=date.today(), label="Date")
    return (date_field,)


@app.cell
def _(date_field, inj_vol_field, randomization_field, user_field):
    mo.hstack([randomization_field, date_field, user_field, inj_vol_field], justify="start")
    return


@app.cell
def _(polarity_field):
    # Show polarity field separately (only exists for metabolomics/lipidomics)
    polarity_field if polarity_field is not None else None
    return


@app.cell
def _():
    mo.md(r"""
    ## Sample Selection
    """)
    return


@app.cell
def _(client, container_id):
    plates = client.reader.query("plate", {"containerid": container_id})
    return (plates,)


@app.cell
def _(plates):
    _plate_ids = sorted(_plate.id for _plate in plates.values())
    plates_select = mo.ui.multiselect(_plate_ids, label="Plates")
    return (plates_select,)


@app.cell
def _(plates, plates_select):
    plates_select if plates else None
    return


@app.cell
def _(client, container_id, plates, plates_select):
    selected_plate_ids = plates_select.value
    if selected_plate_ids:
        _plates = [_plate for _uri, _plate in plates.items() if _uri.components.entity_id in selected_plate_ids]
        _samples = functools.reduce(operator.iadd, (_plate.refs.sample for _plate in _plates))
        full_samples_df = pl.from_dicts(_sample.data_dict for _sample in _samples)
    else:
        full_samples_df = client.read(
            "sample", {"containerid": container_id}, max_results=None
        ).to_polars()
    mo.stop(
        full_samples_df.is_empty(),
        mo.md("**No samples found for this container**"),
    )
    full_samples_df = full_samples_df.sort("id")
    return (full_samples_df,)


@app.cell
def _():
    subset_samples_toggle = mo.ui.checkbox(False, label="Subsetting samples")
    return (subset_samples_toggle,)


@app.cell
def _(subset_samples_toggle):
    subset_samples_toggle
    return


@app.cell
def _(full_samples_df, subset_samples_toggle):
    if subset_samples_toggle.value:
        subset_samples_select = mo.ui.table(data=full_samples_df, freeze_columns_left=["name"])
    else:
        subset_samples_select = None
    return (subset_samples_select,)


@app.cell
def _(subset_samples_select):
    subset_samples_select
    return


@app.cell
def _(full_samples_df, subset_samples_select):
    if subset_samples_select is None:
        selected_samples_df = full_samples_df
    else:
        selected_samples_df = subset_samples_select.value
    return (selected_samples_df,)


@app.cell
def _():
    mo.md(r"""
    ## Queue Parameters
    """)
    return


@app.cell
def _(selected_samples_df):
    _optional_columns = []
    if "_position" in selected_samples_df.columns:
        _optional_columns.extend([
            pl.col("_position").alias("Position"),
            pl.col("_gridposition").alias("GridPosition")
        ])
    if "tubeid" in selected_samples_df.columns:
        _optional_columns.append(pl.col("tubeid").alias("Tube ID"))
    sample_df = selected_samples_df.select(
        pl.col("name").alias("Sample Name"),
        pl.col("id").alias("Sample ID"),
        *_optional_columns
    ).sort("Sample ID")
    return (sample_df,)


@app.class_definition
class QueueParameters(BaseModel):
    """New queue parameters matching queue_generator.py input format."""
    container_id: int
    technology: str
    instrument: str
    sampler: str
    output_format: str
    queue_pattern: str
    polarity: list[str] = []
    date: str
    user: str = ""
    method: str = ""
    randomization: bool = False
    inj_vol_override: float | None = None


@app.cell
def _(
    container_id,
    date_field,
    inj_vol_field,
    instrument_field,
    method_field,
    pattern_field,
    polarity_field,
    randomization_field,
    sampler_field,
    output_format_value,
    technology_field,
    user_field,
):
    queue_parameters_err = None
    try:
        _inj_vol = float(inj_vol_field.value) if inj_vol_field.value.strip() else None
        _polarity = list(polarity_field.value) if polarity_field is not None else []
        _randomization = randomization_field.value == "yes"
        _method = method_field.value if method_field is not None else ""
        queue_parameters = QueueParameters.model_validate(
            {
                "container_id": container_id,
                "technology": technology_field.value,
                "instrument": instrument_field.value,
                "sampler": sampler_field.value,
                "output_format": output_format_value,
                "queue_pattern": pattern_field.value,
                "polarity": _polarity,
                "date": date_field.value.strftime("%Y%m%d"),
                "user": user_field.value.strip(),
                "method": _method,
                "randomization": _randomization,
                "inj_vol_override": _inj_vol,
            }
        )
    except pydantic.ValidationError as e:
        queue_parameters_err = e
        queue_parameters = None
    return queue_parameters, queue_parameters_err


@app.cell
def _(sample_df):
    mo.md(f"""
    **{len(sample_df)} samples selected**
    """)
    return


@app.cell
def _(sample_df):
    sample_df
    return


@app.cell
def _(queue_parameters, queue_parameters_err):
    if queue_parameters is not None:
        mo.md("**Parameters:**")
    queue_parameters.model_dump(mode="json") if queue_parameters is not None else queue_parameters_err
    return


@app.cell
def _(queue_parameters):
    save_button = mo.ui.run_button(label="Save Config JSON")
    save_button if queue_parameters is not None else None
    return (save_button,)


@app.cell
def _(CONFIG_DIR, container_id, queue_parameters, sample_df, save_button):
    mo.stop(not save_button.value or queue_parameters is None)
    _queue_input = {
        "parameters": queue_parameters.model_dump(mode="json"),
        "samples": sample_df.to_dicts(),
    }
    _examples_dir = CONFIG_DIR / "examples"
    _examples_dir.mkdir(exist_ok=True)
    _n_samples = len(sample_df)
    _tech = queue_parameters.technology
    _sampler = queue_parameters.sampler.replace(".", "_")
    _filepath = _examples_dir / f"{_tech}_{_sampler}_c{container_id}_n{_n_samples}.json"
    _filepath.write_text(json.dumps(_queue_input, indent=2))
    mo.md(f"**Saved configuration to `{_filepath}`**")
    return


if __name__ == "__main__":
    app.run()
