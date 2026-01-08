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


@app.cell
def _():
    client = Bfabric.connect(config_file_env="TEST")
    return (client,)


@app.cell
def _():
    mo.md(r"""
    ## General inputs
    """)
    return


@app.cell
def _():
    instrument_table = pl.read_csv("instrument.csv", separator="\t", comment_prefix="#")
    return (instrument_table,)


@app.cell
def _():
    with open("proteomics_projects.json") as f:
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
    container_id = str(project_table.value["Container ID"][0])
    selected_area = project_table.value["Area"][0]
    return container_id, selected_area


@app.cell
def _(client, container_id):
    container = client.reader.read_id("container", container_id)
    return (container,)


@app.cell
def _(instrument_table, selected_area):
    _options = instrument_table["area"].unique().sort().to_list()
    _default = selected_area if selected_area in _options else _options[0]
    area_field = mo.ui.dropdown(options=_options, value=_default, label="Area")
    return (area_field,)


@app.cell
def _(area_field, instrument_table):
    instrument_field = mo.ui.dropdown(
        options=instrument_table.filter(area=area_field.value)["instrument"]
        .unique()
        .sort()
        .to_list(),
        label="Instrument",
    )
    return (instrument_field,)


@app.cell
def _(area_field, instrument_field, instrument_table):
    mo.stop(not instrument_field.value)
    _options = instrument_table.filter(
        area=area_field.value, instrument=instrument_field.value
    )["system"].unique().to_list()
    system_field = mo.ui.dropdown(
        options=_options,
        value=_options[0] if _options else None,
        label="System",
    )
    return (system_field,)


@app.cell
def _(area_field, instrument_field, instrument_table, system_field):
    _options = instrument_table.filter(
        area=area_field.value,
        instrument=instrument_field.value,
        system=system_field.value,
    )["lc"].unique().to_list()
    lc_field = mo.ui.dropdown(
        options=_options,
        value=_options[0] if _options else None,
        label="LC",
    )
    return (lc_field,)


@app.cell
def _(container_id):
    # e.g. 37778 no plates
    #  37202 with plates
    mo.md(f"**Selected Container ID:** {container_id}")
    return


@app.cell
def _(area_field, instrument_field):
    mo.hstack([area_field, instrument_field], justify="start")
    return


@app.cell
def _(lc_field, system_field):
    mo.hstack([system_field, lc_field], justify="start")
    return


@app.cell
def _():
    randomization_field = mo.ui.dropdown(["no", "plate", "all"], "no", label="Randomization")
    qc_frequency_field = mo.ui.dropdown([1, 2, 4, 8, 16, 32, 36, 48, 64, 1024], 16, label="QC frequency")
    inj_vol_field = mo.ui.text(value="", label="Injection Volume")
    return inj_vol_field, qc_frequency_field, randomization_field


@app.cell
def _(inj_vol_field, qc_frequency_field, randomization_field):
    mo.hstack([randomization_field, qc_frequency_field, inj_vol_field], justify="start")
    return


@app.cell
def _():
    mo.md(r"""
    ## Sample selection
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
    ## Queue parameters
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
class UserParameters(BaseModel):
    container_id: int
    area: str
    instrument: str
    system: str
    lc: str
    randomization: str
    qc_frequency: int
    inj_vol: float


@app.cell
def _(
    area_field,
    container_id,
    inj_vol_field,
    instrument_field,
    lc_field,
    qc_frequency_field,
    randomization_field,
    system_field,
):
    user_parameters_err = None
    try:
        user_parameters = UserParameters.model_validate(
            {
                "container_id": container_id,
                "area": area_field.value,
                "instrument": instrument_field.value,
                "system": system_field.value,
                "lc": lc_field.value,
                "randomization": randomization_field.value,
                "qc_frequency": qc_frequency_field.value,
                "inj_vol": inj_vol_field.value,
            }
        )
    except pydantic.ValidationError as e:
        user_parameters_err = e
        user_parameters = None
    return user_parameters, user_parameters_err


@app.cell
def _(inj_vol_field, sample_df):
    mo.stop(not inj_vol_field.value, mo.md("**Enter Injection Volume to see samples**"))
    sample_df
    return


@app.cell
def _(user_parameters, user_parameters_err):
    user_parameters.model_dump(mode="json") if user_parameters is not None else user_parameters_err
    return


@app.cell
def _(user_parameters):
    save_button = mo.ui.run_button(label="Save Config")
    save_button if user_parameters is not None else None
    return (save_button,)


@app.cell
def _(container_id, sample_df, save_button, user_parameters):
    mo.stop(not save_button.value or user_parameters is None)
    _config = {
        "parameters": user_parameters.model_dump(mode="json"),
        "samples": sample_df.to_dicts(),
    }
    _config_dir = Path("qg_configs")
    _config_dir.mkdir(exist_ok=True)
    _n_samples = len(sample_df)
    _qc_freq = user_parameters.qc_frequency
    _filepath = _config_dir / f"config_{container_id}_n{_n_samples}_qc{_qc_freq}.json"
    _filepath.write_text(json.dumps(_config, indent=2))
    mo.md(f"**Saved configuration to `{_filepath}`**")
    return


if __name__ == "__main__":
    app.run()
