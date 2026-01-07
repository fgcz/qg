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
    import operator


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
    container_id_field = mo.ui.text(label="Container ID")
    return (container_id_field,)


@app.cell
def _(client, container_id_field):
    container = client.reader.read_id("container", container_id_field.value)
    return (container,)


@app.cell
def _(container, instrument_table):
    _options = sorted(
        set(container["technology"]) & set(instrument_table["area"].unique())
    )

    area_field = mo.ui.dropdown(options=_options, value=_options[0], label="Area")
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
    system_field = mo.ui.dropdown(
        options=instrument_table.filter(
            area=area_field.value, instrument=instrument_field.value
        )["system"]
        .unique()
        .to_list(),
        label="System",
    )
    return (system_field,)


@app.cell
def _(area_field, instrument_field, instrument_table, system_field):
    lc_field = mo.ui.dropdown(
        options=instrument_table.filter(
            area=area_field.value,
            instrument=instrument_field.value,
            system=system_field.value,
        )["lc"]
        .unique()
        .to_list(),
        label="LC",
    )
    return (lc_field,)


@app.cell
def _(container_id_field):
    # e.g. 37778 no plates
    #  37202 with plates
    container_id_field
    return


@app.cell
def _(area_field):
    area_field
    return


@app.cell
def _(instrument_field):
    instrument_field
    return


@app.cell
def _(system_field):
    system_field
    return


@app.cell
def _(lc_field):
    lc_field
    return


@app.cell
def _():
    randomization_field = mo.ui.dropdown(["no", "plate", "all"], "no", label="Randomization")
    qc_frequency_field = mo.ui.dropdown([1, 2, 4, 8, 16, 32, 36, 48, 64, 1024], 16, label="QC frequency")
    inj_vol_field = mo.ui.text(label="Injection Volume")
    return inj_vol_field, qc_frequency_field, randomization_field


@app.cell
def _(randomization_field):
    randomization_field
    return


@app.cell
def _(qc_frequency_field):
    qc_frequency_field
    return


@app.cell
def _(inj_vol_field):
    inj_vol_field
    return


@app.cell
def _():
    mo.md(r"""
    ## Sample selection
    """)
    return


@app.cell
def _(client, container_id_field):
    plates = client.reader.query("plate", {"containerid": container_id_field.value})
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
def _(client, container_id_field, plates, plates_select):
    selected_plate_ids = plates_select.value
    if selected_plate_ids:
        _plates = [_plate for _uri, _plate in plates.items() if _uri.components.entity_id in selected_plate_ids]
        _samples = functools.reduce(operator.iadd, (_plate.refs.sample for _plate in _plates))
        full_samples_df = pl.from_dicts(_sample.data_dict for _sample in _samples)
    else:
        full_samples_df = client.read(
            "sample", {"containerid": container_id_field.value}, max_results=None
        ).to_polars()
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
    if "_position" in selected_samples_df.columns:
        _plate_columns = [
            pl.col("_position").alias("Position"),
            pl.col("_gridposition").alias("GridPosition")
        ]
    else:
        _plate_columns = []
    sample_df = selected_samples_df.select(
        pl.col("name").alias("Sample Name"),
        pl.col("id").alias("Sample ID"),
        pl.col("tubeid").alias("Tube ID"),
        *_plate_columns
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
    container_id_field,
    e,
    inj_vol_field,
    instrument_field,
    lc_field,
    qc_frequency_field,
    randomization_field,
    system_field,
):
    try:
        user_parameters = UserParameters.model_validate(
            {
                "container_id": container_id_field.value,
                "area": area_field.value,
                "instrument": instrument_field.value,
                "system": system_field.value,
                "lc": lc_field.value,
                "randomization": randomization_field.value,
                "qc_frequency": qc_frequency_field.value,
                "inj_vol": inj_vol_field.value,
            }
        )
    except pydantic.ValidationError as user_parameters_err:
        print(e)
        user_parameters = None
    return (user_parameters,)


@app.cell
def _(sample_df):
    sample_df
    return


@app.cell
def _(user_parameters, user_parameters_err):
    user_parameters.model_dump(mode="json") if user_parameters is not None else user_parameters_err
    return


@app.cell
def _():
    return


if __name__ == "__main__":
    app.run()
