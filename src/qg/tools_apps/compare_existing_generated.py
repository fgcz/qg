import marimo

__generated_with = "0.18.4"
app = marimo.App(width="full")


@app.cell
def _():
    import json
    import re
    from pathlib import Path

    import marimo as mo
    import polars as pl
    return json, mo, pl, re, Path


@app.cell
def _(mo):
    mo.md("""
    # Test Results - Queue Generator Validation

    Database of queue generation tests comparing generated queues against original reference data.

    **Scores:** 10 = perfect, 5 = samples OK but QC mismatch, 0 = fail
    """)


@app.cell
def _(pl, Path):
    # Load test results database
    results_path = Path("qg_20260119_dump/results/test_results.csv")
    if results_path.exists():
        df = pl.read_csv(results_path)
    else:
        df = pl.DataFrame({
            "queue_orig": [],
            "queue_generated": [],
            "qg_parameters": [],
            "comparison_result": [],
        })
    return df, results_path


@app.cell
def _(df, mo, pl):
    # Summary stats
    total = len(df)
    perfect = len(df.filter(pl.col("comparison_result") == 10))
    qc_mismatch = len(df.filter(pl.col("comparison_result") == 5))
    failed = len(df.filter(pl.col("comparison_result") == 0))

    summary_df = pl.DataFrame({
        "Metric": ["Total", "Perfect (10)", "QC mismatch (5)", "Failed (0)"],
        "Count": [total, perfect, qc_mismatch, failed],
    })

    # Per-instrument breakdown
    instrument_df = None
    if "instrument" in df.columns:
        rows = []
        for inst in sorted(df["instrument"].unique().to_list()):
            inst_df = df.filter(pl.col("instrument") == inst)
            inst_perfect = len(inst_df.filter(pl.col("comparison_result") == 10))
            inst_total = len(inst_df)
            rows.append({"Instrument": inst, "Perfect": inst_perfect, "Total": inst_total})
        instrument_df = pl.DataFrame(rows)

    # Per-sampler breakdown
    sampler_df = None
    if "sampler" in df.columns:
        rows = []
        for samp in sorted(df["sampler"].unique().to_list()):
            samp_df = df.filter(pl.col("sampler") == samp)
            samp_perfect = len(samp_df.filter(pl.col("comparison_result") == 10))
            samp_total = len(samp_df)
            rows.append({"Sampler": samp, "Perfect": samp_perfect, "Total": samp_total})
        sampler_df = pl.DataFrame(rows)

    summary_table = mo.ui.table(summary_df, selection=None, show_column_summaries=True)
    tables = [summary_table]
    if instrument_df is not None:
        tables.append(mo.ui.table(instrument_df, selection=None, show_column_summaries=True))
    if sampler_df is not None:
        tables.append(mo.ui.table(sampler_df, selection=None, show_column_summaries=True))
    mo.hstack(tables, justify="start", gap=2)


@app.cell
def _(mo):
    # Text filters with debounce for live filtering
    instrument_filter = mo.ui.text(placeholder="regex", label="Instrument", debounce=500)
    sampler_filter = mo.ui.text(placeholder="regex", label="Sampler", debounce=500)
    score_filter = mo.ui.text(placeholder="0, 5, 10", label="Score", debounce=500)
    return instrument_filter, sampler_filter, score_filter


@app.cell
def _(df, instrument_filter, sampler_filter, score_filter, mo, pl):
    # Apply filters (case-insensitive regex)
    filtered_df = df
    if instrument_filter.value.strip():
        pattern = f"(?i){instrument_filter.value}"
        filtered_df = filtered_df.filter(pl.col("instrument").str.contains(pattern, literal=False))
    if sampler_filter.value.strip():
        pattern = f"(?i){sampler_filter.value}"
        filtered_df = filtered_df.filter(pl.col("sampler").str.contains(pattern, literal=False))
    if score_filter.value.strip():
        try:
            filtered_df = filtered_df.filter(pl.col("comparison_result") == int(score_filter.value))
        except ValueError:
            pass

    filter_row = mo.hstack([instrument_filter, sampler_filter, score_filter], justify="start", gap=1)
    results_table = mo.ui.table(filtered_df, selection="single")
    mo.vstack([filter_row, results_table])
    return filtered_df, results_table


@app.cell
def _(results_table, mo, pl, Path):
    # Load data when row selected
    orig_df = None
    gen_df = None
    selected_info = None

    if results_table.value is not None and len(results_table.value) > 0:
        _row = results_table.value[0]
        _orig = str(_row['queue_orig'][0])
        _gen = str(_row['queue_generated'][0])
        _params = str(_row['qg_parameters'][0])
        _score = _row['comparison_result'][0]
        _instrument = str(_row['instrument'][0]) if 'instrument' in _row.columns else ""
        _sampler = str(_row['sampler'][0]) if 'sampler' in _row.columns else ""

        selected_info = {
            "orig_path": _orig,
            "gen_path": _gen,
            "params_path": _params,
            "score": _score,
            "instrument": _instrument,
            "sampler": _sampler,
        }

        _orig_path = Path(_orig)
        _gen_path = Path(_gen)

        if _orig_path.exists():
            orig_df = pl.read_csv(_orig_path)
            orig_df = orig_df.rename({c: f"{c}_orig" for c in orig_df.columns})

        if _gen_path.exists() and _gen_path.stat().st_size > 0:
            gen_df = pl.read_csv(_gen_path)
            gen_df = gen_df.rename({c: f"{c}_gen" for c in gen_df.columns})

    return orig_df, gen_df, selected_info


@app.cell
def _(mo, orig_df, gen_df):
    # Detect joinable columns (columns with matching base names: xxx_orig and xxx_gen)
    join_column_options = []
    if orig_df is not None and gen_df is not None:
        orig_bases = {c.removesuffix("_orig") for c in orig_df.columns if c.endswith("_orig")}
        gen_bases = {c.removesuffix("_gen") for c in gen_df.columns if c.endswith("_gen")}
        join_column_options = sorted(orig_bases & gen_bases)

    join_type_options = ["full", "inner", "left", "right", "semi", "anti", "cross"]

    # Only create dropdowns if we have options
    join_column_dropdown = None
    join_type_dropdown = None
    _dropdown_display = mo.md("")

    if join_column_options:
        default_col = "filename" if "filename" in join_column_options else join_column_options[0]

        join_column_dropdown = mo.ui.dropdown(
            options=join_column_options,
            value=default_col,
            label="Join on"
        )

        join_type_dropdown = mo.ui.dropdown(
            options=join_type_options,
            value="full",
            label="Join type"
        )

        _dropdown_display = mo.hstack([join_column_dropdown, join_type_dropdown], justify="start")

    _dropdown_display
    return join_column_dropdown, join_type_dropdown


@app.cell
def _(mo):
    # Checkbox for toggling between original/generated view (must be in separate cell)
    show_generated = mo.ui.checkbox(label="Show generated (uncheck for original)", value=False)
    return (show_generated,)


@app.cell
def _(results_table, selected_info, orig_df, gen_df, join_column_dropdown, join_type_dropdown, show_generated, mo, pl, json, Path):
    # Display comparison results
    _output = []

    if selected_info is not None:
        if orig_df is not None and gen_df is not None and join_column_dropdown is not None:
            # Perform join with selected options
            join_base = join_column_dropdown.value  # e.g., "filename"
            join_how = join_type_dropdown.value
            left_col = f"{join_base}_orig"
            right_col = f"{join_base}_gen"

            _joined = orig_df.join(
                gen_df,
                left_on=left_col,
                right_on=right_col,
                how=join_how,
            )

            # Order columns nicely: paired columns side by side
            _ordered = [
                "row_orig", "row_gen",
                "filename_orig", "filename_gen",
                "vial_orig", "vial_gen",
                "method_orig", "method_gen",
                "path_orig", "path_gen",
                "run_orig",
            ]
            for col in _joined.columns:
                if col not in _ordered:
                    _ordered.append(col)
            _ordered = [c for c in _ordered if c in _joined.columns]
            _joined = _joined.select(_ordered)

            _output.append(mo.md(f"### Comparison ({join_how} join on {join_base})"))
            _output.append(mo.ui.table(_joined))

        elif orig_df is not None:
            _output.append(mo.md("### Original Queue (no generated file)"))
            _output.append(mo.ui.table(orig_df))
        elif gen_df is not None:
            _output.append(mo.md("### Generated Queue (no original file)"))
            _output.append(mo.ui.table(gen_df))

        # Side panel: Queue Parameters + raw data toggle
        _side_panel = []

        # Load parameters
        _params_path = Path(selected_info['params_path'])
        if _params_path.exists():
            with open(_params_path) as f:
                _params_data = json.load(f)
            _side_panel.append(mo.md("### Queue Parameters"))
            _side_panel.append(mo.tree(_params_data))

        # Raw queue data with toggle
        if orig_df is not None or gen_df is not None:
            _side_panel.append(mo.md("### Raw Queue Data"))
            _side_panel.append(show_generated)
            if show_generated.value and gen_df is not None:
                _side_panel.append(mo.ui.table(gen_df))
            elif orig_df is not None:
                _side_panel.append(mo.ui.table(orig_df))

        _output.append(mo.vstack(_side_panel))
    else:
        _output.append(mo.md("*Select a row above to view details*"))

    mo.vstack(_output)


if __name__ == "__main__":
    app.run()
