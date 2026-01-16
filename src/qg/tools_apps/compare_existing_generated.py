import marimo

__generated_with = "0.18.4"
app = marimo.App(width="full")


@app.cell
def _():
    import marimo as mo
    import polars as pl
    import json
    from pathlib import Path
    return json, mo, pl, Path


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
    results_path = Path("results/test_results.csv")
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

    mo.md(f"""
    ### Summary
    - **Total tests:** {total}
    - **Perfect (10):** {perfect}
    - **QC mismatch (5):** {qc_mismatch}
    - **Failed (0):** {failed}
    """)


@app.cell
def _(df, mo):
    # Results table with selection
    results_table = mo.ui.table(
        df,
        selection="single",
        label="Click a row to view details"
    )
    results_table
    return (results_table,)


@app.cell
def _(results_table, mo, pl, Path, json):
    # Display details when row selected
    _output = []

    if results_table.value is not None and len(results_table.value) > 0:
        _row = results_table.value[0]
        _orig = str(_row['queue_orig'][0])
        _gen = str(_row['queue_generated'][0])
        _params = str(_row['qg_parameters'][0])
        _score = _row['comparison_result'][0]

        _output.append(mo.md(f"""
        ---
        ### Selected Test
        - **Original:** `{_orig}`
        - **Generated:** `{_gen}`
        - **Parameters:** `{_params}`
        - **Score:** {_score}
        """))

        # Load both queues and do outer join
        _orig_path = Path(_orig)
        _gen_path = Path(_gen)

        _orig_df = None
        _gen_df = None

        def _remove_run(filename: str) -> str:
            """Remove _NNN_ run number from filename for joining."""
            import re
            return re.sub(r'_\d{3}_', '_', filename)

        if _orig_path.exists():
            _orig_df = pl.read_csv(_orig_path)
            _orig_df = _orig_df.rename({
                "row": "row_orig", "run": "run_orig", "vial": "vial_orig",
                "filename": "filename_orig", "method": "method_orig", "output_path": "path_orig"
            })
            _orig_df = _orig_df.with_columns(
                pl.col("filename_orig").map_elements(_remove_run, return_dtype=pl.Utf8).alias("join_key")
            )

        if _gen_path.exists() and _gen_path.stat().st_size > 0:
            _gen_df = pl.read_csv(_gen_path)
            _gen_df = _gen_df.rename({
                "File Name": "filename_gen", "Path": "path_gen",
                "Instrument Method": "method_gen", "Position": "vial_gen",
                "Inj Vol": "injvol_gen", "Sample Type": "type_gen", "Sample Name": "sample_gen"
            })
            _gen_df = _gen_df.with_columns(
                pl.col("filename_gen").map_elements(_remove_run, return_dtype=pl.Utf8).alias("join_key")
            )

        if _orig_df is not None and _gen_df is not None:
            # Outer join on filename without run number
            _joined = _orig_df.join(_gen_df, on="join_key", how="full", coalesce=True)
            _ordered = [
                "row_orig",
                "join_key",
                "filename_orig", "filename_gen",
                "vial_orig", "vial_gen",
                "path_orig", "path_gen",
                "method_orig", "method_gen",
                "run_orig",
            ]
            for col in _joined.columns:
                if col not in _ordered:
                    _ordered.append(col)
            _ordered = [c for c in _ordered if c in _joined.columns]
            _joined = _joined.select(_ordered)
            _output.append(mo.md("### Comparison (Original vs Generated)"))
            _output.append(mo.ui.table(_joined))
        elif _orig_df is not None:
            _output.append(mo.md("### Original Queue (no generated file)"))
            _output.append(mo.ui.table(_orig_df))
        elif _gen_df is not None:
            _output.append(mo.md("### Generated Queue (no original file)"))
            _output.append(mo.ui.table(_gen_df))

        # Load parameters
        _params_path = Path(_params)
        if _params_path.exists():
            with open(_params_path) as f:
                _params_data = json.load(f)
            _output.append(mo.md("### Queue Parameters"))
            _output.append(mo.tree(_params_data))
    else:
        _output.append(mo.md("*Select a row above to view details*"))

    mo.vstack(_output)


if __name__ == "__main__":
    app.run()
