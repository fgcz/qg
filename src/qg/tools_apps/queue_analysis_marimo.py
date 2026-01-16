import marimo

__generated_with = "0.18.4"
app = marimo.App(width="medium")


@app.cell
def _():
    import marimo as mo
    import polars as pl
    import altair as alt

    # Enable larger datasets in altair
    alt.data_transformers.enable("vegafusion")
    return alt, mo, pl


@app.cell
def _(mo):
    mo.md("""
    # Queue Analysis - Mass Spectrometry Instruments

    Analysis of Thermo Xcalibur .sld sequence list files across 4 instruments:
    - ASCEND_1, ASTRAL_1, EXPLORIS_2, LUMOS_2
    """)
    return


@app.cell
def _(pl):
    # Load the merged data (created by queue_analysis_merge.py)
    df = pl.read_csv("reference/merged.csv")

    # Ensure numeric columns are properly typed
    df = df.with_columns([
        pl.col("row").cast(pl.Int64, strict=False),
        pl.col("run").cast(pl.Int64, strict=False),
    ])
    return (df,)


@app.cell
def _(df, mo):
    mo.md(f"**Total rows:** {df.height:,} samples from {df['source_file'].n_unique():,} queue files")
    return


@app.cell
def _(mo):
    mo.md("""
    ## Summary by Instrument
    """)
    return


@app.cell
def _(df, mo, pl):
    # Summary stats by instrument
    summary = df.group_by("instrument").agg([
        pl.len().alias("total_samples"),
        pl.col("source_file").n_unique().alias("queue_files"),
        pl.col("sample_type").filter(pl.col("sample_type") == "sample").len().alias("samples"),
        pl.col("sample_type").filter(pl.col("sample_type") == "qc").len().alias("qc"),
        pl.col("sample_type").filter(pl.col("sample_type") == "clean").len().alias("clean"),
        pl.col("sample_type").filter(pl.col("sample_type") == "blank").len().alias("blank"),
    ]).sort("instrument")

    # Add percentages
    summary = summary.with_columns([
        (pl.col("samples") * 100 / pl.col("total_samples")).round(1).alias("samples_pct"),
        (pl.col("qc") * 100 / pl.col("total_samples")).round(1).alias("qc_pct"),
        (pl.col("clean") * 100 / pl.col("total_samples")).round(1).alias("clean_pct"),
    ])

    mo.ui.table(summary)
    return


@app.cell
def _(mo):
    mo.md("""
    ## Sample Type Distribution
    """)
    return


@app.cell
def _(alt, df):
    # Chart: Sample type by instrument
    chart_data = df.group_by(["instrument", "sample_type"]).len().sort("instrument")

    chart = alt.Chart(chart_data.to_pandas()).mark_bar().encode(
        x=alt.X("instrument:N", title="Instrument"),
        y=alt.Y("len:Q", title="Count"),
        color=alt.Color("sample_type:N", title="Sample Type",
                       scale=alt.Scale(domain=["sample", "qc", "clean", "blank"],
                                      range=["#1f77b4", "#ff7f0e", "#2ca02c", "#d62728"])),
        tooltip=["instrument", "sample_type", "len"]
    ).properties(
        width=500,
        height=300,
        title="Sample Types by Instrument"
    )
    chart
    return


@app.cell
def _(mo):
    mo.md("""
    ## Vial/Position Formats
    """)
    return


@app.cell
def _(df, mo, pl):
    # Analyze vial patterns
    vial_analysis = df.filter(pl.col("vial").is_not_null() & (pl.col("vial") != "")).group_by("instrument").agg([
        pl.len().alias("samples_with_vial"),
        pl.col("vial").filter(pl.col("vial").str.starts_with("EvoSlot")).len().alias("evoslot"),
        pl.col("vial").filter(pl.col("vial").str.contains(r"^[YB]:")).len().alias("vials_YB"),
        pl.col("vial").filter(pl.col("vial").str.contains(r"^\d+:")).len().alias("plates"),
    ]).sort("instrument")

    mo.vstack([
        mo.md("**Position format counts by instrument:**"),
        mo.ui.table(vial_analysis),
        mo.md("""
        - **vials_YB**: Letter prefix vials (Y:A1, B:F9) - ASTRAL only
        - **plates**: Numeric prefix plates (1:F, 2:A) - ASCEND, LUMOS
        - **evoslot**: Evosep tip positions (EvoSlot 1:2) - EXPLORIS
        """)
    ])
    return


@app.cell
def _(mo):
    mo.md("""
    ## QC Sample Analysis
    """)
    return


@app.cell
def _(df, mo, pl):
    # Extract QC type from filename
    qc_samples = df.filter(pl.col("sample_type") == "qc").with_columns([
        pl.col("filename").str.extract(r"(autoQC\d*[a-zA-Z]*)", 1).alias("qc_type")
    ])

    qc_summary = qc_samples.group_by(["instrument", "qc_type"]).len().sort(["instrument", "len"], descending=[False, True])

    mo.ui.table(qc_summary.head(30), label="Top QC types by instrument")
    return


@app.cell
def _(mo):
    mo.md("""
    ## Interactive Data Explorer
    """)
    return


@app.cell
def _(df, mo):
    instrument_filter = mo.ui.dropdown(
        options=["All"] + df["instrument"].unique().sort().to_list(),
        value="All",
        label="Instrument"
    )
    sample_type_filter = mo.ui.dropdown(
        options=["All"] + df["sample_type"].unique().drop_nulls().sort().to_list(),
        value="All",
        label="Sample Type"
    )
    mo.hstack([instrument_filter, sample_type_filter])
    return instrument_filter, sample_type_filter


@app.cell
def _(df, instrument_filter, mo, pl, sample_type_filter):
    # Apply filters
    filtered_df = df

    if instrument_filter.value != "All":
        filtered_df = filtered_df.filter(pl.col("instrument") == instrument_filter.value)

    if sample_type_filter.value != "All":
        filtered_df = filtered_df.filter(pl.col("sample_type") == sample_type_filter.value)

    mo.md(f"**Showing {filtered_df.height:,} rows**")
    return (filtered_df,)


@app.cell
def _(filtered_df, mo):
    mo.ui.table(filtered_df.head(100), label="Filtered data (first 100 rows)")
    return


@app.cell
def _(mo):
    mo.md("""
    ## Method Path Analysis
    """)
    return


@app.cell
def _(df, instrument_filter, mo, pl):
    # Analyze method paths
    inst = instrument_filter.value
    method_df = df if inst == "All" else df.filter(pl.col("instrument") == inst)

    method_analysis = method_df.filter(
        pl.col("method").is_not_null()
    ).with_columns([
        pl.col("method").str.extract(r"C:\\Xcalibur\\methods\\([^\\]+)", 1).alias("method_root")
    ]).group_by("method_root").len().sort("len", descending=True).head(15)

    mo.vstack([
        mo.md(f"**Top method roots for {inst}:**"),
        mo.ui.table(method_analysis)
    ])
    return


@app.cell
def _(mo):
    mo.md("""
    ## Output Path Analysis
    """)
    return


@app.cell
def _(df, instrument_filter, mo, pl):
    # Analyze output paths - extract project pattern
    inst2 = instrument_filter.value
    output_df = df if inst2 == "All" else df.filter(pl.col("instrument") == inst2)

    output_analysis = output_df.filter(
        pl.col("output_path").is_not_null()
    ).with_columns([
        pl.when(pl.col("output_path").str.contains(r"\\orders\\"))
          .then(pl.lit("orders"))
          .otherwise(
              pl.col("output_path").str.extract(r"\\(p\d+)\\", 1)
          ).alias("project_type")
    ]).group_by("project_type").len().sort("len", descending=True).head(20)

    mo.vstack([
        mo.md(f"**Output path distribution for {inst2}:**"),
        mo.ui.table(output_analysis)
    ])
    return


@app.cell
def _(mo):
    mo.md("""
    ## Queue Start/End Sequence Analysis

    Analyzing what sample types typically start and end each queue.
    """)
    return


@app.cell
def _(df, mo, pl):
    # First sample of each queue
    first_samples = df.sort("row").group_by("source_file").first()
    first_by_inst = first_samples.group_by(["instrument", "sample_type"]).len().sort(["instrument", "len"], descending=[False, True])

    # Last sample of each queue
    last_samples = df.sort("row").group_by("source_file").last()
    last_by_inst = last_samples.group_by(["instrument", "sample_type"]).len().sort(["instrument", "len"], descending=[False, True])

    mo.vstack([
        mo.md("### First Sample of Each Queue"),
        mo.ui.table(first_by_inst),
        mo.md("### Last Sample of Each Queue"),
        mo.ui.table(last_by_inst),
    ])
    return first_samples, last_samples


@app.cell
def _(df, mo, pl):
    # QC types at start
    _first_qc = df.sort("row").group_by("source_file").first().filter(
        pl.col("sample_type") == "qc"
    ).with_columns([
        pl.col("filename").str.extract(r"(autoQC\d*[a-zA-Z]*)", 1).alias("qc_type")
    ])
    qc_start = _first_qc.group_by(["instrument", "qc_type"]).len().sort(["instrument", "len"], descending=[False, True])

    # QC types at end
    _last_qc = df.sort("row").group_by("source_file").last().filter(
        pl.col("sample_type") == "qc"
    ).with_columns([
        pl.col("filename").str.extract(r"(autoQC\d*[a-zA-Z]*)", 1).alias("qc_type")
    ])
    qc_end = _last_qc.group_by(["instrument", "qc_type"]).len().sort(["instrument", "len"], descending=[False, True])

    mo.vstack([
        mo.md("### QC Types at Queue Start"),
        mo.ui.table(qc_start.head(20)),
        mo.md("### QC Types at Queue End"),
        mo.ui.table(qc_end.head(20)),
    ])
    return qc_end, qc_start


@app.cell
def _(mo):
    mo.md("""
    ## Blank Sample Analysis
    """)
    return


@app.cell
def _(df, mo, pl):
    # Blank analysis
    blanks = df.filter(pl.col("sample_type") == "blank")

    blank_by_inst = blanks.group_by("instrument").agg([
        pl.len().alias("count"),
        pl.col("filename").first().alias("example_filename")
    ]).sort("instrument")

    mo.vstack([
        mo.md(f"**Total blanks:** {blanks.height}"),
        mo.ui.table(blank_by_inst),
    ])
    return (blanks,)


@app.cell
def _(blanks, df, mo, pl):
    # Blank position in queue (start/middle/end)
    _positions = []
    for row in blanks.iter_rows(named=True):
        queue = df.filter(pl.col("source_file") == row["source_file"])
        total = queue.height
        pos = row["row"] if row["row"] else 0
        if pos <= 3:
            rel_pos = "start"
        elif pos >= total - 2:
            rel_pos = "end"
        else:
            rel_pos = "middle"
        _positions.append({"instrument": row["instrument"], "position": rel_pos})

    if _positions:
        blank_positions = pl.DataFrame(_positions).group_by(["instrument", "position"]).len().sort(["instrument", "len"], descending=[False, True])
        mo.vstack([
            mo.md("### Blank Position in Queue"),
            mo.ui.table(blank_positions),
            mo.md("""
            - **start**: First 3 samples of queue
            - **end**: Last 3 samples of queue
            - **middle**: Between start and end
            """)
        ])
    else:
        mo.md("No blanks found")
    return


@app.cell
def _(alt, df, mo, pl):
    # Filter to queues with >10 samples (group by instrument+source_file since same filename can exist on different instruments)
    _queue_sizes = df.group_by(["instrument", "source_file"]).len()
    _large_queues = _queue_sizes.filter(pl.col("len") > 10)
    _df_filtered = df.join(_large_queues.select(["instrument", "source_file"]), on=["instrument", "source_file"])

    # Count queues per instrument for normalization
    _queue_counts = _df_filtered.group_by("instrument").agg(
        pl.col("source_file").n_unique().alias("n_queues")
    )
    _queue_count_dict = {r["instrument"]: r["n_queues"] for r in _queue_counts.iter_rows(named=True)}

    # Analyze by run position (excluding samples)
    _non_samples = _df_filtered.filter(pl.col("sample_type").is_in(["qc", "clean", "blank"]))

    # Start positions (run 1, 2, 3)
    _start_data = []
    for _pos in [1, 2, 3]:
        _pos_df = _non_samples.filter(pl.col("run") == _pos)
        for _inst in ["ASCEND_1", "ASTRAL_1", "EXPLORIS_2", "LUMOS_2"]:
            _inst_df = _pos_df.filter(pl.col("instrument") == _inst)
            _counts = _inst_df.group_by("sample_type").len()
            _count_dict = {r["sample_type"]: r["len"] for r in _counts.iter_rows(named=True)}
            _n_queues = _queue_count_dict.get(_inst, 1)
            _start_data.append({
                "position": f"run {_pos}",
                "instrument": _inst,
                "qc": round(_count_dict.get("qc", 0) * 100 / _n_queues, 1),
                "clean": round(_count_dict.get("clean", 0) * 100 / _n_queues, 1),
                "blank": round(_count_dict.get("blank", 0) * 100 / _n_queues, 1),
            })

    start_patterns = pl.DataFrame(_start_data)

    # Melt for plotting
    _start_melted = start_patterns.unpivot(
        index=["position", "instrument"],
        on=["qc", "clean", "blank"],
        variable_name="sample_type",
        value_name="pct"
    )

    start_chart = alt.Chart(_start_melted.to_pandas()).mark_bar().encode(
        x=alt.X("position:N", title="Run Position", sort=["run 1", "run 2", "run 3"]),
        y=alt.Y("pct:Q", title="% of Queues"),
        color=alt.Color("sample_type:N", title="Type",
                       scale=alt.Scale(domain=["qc", "clean", "blank"],
                                      range=["#ff7f0e", "#2ca02c", "#d62728"])),
        column=alt.Column("instrument:N", title="Instrument"),
        tooltip=["instrument", "position", "sample_type", "pct"]
    ).properties(
        width=120,
        height=200,
        title="Queue Start Patterns (% of queues with >10 samples)"
    )

    mo.vstack([
        mo.md(f"**Queues with >10 samples:** {', '.join([f'{k}={v}' for k,v in sorted(_queue_count_dict.items())])}"),
        start_chart,
        mo.md("### Queue Start Patterns Table (% of queues)"),
        mo.ui.table(start_patterns),
    ])
    return (start_patterns,)


@app.cell
def _(alt, df, mo, pl):
    # Filter to queues with >10 samples (group by instrument+source_file since same filename can exist on different instruments)
    _queue_sizes = df.group_by(["instrument", "source_file"]).len()
    _large_queues = _queue_sizes.filter(pl.col("len") > 10)
    _df_filtered = df.join(_large_queues.select(["instrument", "source_file"]), on=["instrument", "source_file"])

    # Count queues per instrument for normalization
    _queue_counts = _df_filtered.group_by("instrument").agg(
        pl.col("source_file").n_unique().alias("n_queues")
    )
    _queue_count_dict = {r["instrument"]: r["n_queues"] for r in _queue_counts.iter_rows(named=True)}

    # End positions (last, last-1, last-2)
    _max_runs = _df_filtered.group_by(["instrument", "source_file"]).agg(pl.col("run").max().alias("max_run"))
    _with_max = _df_filtered.join(_max_runs, on=["instrument", "source_file"])

    _end_data = []
    for _offset, _label in [(2, "last-2"), (1, "last-1"), (0, "last")]:
        for _inst in ["ASCEND_1", "ASTRAL_1", "EXPLORIS_2", "LUMOS_2"]:
            _pos_df = _with_max.filter(
                (pl.col("instrument") == _inst) &
                (pl.col("run") == pl.col("max_run") - _offset) &
                pl.col("sample_type").is_in(["qc", "clean", "blank"])
            )
            _counts = _pos_df.group_by("sample_type").len()
            _count_dict = {r["sample_type"]: r["len"] for r in _counts.iter_rows(named=True)}
            _n_queues = _queue_count_dict.get(_inst, 1)
            _end_data.append({
                "position": _label,
                "instrument": _inst,
                "qc": round(_count_dict.get("qc", 0) * 100 / _n_queues, 1),
                "clean": round(_count_dict.get("clean", 0) * 100 / _n_queues, 1),
                "blank": round(_count_dict.get("blank", 0) * 100 / _n_queues, 1),
            })

    end_patterns = pl.DataFrame(_end_data)

    # Melt for plotting
    _end_melted = end_patterns.unpivot(
        index=["position", "instrument"],
        on=["qc", "clean", "blank"],
        variable_name="sample_type",
        value_name="pct"
    )

    end_chart = alt.Chart(_end_melted.to_pandas()).mark_bar().encode(
        x=alt.X("position:N", title="Run Position", sort=["last-2", "last-1", "last"]),
        y=alt.Y("pct:Q", title="% of Queues"),
        color=alt.Color("sample_type:N", title="Type",
                       scale=alt.Scale(domain=["qc", "clean", "blank"],
                                      range=["#ff7f0e", "#2ca02c", "#d62728"])),
        column=alt.Column("instrument:N", title="Instrument"),
        tooltip=["instrument", "position", "sample_type", "pct"]
    ).properties(
        width=120,
        height=200,
        title="Queue End Patterns (% of queues with >10 samples)"
    )

    mo.vstack([
        end_chart,
        mo.md("### Queue End Patterns Table (% of queues)"),
        mo.ui.table(end_patterns),
    ])
    return (end_patterns,)


@app.cell
def _(df, mo, pl):
    # QC types at start (run=1) and end (last run)
    _df_qc = df.with_columns([
        pl.col("filename").str.extract(r"(autoQC\d*[a-zA-Z]*)", 1).alias("qc_type")
    ])

    _qc_start = _df_qc.filter((pl.col("run") == 1) & (pl.col("sample_type") == "qc"))
    qc_start_summary = _qc_start.group_by(["instrument", "qc_type"]).len().sort(["instrument", "len"], descending=[False, True])

    _max_runs = _df_qc.group_by(["instrument", "source_file"]).agg(pl.col("run").max().alias("max_run"))
    _with_max = _df_qc.join(_max_runs, on=["instrument", "source_file"])
    _qc_end = _with_max.filter((pl.col("run") == pl.col("max_run")) & (pl.col("sample_type") == "qc"))
    qc_end_summary = _qc_end.group_by(["instrument", "qc_type"]).len().sort(["instrument", "len"], descending=[False, True])

    mo.vstack([
        mo.md("### QC Types at Run Position 1 (Start)"),
        mo.ui.table(qc_start_summary.head(16)),
        mo.md("### QC Types at Last Run Position (End)"),
        mo.ui.table(qc_end_summary.head(16)),
    ])
    return (qc_start_summary, qc_end_summary)


@app.cell
def _(mo):
    mo.md("""
    ## Run Number Gaps Analysis

    Analyzing the spacing between QC/clean/blank samples within queues.
    """)
    return


@app.cell
def _(alt, df, mo, pl):
    # Filter to queues with >10 samples (group by instrument+source_file since same filename can exist on different instruments)
    _queue_sizes = df.group_by(["instrument", "source_file"]).len()
    _large_queues = _queue_sizes.filter(pl.col("len") > 10)
    _df_filtered = df.join(_large_queues.select(["instrument", "source_file"]), on=["instrument", "source_file"])

    # Calculate gaps between consecutive QC/clean samples using polars diff()
    # Sort by source_file to keep queues contiguous, then diff over (instrument, sample_type)
    # Negative diffs indicate queue boundary crossings and are filtered out
    gap_df = (
        _df_filtered
        .filter(pl.col("sample_type").is_in(["qc", "clean"]))
        .sort(["instrument", "source_file", "run"])
        .with_columns([
            pl.col("run").diff().over(["instrument", "sample_type"]).alias("gap")
        ])
        .filter(pl.col("gap") > 0)  # negative = queue boundary crossing, filter out
        .select(["instrument", "sample_type", "gap"])
    )

    # Summary statistics
    gap_stats = gap_df.group_by(["instrument", "sample_type"]).agg([
        pl.col("gap").median().alias("median"),
        pl.col("gap").mean().round(1).alias("mean"),
        pl.col("gap").min().alias("min"),
        pl.col("gap").max().alias("max"),
        pl.len().alias("n")
    ]).sort(["instrument", "sample_type"])

    # Histogram chart
    gap_chart = alt.Chart(gap_df.filter(pl.col("gap") <= 15).to_pandas()).mark_bar(opacity=0.7).encode(
        x=alt.X("gap:Q", bin=alt.Bin(maxbins=15), title="Gap (run numbers)"),
        y=alt.Y("count():Q", title="Frequency"),
        color=alt.Color("sample_type:N", title="Type"),
        column=alt.Column("instrument:N", title="Instrument")
    ).properties(
        width=150,
        height=150,
        title="Gap Distribution Between Consecutive QC/Clean Samples"
    )

    mo.vstack([
        mo.md("### Gap Statistics (run numbers between consecutive QC/clean)"),
        mo.ui.table(gap_stats),
        gap_chart,
        mo.md("""
        **Interpretation:**
        - Gap of 1 = consecutive runs (e.g., QC at run 5, QC at run 6)
        - Gap of 5 = 4 samples between QCs (e.g., QC at run 5, QC at run 10)
        - Typical pattern: QC every 5-6 samples
        """)
    ])
    return (gap_df, gap_stats)


@app.cell
def _(mo):
    mo.md("""
    ## Consecutive Pair Analysis

    Analyzing which sample types appear consecutively (run N followed by run N+1).
    """)
    return


@app.cell
def _(alt, df, mo, pl):
    # Filter to non-samples and find consecutive pairs
    _non_samples = df.filter(pl.col("sample_type").is_in(["qc", "clean", "blank"])).sort(["instrument", "source_file", "run"])

    _pairs = []
    for _inst in ["ASCEND_1", "ASTRAL_1", "EXPLORIS_2", "LUMOS_2"]:
        _inst_df = _non_samples.filter(pl.col("instrument") == _inst)
        for _queue in _inst_df["source_file"].unique().to_list():
            _q_df = _inst_df.filter(pl.col("source_file") == _queue).sort("run")
            _runs = _q_df["run"].to_list()
            _types = _q_df["sample_type"].to_list()

            for _i in range(len(_runs) - 1):
                if _runs[_i + 1] == _runs[_i] + 1:  # Consecutive runs
                    _pair = f"{_types[_i]}→{_types[_i + 1]}"
                    _pairs.append({"instrument": _inst, "pair": _pair})

    pair_df = pl.DataFrame(_pairs)

    # Summary tables
    _overall = pair_df.group_by("pair").len().sort("len", descending=True)
    _by_inst = pair_df.group_by(["instrument", "pair"]).len().sort(["instrument", "len"], descending=[False, True])

    # Chart
    _pair_chart = alt.Chart(_by_inst.to_pandas()).mark_bar().encode(
        x=alt.X("pair:N", title="Consecutive Pair", sort="-y"),
        y=alt.Y("len:Q", title="Count"),
        color=alt.Color("pair:N", legend=None),
        column=alt.Column("instrument:N", title="Instrument"),
        tooltip=["instrument", "pair", "len"]
    ).properties(
        width=120,
        height=200,
        title="Consecutive Pairs (run N → run N+1)"
    )

    mo.vstack([
        mo.md("""
        ### Key Finding: clean→qc is dominant (not qc→clean)

        **Note:** After QC in middle of queue, samples follow directly most of the time (not clean) - potential carry-over concern?
        """),
        mo.md("### Overall Pair Counts"),
        mo.ui.table(_overall),
        _pair_chart,
    ])
    return (pair_df,)


@app.cell
def _(mo):
    mo.md("""
    ## Common Queue Patterns Summary

    ### Start Pattern (run 1, 2, 3)
    | Instrument | run 1 | run 2 | run 3 |
    |------------|-------|-------|-------|
    | **ASCEND_1** | autoQC01 | (sample) | (sample) |
    | **ASTRAL_1** | autoQC03dia/01 | (sample) | clean |
    | **EXPLORIS_2** | autoQC01 | (sample) | clean |
    | **LUMOS_2** | autoQC017/01 | (sample) | clean |

    ### End Pattern (last-2, last-1, last)
    | Instrument | last-2 | last-1 | last |
    |------------|--------|--------|------|
    | **ASCEND_1** | clean | autoQC03dda/dia | autoQC01 |
    | **ASTRAL_1** | (varies) | autoQC03dia | autoQC03dia/clean |
    | **EXPLORIS_2** | clean | autoQC03dia | autoQC01/4L |
    | **LUMOS_2** | clean | autoQC03dda | autoQC4L |

    ### QC Types
    - **autoQC01**: Standard QC - common at start and end
    - **autoQC03dda**: DDA-specific QC - common at end (last-1)
    - **autoQC03dia**: DIA-specific QC - common at end
    - **autoQC4L**: Long QC - end position (EXPLORIS, LUMOS)
    - **autoQC017**: LUMOS-specific (instrument 7) - start position

    ### Typical Sequence
    ```
    Start: QC01 → samples... → clean → QC03dda/dia → QC01/4L :End
    ```
    """)
    return


if __name__ == "__main__":
    app.run()
