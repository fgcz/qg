import marimo

__generated_with = "0.18.4"
app = marimo.App(width="full")

with app.setup:
    import tomllib
    from pathlib import Path

    import marimo as mo
    import polars as pl

    from qg.config_store import config_store


@app.cell
def _():
    # Create ConfigStore instance for reading/writing config files
    store = config_store()
    return (store,)


@app.cell
def _():
    mo.md("""
    # Queue Generation Config Editor
    """)
    return

@app.cell
def _(store):
    # Core configs
    instruments_df = store.get_instruments()
    instruments_editor = mo.ui.data_editor(instruments_df, label="Instruments")
    return instruments_df, instruments_editor

@app.cell
def _(store):
    # UI configs
    instrument_patterns_df = store.get_instrument_patterns()
    instrument_patterns_editor = mo.ui.data_editor(
        instrument_patterns_df, label="Instrument Patterns"
    )
    return instrument_patterns_df, instrument_patterns_editor


@app.cell
def _(store):
    # UI configs
    combinations_df = store.get_combinations()
    combinations_editor = mo.ui.data_editor(combinations_df, label="Combinations")
    return combinations_df, combinations_editor


@app.cell
def _(store):
    # Core configs
    sampler_content = store.get_sampler_toml()
    sampler_editor = mo.ui.code_editor(
        sampler_content, language="toml", min_height=400
    )
    return (sampler_editor,)


@app.cell
def _(store):
    # Core configs
    samples_df = store.get_samples()
    samples_editor = mo.ui.data_editor(samples_df, label="Samples")
    return samples_df, samples_editor


@app.cell
def _(store):
    # Core configs
    patterns_content = store.get_queue_patterns_toml()
    patterns_editor = mo.ui.code_editor(
        patterns_content, language="toml", min_height=400
    )
    return (patterns_editor,)


@app.cell
def _(store):
    # Core configs - QC layouts (two CSV files)
    qc_layouts_grid_df = store.get_qc_layouts_grid()
    qc_layouts_grid_editor = mo.ui.data_editor(
        qc_layouts_grid_df, label="QC Layouts - Grid (Vanquish, MClass48)"
    )
    return qc_layouts_grid_df, qc_layouts_grid_editor


@app.cell
def _(store):
    qc_layouts_evosep_df = store.get_qc_layouts_evosep()
    qc_layouts_evosep_editor = mo.ui.data_editor(
        qc_layouts_evosep_df, label="QC Layouts - Evosep"
    )
    return qc_layouts_evosep_df, qc_layouts_evosep_editor


@app.cell
def _(store):
    # Core configs
    output_formats_content = store.get_output_formats_toml()
    output_formats_editor = mo.ui.code_editor(
        output_formats_content, language="toml", min_height=400
    )
    return (output_formats_editor,)


@app.cell
def _(store):
    # Core configs - methods
    methods_files = store.list_methods_files()
    methods_options = {store.get_methods_relative_path(f): f for f in methods_files}
    methods_dropdown = mo.ui.dropdown(
        options=list(methods_options.keys()),
        value=list(methods_options.keys())[0] if methods_options else None,
        label="Select methods file",
    )
    return methods_dropdown, methods_options


@app.cell
def _(methods_dropdown, methods_options, store):
    mo.stop(not methods_dropdown.value)
    selected_methods_path = methods_options[methods_dropdown.value]
    methods_df = store.get_methods(selected_methods_path)
    methods_editor = mo.ui.data_editor(methods_df, label="Methods")
    return (methods_editor,)


@app.cell
def _(instrument_patterns_editor, instruments_editor):
    instruments_tab = mo.vstack([
        mo.md("### Instruments (instruments.csv)"),
        instruments_editor,
        mo.md("### Instrument Patterns (instrument_patterns.csv)"),
        instrument_patterns_editor,
    ])
    return (instruments_tab,)


@app.cell
def _(sampler_editor):
    samplers_tab = mo.vstack([
        mo.md("### Samplers (sampler.toml)"),
        sampler_editor,
    ])
    return (samplers_tab,)


@app.cell
def _(samples_editor):
    samples_tab = mo.vstack([
        mo.md("### Samples (samples.csv)"),
        samples_editor,
    ])
    return (samples_tab,)


@app.cell
def _(patterns_editor):
    patterns_tab = mo.vstack([
        mo.md("### Queue Patterns (queue_patterns.toml)"),
        patterns_editor,
    ])
    return (patterns_tab,)


@app.cell
def _(qc_layouts_grid_editor, qc_layouts_evosep_editor):
    qc_layouts_tab = mo.vstack([
        mo.md("### QC Layouts - Grid Samplers (qc_layouts_grid.csv)"),
        mo.md("_Vanquish and MClass48 positions: plate, row, col_"),
        qc_layouts_grid_editor,
        mo.md("### QC Layouts - Evosep (qc_layouts_evosep.csv)"),
        mo.md("_Evosep tip ranges: tray, position_start, position_end_"),
        qc_layouts_evosep_editor,
    ])
    return (qc_layouts_tab,)


@app.cell
def _(combinations_editor):
    combinations_tab = mo.vstack([
        mo.md("### Combinations (combinations.csv)"),
        combinations_editor,
    ])
    return (combinations_tab,)


@app.cell
def _(output_formats_editor):
    output_formats_tab = mo.vstack([
        mo.md("### Output Formats (output_formats.toml)"),
        output_formats_editor,
    ])
    return (output_formats_tab,)


@app.cell
def _(methods_dropdown, methods_editor):
    methods_tab = mo.vstack([
        mo.md("### Methods Files"),
        methods_dropdown,
        methods_editor,
    ])
    return (methods_tab,)


# =============================================================================
# Interactive Viewer Tab - Cascading selection
# =============================================================================


@app.cell
def _(sampler_editor, samples_editor, patterns_editor, qc_layouts_grid_editor, qc_layouts_evosep_editor, output_formats_editor):
    # Parse TOML content for visualization
    def safe_parse_toml(content: str) -> dict:
        try:
            return tomllib.loads(content)
        except Exception:
            return {"error": "Failed to parse TOML"}

    # Convert samples DataFrame to dict: {technology: {sample_id: {...}}}
    def samples_df_to_dict(df) -> dict:
        result = {}
        for row in df.iter_rows(named=True):
            tech = row["technology"]
            sid = row["sample_id"]
            if tech not in result:
                result[tech] = {}
            result[tech][sid] = {
                "sample_name": row.get("sample_name", ""),
                "description": row.get("description", ""),
                "inj_vol": row.get("inj_vol", 0),
                "file_name_template": row.get("file_name_template", ""),
            }
        return result

    # Convert QC layout CSVs to nested dict: {tech: {sampler: {sample_id: position}}}
    def qc_layouts_df_to_dict(grid_df, evosep_df) -> dict:
        result = {}
        # Process grid positions
        for row in grid_df.iter_rows(named=True):
            tech = row["technology"]
            sampler = row["sampler"]
            sample_id = row["sample_id"]
            if tech not in result:
                result[tech] = {}
            # Parse sampler (e.g., "Vanquish.vial" -> nested dict)
            parts = sampler.split(".")
            if len(parts) == 2:
                base, container = parts
                if base not in result[tech]:
                    result[tech][base] = {}
                if container not in result[tech][base]:
                    result[tech][base][container] = {}
                result[tech][base][container][sample_id] = f"{row['plate']}:{row['row']}{row['col']}"
            else:
                if sampler not in result[tech]:
                    result[tech][sampler] = {}
                result[tech][sampler][sample_id] = f"{row['plate']}:{row['row']}{row['col']}"
        # Process Evosep positions
        for row in evosep_df.iter_rows(named=True):
            tech = row["technology"]
            sampler = row["sampler"]
            sample_id = row["sample_id"]
            if tech not in result:
                result[tech] = {}
            if sampler not in result[tech]:
                result[tech][sampler] = {}
            result[tech][sampler][sample_id] = {
                "tray": row["tray"],
                "position_start": row["position_start"],
                "position_end": row["position_end"],
            }
        return result

    sampler_data = safe_parse_toml(sampler_editor.value)
    samples_data = samples_df_to_dict(samples_editor.value)
    patterns_data = safe_parse_toml(patterns_editor.value)
    qc_layouts_data = qc_layouts_df_to_dict(qc_layouts_grid_editor.value, qc_layouts_evosep_editor.value)
    output_formats_data = safe_parse_toml(output_formats_editor.value)

    return sampler_data, samples_data, patterns_data, qc_layouts_data, output_formats_data, safe_parse_toml


@app.cell
def _(instruments_df):
    # Step 1: Technology dropdown
    technologies = sorted(instruments_df["technology"].unique().to_list())
    technology_dropdown = mo.ui.dropdown(
        options=technologies,
        value=technologies[0] if technologies else None,
        label="Technology",
    )
    return (technology_dropdown,)


@app.cell
def _(technology_dropdown, instruments_df):
    # Step 2: Instrument dropdown (filtered by technology)
    mo.stop(not technology_dropdown.value)
    filtered_instruments = (
        instruments_df
        .filter(pl.col("technology") == technology_dropdown.value)
        ["instrument"]
        .unique()
        .sort()
        .to_list()
    )
    instrument_dropdown = mo.ui.dropdown(
        options=filtered_instruments,
        value=filtered_instruments[0] if filtered_instruments else None,
        label="Instrument",
    )
    return (instrument_dropdown,)


@app.cell
def _(instrument_dropdown, combinations_df):
    # Step 3: Sampler dropdown (filtered by instrument)
    mo.stop(not instrument_dropdown.value)
    filtered_samplers = (
        combinations_df
        .filter(pl.col("instrument") == instrument_dropdown.value)
        ["sampler"]
        .unique()
        .sort()
        .to_list()
    )
    sampler_dropdown = mo.ui.dropdown(
        options=filtered_samplers,
        value=filtered_samplers[0] if filtered_samplers else None,
        label="Sampler",
    )
    return (sampler_dropdown,)


@app.cell
def _(technology_dropdown, instrument_dropdown, instrument_patterns_df):
    # Step 4: Pattern dropdown (filtered by technology + instrument)
    mo.stop(not technology_dropdown.value or not instrument_dropdown.value)
    filtered_patterns = (
        instrument_patterns_df
        .filter(
            (pl.col("technology") == technology_dropdown.value) &
            (pl.col("instrument") == instrument_dropdown.value)
        )
        .sort("is_default", descending=True)
        ["queue_pattern"]
        .to_list()
    )
    pattern_dropdown = mo.ui.dropdown(
        options=filtered_patterns,
        value=filtered_patterns[0] if filtered_patterns else None,
        label="Pattern",
    )
    return (pattern_dropdown,)


@app.cell
def _(technology_dropdown, samples_data):
    # Step 5: Sample dropdown (filtered by technology)
    mo.stop(not technology_dropdown.value)
    tech_samples = samples_data.get(technology_dropdown.value, {})
    sample_ids = [sid for sid in tech_samples.keys() if isinstance(tech_samples[sid], dict)]
    sample_dropdown = mo.ui.dropdown(
        options=sample_ids,
        value=sample_ids[0] if sample_ids else None,
        label="Sample",
    )
    return sample_dropdown, tech_samples


@app.cell
def _(
    technology_dropdown,
    instrument_dropdown,
    sampler_dropdown,
    pattern_dropdown,
    sampler_data,
    samples_data,
    patterns_data,
    qc_layouts_data,
    output_formats_data,
    combinations_df,
    instruments_df,
    store,
):
    # Build the selected configuration view
    mo.stop(
        not technology_dropdown.value
        or not instrument_dropdown.value
        or not sampler_dropdown.value
    )

    tech = technology_dropdown.value
    instr = instrument_dropdown.value
    sampler_key = sampler_dropdown.value  # e.g., "Vanquish.vial"
    pattern_key = pattern_dropdown.value if pattern_dropdown.value else None

    # Parse sampler key
    sampler_parts = sampler_key.split(".")
    sampler_base = sampler_parts[0]  # e.g., "Vanquish"
    container = sampler_parts[1] if len(sampler_parts) > 1 else None  # e.g., "vial"

    # Get sampler config (merge base + container)
    base_config = sampler_data.get(sampler_base, {})
    container_config = base_config.get(container, {}) if container else {}
    # Merge: base properties + container overrides (excluding nested tables)
    merged_sampler = {
        k: v for k, v in base_config.items()
        if not isinstance(v, dict)
    }
    merged_sampler.update(container_config)

    # Get QC layout: navigate nested structure
    # TOML [proteomics.Vanquish.vial] becomes qc_layouts["proteomics"]["Vanquish"]["vial"]
    tech_qc_layouts = qc_layouts_data.get(tech, {})
    if container:
        # Try nested: tech -> sampler_base -> container (e.g., Vanquish.vial)
        qc_layout = tech_qc_layouts.get(sampler_base, {}).get(container, {})
        # Fall back to sampler_base if container-specific doesn't exist
        if not qc_layout:
            base_layout = tech_qc_layouts.get(sampler_base, {})
            # Check if it's a nested structure (has vial/plate sub-keys) vs actual QC positions
            # Evosep dicts have 'tray' key, nested structures have 'vial'/'plate' keys
            if base_layout and not any(k in base_layout for k in ["vial", "plate"]):
                qc_layout = base_layout  # It's actual QC positions (like Evosep, MClass48)
    else:
        qc_layout = tech_qc_layouts.get(sampler_base, {})

    # Get output format from combinations
    combo_row = combinations_df.filter(
        (pl.col("instrument") == instr) & (pl.col("sampler") == sampler_key)
    )
    output_format_key = combo_row["output_format"][0] if len(combo_row) > 0 else None
    output_format = output_formats_data.get(output_format_key, {}) if output_format_key else {}

    # Get pattern config
    pattern_config = patterns_data.get(tech, {}).get(pattern_key, {}) if pattern_key else {}

    # Get samples for this technology (private, used only in selected_config)
    _tech_samples = samples_data.get(tech, {})

    # Get methods file
    instr_row = instruments_df.filter(
        (pl.col("technology") == tech) & (pl.col("instrument") == instr)
    )
    _methods_file = instr_row["methods_file"][0] if len(instr_row) > 0 else None
    _methods_path = store.config_dir / _methods_file if _methods_file else None
    _methods_preview = None
    if _methods_path and _methods_path.exists():
        try:
            _methods_df = store.get_methods(_methods_path)
            _methods_preview = _methods_df.head(10)
        except Exception:
            _methods_preview = None

    selected_config = {
        "tech": tech,
        "instr": instr,
        "sampler_key": sampler_key,
        "pattern_key": pattern_key,
        "merged_sampler": merged_sampler,
        "qc_layout": qc_layout,
        "output_format_key": output_format_key,
        "output_format": output_format,
        "pattern_config": pattern_config,
        "tech_samples": _tech_samples,
        "methods_file": _methods_file,
        "methods_preview": _methods_preview,
    }
    return (selected_config,)


@app.cell
def _(
    technology_dropdown,
    instrument_dropdown,
    sampler_dropdown,
    pattern_dropdown,
    sample_dropdown,
    tech_samples,
    selected_config,
):
    # Helper: dict to DataFrame for table display
    def _dict_to_df(d, key_col="Key", val_col="Value"):
        rows = []
        for k, v in d.items():
            if isinstance(v, dict):
                # Evosep-style: {tray, position_start, position_end}
                v = f"tray {v.get('tray')}: {v.get('position_start')}-{v.get('position_end')}"
            elif isinstance(v, list):
                v = ", ".join(str(x) for x in v)
            rows.append({key_col: k, val_col: str(v)})
        return pl.DataFrame(rows) if rows else None

    # Sampler table
    _sampler_df = _dict_to_df(selected_config["merged_sampler"], "Property", "Value")

    # QC positions table
    _qc_df = _dict_to_df(selected_config["qc_layout"], "Sample", "Position")

    # Pattern table - just convert the whole dict
    _pattern_df = _dict_to_df(selected_config["pattern_config"], "Property", "Value") if selected_config["pattern_config"] else None

    # Selected sample details
    _selected_sample = tech_samples.get(sample_dropdown.value, {}) if sample_dropdown.value else {}
    _sample_df = _dict_to_df(_selected_sample, "Property", "Value") if _selected_sample else None

    # Output format as table
    _fmt = selected_config["output_format"]
    _output_df = _dict_to_df(_fmt.get("columns", {}), "Output Column", "Internal Field") if _fmt else None

    # Dropdowns row 1: main config
    _dropdowns1 = mo.hstack([
        technology_dropdown,
        instrument_dropdown,
        sampler_dropdown,
        pattern_dropdown,
    ], gap=1)

    # Build view
    def _section(title, df):
        return mo.vstack([
            mo.md(f"### {title}"),
            mo.ui.table(df, selection=None) if df is not None else mo.md("_None_")
        ])

    # Sample section with dropdown
    _sample_section = mo.vstack([
        mo.hstack([mo.md("### Sample"), sample_dropdown], justify="start", gap=1),
        mo.ui.table(_sample_df, selection=None) if _sample_df is not None else mo.md("_Select a sample_"),
    ])

    viewer_content = mo.vstack([
        mo.md("## Configuration Viewer"),
        _dropdowns1,
        mo.md("---"),
        mo.hstack([
            _section(f"Pattern: {selected_config['pattern_key']}", _pattern_df),
            _section("QC Positions", _qc_df),
        ], widths="equal", gap=2),
        mo.md("---"),
        mo.hstack([
            _section(f"Sampler: {selected_config['sampler_key']}", _sampler_df),
            _sample_section,
        ], widths="equal", gap=2),
        mo.md("---"),
        mo.hstack([
            _section(f"Output: {selected_config['output_format_key']}", _output_df),
            _section(f"Methods: {selected_config['methods_file'] or '—'}", selected_config["methods_preview"]),
        ], widths="equal", gap=2),
    ])
    return (viewer_content,)


@app.cell
def _(viewer_content):
    visualize_tab = viewer_content
    return (visualize_tab,)


@app.cell
def _(
    combinations_tab,
    instruments_tab,
    methods_tab,
    output_formats_tab,
    patterns_tab,
    qc_layouts_tab,
    samplers_tab,
    samples_tab,
    visualize_tab,
):
    tabs = mo.ui.tabs({
        "Overview": visualize_tab,
        "Instruments": instruments_tab,
        "Samplers": samplers_tab,
        "Samples": samples_tab,
        "Patterns": patterns_tab,
        "QC Layouts": qc_layouts_tab,
        "Combinations": combinations_tab,
        "Output Formats": output_formats_tab,
        "Methods": methods_tab,
    })
    return (tabs,)


@app.cell
def _(tabs):
    tabs
    return




@app.cell
def _():
    validate_button = mo.ui.run_button(label="Validate", kind="neutral")
    save_button = mo.ui.run_button(label="Save All", kind="success")
    return save_button, validate_button


@app.cell
def _(save_button, validate_button):
    mo.hstack([validate_button, save_button], justify="start")
    return


@app.cell
def _(
    combinations_editor,
    instrument_patterns_editor,
    instruments_editor,
    methods_dropdown,
    methods_editor,
    methods_options,
    output_formats_editor,
    patterns_editor,
    qc_layouts_grid_editor,
    qc_layouts_evosep_editor,
    sampler_editor,
    samples_editor,
    store,
    validate_button,
):
    mo.stop(not validate_button.value)

    # Store all editor values in ConfigStore for validation
    store.set_instruments(instruments_editor.value)
    store.set_samples(samples_editor.value)
    store.set_qc_layouts_grid(qc_layouts_grid_editor.value)
    store.set_qc_layouts_evosep(qc_layouts_evosep_editor.value)
    store.set_instrument_patterns(instrument_patterns_editor.value)
    store.set_combinations(combinations_editor.value)
    store.set_sampler_toml(sampler_editor.value)
    store.set_queue_patterns_toml(patterns_editor.value)
    store.set_output_formats_toml(output_formats_editor.value)
    if methods_dropdown.value:
        _methods_path = methods_options[methods_dropdown.value]
        store.set_methods(_methods_path, methods_editor.value)

    # Validate using ConfigBundle
    try:
        store.validate()
        validation_result = mo.callout(
            mo.md("**All validations passed!**"),
            kind="success",
        )
    except Exception as e:
        validation_result = mo.callout(
            mo.vstack([
                mo.md("**Validation failed:**"),
                mo.md(f"```\n{e}\n```"),
            ]),
            kind="danger",
        )

    validation_result
    return


@app.cell
def _(
    combinations_editor,
    instrument_patterns_editor,
    instruments_editor,
    methods_dropdown,
    methods_editor,
    methods_options,
    output_formats_editor,
    patterns_editor,
    qc_layouts_grid_editor,
    qc_layouts_evosep_editor,
    sampler_editor,
    samples_editor,
    save_button,
    store,
):
    mo.stop(not save_button.value)

    # Store all editor values in ConfigStore
    store.set_instruments(instruments_editor.value)
    store.set_samples(samples_editor.value)
    store.set_qc_layouts_grid(qc_layouts_grid_editor.value)
    store.set_qc_layouts_evosep(qc_layouts_evosep_editor.value)
    store.set_instrument_patterns(instrument_patterns_editor.value)
    store.set_combinations(combinations_editor.value)
    store.set_sampler_toml(sampler_editor.value)
    store.set_queue_patterns_toml(patterns_editor.value)
    store.set_output_formats_toml(output_formats_editor.value)

    # Store selected methods file if any
    if methods_dropdown.value:
        _methods_path = methods_options[methods_dropdown.value]
        store.set_methods(_methods_path, methods_editor.value)

    # Write all changes to disk (validates via ConfigBundle first)
    try:
        saved_files = store.save()
        save_result = mo.callout(
            mo.md(f"**Saved {len(saved_files)} file(s) successfully!**"),
            kind="success",
        )
    except Exception as e:
        save_result = mo.callout(
            mo.vstack([
                mo.md("**Cannot save: validation failed:**"),
                mo.md(f"```\n{e}\n```"),
            ]),
            kind="danger",
        )

    save_result
    return


if __name__ == "__main__":
    app.run()
