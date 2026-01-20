import marimo

__generated_with = "0.18.4"
app = marimo.App(width="full")

with app.setup:
    import marimo as mo
    import polars as pl
    import tomllib
    import tomli_w
    from pathlib import Path


@app.cell
def _():
    # Config directory is qg_configs/ relative to the project root
    # Navigate from src/qg/apps/ up to project root, then to qg_configs/
    CONFIG_DIR = Path(__file__).parent.parent.parent.parent / "qg_configs"
    return (CONFIG_DIR,)


@app.cell
def _():
    mo.md("""
    # Queue Generation Config Editor
    """)
    return

# TODO: replace with get_core_config_dir() # return Path
@app.cell
def _(CONFIG_DIR):
    # Core configs
    instruments_df = pl.read_csv(CONFIG_DIR / "core" / "instruments.csv")
    instruments_editor = mo.ui.data_editor(instruments_df, label="Instruments")
    return instruments_df, instruments_editor

# TODO: replace with get_ui_config_dir() # return Path
@app.cell
def _(CONFIG_DIR):
    # UI configs
    instrument_patterns_df = pl.read_csv(CONFIG_DIR / "ui" / "instrument_patterns.csv")
    instrument_patterns_editor = mo.ui.data_editor(
        instrument_patterns_df, label="Instrument Patterns"
    )
    return instrument_patterns_df, instrument_patterns_editor


@app.cell
def _(CONFIG_DIR):
    # UI configs
    combinations_df = pl.read_csv(CONFIG_DIR / "ui" / "combinations.csv")
    combinations_editor = mo.ui.data_editor(combinations_df, label="Combinations")
    return combinations_df, combinations_editor


@app.cell
def _(CONFIG_DIR):
    # Core configs
    sampler_content = (CONFIG_DIR / "core" / "sampler.toml").read_text()
    sampler_editor = mo.ui.code_editor(
        sampler_content, language="toml", min_height=400
    )
    return (sampler_editor,)


@app.cell
def _(CONFIG_DIR):
    # Core configs
    samples_df = pl.read_csv(CONFIG_DIR / "core" / "samples.csv")
    samples_editor = mo.ui.data_editor(samples_df, label="Samples")
    return samples_df, samples_editor


@app.cell
def _(CONFIG_DIR):
    # Core configs
    patterns_content = (CONFIG_DIR / "core" / "queue_patterns.toml").read_text()
    patterns_editor = mo.ui.code_editor(
        patterns_content, language="toml", min_height=400
    )
    return (patterns_editor,)


@app.cell
def _(CONFIG_DIR):
    # Core configs
    qc_layouts_content = (CONFIG_DIR / "core" / "qc_layouts.toml").read_text()
    qc_layouts_editor = mo.ui.code_editor(
        qc_layouts_content, language="toml", min_height=400
    )
    return (qc_layouts_editor,)


@app.cell
def _(CONFIG_DIR):
    # Core configs
    output_formats_content = (CONFIG_DIR / "core" / "output_formats.toml").read_text()
    output_formats_editor = mo.ui.code_editor(
        output_formats_content, language="toml", min_height=400
    )
    return (output_formats_editor,)


@app.cell
def _(CONFIG_DIR):
    # Core configs - methods
    methods_files = sorted((CONFIG_DIR / "core").glob("methods/**/*.csv"))
    methods_options = {f.relative_to(CONFIG_DIR).as_posix(): f for f in methods_files}
    methods_dropdown = mo.ui.dropdown(
        options=list(methods_options.keys()),
        value=list(methods_options.keys())[0] if methods_options else None,
        label="Select methods file",
    )
    return methods_dropdown, methods_options


@app.cell
def _(methods_dropdown, methods_options):
    mo.stop(not methods_dropdown.value)
    selected_methods_path = methods_options[methods_dropdown.value]
    methods_df = pl.read_csv(selected_methods_path)
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
def _(qc_layouts_editor):
    qc_layouts_tab = mo.vstack([
        mo.md("### QC Layouts (qc_layouts.toml)"),
        qc_layouts_editor,
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
def _(sampler_editor, samples_editor, patterns_editor, qc_layouts_editor, output_formats_editor):
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

    sampler_data = safe_parse_toml(sampler_editor.value)
    samples_data = samples_df_to_dict(samples_editor.value)
    patterns_data = safe_parse_toml(patterns_editor.value)
    qc_layouts_data = safe_parse_toml(qc_layouts_editor.value)
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
    CONFIG_DIR,
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
    _methods_path = CONFIG_DIR / _methods_file if _methods_file else None
    _methods_preview = None
    if _methods_path and _methods_path.exists():
        try:
            _methods_df = pl.read_csv(_methods_path)
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
    def validate_toml(content: str, name: str) -> list[str]:
        """Validate TOML syntax."""
        errors = []
        try:
            tomllib.loads(content)
        except tomllib.TOMLDecodeError as e:
            errors.append(f"{name}: TOML syntax error - {e}")
        return errors

    def validate_configs(
        instruments_df,
        instrument_patterns_df,
        combinations_df,
        sampler_content,
        samples_df,
        patterns_content,
        qc_layouts_content,
        output_formats_content,
    ) -> tuple[list[str], list[str]]:
        """Validate all configurations. Returns (errors, warnings)."""
        errors = []
        warnings = []

        # Validate TOML syntax
        for content, name in [
            (sampler_content, "sampler.toml"),
            (patterns_content, "queue_patterns.toml"),
            (qc_layouts_content, "qc_layouts.toml"),
            (output_formats_content, "output_formats.toml"),
        ]:
            errors.extend(validate_toml(content, name))

        # If TOML syntax errors, stop here
        if errors:
            return errors, warnings

        # Parse TOML files
        try:
            samplers = tomllib.loads(sampler_content)
            patterns = tomllib.loads(patterns_content)
            qc_layouts = tomllib.loads(qc_layouts_content)
            output_formats = tomllib.loads(output_formats_content)
        except Exception as e:
            errors.append(f"Failed to parse TOML: {e}")
            return errors, warnings

        # Convert samples_df to dict for validation
        samples = {}
        for row in samples_df.iter_rows(named=True):
            tech = row["technology"]
            if tech not in samples:
                samples[tech] = {}
            samples[tech][row["sample_id"]] = row

        # Derive valid technologies from samples.csv (source of truth)
        valid_technologies = set(samples.keys())

        # Validate instruments.csv
        for row in instruments_df.iter_rows(named=True):
            tech = row.get("technology", "")
            if tech not in valid_technologies:
                errors.append(f"instruments.csv: Unknown technology '{tech}' (not in samples.csv)")

        # Validate instrument_patterns.csv references
        instrument_keys = set(
            f"{r['technology']}.{r['instrument']}"
            for r in instruments_df.iter_rows(named=True)
        )
        for row in instrument_patterns_df.iter_rows(named=True):
            tech = row.get("technology", "")
            instr = row.get("instrument", "")
            pattern = row.get("queue_pattern", "")
            key = f"{tech}.{instr}"
            if key not in instrument_keys:
                errors.append(f"instrument_patterns.csv: Unknown instrument {key}")
            # Check pattern exists
            pattern_key = f"{tech}.{pattern}"
            if tech in patterns and pattern not in patterns.get(tech, {}):
                errors.append(
                    f"instrument_patterns.csv: {key} references unknown pattern '{pattern}'"
                )

        # Validate combinations.csv
        sampler_names = set(samplers.keys())
        output_format_names = set(output_formats.keys())
        instrument_names = set(
            r["instrument"] for r in instruments_df.iter_rows(named=True)
        )
        for row in combinations_df.iter_rows(named=True):
            instr = row.get("instrument", "")
            sampler = row.get("sampler", "")
            fmt = row.get("output_format", "")
            if instr not in instrument_names:
                errors.append(f"combinations.csv: Unknown instrument '{instr}'")
            sampler_base = sampler.split(".")[0]
            if sampler_base not in sampler_names:
                errors.append(f"combinations.csv: Unknown sampler '{sampler_base}'")
            if fmt not in output_format_names:
                errors.append(f"combinations.csv: Unknown output_format '{fmt}'")

        return errors, warnings
    return (validate_configs,)


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
    output_formats_editor,
    patterns_editor,
    qc_layouts_editor,
    sampler_editor,
    samples_editor,
    validate_button,
    validate_configs,
):
    mo.stop(not validate_button.value)
    validation_errors, validation_warnings = validate_configs(
        instruments_editor.value,
        instrument_patterns_editor.value,
        combinations_editor.value,
        sampler_editor.value,
        samples_editor.value,
        patterns_editor.value,
        qc_layouts_editor.value,
        output_formats_editor.value,
    )
    if validation_errors:
        validation_result = mo.callout(
            mo.vstack([
                mo.md(f"**Validation failed with {len(validation_errors)} error(s):**"),
                mo.md("\n".join(f"- {e}" for e in validation_errors)),
            ]),
            kind="danger",
        )
    elif validation_warnings:
        validation_result = mo.callout(
            mo.vstack([
                mo.md(f"**Validation passed with {len(validation_warnings)} warning(s):**"),
                mo.md("\n".join(f"- {w}" for w in validation_warnings)),
            ]),
            kind="warn",
        )
    else:
        validation_result = mo.callout(
            mo.md("**All validations passed!**"),
            kind="success",
        )
    validation_result
    return


@app.cell
def _(
    CONFIG_DIR,
    combinations_editor,
    instrument_patterns_editor,
    instruments_editor,
    methods_dropdown,
    methods_editor,
    methods_options,
    output_formats_editor,
    patterns_editor,
    qc_layouts_editor,
    sampler_editor,
    samples_editor,
    save_button,
    validate_configs,
):
    mo.stop(not save_button.value)

    # Validate before saving
    save_errors, _ = validate_configs(
        instruments_editor.value,
        instrument_patterns_editor.value,
        combinations_editor.value,
        sampler_editor.value,
        samples_editor.value,
        patterns_editor.value,
        qc_layouts_editor.value,
        output_formats_editor.value,
    )

    if save_errors:
        save_result = mo.callout(
            mo.md("**Cannot save: validation errors exist. Run Validate first.**"),
            kind="danger",
        )
    else:
        # Save core CSV files
        instruments_editor.value.write_csv(CONFIG_DIR / "core" / "instruments.csv")
        samples_editor.value.write_csv(CONFIG_DIR / "core" / "samples.csv")

        # Save UI CSV files
        instrument_patterns_editor.value.write_csv(CONFIG_DIR / "ui" / "instrument_patterns.csv")
        combinations_editor.value.write_csv(CONFIG_DIR / "ui" / "combinations.csv")

        # Save core TOML files
        (CONFIG_DIR / "core" / "sampler.toml").write_text(sampler_editor.value)
        (CONFIG_DIR / "core" / "queue_patterns.toml").write_text(patterns_editor.value)
        (CONFIG_DIR / "core" / "qc_layouts.toml").write_text(qc_layouts_editor.value)
        (CONFIG_DIR / "core" / "output_formats.toml").write_text(output_formats_editor.value)

        # Save selected methods file
        if methods_dropdown.value:
            methods_path = methods_options[methods_dropdown.value]
            methods_editor.value.write_csv(methods_path)

        save_result = mo.callout(
            mo.md("**All files saved successfully!**"),
            kind="success",
        )

    save_result
    return


if __name__ == "__main__":
    app.run()
