import marimo

__generated_with = "0.18.4"
app = marimo.App(width="full")

with app.setup:
    import os
    from pathlib import Path

    import marimo as mo
    import polars as pl
    import tomli_w
    from loguru import logger

    from qg.logging_setup import configure_logging

    configure_logging()

    from qg.apps import editor_core
    from qg.bfabric_utils import SessionError, resolve_app_session
    from qg.config_models.loader import qg_configuration


# =============================================================================
# Authentication — strict employee gate
# =============================================================================


@app.cell
def _():
    try:
        _session = resolve_app_session(
            mo.app_meta().request,
            allow_unauthenticated=os.environ.get("QG_ALLOW_UNAUTHENTICATED") == "1",
        )
    except SessionError as _exc:
        mo.stop(True, mo.callout(mo.md(f"**{_exc.message}**"), kind="danger"))
    mo.stop(
        not _session.is_employee,
        mo.callout(mo.md("**Config editor is restricted to FGCZ employees.**"), kind="danger"),
    )
    authenticated_login = _session.client.auth.login
    mo.md(_session.banner_message)
    return (authenticated_login,)


# =============================================================================
# Helper functions (must be in a cell, not app.setup, for marimo name resolution)
# =============================================================================


@app.cell
def _():
    # Shared editor substance (compact_toml, section contracts, reconstruction) lives
    # in qg.apps.editor_core, used by both this marimo editor and the Dash editor.
    compact_toml = editor_core.compact_toml

    def build_config_from_editors(editors, methods, *, preserve_comments=False):
        """Build a validated QGConfiguration from editor widget values.

        Adapts the marimo editor's per-widget values (CSV tables as DataFrames, TOML
        as strings, one active methods table) to the framework-neutral core: the full
        methods store starts from the loaded config and the single active table is
        overlaid before reconstruction.
        """
        csv_tables = {name: editors[name] for name in editor_core.CSV_TABLE_COLUMNS}
        toml = {name: editors[name] for name in editor_core.TOML_CONFIG_CLASSES}
        methods_store = editor_core.methods_store_from_config(methods)
        methods_df = editors.get("methods_editor_df")
        if methods_df is not None and not methods_df.is_empty():
            key = editor_core.method_key(editors["methods_tech_area"], editors["methods_instrument"])
            methods_store[key] = methods_df.to_dicts()
        return editor_core.config_from_dataframes(
            csv_tables, toml, methods_store, preserve_comments=preserve_comments
        )

    def error_callout(title: str, error: Exception):
        """Build a danger callout with title and formatted error message."""
        return mo.callout(
            mo.vstack([mo.md(f"**{title}**"), mo.md(f"```\n{error}\n```")]),
            kind="danger",
        )

    return build_config_from_editors, compact_toml, error_callout


@app.cell
def _():
    # Config directory — from CLI arg or default
    cli_args = mo.cli_args()
    config_dir = (
        Path(cli_args["config-dir"])
        if "config-dir" in cli_args
        else Path(__file__).parent.parent.parent.parent / "qg_configs"
    )
    cfg = qg_configuration(config_dir)
    # Capture original contents for in-memory diff (review mode)
    original_contents = cfg.serialize_all()
    REVIEW_MODE_STARTUP = "no-review" not in cli_args
    logger.info("Config editor started | config_dir={} | review_mode={}", config_dir, REVIEW_MODE_STARTUP)
    return cfg, config_dir, original_contents


@app.cell
def _():
    mo.md("""
    # Queue Generation Config Editor
    """)
    return


# =============================================================================
# CSV & TOML Editors (initialized once from cfg — never re-created)
# =============================================================================


@app.cell
def _(cfg, compact_toml):
    instruments_editor = mo.ui.data_editor(cfg.instruments.to_table(), label="Instruments")
    output_formats_editor = mo.ui.code_editor(
        cfg.output_formats.header_comments + compact_toml(tomli_w.dumps(cfg.output_formats.to_dict())),
        language="toml",
        min_height=400,
    )
    formatting_tab = mo.vstack(
        [
            mo.md("### Instruments (instruments.csv)"),
            instruments_editor,
            mo.md("### Output Formats (output_formats.toml)"),
            output_formats_editor,
        ]
    )
    return formatting_tab, instruments_editor, output_formats_editor


@app.cell
def _(cfg):
    samples_editor = mo.ui.data_editor(cfg.samples.to_table(), label="Samples")
    samples_tab = mo.vstack(
        [
            mo.md("### Samples (samples.csv)"),
            samples_editor,
        ]
    )
    return samples_editor, samples_tab


@app.cell
def _(cfg):
    qc_layouts_well_editor = mo.ui.data_editor(cfg.qc_layouts_well.to_table(), label="QC Layouts - Well Plates")
    qc_layouts_well_tab = mo.vstack(
        [
            mo.md("### QC Layouts Well (qc_layouts_well.csv)"),
            mo.md("_Columns: tech_area, qc_layout_name, plate_layout, sample_id, tray, row, col_"),
            qc_layouts_well_editor,
        ]
    )
    return qc_layouts_well_editor, qc_layouts_well_tab


@app.cell
def _(cfg):
    qc_layouts_tip_editor = mo.ui.data_editor(cfg.qc_layouts_tip.to_table(), label="QC Layouts - Tip Plates")
    qc_layouts_tip_tab = mo.vstack(
        [
            mo.md("### QC Layouts Tip (qc_layouts_tip.csv)"),
            mo.md("_Columns: tech_area, qc_layout_name, plate_layout, sample_id, tray, position_start, position_end_"),
            qc_layouts_tip_editor,
        ]
    )
    return qc_layouts_tip_editor, qc_layouts_tip_tab


@app.cell
def _(cfg):
    instrument_configs_editor = mo.ui.data_editor(cfg.instrument_configs.to_table(), label="Instrument Configs")
    instrument_config_tab = mo.vstack(
        [
            mo.md("### Instrument Configs (instrument_config.csv)"),
            mo.md("_Valid (instrument, sampler, output_format, default_pattern) combinations_"),
            instrument_configs_editor,
        ]
    )
    return instrument_configs_editor, instrument_config_tab


@app.cell
def _(cfg, compact_toml):
    tech_area_defaults_editor = mo.ui.code_editor(
        cfg.tech_area_defaults.header_comments + compact_toml(tomli_w.dumps(cfg.tech_area_defaults.to_dict())),
        language="toml",
        min_height=200,
    )
    tech_area_defaults_tab = mo.vstack(
        [
            mo.md("### Tech Area Defaults (tech_area_defaults.toml)"),
            mo.md(
                "_Per-tech-area UI defaults: one TOML table per tech_area with `default_user` "
                '(empty ⇒ logged-in user), `default_polarities` as a list (e.g. `["pos", "neg"]`), '
                "`bfabric_areas` listing the B-Fabric Area values whose orders belong to this "
                "tech area (used to filter the order browser; empty ⇒ no filtering), and "
                '`sample_name_suffixes` listing the prep-type labels offered in the sample-name '
                'suffix dropdown (e.g. `["enriched", "total", "lip"]`; empty ⇒ only the "none" no-op)._'
            ),
            tech_area_defaults_editor,
        ]
    )
    return tech_area_defaults_editor, tech_area_defaults_tab


@app.cell
def _(cfg, compact_toml):
    samplers_editor = mo.ui.code_editor(
        cfg.samplers.header_comments + compact_toml(tomli_w.dumps(cfg.samplers.to_dict())),
        language="toml",
        min_height=200,
    )
    plate_layouts_editor = mo.ui.code_editor(
        cfg.plate_layouts.header_comments + compact_toml(tomli_w.dumps(cfg.plate_layouts.to_dict())),
        language="toml",
        min_height=200,
    )
    sampler_plate_layouts_editor = mo.ui.data_editor(
        cfg.sampler_plate_layouts.to_table(), label="Sampler Plate Layouts"
    )
    position_tab = mo.vstack(
        [
            mo.md("### Samplers (sampler.toml)"),
            samplers_editor,
            mo.md("### Plate Layouts (plate_layouts.toml)"),
            plate_layouts_editor,
            mo.md("### Sampler Plate Layouts (sampler_plate_layouts.csv)"),
            sampler_plate_layouts_editor,
        ]
    )
    return plate_layouts_editor, position_tab, sampler_plate_layouts_editor, samplers_editor


# =============================================================================
# Collected editor values (single dependency for validate/save/review cells)
# =============================================================================


@app.cell
def _(
    instruments_editor,
    samples_editor,
    qc_layouts_well_editor,
    qc_layouts_tip_editor,
    instrument_configs_editor,
    tech_area_defaults_editor,
    sampler_plate_layouts_editor,
    samplers_editor,
    plate_layouts_editor,
    queue_patterns_editor,
    output_formats_editor,
    methods_editor,
    tech_area,
    instrument,
):
    editor_values = {
        "instruments": instruments_editor.value,
        "samples": samples_editor.value,
        "qc_layouts_well": qc_layouts_well_editor.value,
        "qc_layouts_tip": qc_layouts_tip_editor.value,
        "instrument_configs": instrument_configs_editor.value,
        "tech_area_defaults": tech_area_defaults_editor.value,
        "sampler_plate_layouts": sampler_plate_layouts_editor.value,
        "samplers": samplers_editor.value,
        "plate_layouts": plate_layouts_editor.value,
        "queue_patterns": queue_patterns_editor.value,
        "output_formats": output_formats_editor.value,
        "methods_editor_df": methods_editor.value,
        "methods_tech_area": tech_area,
        "methods_instrument": instrument,
    }
    return (editor_values,)


@app.cell
def _(cfg, compact_toml):
    queue_patterns_editor = mo.ui.code_editor(
        cfg.queue_patterns.header_comments + compact_toml(tomli_w.dumps(cfg.queue_patterns.to_dict())),
        language="toml",
        min_height=400,
    )
    structure_tab = mo.vstack(
        [
            mo.md("### Queue Patterns (queue_patterns.toml)"),
            queue_patterns_editor,
        ]
    )
    return queue_patterns_editor, structure_tab


# =============================================================================
# Methods Editor
# =============================================================================


@app.cell
def _(cfg):
    # Build dropdown options from instruments
    methods_options = {}
    for instr in cfg.instruments.instruments:
        key = f"{instr.tech_area}/{instr.instrument}"
        methods_options[key] = (instr.tech_area, instr.instrument)

    methods_dropdown = mo.ui.dropdown(
        options=list(methods_options.keys()),
        value=list(methods_options.keys())[0] if methods_options else None,
        label="Select instrument",
    )
    return methods_dropdown, methods_options


@app.cell
def _(cfg, methods_dropdown, methods_options):
    mo.stop(not methods_dropdown.value)
    tech_area, instrument = methods_options[methods_dropdown.value]
    instr_methods = cfg.methods.get_methods(tech_area, instrument)
    methods_df = instr_methods.to_table() if instr_methods else pl.DataFrame()
    methods_editor = mo.ui.data_editor(methods_df, label="Methods")
    methods_tab = mo.vstack(
        [
            mo.md("### Methods"),
            methods_dropdown,
            methods_editor,
        ]
    )
    return methods_editor, methods_tab, tech_area, instrument


# =============================================================================
# Overview Tab - Cascading Selection using Denormalized Table
# =============================================================================


@app.cell
def _(cfg):
    # Get denormalized overview table
    overview_df = cfg.to_overview_table()
    return (overview_df,)


@app.cell
def _(overview_df):
    # Step 1: tech_area dropdown
    tech_options = overview_df["tech_area"].unique().sort().to_list()
    tech_dropdown = mo.ui.dropdown(
        options=tech_options,
        value=tech_options[0] if tech_options else None,
        label="Technology",
    )
    return (tech_dropdown,)


@app.cell
def _(tech_dropdown, overview_df):
    # Step 2: instrument dropdown (filtered by tech_area)
    mo.stop(not tech_dropdown.value)
    filtered_overview = overview_df.filter(pl.col("tech_area") == tech_dropdown.value)
    instr_options = filtered_overview["instrument"].unique().sort().to_list()
    instr_dropdown = mo.ui.dropdown(
        options=instr_options,
        value=instr_options[0] if instr_options else None,
        label="Instrument",
    )
    return instr_dropdown, filtered_overview


@app.cell
def _(instr_dropdown, filtered_overview):
    # Step 3: sampler dropdown (filtered by instrument)
    mo.stop(not instr_dropdown.value)
    filtered_by_instr = filtered_overview.filter(pl.col("instrument") == instr_dropdown.value)
    sampler_options = filtered_by_instr["sampler"].unique().sort().to_list()
    sampler_dropdown = mo.ui.dropdown(
        options=sampler_options,
        value=sampler_options[0] if sampler_options else None,
        label="Sampler",
    )
    return sampler_dropdown, filtered_by_instr


@app.cell
def _(sampler_dropdown, filtered_by_instr):
    # Step 4: plate_layout dropdown (filtered by sampler)
    mo.stop(not sampler_dropdown.value)
    filtered_by_sampler = filtered_by_instr.filter(pl.col("sampler") == sampler_dropdown.value)
    # Create options as "plate_layout (queue_type)"
    layout_rows = filtered_by_sampler.select(["plate_layout", "queue_type"]).unique().sort("plate_layout")
    layout_options = [f"{row['plate_layout']} ({row['queue_type']})" for row in layout_rows.iter_rows(named=True)]
    plate_layout_dropdown = mo.ui.dropdown(
        options=layout_options,
        value=layout_options[0] if layout_options else None,
        label="Plate Layout",
    )
    return plate_layout_dropdown, filtered_by_sampler


@app.cell
def _(plate_layout_dropdown, filtered_by_sampler):
    # Step 5: qc_layout dropdown (filtered by plate_layout)
    mo.stop(not plate_layout_dropdown.value)
    _plate_layout = plate_layout_dropdown.value.split(" (")[0]
    filtered_by_plate = filtered_by_sampler.filter(pl.col("plate_layout") == _plate_layout)
    qc_layout_options = filtered_by_plate["qc_layout_name"].unique().sort().to_list()
    qc_layout_dropdown = mo.ui.dropdown(
        options=qc_layout_options,
        value=qc_layout_options[0] if qc_layout_options else None,
        label="QC Layout",
    )
    return qc_layout_dropdown, filtered_by_plate


@app.cell
def _(qc_layout_dropdown, filtered_by_plate):
    # Step 6: pattern dropdown (filtered by qc_layout compatibility)
    mo.stop(not qc_layout_dropdown.value)
    _filtered = filtered_by_plate.filter(pl.col("qc_layout_name") == qc_layout_dropdown.value)
    pattern_options = _filtered["pattern_name"].unique().sort().to_list()
    pattern_dropdown = mo.ui.dropdown(
        options=pattern_options,
        value=pattern_options[0] if pattern_options else None,
        label="Pattern",
    )
    return (pattern_dropdown,)


@app.cell
def _(
    tech_dropdown,
    instr_dropdown,
    sampler_dropdown,
    plate_layout_dropdown,
    qc_layout_dropdown,
    pattern_dropdown,
    cfg,
):
    # Build selected config view
    mo.stop(not all([tech_dropdown.value, instr_dropdown.value, sampler_dropdown.value]))

    _tech = tech_dropdown.value
    _instr = instr_dropdown.value
    _sampler = sampler_dropdown.value
    _pattern_name = pattern_dropdown.value
    _plate_layout = plate_layout_dropdown.value.split(" (")[0] if plate_layout_dropdown.value else ""
    _qc_layout_name = qc_layout_dropdown.value or ""

    # Get sampler config
    _sampler_cfg = cfg.samplers.get_sampler(_sampler)
    _sampler_info = {
        "trays": _sampler_cfg.trays if _sampler_cfg else [],
        "position_fun": _sampler_cfg.position_fun if _sampler_cfg else "",
    }

    # Get pattern config
    _pattern_cfg = cfg.queue_patterns.get_pattern(_tech, _pattern_name) if _pattern_name else None
    _pattern_info = {}
    if _pattern_cfg:
        _pattern_info = {
            "run_QC_after_n_samples": _pattern_cfg.run_QC_after_n_samples,
            "start": _pattern_cfg.start,
            "middle": _pattern_cfg.middle,
            "end": _pattern_cfg.end,
        }

    # Get instrument config
    _instr_cfg = cfg.instrument_configs.get_config(_instr, _sampler)
    _instr_info = {
        "output_format": _instr_cfg.output_format if _instr_cfg else "",
        "default_pattern": _instr_cfg.default_pattern if _instr_cfg else "",
    }

    # Get methods
    _methods = cfg.methods.get_methods(_tech, _instr)
    _methods_preview = _methods.to_table().head(5) if _methods else None

    # Get QC layout samples
    _sampler_obj = cfg.samplers.get_sampler(_sampler)
    if _sampler_obj and _sampler_obj.is_tip:
        _qc_samples = cfg.qc_layouts_tip.get_samples(_tech, _qc_layout_name, _plate_layout)
    else:
        _qc_samples = cfg.qc_layouts_well.get_samples(_tech, _qc_layout_name, _plate_layout)
    _qc_preview = pl.DataFrame([s.model_dump() for s in _qc_samples]) if _qc_samples else None

    selected_cfg = {
        "tech": _tech,
        "instrument": _instr,
        "sampler": _sampler,
        "pattern": _pattern_name,
        "sampler_info": _sampler_info,
        "pattern_info": _pattern_info,
        "instr_info": _instr_info,
        "methods_preview": _methods_preview,
        "qc_layout_name": _qc_layout_name,
        "qc_preview": _qc_preview,
    }
    return (selected_cfg,)


@app.cell
def _(
    tech_dropdown,
    instr_dropdown,
    sampler_dropdown,
    plate_layout_dropdown,
    qc_layout_dropdown,
    pattern_dropdown,
    selected_cfg,
):
    def _dict_to_table(d, key_col="Property", val_col="Value"):
        rows = []
        for k, v in d.items():
            if isinstance(v, list):
                v = ", ".join(str(x) for x in v)
            rows.append({key_col: k, val_col: str(v)})
        return pl.DataFrame(rows) if rows else None

    _dropdowns = mo.hstack(
        [tech_dropdown, instr_dropdown, sampler_dropdown, plate_layout_dropdown, qc_layout_dropdown, pattern_dropdown],
        gap=1,
    )

    _sampler_df = _dict_to_table(selected_cfg["sampler_info"])
    _pattern_df = _dict_to_table(selected_cfg["pattern_info"])
    _instr_df = _dict_to_table(selected_cfg["instr_info"])

    def _section(title, df):
        return mo.vstack(
            [
                mo.md(f"**{title}**"),
                mo.ui.table(df, selection=None) if df is not None else mo.md("_None_"),
            ]
        )

    overview_tab = mo.vstack(
        [
            mo.md("## Configuration Overview"),
            _dropdowns,
            mo.md("---"),
            mo.hstack(
                [
                    _section(f"Position → Samplers: {selected_cfg['sampler']}", _sampler_df),
                    _section(f"Queue Patterns: {selected_cfg['pattern']}", _pattern_df),
                ],
                widths="equal",
                gap=2,
            ),
            mo.md("---"),
            mo.hstack(
                [
                    _section("UI → Instrument Configs", _instr_df),
                    _section(f"QC Layouts → {selected_cfg['qc_layout_name']}", selected_cfg["qc_preview"]),
                ],
                widths="equal",
                gap=2,
            ),
            mo.md("---"),
            _section("Methods", selected_cfg["methods_preview"]),
        ]
    )
    return (overview_tab,)


# =============================================================================
# All Combinations Tab - Full Overview Table
# =============================================================================


@app.cell
def _(overview_df):
    all_combinations_tab = mo.vstack(
        [
            mo.md("## All Valid Configuration Combinations"),
            mo.md(f"_Denormalized view: {overview_df.shape[0]} rows × {overview_df.shape[1]} columns_"),
            mo.ui.table(overview_df, selection=None, page_size=20),
        ]
    )
    return (all_combinations_tab,)


# =============================================================================
# Main Tabs UI
# =============================================================================


@app.cell
def _():
    _section_names = [
        "Overview",
        "Queue Patterns",
        "Samples",
        "QC Layouts Well",
        "QC Layouts Tip",
        "Instrument Config",
        "Tech Area Defaults",
        "Formatting",
        "Position",
        "Methods",
        "All Combinations",
    ]
    section_selector = mo.ui.radio(
        options=_section_names,
        value="Overview",
        inline=True,
    )
    return (section_selector,)


@app.cell
def _(
    section_selector,
    overview_tab,
    all_combinations_tab,
    formatting_tab,
    position_tab,
    qc_layouts_well_tab,
    qc_layouts_tip_tab,
    structure_tab,
    samples_tab,
    methods_tab,
    instrument_config_tab,
    tech_area_defaults_tab,
):
    _sections = {
        "Overview": overview_tab,
        "All Combinations": all_combinations_tab,
        "Formatting": formatting_tab,
        "Position": position_tab,
        "QC Layouts Well": qc_layouts_well_tab,
        "QC Layouts Tip": qc_layouts_tip_tab,
        "Queue Patterns": structure_tab,
        "Samples": samples_tab,
        "Methods": methods_tab,
        "Instrument Config": instrument_config_tab,
        "Tech Area Defaults": tech_area_defaults_tab,
    }
    # CSS display:none keeps all widgets in the DOM (data_editor preserves visual state)
    _panels = [
        mo.md(f'<div style="display: {"block" if _name == section_selector.value else "none"}">{_content}</div>')
        for _name, _content in _sections.items()
    ]
    mo.vstack([section_selector, *_panels])
    return


# =============================================================================
# Save/Validate Buttons
# =============================================================================


@app.cell
def _():
    _args = mo.cli_args()
    # Review ON by default; pass --no-review to disable
    REVIEW_MODE = "no-review" not in _args
    return (REVIEW_MODE,)


@app.cell
def _(REVIEW_MODE):
    validate_button = mo.ui.run_button(label="Validate", kind="neutral")
    save_button = mo.ui.run_button(label="Save All", kind="success") if not REVIEW_MODE else None
    return save_button, validate_button


@app.cell
def _(save_button, validate_button):
    buttons = [validate_button]
    if save_button is not None:
        buttons.append(save_button)
    mo.hstack(buttons, justify="start")
    return


# =============================================================================
# Validation Logic
# =============================================================================


@app.cell
def _(build_config_from_editors, error_callout, cfg, editor_values, validate_button):
    mo.stop(not validate_button.value)

    try:
        build_config_from_editors(editor_values, cfg.methods)
        validation_result = mo.callout(mo.md("**All validations passed!**"), kind="success")
        logger.info("Validation passed")
    except Exception as e:
        validation_result = error_callout("Validation failed:", e)
        logger.warning("Validation failed: {}", e)

    validation_result
    return


# =============================================================================
# Save Logic
# =============================================================================


@app.cell
def _(build_config_from_editors, error_callout, cfg, config_dir, original_contents, editor_values, save_button):
    mo.stop(save_button is None or not save_button.value)

    try:
        cfg_to_save = build_config_from_editors(editor_values, cfg.methods, preserve_comments=True)
        written = cfg_to_save.write_all(config_dir, original_contents=original_contents)
        if written:
            save_result = mo.callout(
                mo.md(f"**Saved {len(written)} changed file(s):** {', '.join(written)}"),
                kind="success",
            )
        else:
            save_result = mo.callout(
                mo.md("**Nothing changed** — no config files differ from the loaded version."),
                kind="info",
            )
        logger.info("Saved {} changed config file(s) to {}", len(written), config_dir)
    except Exception as e:
        save_result = error_callout("Cannot save:", e)
        logger.error("Save failed: {}", e)

    save_result
    return


# =============================================================================
# Submit for Review (GitLab MR)
# =============================================================================


@app.cell
def _(REVIEW_MODE):
    # Check if GitLab integration is available (only in review mode)
    gitlab_available = False
    gitlab_unavailable_reason = ""
    if REVIEW_MODE:
        try:
            from qg.gitlab.config_bridge import submit_config_changes as _submit  # noqa: F401
            from qg.gitlab.settings import load_gitlab_settings

            load_gitlab_settings()
            gitlab_available = True
        except FileNotFoundError as e:
            gitlab_unavailable_reason = str(e)
        except ValueError as e:
            gitlab_unavailable_reason = str(e)
    return gitlab_available, gitlab_unavailable_reason


@app.cell
def _(gitlab_available):
    if gitlab_available:
        review_description_input = mo.ui.text(
            label="Change description",
            placeholder="what did you change and why?",
            full_width=True,
        )
        submit_review_button = mo.ui.run_button(label="Submit for Review", kind="warn")
    else:
        review_description_input = None
        submit_review_button = None
    return review_description_input, submit_review_button


@app.cell
def _(
    authenticated_login,
    gitlab_available,
    gitlab_unavailable_reason,
    review_description_input,
    submit_review_button,
):
    if gitlab_available:
        review_ui = mo.vstack(
            [
                mo.md("### Submit for Review"),
                mo.md(
                    f"_Submits changes as a GitLab MR. Authored as **{authenticated_login}**;"
                    f" opened by the `qg-config-bot` user._"
                ),
                review_description_input,
                submit_review_button,
            ]
        )
    elif gitlab_unavailable_reason:
        review_ui = mo.callout(
            mo.md(f"**Submit for Review unavailable:** {gitlab_unavailable_reason}"),
            kind="warn",
        )
    else:
        review_ui = mo.md("")

    review_ui
    return


@app.cell
def _(
    authenticated_login,
    build_config_from_editors,
    error_callout,
    gitlab_available,
    cfg,
    original_contents,
    editor_values,
    review_description_input,
    submit_review_button,
):
    mo.stop(not gitlab_available or not submit_review_button or not submit_review_button.value)

    from qg.gitlab.config_bridge import submit_config_changes

    _description = review_description_input.value if review_description_input else ""

    if not _description:
        review_result = mo.callout(
            mo.md("**Please fill in a change description before submitting.**"),
            kind="warn",
        )
    else:
        try:
            _cfg_to_submit = build_config_from_editors(editor_values, cfg.methods, preserve_comments=True)

            # In-memory diff: serialize edited config, compare to original snapshot
            _edited_contents = _cfg_to_submit.serialize_all()
            _changed_count = sum(1 for k in _edited_contents if _edited_contents[k] != original_contents.get(k))
            logger.info("Review submitted | author={} | changed_files={}", authenticated_login, _changed_count)
            mr_url = submit_config_changes(
                original_contents, _edited_contents, author=authenticated_login, description=_description
            )
            if mr_url:
                review_result = mo.callout(
                    mo.md(f"**Merge request created!** [Open MR]({mr_url})"),
                    kind="success",
                )
                logger.info("MR created: {}", mr_url)
            else:
                review_result = mo.callout(
                    mo.md("**Nothing changed** -- no config files differ from the current version."),
                    kind="info",
                )
                logger.info("Review submitted but no files changed")
        except Exception as e:
            logger.exception("GitLab submission failed")
            review_result = error_callout("Submission failed:", e)

    review_result
    return


if __name__ == "__main__":
    app.run()
