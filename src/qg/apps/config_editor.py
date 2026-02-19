import marimo

__generated_with = "0.18.4"
app = marimo.App(width="full")

with app.setup:
    import re
    import tomllib
    from pathlib import Path

    import marimo as mo
    import polars as pl
    import tomli_w
    from loguru import logger

    from qg.logging_setup import configure_logging

    configure_logging()

    from qg.config_models.formatting import (
        InstrumentsConfig,
        OutputFormatsConfig,
    )
    from qg.config_models.loader import (
        ConfigValidationError,
        QGConfiguration,
        qg_configuration,
        read_header_comments,
    )
    from qg.config_models.positions import (
        PlateLayoutsConfig,
        QCLayoutsTipConfig,
        QCLayoutsWellConfig,
        SamplerPlateLayoutsConfig,
        SamplersConfig,
    )
    from qg.config_models.structure import QueuePatternsConfig, SamplesConfig
    from qg.config_models.ui import InstrumentConfigsConfig

    def compact_toml(toml_str: str) -> str:
        """Convert multiline arrays to inline format for readability."""
        # Match arrays that span multiple lines
        pattern = r"(\w+)\s*=\s*\[\s*\n((?:\s+[^\]]+,?\s*\n)+)\s*\]"

        def replace_array(match):
            key = match.group(1)
            items_block = match.group(2)
            # Extract items, strip whitespace
            items = [item.strip().rstrip(",") for item in items_block.strip().split("\n") if item.strip()]
            return f"{key} = [{', '.join(items)}]"

        return re.sub(pattern, replace_array, toml_str)


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
    # Queue Generation Config Editor (New Config)
    """)
    return


# =============================================================================
# CSV Editors
# =============================================================================


@app.cell
def _(cfg):
    instruments_df = cfg.instruments.to_table()
    instruments_editor = mo.ui.data_editor(instruments_df, label="Instruments")
    return instruments_df, instruments_editor


@app.cell
def _(cfg):
    samples_df = cfg.samples.to_table()
    samples_editor = mo.ui.data_editor(samples_df, label="Samples")
    return samples_df, samples_editor


@app.cell
def _(cfg):
    qc_layouts_well_df = cfg.qc_layouts_well.to_table()
    qc_layouts_well_editor = mo.ui.data_editor(qc_layouts_well_df, label="QC Layouts - Well Plates")
    return qc_layouts_well_df, qc_layouts_well_editor


@app.cell
def _(cfg):
    qc_layouts_tip_df = cfg.qc_layouts_tip.to_table()
    qc_layouts_tip_editor = mo.ui.data_editor(qc_layouts_tip_df, label="QC Layouts - Tip Plates")
    return qc_layouts_tip_df, qc_layouts_tip_editor


@app.cell
def _(cfg):
    instrument_configs_df = cfg.instrument_configs.to_table()
    instrument_configs_editor = mo.ui.data_editor(instrument_configs_df, label="Instrument Configs")
    return instrument_configs_df, instrument_configs_editor


@app.cell
def _(cfg):
    sampler_plate_layouts_df = cfg.sampler_plate_layouts.to_table()
    sampler_plate_layouts_editor = mo.ui.data_editor(sampler_plate_layouts_df, label="Sampler Plate Layouts")
    return sampler_plate_layouts_df, sampler_plate_layouts_editor


# =============================================================================
# TOML Editors
# =============================================================================


@app.cell
def _(cfg, compact_toml):
    samplers_toml = cfg.samplers.header_comments + compact_toml(tomli_w.dumps(cfg.samplers.to_dict()))
    samplers_editor = mo.ui.code_editor(samplers_toml, language="toml", min_height=200)
    return (samplers_editor,)


@app.cell
def _(cfg, compact_toml):
    plate_layouts_toml = cfg.plate_layouts.header_comments + compact_toml(tomli_w.dumps(cfg.plate_layouts.to_dict()))
    plate_layouts_editor = mo.ui.code_editor(plate_layouts_toml, language="toml", min_height=200)
    return (plate_layouts_editor,)


@app.cell
def _(cfg, compact_toml):
    queue_patterns_toml = cfg.queue_patterns.header_comments + compact_toml(tomli_w.dumps(cfg.queue_patterns.to_dict()))
    queue_patterns_editor = mo.ui.code_editor(queue_patterns_toml, language="toml", min_height=400)
    return (queue_patterns_editor,)


@app.cell
def _(cfg, compact_toml):
    output_formats_toml = cfg.output_formats.header_comments + compact_toml(tomli_w.dumps(cfg.output_formats.to_dict()))
    output_formats_editor = mo.ui.code_editor(output_formats_toml, language="toml", min_height=400)
    return (output_formats_editor,)


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
    return methods_editor, tech_area, instrument


# =============================================================================
# Tabs (organized by config folder structure)
# =============================================================================


@app.cell
def _(instruments_editor, output_formats_editor):
    # core/formatting/ - instruments.csv, output_formats.toml
    formatting_tab = mo.vstack(
        [
            mo.md("## core/formatting/"),
            mo.md("### Instruments (instruments.csv)"),
            instruments_editor,
            mo.md("### Output Formats (output_formats.toml)"),
            output_formats_editor,
        ]
    )
    return (formatting_tab,)


@app.cell
def _(
    samplers_editor,
    plate_layouts_editor,
    sampler_plate_layouts_editor,
):
    # core/position/ - sampler.toml, plate_layouts.toml, sampler_plate_layouts.csv
    position_tab = mo.vstack(
        [
            mo.md("## core/position/"),
            mo.md("### Samplers (sampler.toml)"),
            samplers_editor,
            mo.md("### Plate Layouts (plate_layouts.toml)"),
            plate_layouts_editor,
            mo.md("### Sampler Plate Layouts (sampler_plate_layouts.csv)"),
            sampler_plate_layouts_editor,
        ]
    )
    return (position_tab,)


@app.cell
def _(qc_layouts_well_editor):
    # core/position/ - qc_layouts_well.csv
    qc_layouts_well_tab = mo.vstack(
        [
            mo.md("## core/position/"),
            mo.md("### QC Layouts - Well Plates (qc_layouts_well.csv)"),
            mo.md("_Columns: tech_area, qc_layout_name, plate_layout, sample_id, tray, row, col_"),
            qc_layouts_well_editor,
        ]
    )
    return (qc_layouts_well_tab,)


@app.cell
def _(qc_layouts_tip_editor):
    # core/position/ - qc_layouts_tip.csv
    qc_layouts_tip_tab = mo.vstack(
        [
            mo.md("## core/position/"),
            mo.md("### QC Layouts - Tip Plates (qc_layouts_tip.csv)"),
            mo.md("_Columns: tech_area, qc_layout_name, plate_layout, sample_id, tray, position_start, position_end_"),
            qc_layouts_tip_editor,
        ]
    )
    return (qc_layouts_tip_tab,)


@app.cell
def _(queue_patterns_editor):
    # core/structure/ - queue_patterns.toml
    structure_tab = mo.vstack(
        [
            mo.md("## core/structure/"),
            mo.md("### Queue Patterns (queue_patterns.toml)"),
            queue_patterns_editor,
        ]
    )
    return (structure_tab,)


@app.cell
def _(samples_editor):
    # core/structure/ - samples.csv
    samples_tab = mo.vstack(
        [
            mo.md("## core/structure/"),
            mo.md("### Samples (samples.csv)"),
            samples_editor,
        ]
    )
    return (samples_tab,)


@app.cell
def _(methods_dropdown, methods_editor):
    # core/methods/ - per-instrument method CSV files
    methods_tab = mo.vstack(
        [
            mo.md("## core/methods/"),
            mo.md("### Methods Files"),
            methods_dropdown,
            methods_editor,
        ]
    )
    return (methods_tab,)


@app.cell
def _(instrument_configs_editor):
    # ui/ - instrument_config.csv
    ui_tab = mo.vstack(
        [
            mo.md("## ui/"),
            mo.md("### Instrument Configs (instrument_config.csv)"),
            mo.md("_Valid (instrument, sampler, output_format, default_pattern) combinations for the UI_"),
            instrument_configs_editor,
        ]
    )
    return (ui_tab,)


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
def _(filtered_by_sampler):
    # Step 5: pattern dropdown (filtered by sampler selection)
    pattern_options = filtered_by_sampler["pattern_name"].unique().sort().to_list()
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
    pattern_dropdown,
    cfg,
):
    # Build selected config view
    mo.stop(not all([tech_dropdown.value, instr_dropdown.value, sampler_dropdown.value]))

    _tech = tech_dropdown.value
    _instr = instr_dropdown.value
    _sampler = sampler_dropdown.value
    _pattern_name = pattern_dropdown.value

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
            "qc_layout_name": _pattern_cfg.qc_layout_name,
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

    selected_cfg = {
        "tech": _tech,
        "instrument": _instr,
        "sampler": _sampler,
        "pattern": _pattern_name,
        "sampler_info": _sampler_info,
        "pattern_info": _pattern_info,
        "instr_info": _instr_info,
        "methods_preview": _methods_preview,
    }
    return (selected_cfg,)


@app.cell
def _(
    tech_dropdown,
    instr_dropdown,
    sampler_dropdown,
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
        [tech_dropdown, instr_dropdown, sampler_dropdown, pattern_dropdown],
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
                    _section(f"Sampler: {selected_cfg['sampler']}", _sampler_df),
                    _section(f"Pattern: {selected_cfg['pattern']}", _pattern_df),
                ],
                widths="equal",
                gap=2,
            ),
            mo.md("---"),
            mo.hstack(
                [
                    _section("Instrument Config", _instr_df),
                    _section("Methods (first 5)", selected_cfg["methods_preview"]),
                ],
                widths="equal",
                gap=2,
            ),
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
def _(
    overview_tab,
    all_combinations_tab,
    formatting_tab,
    position_tab,
    qc_layouts_well_tab,
    qc_layouts_tip_tab,
    structure_tab,
    samples_tab,
    methods_tab,
    ui_tab,
):
    tabs = mo.ui.tabs(
        {
            "Overview": overview_tab,
            "All Combinations": all_combinations_tab,
            "Formatting": formatting_tab,
            "Position": position_tab,
            "QC Layouts Well": qc_layouts_well_tab,
            "QC Layouts Tip": qc_layouts_tip_tab,
            "Queue Patterns": structure_tab,
            "Samples": samples_tab,
            "Methods": methods_tab,
            "UI": ui_tab,
        }
    )
    return (tabs,)


@app.cell
def _(tabs):
    tabs
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
def _(
    cfg,
    instruments_editor,
    samples_editor,
    qc_layouts_well_editor,
    qc_layouts_tip_editor,
    instrument_configs_editor,
    sampler_plate_layouts_editor,
    samplers_editor,
    plate_layouts_editor,
    queue_patterns_editor,
    output_formats_editor,
    validate_button,
):
    mo.stop(not validate_button.value)

    try:
        # Rebuild configs from editor values (validation happens in create())
        QGConfiguration.create(
            instruments=InstrumentsConfig.from_table(instruments_editor.value),
            samples=SamplesConfig.from_table(samples_editor.value),
            qc_layouts_well=QCLayoutsWellConfig.from_table(qc_layouts_well_editor.value),
            qc_layouts_tip=QCLayoutsTipConfig.from_table(qc_layouts_tip_editor.value),
            instrument_configs=InstrumentConfigsConfig.from_table(instrument_configs_editor.value),
            sampler_plate_layouts=SamplerPlateLayoutsConfig.from_table(sampler_plate_layouts_editor.value),
            samplers=SamplersConfig.from_dict(tomllib.loads(samplers_editor.value)),
            plate_layouts=PlateLayoutsConfig.from_dict(tomllib.loads(plate_layouts_editor.value)),
            queue_patterns=QueuePatternsConfig.from_dict(tomllib.loads(queue_patterns_editor.value)),
            output_formats=OutputFormatsConfig.from_dict(tomllib.loads(output_formats_editor.value)),
            methods=cfg.methods,  # Keep existing methods (edited separately)
        )
        validation_result = mo.callout(
            mo.md("**All validations passed!**"),
            kind="success",
        )
        logger.info("Validation passed")
    except (ConfigValidationError, Exception) as e:
        validation_result = mo.callout(
            mo.vstack(
                [
                    mo.md("**Validation failed:**"),
                    mo.md(f"```\n{e}\n```"),
                ]
            ),
            kind="danger",
        )
        logger.warning("Validation failed: {}", e)

    validation_result
    return


# =============================================================================
# Save Logic
# =============================================================================


@app.cell
def _(
    cfg,
    config_dir,
    instruments_editor,
    samples_editor,
    qc_layouts_well_editor,
    qc_layouts_tip_editor,
    instrument_configs_editor,
    sampler_plate_layouts_editor,
    samplers_editor,
    plate_layouts_editor,
    queue_patterns_editor,
    output_formats_editor,
    save_button,
):
    mo.stop(save_button is None or not save_button.value)

    try:
        # Parse TOML editors, preserving header comments from editor text
        samplers_cfg = SamplersConfig.from_dict(tomllib.loads(samplers_editor.value))
        samplers_cfg.header_comments = read_header_comments(samplers_editor.value)

        plate_layouts_cfg = PlateLayoutsConfig.from_dict(tomllib.loads(plate_layouts_editor.value))
        plate_layouts_cfg.header_comments = read_header_comments(plate_layouts_editor.value)

        queue_patterns_cfg = QueuePatternsConfig.from_dict(tomllib.loads(queue_patterns_editor.value))
        queue_patterns_cfg.header_comments = read_header_comments(queue_patterns_editor.value)

        output_formats_cfg = OutputFormatsConfig.from_dict(tomllib.loads(output_formats_editor.value))
        output_formats_cfg.header_comments = read_header_comments(output_formats_editor.value)

        # Rebuild configs from editor values
        cfg_to_save = QGConfiguration.create(
            instruments=InstrumentsConfig.from_table(instruments_editor.value),
            samples=SamplesConfig.from_table(samples_editor.value),
            qc_layouts_well=QCLayoutsWellConfig.from_table(qc_layouts_well_editor.value),
            qc_layouts_tip=QCLayoutsTipConfig.from_table(qc_layouts_tip_editor.value),
            instrument_configs=InstrumentConfigsConfig.from_table(instrument_configs_editor.value),
            sampler_plate_layouts=SamplerPlateLayoutsConfig.from_table(sampler_plate_layouts_editor.value),
            samplers=samplers_cfg,
            plate_layouts=plate_layouts_cfg,
            queue_patterns=queue_patterns_cfg,
            output_formats=output_formats_cfg,
            methods=cfg.methods,  # Keep existing methods
        )

        # Write all configs to disk
        written = cfg_to_save.write_all(config_dir)
        save_result = mo.callout(
            mo.md(f"**Saved {len(written)} file(s) successfully!**"),
            kind="success",
        )
        logger.info("Saved {} config file(s) to {}", len(written), config_dir)
    except (ConfigValidationError, Exception) as e:
        save_result = mo.callout(
            mo.vstack(
                [
                    mo.md("**Cannot save:**"),
                    mo.md(f"```\n{e}\n```"),
                ]
            ),
            kind="danger",
        )
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
        review_author_input = mo.ui.text(label="Author", placeholder="your name")
        review_description_input = mo.ui.text(
            label="Change description",
            placeholder="what did you change and why?",
            full_width=True,
        )
        submit_review_button = mo.ui.run_button(label="Submit for Review", kind="warn")
    else:
        review_author_input = None
        review_description_input = None
        submit_review_button = None
    return review_author_input, review_description_input, submit_review_button


@app.cell
def _(
    gitlab_available,
    gitlab_unavailable_reason,
    review_author_input,
    review_description_input,
    submit_review_button,
):
    if gitlab_available:
        review_ui = mo.vstack(
            [
                mo.md("### Submit for Review"),
                mo.md("_Saves editor contents and submits changes as a GitLab merge request for review._"),
                mo.hstack([review_author_input, review_description_input], widths=[1, 3]),
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
    gitlab_available,
    cfg,
    original_contents,
    instruments_editor,
    samples_editor,
    qc_layouts_well_editor,
    qc_layouts_tip_editor,
    instrument_configs_editor,
    sampler_plate_layouts_editor,
    samplers_editor,
    plate_layouts_editor,
    queue_patterns_editor,
    output_formats_editor,
    review_author_input,
    review_description_input,
    submit_review_button,
):
    mo.stop(not gitlab_available or not submit_review_button or not submit_review_button.value)

    from qg.gitlab.config_bridge import submit_config_changes

    _author = review_author_input.value if review_author_input else ""
    _description = review_description_input.value if review_description_input else ""

    if not _author or not _description:
        review_result = mo.callout(
            mo.md("**Please fill in both author and description before submitting.**"),
            kind="warn",
        )
    else:
        try:
            # Build validated config from editor contents (no disk write)
            _samplers_cfg = SamplersConfig.from_dict(tomllib.loads(samplers_editor.value))
            _samplers_cfg.header_comments = read_header_comments(samplers_editor.value)
            _plate_layouts_cfg = PlateLayoutsConfig.from_dict(tomllib.loads(plate_layouts_editor.value))
            _plate_layouts_cfg.header_comments = read_header_comments(plate_layouts_editor.value)
            _queue_patterns_cfg = QueuePatternsConfig.from_dict(tomllib.loads(queue_patterns_editor.value))
            _queue_patterns_cfg.header_comments = read_header_comments(queue_patterns_editor.value)
            _output_formats_cfg = OutputFormatsConfig.from_dict(tomllib.loads(output_formats_editor.value))
            _output_formats_cfg.header_comments = read_header_comments(output_formats_editor.value)

            _cfg_to_submit = QGConfiguration.create(
                instruments=InstrumentsConfig.from_table(instruments_editor.value),
                samples=SamplesConfig.from_table(samples_editor.value),
                qc_layouts_well=QCLayoutsWellConfig.from_table(qc_layouts_well_editor.value),
                qc_layouts_tip=QCLayoutsTipConfig.from_table(qc_layouts_tip_editor.value),
                instrument_configs=InstrumentConfigsConfig.from_table(instrument_configs_editor.value),
                sampler_plate_layouts=SamplerPlateLayoutsConfig.from_table(sampler_plate_layouts_editor.value),
                samplers=_samplers_cfg,
                plate_layouts=_plate_layouts_cfg,
                queue_patterns=_queue_patterns_cfg,
                output_formats=_output_formats_cfg,
                methods=cfg.methods,
            )

            # In-memory diff: serialize edited config, compare to original snapshot
            _edited_contents = _cfg_to_submit.serialize_all()
            _changed_count = sum(1 for k in _edited_contents if _edited_contents[k] != original_contents.get(k))
            logger.info("Review submitted | author={} | changed_files={}", _author, _changed_count)
            mr_url = submit_config_changes(
                original_contents, _edited_contents, author=_author, description=_description
            )
            if mr_url:
                review_result = mo.callout(
                    mo.md(f"**Merge request created!** [Open MR]({mr_url})"),
                    kind="success",
                )
                logger.info("MR created: {}", mr_url)
            else:
                review_result = mo.callout(
                    mo.md("**Nothing changed** — no config files differ from the current version."),
                    kind="info",
                )
                logger.info("Review submitted but no files changed")
        except Exception as e:
            logger.exception("GitLab submission failed")
            review_result = mo.callout(
                mo.md(f"**Submission failed:** {type(e).__name__} — check logs for details."),
                kind="danger",
            )

    review_result
    return


if __name__ == "__main__":
    app.run()
