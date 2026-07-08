"""Tests for the framework-neutral config-editor core (qg.apps.editor_core).

The core is shared by the marimo config editor and the Dash editor. These tests
exercise it directly, without any GUI, so they run in the core (no-portal) env.
"""

from pathlib import Path

import pytest

from qg.apps import editor_core
from qg.config_models.loader import qg_configuration

CONFIG_DIR = Path(__file__).parent.parent / "qg_configs"


@pytest.fixture
def config():
    qg_configuration.cache_clear()
    return qg_configuration(CONFIG_DIR)


def _copy_tables(tables):
    return {name: [dict(row) for row in rows] for name, rows in tables.items()}


# ---------------------------------------------------------------------------
# validate / reconstruct
# ---------------------------------------------------------------------------


def test_unchanged_editor_state_validates(config):
    initial = editor_core.initial_editor_state(config)
    outcome = editor_core.validate_payload(initial.tables, initial.toml, initial.methods)
    assert outcome.ok
    assert "All validations passed" in outcome.message


def test_invalid_table_payload_reports_validation_error(config):
    initial = editor_core.initial_editor_state(config)
    tables = _copy_tables(initial.tables)
    tables["samples"][0]["file_name_template"] = "{invalid_placeholder}"
    outcome = editor_core.validate_payload(tables, initial.toml, initial.methods)
    assert not outcome.ok
    assert "Invalid placeholders" in outcome.message


def test_invalid_toml_reports_validation_error(config):
    initial = editor_core.initial_editor_state(config)
    toml = dict(initial.toml)
    toml["queue_patterns"] = "this is = not valid TOML ["
    outcome = editor_core.validate_payload(initial.tables, toml, initial.methods)
    assert not outcome.ok


def test_methods_edits_are_applied_in_memory_only(config):
    initial = editor_core.initial_editor_state(config)
    method_key = initial.method_options[0]
    methods = _copy_tables(initial.methods)
    methods[method_key][0]["method_path"] = "/tmp/demo-method.raw"

    edited = editor_core.build_config_from_payload(initial.tables, initial.toml, methods)
    tech_area, instrument = editor_core.split_method_key(method_key)

    assert edited.methods.get_methods(tech_area, instrument).methods[0].method_path == "/tmp/demo-method.raw"
    assert config.methods.get_methods(tech_area, instrument).methods[0].method_path != "/tmp/demo-method.raw"


# ---------------------------------------------------------------------------
# preserve_comments (save vs validate-only)
# ---------------------------------------------------------------------------


def test_preserve_comments_keeps_toml_header(config):
    initial = editor_core.initial_editor_state(config)
    rebuilt = editor_core.build_config_from_payload(
        initial.tables, initial.toml, initial.methods, preserve_comments=True
    )
    assert rebuilt.tech_area_defaults.header_comments == config.tech_area_defaults.header_comments
    assert rebuilt.tech_area_defaults.header_comments != ""


def test_default_drops_toml_header(config):
    initial = editor_core.initial_editor_state(config)
    rebuilt = editor_core.build_config_from_payload(initial.tables, initial.toml, initial.methods)
    assert rebuilt.tech_area_defaults.header_comments == ""


# ---------------------------------------------------------------------------
# methods store + DataFrame adapter
# ---------------------------------------------------------------------------


def test_methods_store_from_config_keys_match_iter(config):
    store = editor_core.methods_store_from_config(config.methods)
    expected = {editor_core.method_key(t, i) for (t, i), _ in config.methods.iter_methods()}
    assert set(store) == expected
    assert store  # non-empty


def test_config_from_dataframes_matches_row_dict_path(config):
    initial = editor_core.initial_editor_state(config)
    df_tables = {name: config_table for name, config_table in _config_tables(config).items()}
    from_df = editor_core.config_from_dataframes(df_tables, initial.toml, initial.methods)
    from_rows = editor_core.build_config_from_payload(initial.tables, initial.toml, initial.methods)
    assert from_df.serialize_all() == from_rows.serialize_all()


# ---------------------------------------------------------------------------
# round-trip losslessness — the marimo save path must not spuriously rewrite
# ---------------------------------------------------------------------------


def test_roundtrip_preserves_serialized_config(config):
    """build_config_from_payload on the packaged config, with comments preserved,
    must serialize byte-identically to the original — otherwise the editors' save
    path would rewrite unchanged files."""
    initial = editor_core.initial_editor_state(config)
    rebuilt = editor_core.build_config_from_payload(
        initial.tables, initial.toml, initial.methods, preserve_comments=True
    )
    original_contents = config.serialize_all()
    rebuilt_contents = rebuilt.serialize_all()
    assert set(rebuilt_contents) == set(original_contents)
    mismatched = [name for name in original_contents if rebuilt_contents[name] != original_contents[name]]
    assert not mismatched, f"round-trip changed: {mismatched}"


def _config_tables(config):
    return {
        "instruments": config.instruments.to_table(),
        "samples": config.samples.to_table(),
        "qc_layouts_well": config.qc_layouts_well.to_table(),
        "qc_layouts_tip": config.qc_layouts_tip.to_table(),
        "instrument_configs": config.instrument_configs.to_table(),
        "sampler_plate_layouts": config.sampler_plate_layouts.to_table(),
    }
