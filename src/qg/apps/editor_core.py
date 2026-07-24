"""Framework-neutral config-editor core shared by the marimo and Dash editors.

Owns the editor substance that both GUI surfaces need: table/TOML section
contracts, ``compact_toml``, TOML (de)serialization, reconstruction of a validated
``QGConfiguration`` from browser/table payloads, and methods-store handling. This
module must stay free of marimo, dash, B-Fabric, and GitLab imports so it can be
used by the core-installable local Dash editor and unit-tested without a GUI.
"""

from __future__ import annotations

import re
import tomllib
from dataclasses import dataclass
from typing import Any

import polars as pl
import tomli_w

from qg.config_models.formatting import InstrumentsConfig, OutputFormatsConfig
from qg.config_models.loader import QGConfiguration, read_header_comments
from qg.config_models.methods import MethodsConfig, MethodsForInstrument
from qg.config_models.positions import (
    PlateLayoutsConfig,
    QCLayoutsTipConfig,
    QCLayoutsWellConfig,
    SamplerPlateLayoutsConfig,
    SamplersConfig,
)
from qg.config_models.structure import QueuePatternsConfig, SamplesConfig
from qg.config_models.ui import InstrumentConfigsConfig, TechAreaDefaultsConfig

type TableRows = list[dict[str, Any]]
type TableStore = dict[str, TableRows]
type TomlStore = dict[str, str]
type MethodStore = dict[str, TableRows]

CSV_TABLE_COLUMNS: dict[str, list[str]] = {
    "instruments": ["tech_area", "instrument", "methods_file", "path_template"],
    "samples": [
        "tech_area",
        "sample_id",
        "sample_name",
        "sample_type",
        "qc_class",
        "level",
        "description",
        "inj_vol",
        "file_name_template",
    ],
    "qc_layouts_well": ["tech_area", "qc_layout_name", "plate_layout", "sample_id", "tray", "row", "col"],
    "qc_layouts_tip": [
        "tech_area",
        "qc_layout_name",
        "plate_layout",
        "sample_id",
        "tray",
        "position_start",
        "position_end",
    ],
    "instrument_configs": ["tech_area", "instrument", "sampler", "output_format", "default_pattern"],
    "sampler_plate_layouts": ["sampler", "plate_layout", "queue_type"],
}

TOML_CONFIG_CLASSES: dict[str, type] = {
    "output_formats": OutputFormatsConfig,
    "tech_area_defaults": TechAreaDefaultsConfig,
    "samplers": SamplersConfig,
    "plate_layouts": PlateLayoutsConfig,
    "queue_patterns": QueuePatternsConfig,
}

METHOD_COLUMNS = ["sample_type", "polarity", "method_name", "method_path"]


@dataclass(slots=True, frozen=True)
class EditorState:
    """JSON-serializable initial state for the Dash editor."""

    tables: TableStore
    table_columns: dict[str, list[str]]
    toml: TomlStore
    methods: MethodStore
    method_columns: list[str]
    method_options: list[str]
    overview: TableRows


@dataclass(slots=True, frozen=True)
class ValidationOutcome:
    """Result of validating the current browser-side editor state."""

    ok: bool
    message: str


def compact_toml(toml_str: str) -> str:
    """Convert simple multiline TOML arrays to inline arrays for readability."""
    pattern = r"(\w+)\s*=\s*\[\s*\n((?:\s+[^\]]+,?\s*\n)+)\s*\]"

    def replace_array(match: re.Match[str]) -> str:
        key = match.group(1)
        items_block = match.group(2)
        items = [item.strip().rstrip(",") for item in items_block.strip().split("\n") if item.strip()]
        return f"{key} = [{', '.join(items)}]"

    return re.sub(pattern, replace_array, toml_str)


def method_key(tech_area: str, instrument: str) -> str:
    """Return the browser-store key for one methods table."""
    return f"{tech_area}/{instrument}"


def split_method_key(key: str) -> tuple[str, str]:
    """Split a browser-store methods key into ``(tech_area, instrument)``."""
    tech_area, instrument = key.split("/", 1)
    return tech_area, instrument


def initial_editor_state(config: QGConfiguration) -> EditorState:
    """Build the initial browser-side editor state from a loaded config."""
    tables = {
        "instruments": _records(config.instruments.to_table()),
        "samples": _records(config.samples.to_table()),
        "qc_layouts_well": _records(config.qc_layouts_well.to_table()),
        "qc_layouts_tip": _records(config.qc_layouts_tip.to_table()),
        "instrument_configs": _records(config.instrument_configs.to_table()),
        "sampler_plate_layouts": _records(config.sampler_plate_layouts.to_table()),
    }
    toml = {
        "output_formats": _toml_text(config.output_formats.header_comments, config.output_formats.to_dict()),
        "tech_area_defaults": _toml_text(
            config.tech_area_defaults.header_comments,
            config.tech_area_defaults.to_dict(),
        ),
        "samplers": _toml_text(config.samplers.header_comments, config.samplers.to_dict()),
        "plate_layouts": _toml_text(config.plate_layouts.header_comments, config.plate_layouts.to_dict()),
        "queue_patterns": _toml_text(config.queue_patterns.header_comments, config.queue_patterns.to_dict()),
    }
    methods = {
        method_key(tech_area, instrument): _records(methods_for_instrument.to_table())
        for (tech_area, instrument), methods_for_instrument in config.methods.iter_methods()
    }
    return EditorState(
        tables=tables,
        table_columns=CSV_TABLE_COLUMNS,
        toml=toml,
        methods=methods,
        method_columns=METHOD_COLUMNS,
        method_options=sorted(methods),
        overview=_records(config.to_overview_table()),
    )


def build_config_from_payload(
    tables: TableStore,
    toml: TomlStore,
    methods: MethodStore,
    *,
    preserve_comments: bool = False,
) -> QGConfiguration:
    """Reconstruct a validated config from editor table/TOML payloads.

    ``preserve_comments`` re-attaches each TOML section's header comments to the
    rebuilt config. The validate-only (local Dash) path leaves it ``False``; the
    save and Git-review paths (marimo editor, full Dash editor) pass ``True`` so
    ``QGConfiguration.write_all`` / ``serialize_all`` round-trip the comments.
    """
    toml_configs = {
        name: _parse_toml_config(toml[name], config_cls, preserve_comments=preserve_comments)
        for name, config_cls in TOML_CONFIG_CLASSES.items()
    }
    return QGConfiguration.create(
        instruments=InstrumentsConfig.from_table(_table(tables["instruments"], CSV_TABLE_COLUMNS["instruments"])),
        samples=SamplesConfig.from_table(_table(tables["samples"], CSV_TABLE_COLUMNS["samples"])),
        qc_layouts_well=QCLayoutsWellConfig.from_table(
            _table(tables["qc_layouts_well"], CSV_TABLE_COLUMNS["qc_layouts_well"])
        ),
        qc_layouts_tip=QCLayoutsTipConfig.from_table(
            _table(tables["qc_layouts_tip"], CSV_TABLE_COLUMNS["qc_layouts_tip"])
        ),
        instrument_configs=InstrumentConfigsConfig.from_table(
            _table(tables["instrument_configs"], CSV_TABLE_COLUMNS["instrument_configs"])
        ),
        sampler_plate_layouts=SamplerPlateLayoutsConfig.from_table(
            _table(tables["sampler_plate_layouts"], CSV_TABLE_COLUMNS["sampler_plate_layouts"])
        ),
        methods=_methods_from_store(methods),
        **toml_configs,
    )


def validate_payload(
    tables: TableStore,
    toml: TomlStore,
    methods: MethodStore,
) -> ValidationOutcome:
    """Validate the current editor payload without writing anything."""
    try:
        build_config_from_payload(tables, toml, methods)
    except Exception as exc:
        return ValidationOutcome(ok=False, message=str(exc))
    return ValidationOutcome(ok=True, message="All validations passed.")


def config_from_dataframes(
    tables: dict[str, pl.DataFrame],
    toml: TomlStore,
    methods: MethodStore,
    *,
    preserve_comments: bool = False,
) -> QGConfiguration:
    """Reconstruct a config when table payloads are ``pl.DataFrame`` (marimo editor).

    Adapter over :func:`build_config_from_payload` for the marimo data-editor, whose
    values are DataFrames rather than the row-dicts the Dash grids produce.
    """
    row_tables = {name: df.to_dicts() for name, df in tables.items()}
    return build_config_from_payload(row_tables, toml, methods, preserve_comments=preserve_comments)


def _toml_text(header_comments: str, data: dict[str, Any]) -> str:
    return header_comments + compact_toml(tomli_w.dumps(data))


def _records(df: pl.DataFrame) -> TableRows:
    return df.to_dicts()


def _table(rows: TableRows, columns: list[str]) -> pl.DataFrame:
    if rows:
        normalized = [{column: row.get(column) for column in columns} for row in rows]
        return pl.DataFrame(normalized)
    return pl.DataFrame({column: [] for column in columns})


def _parse_toml_config(text: str, config_cls: type, *, preserve_comments: bool = False) -> Any:
    cfg = config_cls.from_dict(tomllib.loads(text))
    if preserve_comments:
        cfg.header_comments = read_header_comments(text)
    return cfg


def methods_store_from_config(methods: MethodsConfig) -> MethodStore:
    """Row-dict store of every methods table, keyed ``tech_area/instrument``.

    The marimo editor edits one methods table at a time; it starts from this
    full store (so unedited tables round-trip unchanged) and overlays the single
    active table before reconstruction.
    """
    return {
        method_key(tech_area, instrument): _records(methods_for_instrument.to_table())
        for (tech_area, instrument), methods_for_instrument in methods.iter_methods()
    }


def _methods_from_store(methods: MethodStore) -> MethodsConfig:
    config = MethodsConfig()
    for key, rows in methods.items():
        tech_area, instrument = split_method_key(key)
        config.add_instrument_methods(
            tech_area,
            instrument,
            MethodsForInstrument.from_table(
                _table(rows, METHOD_COLUMNS),
                config_path=MethodsConfig.config_folder / tech_area / f"{instrument}_methods.csv",
            ),
        )
    return config
