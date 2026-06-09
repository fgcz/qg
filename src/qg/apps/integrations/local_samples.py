"""Parse user-uploaded sample tables (CSV/XLSX) into the normalized schema.

Pure functions — no marimo, no B-Fabric — so ``queue_app_local.py`` and the tests
can call them directly. The output ``DataFrame`` matches the source-neutral row
schemas in :mod:`qg.sample_rows` (``VialSampleRow`` / ``PlateSampleRow``), which is
exactly what ``QueueBuilder.add_samples_from_dataframe`` consumes.
"""

from __future__ import annotations

import io
from dataclasses import dataclass
from typing import Literal

import polars as pl

from qg.sample_rows import PlateSampleRow, VialSampleRow

# Source column header (lower-cased, stripped) -> normalized snake_case name.
# Covers the internal names, the common exported display names, and the B-Fabric
# export names so a table dumped from either source loads without manual editing.
_ALIASES: dict[str, str] = {
    "sample name": "sample_name",
    "sample id": "sample_id",
    "tube id": "tube_id",
    "container id": "container_id",
    "plate id": "plate_id",
    "grid position": "grid_position",
    "grouping var": "grouping_var",
    # B-Fabric export names
    "name": "sample_name",
    "id": "sample_id",
    "tubeid": "tube_id",
    "_gridposition": "grid_position",
    "_position": "position",
    "groupingvar_name": "grouping_var",
}

_VIAL_COLUMNS = tuple(VialSampleRow.model_fields)
_PLATE_COLUMNS = tuple(PlateSampleRow.model_fields)
_VIAL_REQUIRED = {"sample_name", "sample_id", "container_id"}
_PLATE_REQUIRED = {"sample_name", "sample_id", "container_id", "plate_id", "grid_position"}
# Columns that must be integer-typed for the builder / pydantic models.
_INT_COLUMNS = ("sample_id", "container_id", "plate_id", "position")


@dataclass(frozen=True)
class ParsedSamples:
    """A normalized sample table plus the queue mode it implies."""

    df: pl.DataFrame
    mode: Literal["vial", "plate"]


def _read_frame(data: bytes, filename: str) -> pl.DataFrame:
    ext = filename.rsplit(".", 1)[-1].lower() if "." in filename else ""
    try:
        if ext == "csv":
            return pl.read_csv(io.BytesIO(data))
        if ext in ("xlsx", "xls"):
            return pl.read_excel(io.BytesIO(data))
    except Exception as exc:  # noqa: BLE001 — surface any reader failure as a clear domain error
        raise ValueError(f"Could not read {filename!r}: {exc}") from exc
    raise ValueError(f"Unsupported file type {ext!r} — upload a .csv or .xlsx file.")


def _normalize_columns(df: pl.DataFrame) -> pl.DataFrame:
    rename = {col: _ALIASES.get(col.strip().lower(), col.strip()) for col in df.columns}
    normalized = list(rename.values())
    dupes = {n for n in normalized if normalized.count(n) > 1}
    if dupes:
        raise ValueError(f"Columns map to the same name after normalization: {sorted(dupes)}")
    return df.rename(rename)


def parse_sample_table(data: bytes, filename: str) -> ParsedSamples:
    """Parse uploaded ``data`` (named ``filename``) into a normalized sample table.

    Args:
        data: Raw uploaded file bytes.
        filename: Original file name; its extension selects the reader (.csv/.xlsx).

    Returns:
        A :class:`ParsedSamples` with the normalized ``DataFrame`` (only the
        canonical schema columns) and the inferred ``mode`` — ``"plate"`` when both
        ``plate_id`` and ``grid_position`` are present, otherwise ``"vial"``.

    Raises:
        ValueError: unreadable file, unsupported extension, missing required
            columns, non-integer ID columns, or duplicate ``sample_id``.
    """
    df = _normalize_columns(_read_frame(data, filename))
    present = set(df.columns)

    mode: Literal["vial", "plate"] = "plate" if {"plate_id", "grid_position"} <= present else "vial"
    required = _PLATE_REQUIRED if mode == "plate" else _VIAL_REQUIRED
    if missing := required - present:
        raise ValueError(f"{mode.capitalize()} table is missing required column(s): {sorted(missing)}")

    # Keep only the canonical columns for the mode (drop unknown extras so they are
    # not forwarded as unexpected kwargs downstream).
    keep = [c for c in (_PLATE_COLUMNS if mode == "plate" else _VIAL_COLUMNS) if c in present]
    df = df.select(keep)

    # The builder's plate path requires a `tray` column; default it to null so the
    # start-tray parameter can supply it.
    if mode == "plate" and "tray" not in df.columns:
        df = df.with_columns(pl.lit(None).alias("tray"))

    df = _cast_int_columns(df)

    if df["sample_id"].is_duplicated().any():
        dupes = df.filter(pl.col("sample_id").is_duplicated())["sample_id"].unique().to_list()
        raise ValueError(f"Duplicate sample_id values are not allowed: {sorted(dupes)}")

    return ParsedSamples(df=df, mode=mode)


def _cast_int_columns(df: pl.DataFrame) -> pl.DataFrame:
    for col in (c for c in _INT_COLUMNS if c in df.columns):
        try:
            df = df.with_columns(pl.col(col).cast(pl.Int64, strict=True))
        except pl.exceptions.InvalidOperationError as exc:
            raise ValueError(f"Column {col!r} must contain whole numbers: {exc}") from exc
    return df
