"""Queue file writers."""

from __future__ import annotations

from collections.abc import Callable
from io import BytesIO

import polars as pl

WriterFn = Callable[[pl.DataFrame], str]


def _write_csv(df: pl.DataFrame) -> str:
    return df.write_csv()


def _write_xcalibur_csv(df: pl.DataFrame) -> str:
    """Write Xcalibur CSV with Bracket Type preamble."""
    ncols = len(df.columns)
    preamble = "Bracket Type=4" + "," * (ncols - 1) + "\n"
    return preamble + df.write_csv()


def _write_hystar_xml(df: pl.DataFrame) -> str:
    """Write HyStar XML, returning string."""
    from qg.hystar_xml_writer import write_hystar_xml

    buffer = BytesIO()
    write_hystar_xml(df, buffer)
    return buffer.getvalue().decode("utf-8")


_WRITERS: dict[str, WriterFn] = {
    "csv": _write_csv,
    "xcalibur_csv": _write_xcalibur_csv,
    "hystar_xml": _write_hystar_xml,
}


def get_writer(name: str) -> WriterFn:
    """Return writer function by name."""
    if name not in _WRITERS:
        raise KeyError(f"Unknown writer '{name}'. Available: {list(_WRITERS.keys())}")
    return _WRITERS[name]
