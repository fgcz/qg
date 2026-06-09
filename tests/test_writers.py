"""Tests for qg.writers module — get_writer dispatch and writer edge cases."""

from __future__ import annotations

import polars as pl
import pytest

from qg.writers import get_writer


class TestGetWriter:
    @pytest.mark.parametrize("name", ["csv", "chronos_csv", "xcalibur_csv", "hystar_xml"])
    def test_returns_callable_for_each_known_writer(self, name: str):
        writer = get_writer(name)
        assert callable(writer)

    def test_raises_key_error_for_unknown(self):
        with pytest.raises(KeyError, match="nonexistent"):
            get_writer("nonexistent")


class TestWriteCsv:
    def test_empty_dataframe(self):
        df = pl.DataFrame({"A": [], "B": []}).cast(pl.String)
        result = get_writer("csv")(df)
        assert result.strip() == "A,B"

    def test_single_row(self):
        df = pl.DataFrame({"Name": ["sample1"], "ID": ["42"]})
        result = get_writer("csv")(df)
        lines = result.strip().split("\n")
        assert lines[0] == "Name,ID"
        assert lines[1] == "sample1,42"


class TestWriteXcaliburCsv:
    def test_preamble_format(self):
        df = pl.DataFrame({"A": ["1"], "B": ["2"], "C": ["3"]})
        result = get_writer("xcalibur_csv")(df)
        first_line = result.split("\n")[0]
        assert first_line == "Bracket Type=4,,"

    def test_empty_dataframe(self):
        df = pl.DataFrame({"A": [], "B": []}).cast(pl.String)
        result = get_writer("xcalibur_csv")(df)
        lines = result.strip().split("\n")
        assert lines[0] == "Bracket Type=4,"
        assert lines[1] == "A,B"


class TestWriteChronosCsv:
    def test_counter_column_prepended(self):
        df = pl.DataFrame({"Name": ["a", "b", "c"]})
        result = get_writer("chronos_csv")(df)
        lines = result.strip().split("\n")
        # Header: empty column name (quoted by polars) + Name
        assert lines[0] == '"",Name'
        # Data rows: 1-based counter
        assert lines[1].startswith("1,")
        assert lines[2].startswith("2,")
        assert lines[3].startswith("3,")

    def test_empty_dataframe(self):
        df = pl.DataFrame({"Name": []}).cast(pl.String)
        result = get_writer("chronos_csv")(df)
        lines = result.strip().split("\n")
        assert lines[0] == '"",Name'
        assert len(lines) == 1
