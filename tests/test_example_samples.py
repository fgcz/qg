"""Tests for the bundled example sample-table catalog.

Covers the catalog API, parser-mode agreement, package-data loading via
``importlib.resources`` (not repo paths), and byte equality between the
authoritative package copy and the ``docs/examples/`` mirror.
"""

from __future__ import annotations

from importlib.resources import files
from pathlib import Path

import pytest

from qg.apps.integrations.example_samples import (
    list_example_sample_tables,
    read_example_sample_table,
)
from qg.apps.integrations.local_samples import parse_sample_table

_PACKAGE = "qg.examples.sample_tables"
_DOCS_EXAMPLES = Path(__file__).parents[1] / "docs" / "examples"


def _catalog_ids() -> list[str]:
    return [e.id for e in list_example_sample_tables()]


class TestCatalog:
    def test_has_vial_and_plate_entries(self):
        modes = {e.mode for e in list_example_sample_tables()}
        assert "vial" in modes
        assert "plate" in modes

    def test_ids_are_unique(self):
        ids = _catalog_ids()
        assert len(ids) == len(set(ids))

    def test_unknown_id_raises(self):
        with pytest.raises(KeyError, match="Unknown example"):
            read_example_sample_table("does-not-exist")

    @pytest.mark.parametrize("example_id", _catalog_ids())
    def test_entry_parses_and_mode_agrees(self, example_id):
        entry, data = read_example_sample_table(example_id)

        # Filename has a supported extension.
        assert entry.filename.endswith((".csv", ".xlsx"))

        parsed = parse_sample_table(data, entry.filename)

        # The catalog mode is a cross-check: the parser is the authority.
        assert parsed.mode == entry.mode
        # sample_id is unique (parser raises otherwise, but assert the contract).
        assert not parsed.df["sample_id"].is_duplicated().any()


class TestPackaging:
    """Load via importlib.resources so a wheel/package-data mistake fails here."""

    @pytest.mark.parametrize("example_id", _catalog_ids())
    def test_resource_is_readable_via_importlib(self, example_id):
        entry, _ = read_example_sample_table(example_id)
        resource = files(_PACKAGE) / entry.filename
        assert resource.is_file()
        assert resource.read_bytes()  # non-empty


class TestDocsMirror:
    """The docs/examples copy must be byte-identical to the packaged authority."""

    @pytest.mark.parametrize("example_id", _catalog_ids())
    def test_docs_copy_matches_package(self, example_id):
        entry, data = read_example_sample_table(example_id)
        docs_path = _DOCS_EXAMPLES / entry.filename
        assert docs_path.exists(), f"docs/examples mirror missing: {entry.filename}"
        assert docs_path.read_bytes() == data, (
            f"{entry.filename} differs between docs/examples and src/qg/examples/sample_tables"
        )
