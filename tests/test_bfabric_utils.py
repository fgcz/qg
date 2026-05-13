"""Tests for B-Fabric helpers — instance slug, per-instance cache layout."""

from pathlib import Path
from unittest.mock import MagicMock

import polars as pl
import pytest

from qg.bfabric_utils import BfabricHelper, ContainerComposition, instance_slug
from qg.cli.find_projects import ContainerCache, get_cache_dir


class _FakeClient:
    """Minimal duck-type for `Bfabric` — exposes `config.base_url` only."""

    def __init__(self, base_url: str) -> None:
        self.config = MagicMock()
        self.config.base_url = base_url


@pytest.mark.parametrize(
    ("url", "expected"),
    [
        ("https://fgcz-bfabric.uzh.ch/bfabric", "fgcz-bfabric.uzh.ch"),
        ("https://fgcz-bfabric-test.uzh.ch/bfabric/", "fgcz-bfabric-test.uzh.ch"),
        ("https://example.org:8443/bfabric", "example.org:8443"),
    ],
)
def test_instance_slug(url: str, expected: str) -> None:
    assert instance_slug(_FakeClient(url)) == expected


def test_get_cache_dir_per_instance() -> None:
    """Two instances with distinct base URLs resolve to distinct subdirectories."""
    prod = _FakeClient("https://fgcz-bfabric.uzh.ch/bfabric")
    test = _FakeClient("https://fgcz-bfabric-test.uzh.ch/bfabric")
    assert get_cache_dir(prod) != get_cache_dir(test)
    assert get_cache_dir(prod).name == "fgcz-bfabric.uzh.ch"
    assert get_cache_dir(test).name == "fgcz-bfabric-test.uzh.ch"


def test_container_cache_writes_per_instance_subdir(monkeypatch, tmp_path: Path) -> None:
    """Two clients on different instances must not overwrite each other's cache."""
    monkeypatch.setattr("qg.cli.find_projects.get_cache_dir", lambda c: tmp_path / instance_slug(c))

    prod = _FakeClient("https://fgcz-bfabric.uzh.ch/bfabric")
    test = _FakeClient("https://fgcz-bfabric-test.uzh.ch/bfabric")

    prod_cache = ContainerCache(prod)
    test_cache = ContainerCache(test)

    prod_path = prod_cache._write("bfabric_container", pl.DataFrame({"Container ID": [1]}))
    test_path = test_cache._write("bfabric_container", pl.DataFrame({"Container ID": [2]}))

    assert prod_path == tmp_path / "fgcz-bfabric.uzh.ch" / "bfabric_container.csv"
    assert test_path == tmp_path / "fgcz-bfabric-test.uzh.ch" / "bfabric_container.csv"
    assert prod_path != test_path
    assert pl.read_csv(prod_path)["Container ID"].to_list() == [1]
    assert pl.read_csv(test_path)["Container ID"].to_list() == [2]


def test_get_cache_dir_honors_env_var(monkeypatch, tmp_path: Path) -> None:
    monkeypatch.setenv("QG_CACHE_DIR", str(tmp_path))
    client = _FakeClient("https://fgcz-bfabric.uzh.ch/bfabric")
    assert get_cache_dir(client) == tmp_path / "fgcz-bfabric.uzh.ch"


def test_get_cache_dir_default_under_repo(monkeypatch) -> None:
    monkeypatch.delenv("QG_CACHE_DIR", raising=False)
    client = _FakeClient("https://fgcz-bfabric.uzh.ch/bfabric")
    path = get_cache_dir(client)
    assert path.name == instance_slug(client)
    assert path.parent.name == "bfabric_cache"


def _fake_plates_dict(sample_dicts: list[dict], plate_id: int = 42) -> dict:
    """Build a `{uri: plate}` dict shaped like what `client.reader.query("plate", ...)` returns."""
    fake_plate = MagicMock()
    fake_plate.refs.sample = sample_dicts
    fake_uri = MagicMock()
    fake_uri.components.entity_id = plate_id
    return {fake_uri: fake_plate}


_PLATE_SAMPLES = [
    {"name": "S1", "id": 1, "_position": None, "_gridposition": None, "groupingvar": None},
    {"name": "S2", "id": 2, "_position": None, "_gridposition": None, "groupingvar": None},
    {"name": "FOREIGN", "id": 99, "_position": None, "_gridposition": None, "groupingvar": None},
]


def test_get_samples_plates_filters_foreign_when_restricted() -> None:
    """Non-employee mode: shared-plate samples not in the user's container are dropped."""
    fake_client = MagicMock()
    fake_client.reader.query.return_value = _fake_plates_dict(_PLATE_SAMPLES)
    fake_client.read.return_value.to_polars.return_value = pl.DataFrame({"id": [1, 2]})

    helper = BfabricHelper(fake_client, restrict_to_container_id=100)
    df = helper.get_samples(100, "Plates")

    assert sorted(df["sample_id"].to_list()) == [1, 2]
    assert "FOREIGN" not in df["sample_name"].to_list()


@pytest.mark.parametrize(
    ("all_ids", "plate_sample_ids", "expected"),
    [
        # plates-only: every sample in the container sits on a plate
        ([1, 2, 3], [1, 2, 3], ContainerComposition(has_plates=True, has_vials=False)),
        # vials-only: no plates returned at all
        ([10, 11], [], ContainerComposition(has_plates=False, has_vials=True)),
        # mixed: some samples on plate, some not
        ([1, 2, 3, 4], [1, 2], ContainerComposition(has_plates=True, has_vials=True)),
        # plate exists but its samples belong to other containers — counts as vials-only here
        ([5, 6], [99], ContainerComposition(has_plates=False, has_vials=True)),
        # empty container
        ([], [], ContainerComposition(has_plates=False, has_vials=False)),
    ],
)
def test_get_container_composition(
    all_ids: list[int],
    plate_sample_ids: list[int],
    expected: ContainerComposition,
) -> None:
    fake_client = MagicMock()
    fake_client.reader.query.return_value = (
        _fake_plates_dict(
            [{"id": sid} for sid in plate_sample_ids],
        )
        if plate_sample_ids
        else {}
    )
    fake_client.read.return_value.to_polars.return_value = pl.DataFrame({"id": all_ids})

    helper = BfabricHelper(fake_client)
    assert helper.get_container_composition(100) == expected


def test_get_samples_plates_no_filter_when_unrestricted() -> None:
    """Employee mode: all samples on the plate are returned, even foreign ones."""
    fake_client = MagicMock()
    fake_client.reader.query.return_value = _fake_plates_dict(_PLATE_SAMPLES)

    helper = BfabricHelper(fake_client)
    df = helper.get_samples(100, "Plates")

    assert sorted(df["sample_id"].to_list()) == [1, 2, 99]
    # Inventory fetch must be skipped when no restriction is set.
    fake_client.read.assert_not_called()
