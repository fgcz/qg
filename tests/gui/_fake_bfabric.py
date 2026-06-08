"""Duck-typed `Bfabric` client and `AppSession` builder for hermetic GUI tests.

The real `Bfabric` client talks to a B-Fabric instance over JSON-RPC. For GUI tests
we replace it with `FakeBfabric`, which serves canned responses from JSON files on
disk. `BfabricHelper` only touches the client through two entry points:

    - ``client.read("sample"|..., params, max_results=None).to_polars(flatten=True)``
    - ``client.reader.query("plate", {"containerid": id})`` → dict[URI, plate]

The helper also reads ``client.config.base_url`` for the per-instance cache slug.

Fixtures live in ``tests/gui/fixtures/bfabric/`` as JSON; see ``samples_37180.json``
for the schema. Plates may be omitted for vial-only scenarios.
"""

from __future__ import annotations

import json
from dataclasses import dataclass
from pathlib import Path
from types import SimpleNamespace
from typing import Any

import polars as pl


@dataclass(frozen=True)
class _FakeUri:
    """Minimal stand-in for the real B-Fabric URI object."""

    entity_id: int

    @property
    def components(self) -> _FakeUri:  # mirrors `uri.components.entity_id`
        return self


@dataclass(frozen=True)
class _FakePlate:
    """Plate entity. `refs.sample` is a list of dicts with `id`, `name`, optional `_position`, etc."""

    id: int
    refs: SimpleNamespace  # has .sample: list[dict]


def _flatten_row(row: dict[str, Any]) -> dict[str, Any]:
    """Expand one-level-nested dicts into ``parent_child`` columns.

    Mirrors the real ``Bfabric.read(...).to_polars(flatten=True)`` behaviour for
    the fields the production code reads (e.g. ``groupingvar.name`` becomes
    ``groupingvar_name``, ``ref_id.entity_id`` becomes ``ref_id_entity_id``).
    Only goes one level deep; nested lists are left as-is.
    """
    out: dict[str, Any] = {}
    for key, value in row.items():
        if isinstance(value, dict):
            for sub_key, sub_value in value.items():
                out[f"{key}_{sub_key}"] = sub_value
        else:
            out[key] = value
    return out


class _FakeReadResult:
    """Result of `client.read(...)` — only `.to_polars(flatten=True)` and `len()` are used."""

    def __init__(self, rows: list[dict[str, Any]]) -> None:
        self._rows = rows

    def to_polars(self, *, flatten: bool = False) -> pl.DataFrame:
        """Return rows as a polars DataFrame, optionally flattening nested dicts.

        With ``flatten=True``, nested dicts in any row are expanded into
        ``parent_child`` columns — matching real B-Fabric's behaviour for
        relationships like ``groupingvar`` and ``orderId.workunit``. The
        production code under test passes ``flatten=True`` exclusively
        (``bfabric_utils.py:118``, ``:132``), so divergence here would silently
        let mock-only schemas through.
        """
        if not self._rows:
            return pl.DataFrame()
        rows = [_flatten_row(r) for r in self._rows] if flatten else self._rows
        # ``infer_schema_length=None`` lets polars scan all rows when columns
        # may be sparsely populated across rows (typical for B-Fabric reads).
        return pl.DataFrame(rows, infer_schema_length=None)

    def __len__(self) -> int:
        return len(self._rows)


class _FakeReader:
    def __init__(self, owner: FakeBfabric) -> None:
        self._owner = owner

    def query(self, endpoint: str, params: dict) -> dict[_FakeUri, _FakePlate]:
        if endpoint != "plate":
            raise NotImplementedError(f"FakeBfabric.reader.query for {endpoint!r}")
        container_id = int(params["containerid"])
        return self._owner._plates_for(container_id)


class FakeBfabric:
    """Stub `Bfabric` driven by fixture JSON in a directory."""

    def __init__(self, fixtures_dir: Path, *, base_url: str = "https://fake.bfabric.test/bfabric") -> None:
        self._fixtures = fixtures_dir
        self.config = SimpleNamespace(base_url=base_url)
        self.auth = SimpleNamespace(login="testuser")
        self.reader = _FakeReader(self)

    def read(self, endpoint: str, params: dict, max_results: int | None = None) -> _FakeReadResult:  # noqa: ARG002
        if endpoint == "sample":
            container_id = int(params["containerid"])
            return _FakeReadResult(self._samples_for(container_id))
        if endpoint == "plate":
            container_id = int(params["containerid"])
            plates = self._plates_for(container_id)
            return _FakeReadResult([{"id": p.id} for p in plates.values()])
        if endpoint == "container":
            # `ContainerCache.fetch_container_row` looks a single container up by id.
            return _FakeReadResult(self._container_record_for(int(params["id"])))
        raise NotImplementedError(f"FakeBfabric.read for endpoint={endpoint!r}")

    def _samples_for(self, container_id: int) -> list[dict[str, Any]]:
        path = self._fixtures / f"samples_{container_id}.json"
        if not path.exists():
            return []
        return json.loads(path.read_text())

    def _container_record_for(self, container_id: int) -> list[dict[str, Any]]:
        """Serve one raw container record by id from ``container_<id>.json``.

        The launching-order pre-load fetches the container the app was opened from via
        ``read("container", {"id": ...})``. Missing fixture → empty list, mirroring real
        B-Fabric's "not found" result; this keeps sessions pinned to a container without
        a fixture from crashing in the pre-load cell.
        """
        path = self._fixtures / f"container_{container_id}.json"
        if not path.exists():
            return []
        return json.loads(path.read_text())

    def _plates_for(self, container_id: int) -> dict[_FakeUri, _FakePlate]:
        path = self._fixtures / f"plates_{container_id}.json"
        if not path.exists():
            return {}
        raw = json.loads(path.read_text())
        return {
            _FakeUri(entity_id=p["id"]): _FakePlate(id=p["id"], refs=SimpleNamespace(sample=p.get("samples", [])))
            for p in raw
        }


# Sentinel: an unspecified `entity_class` derives the default below; ``None`` is itself a
# valid value (a plain employee session), so it can't double as "caller said nothing".
_DEFAULT_ENTITY_CLASS = "__default__"


def build_test_session(
    fixtures_dir: Path,
    *,
    is_employee: bool = True,
    entity_id: int | None = None,
    entity_class: str | None = _DEFAULT_ENTITY_CLASS,
):
    """Return an `AppSession` backed by `FakeBfabric` for use with `_TEST_SESSION_FACTORY`.

    ``entity_class`` defaults to the launch context the app sees today (``None`` for
    employees, ``"Container"`` for non-employees). Pass an explicit value — e.g.
    ``entity_class="Order"`` — to simulate an employee who opened the app *from* an
    order, which is what triggers the launching-order pre-load.

    Imports are local so this module is importable without the rest of the qg package
    (e.g. from a uvicorn entrypoint that doesn't need other heavy deps).
    """
    from qg.bfabric_utils import AppSession, BfabricHelper, MockFeederUploader, instance_slug

    client = FakeBfabric(fixtures_dir)
    helper = BfabricHelper(client) if is_employee else BfabricHelper(client, restrict_to_container_id=entity_id)
    slug = instance_slug(client)
    resolved_entity_class = (
        (None if is_employee else "Container") if entity_class == _DEFAULT_ENTITY_CLASS else entity_class
    )
    return AppSession(
        bfabric=helper,
        client=client,
        feeder_uploader=MockFeederUploader(),
        application_id=401,
        is_employee=is_employee,
        entity_class=resolved_entity_class,
        entity_id=entity_id,
        instance_slug=slug,
        base_url=client.config.base_url,
        banner_message=f"### Test session\nConnected to **{slug}** (fake).",
    )
