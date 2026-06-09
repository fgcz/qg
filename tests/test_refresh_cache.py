"""Tests for the qg-refresh-cache instance-selection logic."""

from types import SimpleNamespace

import cyclopts
import pytest

pytest.importorskip("bfabric")  # core install (no qg[bfabric]) skips this module

from qg.cli.refresh_cache import _resolve_instances  # noqa: E402

pytestmark = pytest.mark.bfabric


def _fake_config(*urls: str):
    return SimpleNamespace(feeder_user_credentials={u: object() for u in urls})


def test_resolve_all() -> None:
    cfg = _fake_config("https://prod/bfabric", "https://test/bfabric")
    assert _resolve_instances((), True, cfg) == ["https://prod/bfabric", "https://test/bfabric"]


def test_resolve_explicit() -> None:
    cfg = _fake_config("https://prod/bfabric", "https://test/bfabric")
    assert _resolve_instances(("https://prod/bfabric",), False, cfg) == ["https://prod/bfabric"]


def test_resolve_no_choice_errors() -> None:
    cfg = _fake_config("https://prod/bfabric")
    with pytest.raises(cyclopts.ValidationError):
        _resolve_instances((), False, cfg)


def test_resolve_all_and_explicit_conflict() -> None:
    cfg = _fake_config("https://prod/bfabric")
    with pytest.raises(cyclopts.ValidationError):
        _resolve_instances(("https://prod/bfabric",), True, cfg)


def test_resolve_unknown_instance() -> None:
    cfg = _fake_config("https://prod/bfabric")
    with pytest.raises(cyclopts.ValidationError):
        _resolve_instances(("https://typo/bfabric",), False, cfg)
