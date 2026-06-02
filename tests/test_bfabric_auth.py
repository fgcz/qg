"""Regression tests for the BfabricUser ↔ scope["meta"] round-trip.

Marimo >= 0.23.4 coerces ``scope["user"]`` into a `{username, is_authenticated,
display_name}` dict before exposing it to cells (PR #9406), which destroys the
`BfabricUser` we need at render time. The fix stashes
``user._session_data.model_dump()`` in ``scope["meta"]["bfabric_session_data"]``
and ``resolve_app_session`` rebuilds the user from there. This test pins both
ends of that path so an upstream change cannot silently break authentication
again.
"""

import asyncio
from types import SimpleNamespace
from unittest.mock import MagicMock

import pytest
from bfabric_asgi_auth.session_data import SessionData
from bfabric_asgi_auth.user import BfabricUser

from qg.apps._bfabric_auth import _InjectMetaMiddleware
from qg.bfabric_utils import AppSession, SessionError, resolve_app_session

_SESSION_KWARGS = {
    "bfabric_instance": "https://fgcz-bfabric-test.uzh.ch/bfabric",
    "bfabric_auth_login": "alice",
    "bfabric_auth_password": "x" * 32,
    "entity_class": "Container",
    "entity_id": 12345,
    "job_id": 999,
    "application_id": 401,
}


def _drive_inject_meta(user: object) -> dict:
    """Run `_InjectMetaMiddleware` against a synthetic HTTP scope and return scope['meta']."""
    inner_called = []

    async def inner(scope, receive, send):
        inner_called.append(scope)

    scope = {"type": "http", "user": user}
    mw = _InjectMetaMiddleware(inner, app_config=MagicMock(name="app_config"))
    asyncio.run(mw(scope, MagicMock(), MagicMock()))
    assert inner_called, "downstream app was never invoked"
    return inner_called[0]["meta"]


def test_inject_meta_stashes_session_data_for_bfabric_user() -> None:
    user = BfabricUser(SessionData(**_SESSION_KWARGS))
    meta = _drive_inject_meta(user)

    assert meta["bfabric_session_data"] == _SESSION_KWARGS
    assert "app_config" in meta


def test_inject_meta_skips_non_bfabric_user() -> None:
    """A starlette SimpleUser (from `marimo run` without auth) must not produce session data."""

    class _SimpleUser:
        is_authenticated = True
        display_name = "anon"

    meta = _drive_inject_meta(_SimpleUser())
    assert "bfabric_session_data" not in meta


def test_resolve_app_session_rebuilds_user_from_meta(monkeypatch: pytest.MonkeyPatch) -> None:
    """End-to-end: middleware stashes -> resolve_app_session rebuilds -> AppSession reflects user."""
    user = BfabricUser(SessionData(**_SESSION_KWARGS))
    meta = _drive_inject_meta(user)

    monkeypatch.setattr("qg.bfabric_utils._check_is_employee", lambda *_a, **_k: True)
    monkeypatch.setattr("qg.bfabric_utils.make_feeder_client", lambda *_a, **_k: MagicMock(name="feeder"))
    monkeypatch.setattr(
        "qg.bfabric_utils.make_feeder_uploader",
        lambda *_a, **_k: MagicMock(name="uploader"),
    )

    request = SimpleNamespace(meta=meta)
    session = resolve_app_session(request, allow_unauthenticated=False)

    assert isinstance(session, AppSession)
    assert session.is_employee is True
    assert session.entity_class == "Container"
    assert session.entity_id == 12345
    assert session.application_id == 401
    assert session.base_url.rstrip("/") == _SESSION_KWARGS["bfabric_instance"].rstrip("/")
    assert session.instance_slug == "fgcz-bfabric-test.uzh.ch"
    assert "alice" in session.banner_message


def test_resolve_app_session_raises_when_meta_missing_session() -> None:
    """No stashed session data → must raise SessionError, never silently render unauthenticated."""
    request = SimpleNamespace(meta={"app_config": MagicMock()})
    with pytest.raises(SessionError):
        resolve_app_session(request, allow_unauthenticated=False)
