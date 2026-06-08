"""Uvicorn entrypoint for GUI tests.

Two modes, switched by env vars set in ``tests/gui/conftest.py::queue_app_url``:

* Hermetic (Tier A) — ``QG_TEST_FIXTURES_DIR`` is set. Installs
  ``qg.bfabric_utils._TEST_SESSION_FACTORY`` so ``resolve_app_session`` returns a
  fake-backed ``AppSession`` and no real B-Fabric traffic occurs. A middleware
  intercepts ``POST /_test/session`` so per-scenario fixtures can swap the active
  factory at runtime (e.g. employee vs. non-employee, different entity_id) —
  middleware (not a route) because marimo's catch-all mount at ``/`` shadows any
  routes registered after it.

* Real test instance (Tier B) — ``QG_ALLOW_UNAUTHENTICATED=1`` is set. No factory
  is installed and the control middleware is omitted; the marimo app falls through
  to ``Bfabric.connect()`` which loads credentials from ``~/.bfabricpy.yml``.

In both modes the FastAPI wrapper is built with ``test_mode=True`` so the B-Fabric
auth middleware is skipped — neither tier requires a browser JWT round-trip.
"""

import json
import os
from pathlib import Path

from fastapi import FastAPI

from qg import bfabric_utils
from qg.apps._bfabric_auth import create_bfabric_fastapi_app

_fixtures_dir_env = os.environ.get("QG_TEST_FIXTURES_DIR")


def _install_default_factory(fixtures_dir: Path) -> None:
    from tests.gui._fake_bfabric import build_test_session

    is_employee = os.environ.get("QG_TEST_IS_EMPLOYEE", "1") == "1"
    entity_id_env = os.environ.get("QG_TEST_ENTITY_ID")
    entity_id = int(entity_id_env) if entity_id_env else None

    def _factory(_request):
        return build_test_session(fixtures_dir, is_employee=is_employee, entity_id=entity_id)

    bfabric_utils._TEST_SESSION_FACTORY = _factory


class _SessionControlMiddleware:
    """Intercept ``POST /_test/session`` before it hits the marimo mount.

    Body shape: ``{"is_employee": bool, "entity_id": int | null}``.
    Responds ``200 {"ok": true}``; the next page render uses the swapped factory.
    """

    def __init__(self, app, fixtures_dir: Path) -> None:
        self.app = app
        self._fixtures_dir = fixtures_dir

    async def __call__(self, scope, receive, send):
        if scope["type"] == "http" and scope["path"] == "/_test/session" and scope["method"] == "POST":
            body = b""
            while True:
                message = await receive()
                body += message.get("body", b"")
                if not message.get("more_body"):
                    break
            cfg = json.loads(body or b"{}")
            self._swap_factory(cfg)
            await self._respond_ok(send)
            return
        await self.app(scope, receive, send)

    def _swap_factory(self, cfg: dict) -> None:
        from tests.gui._fake_bfabric import build_test_session

        is_employee = bool(cfg.get("is_employee", True))
        entity_id = cfg.get("entity_id")
        entity_id_int = int(entity_id) if entity_id is not None else None

        def _factory(_request):
            return build_test_session(self._fixtures_dir, is_employee=is_employee, entity_id=entity_id_int)

        bfabric_utils._TEST_SESSION_FACTORY = _factory

    @staticmethod
    async def _respond_ok(send) -> None:
        await send({"type": "http.response.start", "status": 200, "headers": [(b"content-type", b"application/json")]})
        await send({"type": "http.response.body", "body": b'{"ok":true}'})


def _build_app() -> FastAPI:
    app = create_bfabric_fastapi_app(
        Path("src/qg/apps/queue_app.py"),
        app_name="queue-gen-test",
        test_mode=True,
    )
    if _fixtures_dir_env:
        fixtures_dir = Path(_fixtures_dir_env)
        _install_default_factory(fixtures_dir)
        app.add_middleware(_SessionControlMiddleware, fixtures_dir=fixtures_dir)
    return app


app = _build_app()
