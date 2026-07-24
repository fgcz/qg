"""Shared B-Fabric authentication wrapper for marimo (ASGI) and Dash (WSGI) apps."""

import secrets
from pathlib import Path

import marimo
from asgiref.wsgi import WsgiToAsgi, WsgiToAsgiInstance
from bfabric.experimental.webapp_integration_settings import WebappIntegrationSettings
from bfabric_asgi_auth import BfabricAuthMiddleware, HTMLRenderer, create_bfabric_validator
from bfabric_asgi_auth.user import BfabricUser
from fastapi import FastAPI
from loguru import logger
from pydantic import Field, SecretStr
from pydantic_settings import BaseSettings
from starlette.middleware.sessions import SessionMiddleware

#: WSGI environ key under which the ASGI ``scope["meta"]`` is surfaced to a
#: mounted WSGI app (Dash), so it can call ``resolve_app_session``.
WSGI_META_KEY = "qg.meta"


def _generate_random_secret_key():
    logger.warning(
        "Generating random secret key, cookies will not be correct after restart. "
        "To fix this, set SECRET_KEY in your environment or config file.",
    )
    return SecretStr(secrets.token_urlsafe(64))


class AppConfig(WebappIntegrationSettings, BaseSettings):
    secret_key: SecretStr = Field(default_factory=_generate_random_secret_key)


class _InjectMetaMiddleware:
    """Pure ASGI middleware that injects app_config (and a serializable copy of
    the authenticated B-Fabric session) into scope["meta"].

    Uses raw ASGI interface instead of BaseHTTPMiddleware to ensure scope
    mutations are visible to downstream apps (e.g. marimo).

    Marimo >= 0.23.4 coerces scope["user"] into a {username, is_authenticated,
    display_name} dict before exposing it via ``mo.app_meta().request.user``
    (PR #9406, for msgspec IPC). That strips the BfabricUser methods we need,
    so we stash the underlying SessionData as a plain dict in
    ``scope["meta"]["bfabric_session_data"]``; ``resolve_app_session`` rebuilds
    a BfabricUser from it.
    """

    def __init__(self, app, app_config: AppConfig) -> None:
        self.app = app
        self.app_config = app_config

    async def __call__(self, scope, receive, send):
        if scope["type"] in ("http", "websocket"):
            meta = scope.setdefault("meta", {})
            meta["app_config"] = self.app_config
            user = scope.get("user")
            if isinstance(user, BfabricUser):
                meta["bfabric_session_data"] = user._session_data.model_dump()
        await self.app(scope, receive, send)


class _MetaExposingWsgiInstance(WsgiToAsgiInstance):
    """WSGI-to-ASGI instance that copies the ASGI ``scope["meta"]`` (set by
    :class:`_InjectMetaMiddleware`) into the WSGI environ under
    :data:`WSGI_META_KEY`, so a mounted Dash/Flask app can recover the B-Fabric
    session with ``resolve_app_session``."""

    def build_environ(self, scope, body):
        environ = super().build_environ(scope, body)
        environ[WSGI_META_KEY] = scope.get("meta", {})
        return environ


class _MetaExposingWsgiToAsgi(WsgiToAsgi):
    """:class:`WsgiToAsgi` whose per-request instances surface ``scope["meta"]``."""

    async def __call__(self, scope, receive, send):
        await _MetaExposingWsgiInstance(self.wsgi_application, self.duplicate_header_limit)(scope, receive, send)


def _apply_auth_stack(app: FastAPI, *, app_name: str, mount_path: str) -> None:
    """Add the B-Fabric session + auth + meta-injection middleware stack to `app`.

    Framework-agnostic: it only sets ``scope["meta"]`` / ``scope["user"]`` around
    whatever app is mounted below it (marimo ASGI or a WSGI-wrapped Dash app).
    """
    app_config = AppConfig()
    token_validator = create_bfabric_validator(settings=app_config)

    app.add_middleware(_InjectMetaMiddleware, app_config=app_config)
    app.add_middleware(
        BfabricAuthMiddleware,
        token_validator=token_validator,
        renderer=HTMLRenderer(),
    )
    app.add_middleware(
        SessionMiddleware,
        secret_key=app_config.secret_key.get_secret_value(),
        session_cookie=f"{app_name}_session",
        path=mount_path,
        max_age=3600,
        https_only=True,
        same_site="lax",
    )


def create_bfabric_fastapi_app(
    marimo_app_path: Path,
    *,
    app_name: str,
    mount_path: str = "/",
    test_mode: bool = False,
) -> FastAPI:
    """Create a FastAPI app with B-Fabric auth wrapping a marimo app.

    `app_name` namespaces the session cookie so co-hosted apps under the same
    domain (portal/Caddy routing /<name>/*) don't share session state.
    `mount_path` scopes the cookie's Path attribute; pass the same value as
    uvicorn's --root-path when set.

    When `test_mode=True`, the B-Fabric auth and config-injection middlewares
    are skipped — the marimo app is mounted directly and `resolve_app_session`
    must be stubbed via `qg.bfabric_utils._TEST_SESSION_FACTORY`.
    """
    app = FastAPI()
    if not test_mode:
        _apply_auth_stack(app, app_name=app_name, mount_path=mount_path)

    server = marimo.create_asgi_app().with_app(path="/", root=str(marimo_app_path.resolve()))
    app.mount("/", server.build())

    return app


def create_bfabric_wsgi_app(
    wsgi_app,
    *,
    app_name: str,
    mount_path: str = "/",
    test_mode: bool = False,
) -> FastAPI:
    """Create a FastAPI app with B-Fabric auth wrapping a WSGI app (e.g. a Dash
    server).

    Same auth stack as :func:`create_bfabric_fastapi_app`; the WSGI app is
    wrapped so the authenticated session (``scope["meta"]``) is exposed to it via
    ``environ[WSGI_META_KEY]``. In `test_mode` the middlewares are skipped and the
    session must be stubbed via `qg.bfabric_utils._TEST_SESSION_FACTORY`.
    """
    app = FastAPI()
    if not test_mode:
        _apply_auth_stack(app, app_name=app_name, mount_path=mount_path)

    app.mount("/", _MetaExposingWsgiToAsgi(wsgi_app))

    return app
