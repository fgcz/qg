"""Shared B-Fabric authentication wrapper for marimo ASGI apps."""

import secrets
from pathlib import Path

import marimo
from bfabric.experimental.webapp_integration_settings import WebappIntegrationSettings
from bfabric_asgi_auth import BfabricAuthMiddleware, HTMLRenderer, create_bfabric_validator
from fastapi import FastAPI
from loguru import logger
from pydantic import Field, SecretStr
from pydantic_settings import BaseSettings
from starlette.middleware.sessions import SessionMiddleware


def _generate_random_secret_key():
    logger.warning(
        "Generating random secret key, cookies will not be correct after restart. "
        "To fix this, set SECRET_KEY in your environment or config file.",
    )
    return SecretStr(secrets.token_urlsafe(64))


class AppConfig(WebappIntegrationSettings, BaseSettings):
    secret_key: SecretStr = Field(default_factory=_generate_random_secret_key)


class _InjectMetaMiddleware:
    """Pure ASGI middleware that injects app_config into scope["meta"].

    Uses raw ASGI interface instead of BaseHTTPMiddleware to ensure
    scope mutations are visible to downstream apps (e.g. marimo).
    """

    def __init__(self, app, app_config: AppConfig) -> None:
        self.app = app
        self.app_config = app_config

    async def __call__(self, scope, receive, send):
        if scope["type"] in ("http", "websocket"):
            scope.setdefault("meta", {})["app_config"] = self.app_config
        await self.app(scope, receive, send)


def create_bfabric_fastapi_app(marimo_app_path: Path) -> FastAPI:
    """Create a FastAPI app with B-Fabric auth wrapping a marimo app."""
    app = FastAPI()
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
        max_age=3600,
    )

    server = marimo.create_asgi_app().with_app(path="/", root=str(marimo_app_path.resolve()))
    app.mount("/", server.build())

    return app
