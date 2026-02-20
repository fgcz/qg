"""Shared B-Fabric authentication wrapper for marimo ASGI apps."""

import secrets
from pathlib import Path

import marimo
from bfabric import Bfabric, BfabricAuth, BfabricClientConfig
from bfabric.config.config_data import ConfigData
from bfabric_asgi_auth import (
    BfabricAuthMiddleware,
    HTMLRenderer,
    create_bfabric_validator,
)
from fastapi import FastAPI
from loguru import logger
from pydantic import Field, SecretStr
from pydantic_settings import BaseSettings
from starlette.authentication import (
    AuthCredentials,
    AuthenticationBackend,
    BaseUser,
)
from starlette.middleware.authentication import AuthenticationMiddleware
from starlette.middleware.sessions import SessionMiddleware
from starlette.requests import HTTPConnection


def _generate_random_secret_key():
    logger.warning(
        "Generating random secret key, cookies will not be correct after restart. "
        "To fix this, set SECRET_KEY in your environment or config file.",
    )
    return SecretStr(secrets.token_urlsafe(64))


class _AppConfig(BaseSettings):
    validation_bfabric_instance: str
    supported_bfabric_instances: list[str]
    secret_key: SecretStr = Field(default_factory=_generate_random_secret_key)
    feeder_user_credentials: dict[str, BfabricAuth]


class BfabricUser(BaseUser):
    """User object that carries the bfabric session fields."""

    def __init__(
        self, login: str, password: str, instance: str, feeder_login: str | None, feeder_password: str | None
    ) -> None:
        self.bfabric_auth_login = login
        self.bfabric_auth_password = password
        self.bfabric_instance = instance
        self.feeder_login = feeder_login
        self.feeder_password = feeder_password

    @property
    def is_authenticated(self) -> bool:
        return True

    @property
    def display_name(self) -> str:
        return self.bfabric_auth_login

    @property
    def identity(self) -> str:
        return self.bfabric_auth_login

    def get_bfabric_user_client(self) -> Bfabric:
        config = ConfigData(
            auth=BfabricAuth(login=self.bfabric_auth_login, password=self.bfabric_auth_password),
            client=BfabricClientConfig(base_url=self.bfabric_instance),
        )
        return Bfabric(config)

    def get_bfabric_feeder_client(self) -> Bfabric:
        config = ConfigData(
            auth=BfabricAuth(login=self.feeder_login, password=self.feeder_password),
            client=BfabricClientConfig(base_url=self.bfabric_instance),
        )
        return Bfabric(config)


class _SessionAuthBackend(AuthenticationBackend):
    """Authentication backend that reads bfabric_session dict from session."""

    def __init__(self, app_config: _AppConfig) -> None:
        super().__init__()
        self._app_config = app_config

    async def authenticate(self, conn: HTTPConnection) -> tuple[AuthCredentials, BfabricUser] | None:
        bfabric_session = conn.scope.get("session", {}).get("bfabric_session")
        if not bfabric_session or not isinstance(bfabric_session, dict):
            return None

        # obtain the feeder user
        feeder_user = self._app_config.feeder_user_credentials.get(bfabric_session["bfabric_instance"])

        user = BfabricUser(
            login=str(bfabric_session["bfabric_auth_login"]),
            password=str(bfabric_session["bfabric_auth_password"]),
            instance=str(bfabric_session["bfabric_instance"]),
            feeder_login=feeder_user.login if feeder_user is not None else None,
            feeder_password=feeder_user.password if feeder_user is not None else None,
        )
        creds = AuthCredentials(["authenticated"])
        return creds, user


def create_bfabric_fastapi_app(marimo_app_path: Path) -> FastAPI:
    """Create a FastAPI app with B-Fabric auth wrapping a marimo app."""
    app = FastAPI()
    app_config = _AppConfig()
    token_validator = create_bfabric_validator(settings=app_config)

    app.add_middleware(AuthenticationMiddleware, backend=_SessionAuthBackend(app_config=app_config))
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
