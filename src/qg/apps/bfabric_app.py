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


class AppConfig(BaseSettings):
    validation_bfabric_instance: str = "https://fgcz-bfabric.uzh.ch/bfabric/"
    supported_bfabric_instances: list[str] = [
        "https://fgcz-bfabric.uzh.ch/bfabric/",
        "https://fgcz-bfabric-test.uzh.ch/bfabric/",
    ]
    secret_key: SecretStr = Field(default_factory=_generate_random_secret_key)


app = FastAPI()
app_config = AppConfig()

token_validator = create_bfabric_validator(settings=app_config)


class BfabricUser(BaseUser):
    """User object that carries the bfabric session fields.
    Add whatever convenience properties you need.
    """

    def __init__(self, login: str, password: str, instance: str) -> None:
        self.bfabric_auth_login = login
        self.bfabric_auth_password = password
        self.bfabric_instance = instance

    @property
    def is_authenticated(self) -> bool:
        return True

    @property
    def display_name(self) -> str:
        return self.bfabric_auth_login

    @property
    def identity(self) -> str:
        return self.bfabric_auth_login

    def get_bfabric_client(self) -> Bfabric:
        config = ConfigData(
            auth=BfabricAuth(login=self.bfabric_auth_login, password=self.bfabric_auth_password),
            client=BfabricClientConfig(base_url=self.bfabric_instance),
        )
        return Bfabric(config)


class SessionAuthBackend(AuthenticationBackend):
    """Authentication backend that reads bfabric_session dict from session."""

    async def authenticate(self, conn: HTTPConnection) -> tuple[AuthCredentials, BfabricUser] | None:
        bfabric_session = conn.scope.get("session", {}).get("bfabric_session")
        if not bfabric_session or not isinstance(bfabric_session, dict):
            return None
        user = BfabricUser(
            login=str(bfabric_session["bfabric_auth_login"]),
            password=str(bfabric_session["bfabric_auth_password"]),
            instance=str(bfabric_session["bfabric_instance"]),
        )
        creds = AuthCredentials(["authenticated"])
        return creds, user


app.add_middleware(AuthenticationMiddleware, backend=SessionAuthBackend())
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

queue_app_path = Path(__file__).parent / "queue_app.py"
server = marimo.create_asgi_app().with_app(path="/", root=str(queue_app_path.resolve()))
app.mount("/", server.build())

if __name__ == "__main__":
    import uvicorn

    uvicorn.run(app, host="localhost", port=8000)
