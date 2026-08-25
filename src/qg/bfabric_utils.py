"""B-Fabric session resolution and workunit upload utilities."""

from collections.abc import Callable
from dataclasses import dataclass
from typing import Any
from urllib.parse import urlparse

from bfabric import Bfabric, BfabricClientConfig
from bfabric.config.config_data import ConfigData
from bfabric_asgi_auth.session_data import SessionData
from bfabric_asgi_auth.user import BfabricUser
from bfabric_rest_proxy.feeder_operations.create_workunit import (
    CreateWorkunitParams,
    create_workunit,
)
from bfabric_rest_proxy.feeder_operations.is_employee import is_employee as _check_is_employee
from loguru import logger

from qg.bfabric_samples import BfabricHelper


def instance_slug(client: Bfabric) -> str:
    """Return a short, human-readable slug for a B-Fabric instance.

    The slug is the URL netloc (e.g. ``fgcz-bfabric.uzh.ch``) — readable in
    ``ls`` output and stable across deployments.
    """
    return urlparse(client.config.base_url).netloc


# =============================================================================
# Feeder uploaders
# =============================================================================


class BfabricFeederUploader:
    """Real uploader using B-Fabric feeder client."""

    def __init__(self, user_client: Bfabric, feeder_client) -> None:
        self._user_client = user_client
        self._feeder_client = feeder_client

    def upload(self, params: CreateWorkunitParams) -> str:
        result = create_workunit(
            user_client=self._user_client,
            feeder_client=self._feeder_client,
            params=params,
        )
        return f"Created [Workunit {result.id}]({result.uri})"


class MockFeederUploader:
    """Mock uploader for local testing."""

    def upload(self, params: CreateWorkunitParams) -> str:
        return (
            f"**[Mock]** Would create workunit in container {params.container_id} "
            f"with {len(params.resources)} resources."
        )


def make_feeder_uploader(
    user_client: Bfabric, feeder_client: Bfabric | None = None
) -> BfabricFeederUploader | MockFeederUploader:
    """Return a real uploader if feeder_client is available, otherwise a mock."""
    if feeder_client is not None:
        return BfabricFeederUploader(user_client, feeder_client)
    return MockFeederUploader()


# =============================================================================
# App session resolution
# =============================================================================


class SessionError(Exception):
    """Raised when the app should refuse to render. `message` is user-facing."""

    def __init__(self, message: str) -> None:
        super().__init__(message)
        self.message = message


def make_feeder_client(app_config, instance_url: str) -> Bfabric:
    """Create a feeder Bfabric client from app config credentials.

    Raises SessionError if no feeder credentials are configured for the instance —
    this function is only called from the authenticated session path, where the
    feeder is required to determine employee status.
    """
    creds = app_config.feeder_user_credentials.get(instance_url)
    if creds is None:
        raise SessionError(
            f"Feeder credentials not configured for instance {instance_url!r}; cannot determine employee status."
        )
    return Bfabric(ConfigData(auth=creds, client=BfabricClientConfig(base_url=instance_url)))


@dataclass(frozen=True)
class AppSession:
    """Resolved per-request B-Fabric context for the queue app."""

    bfabric: BfabricHelper
    client: Bfabric
    feeder_uploader: BfabricFeederUploader | MockFeederUploader
    application_id: int
    is_employee: bool
    entity_class: str | None
    entity_id: int | None
    instance_slug: str
    base_url: str
    banner_message: str


_TEST_SESSION_FACTORY: Callable[[Any], AppSession] | None = None
"""Test-only override. When set, `resolve_app_session` calls it instead of the real auth path.

The factory receives the request object so tests can vary the session per scenario.
Set via `qg.bfabric_utils._TEST_SESSION_FACTORY = ...` from a pytest fixture; reset on teardown.
"""


def resolve_app_session(request: Any, *, allow_unauthenticated: bool) -> AppSession:
    """Resolve the B-Fabric session for an incoming marimo request.

    Raises SessionError with a user-facing message if the app should refuse to render.
    """
    if _TEST_SESSION_FACTORY is not None:
        return _TEST_SESSION_FACTORY(request)
    # `marimo run` injects a starlette SimpleUser with is_authenticated=True but no B-Fabric data;
    # only a real BfabricUser counts as authenticated here. Marimo >= 0.23.4 strips
    # scope["user"] down to a {username, is_authenticated, display_name} dict before reaching
    # the cell, so we recover the BfabricUser from the session-data dict that
    # ``_InjectMetaMiddleware`` stashes in ``scope["meta"]``.
    meta = getattr(request, "meta", None) or {}
    session_dict = meta.get("bfabric_session_data") if isinstance(meta, dict) else None
    user = BfabricUser(SessionData(**session_dict)) if session_dict else None
    if isinstance(user, BfabricUser) and user.is_authenticated:
        client = user.get_bfabric_client()
        feeder_client = make_feeder_client(request.meta["app_config"], user.instance)
        try:
            employee = _check_is_employee(client, feeder_client)
        except Exception as exc:
            logger.exception("is_employee check failed; refusing to render")
            raise SessionError("Could not determine employee status. Refusing to render.") from exc
        if not employee and (user.entity_class not in {"Container", "Order", "Project"} or user.entity_id is None):
            raise SessionError(
                "Non-employee mode needs a Container, Order, or Project in the request "
                f"(got entity_class={user.entity_class!r}, entity_id={user.entity_id!r})."
            )
        slug = instance_slug(client)
        # Non-employees can't read plate/sample with their own creds; route reads through
        # the feeder and filter shared-plate samples to the request's container_id.
        helper = (
            BfabricHelper(client) if employee else BfabricHelper(feeder_client, restrict_to_container_id=user.entity_id)
        )
        return AppSession(
            bfabric=helper,
            client=client,
            feeder_uploader=make_feeder_uploader(client, feeder_client),
            application_id=user.application_id,
            is_employee=employee,
            entity_class=user.entity_class,
            entity_id=user.entity_id,
            instance_slug=slug,
            base_url=client.config.base_url,
            banner_message=(
                f"**Hi {client.auth.login}!** Connected to **{slug}**" + (" _(employee)_" if employee else "")
            ),
        )
    if allow_unauthenticated:
        client = Bfabric.connect()
        slug = instance_slug(client)
        return AppSession(
            bfabric=BfabricHelper(client),
            client=client,
            feeder_uploader=make_feeder_uploader(client, None),
            application_id=401,
            is_employee=True,
            entity_class=None,
            entity_id=None,
            instance_slug=slug,
            base_url=client.config.base_url,
            banner_message=(
                f"**Hi there!** Running unauthenticated as employee on **{slug}** (`QG_ALLOW_UNAUTHENTICATED=1`)."
            ),
        )
    raise SessionError("Authentication required.")
