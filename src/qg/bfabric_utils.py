"""Utilities for loading B-Fabric data into typed DataFrames."""

from dataclasses import dataclass
from pathlib import Path
from typing import Any, NamedTuple
from urllib.parse import urlparse

import polars as pl
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

from qg.sample_rows import PlateSampleRow, VialSampleRow


def instance_slug(client: Bfabric) -> str:
    """Return a short, human-readable slug for a B-Fabric instance.

    The slug is the URL netloc (e.g. ``fgcz-bfabric.uzh.ch``) — readable in
    ``ls`` output and stable across deployments.
    """
    return urlparse(client.config.base_url).netloc


# =============================================================================
# Public API
# =============================================================================


class ContainerComposition(NamedTuple):
    """Three-state classification of a B-Fabric container's sample placement."""

    has_plates: bool
    has_vials: bool


class BfabricHelper:
    def __init__(self, client: Bfabric, *, restrict_to_container_id: int | None = None) -> None:
        self.client = client
        self._restrict_to_container_id = restrict_to_container_id

    def get_samples(
        self,
        container_id: int,
        container_type: str,
        plate_ids: list[int] | None = None,
        dump_dir: Path | None = None,
        filename_prefix: str | None = None,
    ) -> pl.DataFrame:
        """Load samples from B-Fabric as a typed DataFrame.

        Args:
            container_id: Container ID.
            container_type: "Vials" or "Plates".
            plate_ids: Filter to specific plates (only for Plates type).
            dump_dir: If set, write the DataFrame to a CSV in this directory.
            filename_prefix: If set, prepend to the dump filename.

        Returns:
            DataFrame with VialSampleRow or PlateSampleRow schema.
        """
        if container_type == "Plates":
            plates = self.get_plates(container_id)
            allowed_sample_ids = (
                self._fetch_allowed_sample_ids(container_id) if self._restrict_to_container_id is not None else None
            )
            rows = self._load_plate_samples(plates, container_id, plate_ids, allowed_sample_ids)
        else:
            rows = self._load_vial_samples(container_id)

        if not rows:
            return pl.DataFrame()
        df = pl.DataFrame([r.model_dump() for r in rows])

        if dump_dir is not None:
            dump_dir = Path(dump_dir)
            dump_dir.mkdir(parents=True, exist_ok=True)
            _base = f"samples_{container_id}_{container_type}.csv"
            _name = f"{filename_prefix}_{_base}" if filename_prefix else _base
            path = dump_dir / _name
            df.write_csv(path)
            logger.info("Dumped {} samples to {}", len(df), path)

        return df

    def get_plates(self, container_id: int) -> dict:
        """Query plates for a container."""
        return self.client.reader.query("plate", {"containerid": container_id})

    def has_plates(self, container_id: int) -> bool:
        """Check whether a container has plates."""
        return bool(self.get_plates(container_id))

    def get_container_composition(self, container_id: int) -> "ContainerComposition":
        """Classify a container: does it hold plate samples, vial samples, or both?

        A sample is "on a plate" if its ID appears in any plate.refs.sample list.
        Any sample in the container that is not on a plate is treated as a vial.
        """
        plates = self.get_plates(container_id)
        plate_sample_ids = {s["id"] for plate in plates.values() for s in plate.refs.sample}
        all_sample_ids = self._fetch_allowed_sample_ids(container_id)
        on_plate = all_sample_ids & plate_sample_ids
        return ContainerComposition(
            has_plates=bool(on_plate),
            has_vials=bool(all_sample_ids - on_plate),
        )

    def _load_vial_samples(self, container_id: int) -> list[VialSampleRow]:
        """Load samples for a vial container."""
        df = self.client.read("sample", {"containerid": container_id}, max_results=None).to_polars(flatten=True)
        return [
            VialSampleRow(
                sample_name=row["name"],
                sample_id=row["id"],
                tube_id=row.get("tubeid"),
                container_id=container_id,
                grouping_var=row.get("groupingvar_name"),
            )
            for row in df.iter_rows(named=True)
        ]

    def _fetch_allowed_sample_ids(self, container_id: int) -> set[int]:
        """Authoritative set of sample IDs in `container_id`, used to filter shared plates."""
        df = self.client.read("sample", {"containerid": container_id}, max_results=None).to_polars(flatten=True)
        if df.is_empty():
            return set()
        return set(df["id"].to_list())

    @staticmethod
    def _load_plate_samples(
        plates: dict,
        container_id: int,
        plate_ids: list[int] | None = None,
        allowed_sample_ids: set[int] | None = None,
    ) -> list[PlateSampleRow]:
        """Load samples from plates.

        If `allowed_sample_ids` is provided, samples whose ID is not in the set are dropped.
        Used in non-employee mode where plates may be shared across containers.
        """
        rows: list[PlateSampleRow] = []
        for uri, plate in plates.items():
            plate_id = uri.components.entity_id
            if plate_ids and plate_id not in plate_ids:
                continue
            for sample in plate.refs.sample:
                if allowed_sample_ids is not None and sample["id"] not in allowed_sample_ids:
                    continue
                _gv = sample.get("groupingvar")
                if isinstance(_gv, dict):
                    _gv = _gv.get("name")
                rows.append(
                    PlateSampleRow(
                        sample_name=sample["name"],
                        sample_id=sample["id"],
                        container_id=container_id,
                        position=sample.get("_position"),
                        grid_position=sample.get("_gridposition"),
                        plate_id=plate_id,
                        grouping_var=_gv,
                    )
                )
        return rows


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


def resolve_app_session(request: Any, *, allow_unauthenticated: bool) -> AppSession:
    """Resolve the B-Fabric session for an incoming marimo request.

    Raises SessionError with a user-facing message if the app should refuse to render.
    """
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
                f"### Hi {client.auth.login}!\nConnected to **{slug}**" + (" _(employee)_" if employee else "")
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
                f"### Hi there!\nRunning unauthenticated as employee on **{slug}** (`QG_ALLOW_UNAUTHENTICATED=1`)."
            ),
        )
    raise SessionError("Authentication required.")
