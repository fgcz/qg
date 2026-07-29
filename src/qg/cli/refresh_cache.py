"""Refresh B-Fabric container caches across configured instances."""

from typing import Annotated, Protocol

import cyclopts
from bfabric import Bfabric, BfabricAuth, BfabricClientConfig
from bfabric.config.config_data import ConfigData
from loguru import logger

from qg.apps._bfabric_auth import AppConfig
from qg.cli.find_projects import ContainerCache


class _HasFeederCredentials(Protocol):
    feeder_user_credentials: dict[str, BfabricAuth]


def _build_client(base_url: str, app_config: _HasFeederCredentials) -> Bfabric:
    auth = app_config.feeder_user_credentials[base_url]
    client = BfabricClientConfig(base_url=base_url, application_ids={}, job_notification_emails="")
    return Bfabric(ConfigData(client=client, auth=auth))


def _resolve_instances(requested: tuple[str, ...], all_instances: bool, app_config: _HasFeederCredentials) -> list[str]:
    available = list(app_config.feeder_user_credentials)
    if all_instances and requested:
        raise cyclopts.ValidationError("Pass either --all or explicit instance(s), not both.")
    if all_instances:
        return available
    if not requested:
        logger.info("Available instances: {}", ", ".join(available))
        raise cyclopts.ValidationError("No instance specified. Pass --all or one or more instance URLs.")
    unknown = [r for r in requested if r not in available]
    if unknown:
        raise cyclopts.ValidationError(f"Unknown instance(s): {unknown}. Available: {available}")
    return list(requested)


app = cyclopts.App(help="Refresh B-Fabric container caches across configured instances.")


@app.default
def run(
    instances: Annotated[
        tuple[str, ...],
        cyclopts.Parameter(help="Instance base URLs (must match feeder_user_credentials keys)."),
    ] = (),
    *,
    all_instances: Annotated[
        bool,
        cyclopts.Parameter(("--all",), help="Refresh every configured instance."),
    ] = False,
    all_projects: Annotated[
        bool,
        cyclopts.Parameter(("--all-projects",), help="Fetch all containers (no status filter)."),
    ] = False,
    check_plates: Annotated[
        bool,
        cyclopts.Parameter(help="Query B-Fabric plate endpoint for each container (slow)."),
    ] = False,
) -> None:
    # BaseSettings supplies the required values from environment sources.
    app_config = AppConfig(**{})
    targets = _resolve_instances(instances, all_instances, app_config)
    failures: dict[str, str] = {}
    for url in targets:
        logger.info("Refreshing {}", url)
        try:
            client = _build_client(url, app_config)
            cache = ContainerCache(client, active_only=not all_projects)
            if check_plates:
                cache.write_containers_with_plates()
            else:
                cache.write_containers()
        except Exception as exc:
            # Batch driver: heterogeneous failure modes (network, auth, B-Fabric API,
            # polars writes) — isolate per-instance and continue. Full traceback logged.
            logger.exception("Failed for instance {}", url)
            failures[url] = str(exc)
    for url in targets:
        mark = "FAIL" if url in failures else "OK"
        logger.info("[{}] {}", mark, url)
    if failures:
        raise SystemExit(1)


if __name__ == "__main__":
    app()
