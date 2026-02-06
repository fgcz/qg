"""Find proteomics and metabolomics containers with samples in B-Fabric."""

import argparse
from collections.abc import Sequence
from pathlib import Path

import polars as pl
from bfabric import Bfabric
from loguru import logger

ACTIVE_STATUSES = [
    "running",
    "accepted",
    "arrived",
    # "submitted",
    "processing",
    "analyzing",
]


def get_cache_dir() -> Path:
    """Get the bfabric_cache directory path.

    :return: Path to project_root/bfabric_cache/.
    """
    return Path(__file__).parents[3] / "bfabric_cache"


def read_containers(
    client: Bfabric,
    endpoint: str,
    max_results: int | None,
    technology_ids: Sequence[int],
    *,
    active_only: bool = True,
) -> pl.DataFrame:
    """Read containers from B-Fabric API.

    :param client: B-Fabric client instance.
    :param endpoint: API endpoint ('order' or 'project').
    :param max_results: Maximum results to fetch.
    :param technology_ids: Technology IDs to filter by.
    :param active_only: If True, filter by active statuses. If False, fetch all.
    :return: Containers with samples.
    """
    query: dict = {"technologyid": technology_ids}
    if active_only:
        query["status"] = ACTIVE_STATUSES
    result = client.read(endpoint, query, max_results=max_results)
    return result.to_polars(flatten=True).filter(pl.col("countsamples") > 0)


def find_containers_with_plates(client: Bfabric, container_ids: Sequence[int]) -> set[int]:
    """Check which containers have plates by querying the plate endpoint.

    :param client: B-Fabric client instance.
    :param container_ids: Container IDs to check.
    :return: Set of container IDs that have plates.
    """
    has_plates: set[int] = set()
    for cid in container_ids:
        result = client.read("plate", {"containerid": cid}, max_results=1)
        if len(result) > 0:
            has_plates.add(cid)
    logger.info(f"Found {len(has_plates)}/{len(container_ids)} containers with plates")
    return has_plates


def extract_output(containers_df: pl.DataFrame, containers_with_plates: set[int]) -> pl.DataFrame:
    """Extract containers for queue app.

    All containers get a "Vials" row. Containers that also have plates
    get an additional "Plates" row.

    :param containers_df: Raw containers DataFrame.
    :param containers_with_plates: Container IDs that have plates.
    :return: Formatted DataFrame with Container ID, Name, Project ID, PI,
        Samples, Type, Status, Area.
    """
    optional_columns = {"project_id"}
    missing_columns = optional_columns - set(containers_df.columns)
    containers_df = containers_df.with_columns(**{col: pl.lit(None) for col in missing_columns})

    base = containers_df.select(
        pl.col("id").alias("Container ID"),
        pl.col("name").alias("Container Name"),
        pl.col("project_id").alias("Project ID"),
        pl.col("billingcustomer").alias("PI"),
        pl.col("countsamples").alias("Samples"),
        pl.col("status").alias("Status"),
        pl.col("technology").list.first().alias("Area"),
    )

    vials = base.with_columns(pl.lit("Vials").alias("Type"))

    if containers_with_plates:
        plates = base.filter(pl.col("Container ID").is_in(containers_with_plates)).with_columns(
            pl.lit("Plates").alias("Type")
        )
        return pl.concat([vials, plates]).sort("Container ID", descending=True)

    return vials


def generate_bfabric_cache(
    client: Bfabric,
    update_orders: bool = True,
    update_projects: bool = True,
    *,
    active_only: bool = True,
) -> None:
    """Generate B-Fabric cache files.

    Writes CSV files to bfabric_cache/:
    - active_only=True:  bfabric_order.csv, bfabric_project.csv
    - active_only=False: bfabric_order_all.csv, bfabric_project_all.csv

    :param client: B-Fabric client instance.
    :param update_orders: Fetch and cache orders.
    :param update_projects: Fetch and cache projects.
    :param active_only: If True, filter by active statuses. If False, fetch all.
    """
    technology_ids = [2, 4]
    suffix = "" if active_only else "_all"

    dfs = {}
    if update_orders:
        dfs["order"] = read_containers(
            client, "order", max_results=None, technology_ids=technology_ids, active_only=active_only
        )
    if update_projects:
        dfs["project"] = read_containers(
            client, "project", max_results=None, technology_ids=technology_ids, active_only=active_only
        )

    all_ids = [cid for df in dfs.values() for cid in df["id"].to_list()]
    containers_with_plates = find_containers_with_plates(client, all_ids)

    outputs = {key: extract_output(df, containers_with_plates) for key, df in dfs.items()}

    cache_path = get_cache_dir()
    cache_path.mkdir(exist_ok=True)
    for key, output in outputs.items():
        path = cache_path / f"bfabric_{key}{suffix}.csv"
        output.write_csv(path)
        logger.success(f"Written {len(output)} entries to {path}")


def main() -> None:
    """CLI entry point for qg-find-projects."""
    parser = argparse.ArgumentParser(description="Fetch B-Fabric containers and cache locally.")
    parser.add_argument(
        "--all",
        action="store_true",
        help="Fetch all containers (no status filter). Writes to bfabric_order_all.csv / bfabric_project_all.csv.",
    )
    args = parser.parse_args()

    client = Bfabric.connect(config_file_env="PRODUCTION")
    generate_bfabric_cache(client=client, active_only=not args.all)


if __name__ == "__main__":
    main()
