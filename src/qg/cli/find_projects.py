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


def extract_output(containers_df: pl.DataFrame) -> pl.DataFrame:
    """Extract containers for queue app.

    :param containers_df: Raw containers DataFrame.
    :return: Formatted DataFrame with Container ID, Name, Project ID, PI,
        Samples, Type, Status, Area.
    """
    optional_columns = {"project_id", "processesplates"}
    missing_columns = optional_columns - set(containers_df.columns)
    containers_df = containers_df.with_columns(**{col: pl.lit(None) for col in missing_columns})

    return containers_df.select(
        pl.col("id").alias("Container ID"),
        pl.col("name").alias("Container Name"),
        pl.col("project_id").alias("Project ID"),
        pl.col("billingcustomer").alias("PI"),
        pl.col("countsamples").alias("Samples"),
        pl.when(pl.col("processesplates").cast(pl.Boolean))
        .then(pl.lit("Plates"))
        .otherwise(pl.lit("Vials"))
        .alias("Type"),
        pl.col("status").alias("Status"),
        pl.col("technology").list.first().alias("Area"),
    )


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

    outputs = {key: extract_output(df) for key, df in dfs.items()}

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
