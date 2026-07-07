"""Find proteomics and metabolomics containers in B-Fabric and write cache CSVs."""

import os
from collections.abc import Sequence
from pathlib import Path
from typing import Annotated

import cyclopts
import polars as pl
from bfabric import Bfabric
from bfabric.utils.cli_integration import use_client
from loguru import logger

from qg.bfabric_utils import instance_slug

ACTIVE_STATUSES = [
    "running",
    "accepted",
    "arrived",
    # "submitted",
    "processing",
    "processed",
    "analyzing",
    "analyzed",
]

TECHNOLOGY_IDS = [2, 4]


def get_cache_dir(client: Bfabric) -> Path:
    """Per-instance cache directory. Root is `$QG_CACHE_DIR` or `<repo>/bfabric_cache`."""
    env = os.environ.get("QG_CACHE_DIR")
    root = Path(env) if env else Path(__file__).parents[3] / "bfabric_cache"
    return root / instance_slug(client)


class ContainerCache:
    """Fetches B-Fabric containers (orders + projects) and writes cache CSVs.

    Two cache writers, each writes exactly one file:
    - write_containers()            → <instance>/bfabric_container{suffix}.csv
    - write_containers_with_plates() → <instance>/bfabric_container_type{suffix}.csv

    Plus a read helper and a single-row fetch that write nothing:
    - read_containers()             → cached list, or an empty frame if never refreshed
    - fetch_container_row()         → one container by ID, formatted like the cache
    """

    # Column schema of the cache CSVs (see _format_one). Used to build an empty
    # frame when no cache exists yet, so callers can render an empty list instead
    # of crashing on a missing file. bfabric_container_type also carries "Type".
    _CONTAINER_SCHEMA: dict[str, type[pl.DataType]] = {
        "Container ID": pl.Int64,
        "Container Name": pl.String,
        "Project ID": pl.Int64,
        "PI": pl.String,
        "Samples": pl.Int64,
        "Status": pl.String,
        "Area": pl.String,
    }

    def __init__(self, client: Bfabric, *, active_only: bool = True) -> None:
        self._client = client
        self._active_only = active_only
        self._suffix = "" if active_only else "_all"
        self._cache_dir = get_cache_dir(client)
        self._cache_dir.mkdir(parents=True, exist_ok=True)

    def _fetch(self) -> list[pl.DataFrame]:
        """Query orders and projects from B-Fabric."""
        return [self._read_endpoint(endpoint) for endpoint in ("order", "project")]

    def _read_endpoint(self, endpoint: str) -> pl.DataFrame:
        """Read containers from a single B-Fabric endpoint."""
        query: dict = {"technologyid": TECHNOLOGY_IDS}
        if self._active_only:
            query["status"] = ACTIVE_STATUSES
        result = self._client.read(endpoint, query, max_results=None)
        return result.to_polars(flatten=True).filter(pl.col("countsamples") > 0)

    def _find_plates(self, container_ids: Sequence[int]) -> set[int]:
        """Check which containers have plates (one query per container, slow)."""
        has_plates: set[int] = set()
        for cid in container_ids:
            result = self._client.read("plate", {"containerid": cid}, max_results=1)
            if len(result) > 0:
                has_plates.add(cid)
        logger.info(f"Found {len(has_plates)}/{len(container_ids)} containers with plates")
        return has_plates

    def _format(self, dfs: list[pl.DataFrame], containers_with_plates: set[int]) -> pl.DataFrame:
        """Format and merge raw container DataFrames.

        Each container gets a "Vials" row. Containers in containers_with_plates
        also get a "Plates" row.
        """
        return pl.concat(
            [self._format_one(df, containers_with_plates) for df in dfs],
            how="diagonal_relaxed",
        ).sort("Container ID", descending=True)

    @staticmethod
    def _format_one(containers_df: pl.DataFrame, containers_with_plates: set[int]) -> pl.DataFrame:
        """Format a single raw DataFrame (orders or projects)."""
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

    def _write(self, name: str, df: pl.DataFrame) -> Path:
        """Write a DataFrame to the cache directory and return the path."""
        path = self._cache_dir / f"{name}{self._suffix}.csv"
        df.write_csv(path)
        logger.success(f"Written {len(df)} entries to {path}")
        return path

    def write_containers(self) -> Path:
        """Fetch containers and write bfabric_container.csv (fast, no plate detection)."""
        dfs = self._fetch()
        merged = self._format(dfs, containers_with_plates=set())
        output = merged.drop("Type").unique(subset=["Container ID"]).sort("Container ID", descending=True)
        return self._write("bfabric_container", output)

    def write_containers_with_plates(self) -> Path:
        """Fetch containers with plate detection and write bfabric_container_type.csv (slow)."""
        dfs = self._fetch()
        all_ids = [cid for df in dfs for cid in df["id"].to_list()]
        containers_with_plates = self._find_plates(all_ids)
        output = self._format(dfs, containers_with_plates)
        return self._write("bfabric_container_type", output)

    def read_containers(self, *, with_type: bool = False) -> pl.DataFrame:
        """Read the cached container list, sorted by Container ID (descending).

        Mirrors write_containers()/write_containers_with_plates(): with_type
        selects the plate-aware cache (bfabric_container_type) over the fast one
        (bfabric_container), and active_only (from __init__) selects the suffix.

        Returns an empty frame with the cache schema when the file is absent
        (fresh instance / never refreshed / scrubbed derivative), so the app can
        render an empty list and let the user hit "Refresh Projects" instead of
        crashing on a missing file.
        """
        name = "bfabric_container_type" if with_type else "bfabric_container"
        path = self._cache_dir / f"{name}{self._suffix}.csv"
        if not path.exists():
            logger.info("No container cache at {} yet; returning empty list", path)
            schema = dict(self._CONTAINER_SCHEMA)
            if with_type:
                schema["Type"] = pl.String
            return pl.DataFrame(schema=schema)
        return pl.read_csv(path).sort("Container ID", descending=True)

    def fetch_container_row(self, container_id: int, *, with_type: bool = False) -> pl.DataFrame:
        """Fetch one container by ID, formatted like the cache (no disk write).

        Reads the `container` supertype directly — orders and projects are both
        containers, so a single query by ID resolves either kind. Queries by ID only
        (no status/technology filter), which is exactly why the container may be missing
        from the active-only cache; this lets the app surface the launching order even
        when it predates the cache, without a full refresh.

        Returns an empty DataFrame if the ID is not found.
        """
        result = self._client.read("container", {"id": container_id}, max_results=1)
        if len(result) == 0:
            logger.warning("Container {} not found", container_id)
            return pl.DataFrame()
        df = result.to_polars(flatten=True)
        containers_with_plates = self._find_plates([container_id]) if with_type else set()
        formatted = self._format_one(df, containers_with_plates)
        return formatted if with_type else formatted.drop("Type")


def main() -> None:
    """CLI entry point for qg-find-projects."""
    app = cyclopts.App(help="Fetch B-Fabric containers and cache locally.")

    @app.default
    @use_client
    def run(
        *,
        client: Bfabric,
        all_projects: Annotated[
            bool,
            cyclopts.Parameter(("--all",), help="Fetch all containers (no status filter)."),
        ] = False,
        check_plates: Annotated[
            bool,
            cyclopts.Parameter(help="Query B-Fabric plate endpoint for each container (slow)."),
        ] = False,
    ) -> None:
        logger.info("Writing cache for instance {}", instance_slug(client))
        cache = ContainerCache(client, active_only=not all_projects)
        if check_plates:
            cache.write_containers_with_plates()
        else:
            cache.write_containers()

    app()


if __name__ == "__main__":
    main()
