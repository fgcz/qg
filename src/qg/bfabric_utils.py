"""Utilities for loading B-Fabric cached data."""

import functools
import json
import operator
from pathlib import Path

import polars as pl
from bfabric import Bfabric


def load_orders_from_cache(cache_dir: Path) -> pl.DataFrame:
    """Load orders from cached proteomics_projects.json.

    Returns DataFrame with columns: Container ID, Project ID, Project Name,
    PI, Samples, Type, Plates, Status, Area
    """
    with open(cache_dir / "proteomics_projects.json") as f:
        projects_data = json.load(f)

    orders = []
    for p in projects_data:
        for order in p.get("order", []):
            plate_count = order.get("plate_count", 0)
            sample_count = order.get("sample_count", 0)
            orders.append({
                "Container ID": order["id"],
                "Project ID": p["id"],
                "Project Name": p.get("name", ""),
                "PI": p.get("billingcustomer", ""),
                "Samples": sample_count,
                "Type": "Plates" if plate_count > 0 else "Vials",
                "Plates": plate_count,
                "Status": p.get("status", ""),
                "Area": p.get("technology", [""])[0] if p.get("technology") else "",
            })

    return pl.DataFrame(orders).filter(
        (pl.col("Samples") > 0)
        & ~pl.col("Area").str.to_lowercase().is_in(["genomics", "administration"])
    ).sort("Container ID", descending=True)


def load_plates_for_container(client: Bfabric, container_id: int) -> dict:
    """Query plates for a container."""
    return client.reader.query("plate", {"containerid": container_id})


def load_samples_for_container(
    client: Bfabric,
    container_id: int,
    plates: dict | None = None,
    plate_ids: list[int] | None = None,
) -> pl.DataFrame:
    """Load samples for a container.

    Args:
        client: B-Fabric client
        container_id: Container ID
        plates: Pre-queried plates dict (optional, avoids re-query)
        plate_ids: If provided, load samples from these plates (with positions).
                   If None, query samples directly (vial order).

    Returns:
        DataFrame with columns including _position, _gridposition for plate samples.
    """
    if plate_ids:
        if plates is None:
            plates = load_plates_for_container(client, container_id)
        selected = [p for uri, p in plates.items() if uri.components.entity_id in plate_ids]
        samples = functools.reduce(operator.iadd, (p.refs.sample for p in selected), [])
        return pl.from_dicts(s.data_dict for s in samples) if samples else pl.DataFrame()
    else:
        return client.read("sample", {"containerid": container_id}, max_results=None).to_polars()


def normalize_samples_df(df: pl.DataFrame) -> pl.DataFrame:
    """Normalize B-Fabric sample DataFrame to queue generator format.

    Renames B-Fabric columns to standardized names:
        - name → sample_name
        - id → sample_id
        - _position → position (plate samples)
        - _gridposition → grid_position (plate samples)
        - tubeid → tube_id (vial samples)

    Args:
        df: Raw DataFrame from B-Fabric (via load_samples_for_container)

    Returns:
        DataFrame with columns: sample_name, sample_id, and optionally
        position, grid_position, tube_id. Sorted by sample_id.
    """
    columns = [
        pl.col("name").alias("sample_name"),
        pl.col("id").alias("sample_id"),
    ]
    if "_position" in df.columns:
        columns.extend([
            pl.col("_position").alias("position"),
            pl.col("_gridposition").alias("grid_position"),
        ])
    if "tubeid" in df.columns:
        columns.append(pl.col("tubeid").alias("tube_id"))
    return df.select(columns).sort("sample_id")
