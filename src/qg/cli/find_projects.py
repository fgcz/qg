#!/usr/bin/env python
"""Find proteomics projects with samples in B-Fabric."""

import json
from pathlib import Path

import polars as pl
from bfabric import Bfabric


def get_proteomics_projects_with_samples(
    client: Bfabric,
    only_running: bool = True,
    max_projects: int | None = None,
) -> list[dict]:
    """Query B-Fabric for proteomics projects that have samples.

    Args:
        client: B-Fabric client instance.
        only_running: If True, only return projects with status 'running'.
        max_projects: Maximum number of projects to return. None for all.

    Returns:
        List of project dictionaries that have samples.
    """
    # Build query filter - status can be 'pending', 'rejected', 'running'
    query = {}
    if only_running:
        query["status"] = "running"

    # Query projects
    projects = client.read("project", query, max_results=max_projects)
    if not projects:
        return []

    # Filter to proteomics projects with samples
    # - technology is a list field, filter client-side
    # - countsamples tells us if project has samples
    projects_with_samples = []
    for project in projects:
        technologies = project.get("technology", [])
        if "Proteomics" not in technologies:
            continue

        # Check if project has samples (via containers/orders)
        count_samples = project.get("countsamples", 0)
        if count_samples > 0:
            projects_with_samples.append(project)

    return projects_with_samples


def get_order_info(client: Bfabric, order_ids: list[int]) -> dict[int, dict]:
    """Query plate and sample counts for orders.

    Args:
        client: B-Fabric client instance.
        order_ids: List of order/container IDs.

    Returns:
        Dict mapping order_id to {plate_count, sample_count}.
    """
    order_info = {oid: {"plate_count": 0, "sample_count": 0} for oid in order_ids}
    for order_id in order_ids:
        # Get plate count
        plates = client.read("plate", {"containerid": order_id}, max_results=1000)
        order_info[order_id]["plate_count"] = len(plates) if plates else 0
        # Get sample count
        samples = client.read("sample", {"containerid": order_id}, max_results=1)
        if samples:
            # Use the response to get total count - query with count only
            all_samples = client.read("sample", {"containerid": order_id}, max_results=10000)
            order_info[order_id]["sample_count"] = len(all_samples) if all_samples else 0
    return order_info


def _get_cache_dir() -> Path:
    """Get the bfabric_cache directory path (project_root/bfabric_cache/)."""
    # Navigate from src/qg/cli/ up to project root
    return Path(__file__).parent.parent.parent.parent / "bfabric_cache"


def main():
    """Main entry point for finding proteomics projects."""
    # Connect to TEST environment
    client = Bfabric.connect(config_file_env="TEST")

    print("Querying B-Fabric TEST for running proteomics projects with samples...")
    projects = get_proteomics_projects_with_samples(client, only_running=True)

    print(f"\nFound {len(projects)} projects:\n")
    for p in projects:
        print(f"  [{p['id']}] {p.get('name', 'N/A')} (samples: {p.get('countsamples', 0)})")

    # Get all order IDs
    all_order_ids = []
    for p in projects:
        for order in p.get("order", []):
            all_order_ids.append(order["id"])

    print(f"\nQuerying plate and sample info for {len(all_order_ids)} orders...")
    order_info = get_order_info(client, all_order_ids)

    # Add plate and sample counts to orders in projects
    for p in projects:
        for order in p.get("order", []):
            info = order_info.get(order["id"], {"plate_count": 0, "sample_count": 0})
            order["plate_count"] = info["plate_count"]
            order["sample_count"] = info["sample_count"]

    # Ensure data directory exists
    output_dir = _get_cache_dir()
    output_dir.mkdir(exist_ok=True)

    # CSV with key fields
    df = pl.DataFrame([
        {
            "id": p["id"],
            "name": p.get("name", ""),
            "status": p.get("status", ""),
            "countsamples": p.get("countsamples", 0),
            "countorders": p.get("countorders", 0),
            "technology": ", ".join(p.get("technology", [])),
            "created": p.get("created", ""),
        }
        for p in projects
    ])
    csv_path = output_dir / "proteomics_projects.csv"
    df.write_csv(csv_path)
    print(f"\nSaved CSV to: {csv_path}")

    # Full JSON for all fields
    json_path = output_dir / "proteomics_projects.json"
    json_path.write_text(json.dumps(projects, indent=2, default=str))
    print(f"Saved JSON to: {json_path}")

    return projects


if __name__ == "__main__":
    main()
