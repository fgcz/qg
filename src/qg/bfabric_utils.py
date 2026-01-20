"""Utilities for loading B-Fabric data."""

import polars as pl
from bfabric import Bfabric

from qg.params_models import InputSample, SampleGroup


def get_samples(
    client: Bfabric,
    container_id: int,
    container_type: str,
    plates: dict | None = None,
    plate_ids: list[int] | None = None,
) -> SampleGroup:
    """Get samples for a container as a SampleGroup.

    Args:
        client: B-Fabric client
        container_id: Container ID
        container_type: "Vials" or "Plates"
        plates: Pre-queried plates dict (for Plates type)
        plate_ids: Filter to specific plates (only for Plates type)

    Returns:
        SampleGroup with container_id and list of InputSample
    """
    if container_type == "Plates" and plates:
        samples = _load_plate_samples(plates, plate_ids)
    else:
        samples = _load_vial_samples(client, container_id)

    return SampleGroup(container_id=container_id, samples=samples)


def _load_vial_samples(client: Bfabric, container_id: int) -> list[InputSample]:
    """Load samples for vial container."""
    df = client.read("sample", {"containerid": container_id}, max_results=None).to_polars()
    return [
        InputSample(
            sample_name=row["name"],
            sample_id=row["id"],
            tube_id=row.get("tubeid"),
        )
        for row in df.to_dicts()
    ]


def _load_plate_samples(
    plates: dict,
    plate_ids: list[int] | None = None,
) -> list[InputSample]:
    """Load samples from plates with plate_id tracking."""
    samples = []
    for uri, plate in plates.items():
        plate_id = uri.components.entity_id
        if plate_ids and plate_id not in plate_ids:
            continue
        for s in plate.refs.sample:
            d = s.data_dict
            samples.append(InputSample(
                sample_name=d["name"],
                sample_id=d["id"],
                position=d.get("_position"),
                grid_position=d.get("_gridposition"),
                plate_id=plate_id,
            ))
    return samples


def get_plates(client: Bfabric, container_id: int) -> dict:
    """Query plates for a container."""
    return client.reader.query("plate", {"containerid": container_id})


def samples_to_dataframe(sample_group: SampleGroup) -> pl.DataFrame:
    """Convert SampleGroup to polars DataFrame for display."""
    if not sample_group.samples:
        return pl.DataFrame()
    return pl.DataFrame([s.model_dump() for s in sample_group.samples])


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
