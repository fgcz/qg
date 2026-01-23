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
        for row in df.iter_rows(named=True)
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
        for sample in plate.refs.sample:
            samples.append(
                InputSample(
                    sample_name=sample["name"],
                    sample_id=sample["id"],
                    position=sample.get("_position"),
                    grid_position=sample.get("_gridposition"),
                    plate_id=plate_id,
                )
            )
    return samples


def get_plates(client: Bfabric, container_id: int) -> dict:
    """Query plates for a container."""
    return client.reader.query("plate", {"containerid": container_id})


def samples_to_dataframe(sample_group: SampleGroup) -> pl.DataFrame:
    """Convert SampleGroup to polars DataFrame for display."""
    if not sample_group.samples:
        return pl.DataFrame()
    return pl.DataFrame([s.model_dump() for s in sample_group.samples])
