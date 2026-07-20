"""Shared test helper functions for creating test data."""

from datetime import date
from pathlib import Path

from qg.config_models.loader import QGConfiguration, qg_configuration
from qg.params_models import (
    ContainerBatch,
    QueueParameters,
    VialQueue,
    VialQueueInput,
    VialSample,
    current_qg_version,
)

CONFIG_DIR = Path(__file__).parent.parent / "qg_configs"


def make_samples(
    num_samples: int,
    container_id: int,
    start_sample_id: int = 1000001,
) -> list[VialSample]:
    """Create a list of VialSample objects for testing.

    Args:
        num_samples: Number of samples to create.
        container_id: Container ID for each sample.
        start_sample_id: Starting sample ID.

    Returns:
        List of VialSample objects.
    """
    return [
        VialSample(
            sample_name=f"Sample_{i + 1}",
            sample_id=start_sample_id + i,
            tube_id=f"{container_id}/{i + 1}",
            container_id=container_id,
        )
        for i in range(num_samples)
    ]


def make_sample_groups(
    groups: list[tuple[int, int]],
) -> tuple[dict[int, ContainerBatch], list[VialSample]]:
    """Create batches dict and flat sample list from (container_id, num_samples) tuples.

    Args:
        groups: List of (container_id, num_samples) tuples.

    Returns:
        Tuple of (batches dict, flat sample list).
    """
    batches: dict[int, ContainerBatch] = {}
    all_samples: list[VialSample] = []
    start_id = 1000001
    for container_id, num_samples in groups:
        batches[container_id] = ContainerBatch(
            container_id=container_id,
            container_name=f"Project_{container_id}",
        )
        samples = make_samples(num_samples, container_id, start_id)
        all_samples.extend(samples)
        start_id += num_samples
    return batches, all_samples


def make_queue_input(
    groups: list[tuple[int, int]] | None = None,
    *,
    num_samples: int = 5,
    tech_area: str = "Proteomics",
    instrument: str = "ASTRAL_1",
    sampler: str = "Vanquish",
    queue_pattern: str = "standard",
    queue_type: str = "Vial",
    plate_layout: str = "Vanquish_54",
    qc_layout_name: str = "standard",
    output_format: str = "xcalibur",
    container_id: int = 12345,
    config: QGConfiguration | None = None,
) -> VialQueueInput:
    """Create a VialQueueInput for testing.

    Can be called two ways:
    1. With `groups` parameter: creates multi-group input
    2. Without `groups`: creates single-group input using num_samples and container_id

    Args:
        groups: Optional list of (container_id, num_samples) tuples for multi-group.
        num_samples: Number of samples (single-group mode only).
        tech_area: Technology identifier.
        instrument: Instrument name.
        sampler: Sampler name (e.g., "Vanquish").
        queue_pattern: Queue pattern name.
        queue_type: Queue type ("Vial" or "Plate").
        plate_layout: Plate layout name.
        output_format: Output format name.
        container_id: Container ID (single-group mode only).

    Returns:
        VialQueueInput object for testing.
    """
    params = QueueParameters(
        tech_area=tech_area,
        instrument=instrument,
        sampler=sampler,
        output_format=output_format,
        queue_pattern=queue_pattern,
        queue_type=queue_type,
        plate_layout=plate_layout,
        qc_layout_name=qc_layout_name,
        polarity=["pos"],
        date=date.today().strftime("%Y%m%d"),
        user="testuser",
        method={},
        randomization="no",
        inj_vol_override=None,
    )

    if groups is not None:
        batches, all_samples = make_sample_groups(groups)
    else:
        batches = {
            container_id: ContainerBatch(
                container_id=container_id,
                container_name=f"Project_{container_id}",
            )
        }
        all_samples = make_samples(num_samples, container_id)

    queue = VialQueue(batches=batches, samples=all_samples)
    resolved_config = (config or qg_configuration(CONFIG_DIR)).subset_for(params)
    return VialQueueInput(
        parameters=params,
        queue=queue,
        qg_version=current_qg_version(),
        resolved_config=resolved_config,
    )
