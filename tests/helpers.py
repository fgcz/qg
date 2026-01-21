"""Shared test helper functions for creating test data."""

from datetime import date

from qg.params_models import InputSample, QueueInput, QueueParameters, SampleGroup


def make_samples(
    num_samples: int,
    container_id: int,
    start_sample_id: int = 1000001,
) -> list[InputSample]:
    """Create a list of InputSample objects for testing.

    Args:
        num_samples: Number of samples to create.
        container_id: Container ID for tube_id field.
        start_sample_id: Starting sample ID.

    Returns:
        List of InputSample objects.
    """
    return [
        InputSample(
            sample_name=f"Sample_{i + 1}",
            sample_id=start_sample_id + i,
            tube_id=f"{container_id}/{i + 1}",
        )
        for i in range(num_samples)
    ]


def make_sample_groups(groups: list[tuple[int, int]]) -> list[SampleGroup]:
    """Create SampleGroup list from (container_id, num_samples) tuples.

    Args:
        groups: List of (container_id, num_samples) tuples.

    Returns:
        List of SampleGroup objects.
    """
    sample_groups = []
    start_id = 1000001
    for container_id, num_samples in groups:
        samples = make_samples(num_samples, container_id, start_id)
        sample_groups.append(
            SampleGroup(
                container_id=container_id,
                group_name=f"Project_{container_id}",
                samples=samples,
            )
        )
        start_id += num_samples
    return sample_groups


def make_queue_input(
    groups: list[tuple[int, int]] | None = None,
    *,
    num_samples: int = 5,
    tech_area: str = "Proteomics",
    instrument: str = "ASTRAL_1",
    sampler: str = "Vanquish.vial",
    queue_pattern: str = "standard",
    output_format: str = "xcalibur",
    container_id: int = 12345,
) -> QueueInput:
    """Create a QueueInput for testing.

    Can be called two ways:
    1. With `groups` parameter: creates multi-group input
    2. Without `groups`: creates single-group input using num_samples and container_id

    Args:
        groups: Optional list of (container_id, num_samples) tuples for multi-group.
        num_samples: Number of samples (single-group mode only).
        tech_area: Technology identifier.
        instrument: Instrument name.
        sampler: Sampler name (e.g., "Vanquish.vial").
        queue_pattern: Queue pattern name.
        output_format: Output format name.
        container_id: Container ID (single-group mode only).

    Returns:
        QueueInput object for testing.
    """
    params = QueueParameters(
        tech_area=tech_area,
        instrument=instrument,
        sampler=sampler,
        output_format=output_format,
        queue_pattern=queue_pattern,
        polarity=[],
        date=date.today().strftime("%Y%m%d"),
        user="testuser",
        method={},
        randomization="no",
        inj_vol_override=None,
    )

    if groups is not None:
        sample_groups = make_sample_groups(groups)
    else:
        sample_groups = [
            SampleGroup(
                container_id=container_id,
                group_name=f"Project_{container_id}",
                samples=make_samples(num_samples, container_id),
            )
        ]

    return QueueInput(parameters=params, sample_groups=sample_groups)
