"""Simulate queue parameters for testing and JSON generation."""

from __future__ import annotations

import json
from datetime import date
from pathlib import Path

from qg.config import CoreConfigBundle
from qg.params_models import InputSample, QueueInput, QueueParameters, SampleGroup


def simulate_samples(
    num_samples: int,
    container_id: int,
    sampler: str,
    start_sample_id: int = 1000001,
) -> list[InputSample]:
    """Generate simulated sample data.

    Args:
        num_samples: Number of samples to generate.
        container_id: Container ID for Tube ID field.
        sampler: Sampler type (affects whether Position is generated).
        start_sample_id: Starting sample ID.

    Returns:
        List of InputSample objects.
    """
    needs_position = sampler.startswith("MClass48") or sampler.startswith("Evosep")
    samples = []

    for i in range(num_samples):
        sample = InputSample(
            sample_name=f"Sample_{i + 1}",
            sample_id=start_sample_id + i,
            tube_id=f"{container_id}/{i + 1}",
            position=f"P{i + 1}" if needs_position else None,
        )
        samples.append(sample)

    return samples


def simulate_params(
    num_samples: int,
    configs: CoreConfigBundle,
    technology: str,
    instrument: str,
    sampler: str,
    queue_pattern: str,
    output_format: str = "xcalibur",
    container_id: int = 12345,
    user: str = "testuser",
    sim_date: date | None = None,
) -> QueueInput:
    """Generate simulated queue parameters (single container convenience wrapper).

    Args:
        num_samples: Number of user samples to generate.
        configs: CoreConfigBundle with core configs.
        technology: Technology (proteomics, metabolomics, lipidomics).
        instrument: Instrument name.
        sampler: Sampler (e.g., "Vanquish.vial").
        queue_pattern: Queue pattern name.
        output_format: Output format (e.g., "xcalibur", "chronos").
        container_id: Container ID.
        user: Username for output path.
        sim_date: Date for queue (defaults to today).

    Returns:
        QueueInput with parameters and sample_groups (single group).

    Raises:
        ValueError: If invalid queue pattern.
    """
    return simulate_multi_group_params(
        groups=[(container_id, num_samples)],
        configs=configs,
        technology=technology,
        instrument=instrument,
        sampler=sampler,
        queue_pattern=queue_pattern,
        output_format=output_format,
        user=user,
        sim_date=sim_date,
    )


def write_params(queue_input: QueueInput, output_path: str | Path) -> Path:
    """Write queue parameters to JSON file.

    Args:
        queue_input: The QueueInput object to serialize.
        output_path: Path to write the JSON file.

    Returns:
        Path to the written file.
    """
    output_path = Path(output_path)

    # Build parameters dict
    params = queue_input.parameters
    params_dict: dict = {
        "technology": params.technology,
        "instrument": params.instrument,
        "sampler": params.sampler,
        "output_format": params.output_format,
        "queue_pattern": params.queue_pattern,
        "polarity": params.polarity,
        "date": params.date,
        "user": params.user,
        "method": params.method,
        "randomization": params.randomization,
        "inj_vol_override": params.inj_vol_override,
    }

    output_dict: dict = {
        "parameters": params_dict,
        "sample_groups": [
            {
                "container_id": group.container_id,
                "group_name": group.group_name,
                "samples": [
                    sample.model_dump(by_alias=True, exclude_none=True)
                    for sample in group.samples
                ],
            }
            for group in queue_input.sample_groups
        ],
    }

    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text(json.dumps(output_dict, indent=2))

    return output_path


def simulate_multi_group_params(
    groups: list[tuple[int, int]],  # (container_id, num_samples)
    configs: CoreConfigBundle,
    technology: str,
    instrument: str,
    sampler: str,
    queue_pattern: str,
    output_format: str = "xcalibur",
    user: str = "testuser",
    sim_date: date | None = None,
) -> QueueInput:
    """Generate simulated queue parameters for multiple groups.

    Args:
        groups: List of (container_id, num_samples) tuples.
        configs: CoreConfigBundle with core configs.
        technology: Technology (proteomics, metabolomics, lipidomics).
        instrument: Instrument name.
        sampler: Sampler (e.g., "Vanquish.vial").
        queue_pattern: Queue pattern name.
        output_format: Output format (e.g., "xcalibur", "chronos").
        user: Username for output path.
        sim_date: Date for queue (defaults to today).

    Returns:
        QueueInput with parameters and sample_groups.

    Raises:
        ValueError: If invalid queue pattern.
    """
    # Validate queue_pattern exists
    if configs.queue_patterns.get_pattern(technology, queue_pattern) is None:
        raise ValueError(f"Queue pattern '{queue_pattern}' not found for {technology}")

    date_str = (sim_date or date.today()).strftime("%Y%m%d")

    # No container_id needed when using sample_groups
    params = QueueParameters(
        technology=technology,
        instrument=instrument,
        sampler=sampler,
        output_format=output_format,
        queue_pattern=queue_pattern,
        polarity=[],  # Model validator sets default for metabolomics/lipidomics
        date=date_str,
        user=user,
        method={},
        randomization="no",
        inj_vol_override=None,
    )

    sample_groups = []
    start_id = 1000001
    for container_id, num_samples in groups:
        samples = simulate_samples(num_samples, container_id, sampler, start_id)
        sample_groups.append(SampleGroup(
            container_id=container_id,
            group_name=f"Project_{container_id}",
            samples=samples,
        ))
        start_id += num_samples

    return QueueInput(parameters=params, sample_groups=sample_groups)
