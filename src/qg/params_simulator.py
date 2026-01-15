"""Simulate queue parameters for testing and JSON generation."""

from __future__ import annotations

import json
from datetime import date
from pathlib import Path

from qg.config import ConfigBundle
from qg.params_models import InputSample, QueueInput, QueueParameters


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
    configs: ConfigBundle,
    technology: str,
    instrument: str,
    sampler: str,
    queue_pattern: str,
    container_id: int = 12345,
    user: str = "testuser",
    sim_date: date | None = None,
) -> QueueInput:
    """Generate simulated queue parameters.

    Args:
        num_samples: Number of user samples to generate.
        configs: ConfigBundle with all loaded configs.
        technology: Technology (proteomics, metabolomics, lipidomics).
        instrument: Instrument name.
        sampler: Sampler (e.g., "Vanquish.vial").
        queue_pattern: Queue pattern name.
        container_id: Container ID.
        user: Username for output path.
        sim_date: Date for queue (defaults to today).

    Returns:
        QueueInput with parameters and simulated samples.

    Raises:
        ValueError: If invalid combination of parameters.
    """
    # Validate combination exists
    combo = configs.combinations.get_combination(instrument, sampler)
    if combo is None:
        raise ValueError(f"Invalid combination: {instrument} + {sampler}")

    # Validate queue_pattern exists
    if configs.queue_patterns.get_pattern(technology, queue_pattern) is None:
        raise ValueError(f"Queue pattern '{queue_pattern}' not found for {technology}")

    date_str = (sim_date or date.today()).strftime("%Y%m%d")

    params = QueueParameters(
        container_id=container_id,
        technology=technology,
        instrument=instrument,
        sampler=sampler,
        output_format=combo.output_format,
        queue_pattern=queue_pattern,
        polarity=[],  # Model validator sets default for metabolomics/lipidomics
        date=date_str,
        user=user,
        method="",
        randomization=False,
        inj_vol_override=None,
    )

    samples = simulate_samples(num_samples, container_id, sampler)

    return QueueInput(parameters=params, samples=samples)


def write_params(queue_input: QueueInput, output_path: str | Path) -> Path:
    """Write queue parameters to JSON file.

    Serializes QueueInput using the expected file format with aliased field names.

    Args:
        queue_input: The QueueInput object to serialize.
        output_path: Path to write the JSON file.

    Returns:
        Path to the written file.
    """
    output_path = Path(output_path)

    # Build parameters dict with file-format field names
    params = queue_input.parameters
    params_dict = {
        "container_id": params.container_id,
        "technology": params.technology,
        "instrument": params.instrument,
        "sampler": params.sampler,
        "software": params.output_format,  # File uses "software"
        "pattern": params.queue_pattern,  # File uses "pattern"
        "polarity": params.polarity,
        "date": params.date,
        "user": params.user,
        "method": params.method,
        "randomization": params.randomization,
        "inj_vol_override": params.inj_vol_override,
    }

    # Serialize samples with aliases ("Sample Name", "Sample ID", etc.)
    samples_list = [
        sample.model_dump(by_alias=True, exclude_none=True)
        for sample in queue_input.samples
    ]

    output_dict = {
        "parameters": params_dict,
        "samples": samples_list,
    }

    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text(json.dumps(output_dict, indent=2))

    return output_path
