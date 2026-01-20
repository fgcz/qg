"""Tests for params_simulator module."""

import pytest

from qg.config import load_all_configs
from qg.params_simulator import simulate_params


@pytest.fixture
def configs():
    """Load configs for testing."""
    return load_all_configs("qg_configs")


@pytest.mark.parametrize(
    "technology,instrument,sampler,queue_pattern,output_format",
    [
        ("Proteomics", "ASTRAL_1", "Vanquish.vial", "standard", "xcalibur"),
        ("Proteomics", "ASTRAL_1", "Vanquish.plate", "frequent", "xcalibur"),
        ("Proteomics", "TIMSTOF_1", "Evosep.vial", "standard", "hystar"),
        ("Metabolomics", "EXPLORIS_3", "Vanquish.vial", "standard", "xcalibur"),
    ],
)
def test_simulate_params(configs, technology, instrument, sampler, queue_pattern, output_format):
    """Test simulate_params generates valid QueueInput for various combinations."""
    result = simulate_params(
        num_samples=5,
        configs=configs,
        technology=technology,
        instrument=instrument,
        sampler=sampler,
        queue_pattern=queue_pattern,
        output_format=output_format,
    )

    assert result.parameters.technology == technology
    assert result.parameters.instrument == instrument
    assert result.parameters.sampler == sampler
    assert result.parameters.output_format == output_format
    assert len(result.sample_groups) == 1
    assert len(result.get_all_samples()) == 5
    assert result.get_all_samples()[0].sample_name == "Sample_1"
