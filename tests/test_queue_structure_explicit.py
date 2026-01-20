"""Explicit tests for multi-group queue structure building.

These tests demonstrate how build_multi_container_queue_structure works with
two projects/containers. Expected lengths are written as explicit formulas.
"""

from pathlib import Path

import pytest

from qg.config import qg_config
from qg.queue_structure import build_multi_container_queue_structure


@pytest.fixture
def configs():
    """Load all configs for testing."""
    return qg_config(Path("qg_configs"))


@pytest.fixture
def pattern(configs):
    """Get Proteomics.standard pattern from actual config."""
    return configs.queue_patterns.get_pattern("Proteomics", "standard")


class TestMultiGroupQueueStructure:
    """Explicit tests showing multi-group structure with separation blocks."""

    def test_both_zero_samples(self, pattern):
        """Two projects, 0 samples each: start + separation + end."""
        groups = [(1001, 0), (1002, 0)]
        structure = build_multi_container_queue_structure(groups, pattern)

        expected = len(pattern.start) + len(pattern.separation) + len(pattern.end)
        assert len(structure) == expected

    def test_first_one_second_zero(self, pattern):
        """First project: 1 sample, second: 0 samples."""
        groups = [(1001, 1), (1002, 0)]
        structure = build_multi_container_queue_structure(groups, pattern)

        expected = len(pattern.start) + 1 + len(pattern.separation) + len(pattern.end)
        assert len(structure) == expected

    def test_first_zero_second_one(self, pattern):
        """First project: 0 samples, second: 1 sample."""
        groups = [(1001, 0), (1002, 1)]
        structure = build_multi_container_queue_structure(groups, pattern)

        expected = len(pattern.start) + len(pattern.separation) + 1 + len(pattern.end)
        assert len(structure) == expected

    def test_both_one_sample(self, pattern):
        """Both projects: 1 sample each."""
        groups = [(1001, 1), (1002, 1)]
        structure = build_multi_container_queue_structure(groups, pattern)

        expected = len(pattern.start) + 1 + len(pattern.separation) + 1 + len(pattern.end)
        assert len(structure) == expected

    def test_first_ten_second_zero_with_middle(self, pattern):
        """First 10, second 0, with run_QC_after_n_samples=5: start + samples + middle + separation + end."""
        # Override pattern to have qc every 5 samples
        pattern.run_QC_after_n_samples = 5

        groups = [(1001, 10), (1002, 0)]
        structure = build_multi_container_queue_structure(groups, pattern)

        # 10 samples with qc every 5 = 1 middle block after sample 5
        expected = len(pattern.start) + 10 + len(pattern.middle) + len(pattern.separation) + len(pattern.end)
        assert len(structure) == expected

    def test_first_zero_second_ten_with_middle(self, pattern):
        """First 0, second 10, with run_QC_after_n_samples=5: start + separation + samples + middle + end."""
        pattern.run_QC_after_n_samples = 5

        groups = [(1001, 0), (1002, 10)]
        structure = build_multi_container_queue_structure(groups, pattern)

        expected = len(pattern.start) + len(pattern.separation) + 10 + len(pattern.middle) + len(pattern.end)
        assert len(structure) == expected

    def test_both_ten_samples_with_middle(self, pattern):
        """Both 10 samples, with run_QC_after_n_samples=5: start + samples + middle + separation + samples + middle + end."""
        pattern.run_QC_after_n_samples = 5

        groups = [(1001, 10), (1002, 10)]
        structure = build_multi_container_queue_structure(groups, pattern)

        expected = (
            len(pattern.start)
            + 10 + len(pattern.middle)
            + len(pattern.separation)
            + 10 + len(pattern.middle)
            + len(pattern.end)
        )
        assert len(structure) == expected


class TestSingleContainerWithMultiGroupFunction:
    """Test that build_multi_container_queue_structure works for single container too."""

    def test_single_container_zero_samples(self, pattern):
        """Single container, 0 samples: start + end (no separation)."""
        groups = [(1001, 0)]
        structure = build_multi_container_queue_structure(groups, pattern)

        expected = len(pattern.start) + len(pattern.end)
        assert len(structure) == expected

    def test_single_container_one_sample(self, pattern):
        """Single container, 1 sample: start + 1 + end."""
        groups = [(1001, 1)]
        structure = build_multi_container_queue_structure(groups, pattern)

        expected = len(pattern.start) + 1 + len(pattern.end)
        assert len(structure) == expected

    def test_single_container_ten_samples_with_middle(self, pattern):
        """Single container, 10 samples, qc every 5: start + 10 + middle + end."""
        pattern.run_QC_after_n_samples = 5

        groups = [(1001, 10)]
        structure = build_multi_container_queue_structure(groups, pattern)

        expected = len(pattern.start) + 10 + len(pattern.middle) + len(pattern.end)
        assert len(structure) == expected


