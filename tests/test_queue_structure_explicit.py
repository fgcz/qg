"""Explicit tests for multi-group queue structure building.

These tests demonstrate how build_multi_container_queue_structure works with
two projects/containers. Expected lengths are written as explicit formulas.
"""

from pathlib import Path

import pytest

from qg.config import qg_config
from qg.queue_structure import build_multi_container_queue_structure, extract_groups

from .helpers import make_queue_input


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


class TestMultiGroupIntegration:
    """Test full pipeline: QueueInput -> extract_groups -> build structure.

    Note: SampleGroup requires min 1 sample per group. Edge cases with 0 samples
    are tested in TestMultiGroupQueueStructure which bypasses model validation.
    """

    @pytest.mark.parametrize(
        "n_first,n_second",
        [
            (1, 1),
            (1, 10),
            (10, 1),
            (10, 10),
            (5, 5),
            (6, 4),
        ],
    )
    def test_two_containers(self, configs, n_first, n_second):
        """Test full pipeline from QueueInput to structure building."""
        queue_input = make_queue_input([(1001, n_first), (1002, n_second)])

        # Extract groups and build structure
        groups = extract_groups(queue_input.sample_groups)
        pattern = configs.queue_patterns.get_pattern("Proteomics", "standard")
        pattern.run_QC_after_n_samples = 5  # Override for test

        structure = build_multi_container_queue_structure(groups, pattern)

        # Verify length with explicit formula
        n_middle_first = (n_first - 1) // 5 if n_first > 1 else 0
        n_middle_second = (n_second - 1) // 5 if n_second > 1 else 0

        expected = (
            len(pattern.start)
            + n_first
            + n_middle_first * len(pattern.middle)
            + len(pattern.separation)
            + n_second
            + n_middle_second * len(pattern.middle)
            + len(pattern.end)
        )
        assert len(structure) == expected

    def test_extract_groups(self, configs):
        """Verify extract_groups returns correct tuples from QueueInput."""
        queue_input = make_queue_input([(1001, 5), (1002, 3), (1003, 7)])

        groups = extract_groups(queue_input.sample_groups)

        assert groups == [(1001, 5), (1002, 3), (1003, 7)]

    def test_container_ids_in_structure(self, configs):
        """Verify container_id is correctly set in SlotEntry for each group."""
        queue_input = make_queue_input([(1001, 2), (1002, 3)])

        groups = extract_groups(queue_input.sample_groups)
        pattern = configs.queue_patterns.get_pattern("Proteomics", "standard")
        pattern.run_QC_after_n_samples = 100  # No middle blocks

        structure = build_multi_container_queue_structure(groups, pattern)

        # Find user slots and check container_ids
        user_slots = [s for s in structure if s.sample_id == "default"]
        assert len(user_slots) == 5  # 2 + 3
        assert [s.container_id for s in user_slots] == [1001, 1001, 1002, 1002, 1002]

        # QC slots should have context-aware container_ids:
        # - Start block: first group's container_id (1001)
        # - Separation block: previous group's container_id (1001)
        # - End block: last group's container_id (1002)
        qc_slots = [s for s in structure if s.sample_id != "default"]
        start_count = len(pattern.start)  # 2
        separation_count = len(pattern.effective_separation)  # 3
        end_count = len(pattern.end)  # 4

        expected_qc_ids = (
            [1001] * start_count  # Start block
            + [1001] * separation_count  # Separation block (belongs to first group)
            + [1002] * end_count  # End block
        )
        assert [s.container_id for s in qc_slots] == expected_qc_ids
