"""Tests for qg.randomize module."""

from __future__ import annotations

import random as py_random
from typing import Literal

from qg.params_models import InputSample, QueueInput, QueueParameters, SampleGroup
from qg.randomize import queue_randomization

# =============================================================================
# Test Fixtures / Helpers
# =============================================================================


def make_samples(n: int, id_offset: int = 10000) -> list[InputSample]:
    """Create a list of n InputSamples."""
    return [InputSample(sample_name=f"sample_{i}", sample_id=id_offset + i) for i in range(1, n + 1)]


def make_grouped_samples(groups: dict[str, int], id_start: int = 1) -> list[InputSample]:
    """Create samples with grouping_var assignments.

    Args:
        groups: Dict mapping grouping_var -> count (e.g., {"A": 2, "B": 3})
        id_start: Starting sample_id
    """
    samples = []
    current_id = id_start
    for group_name, count in groups.items():
        for i in range(count):
            samples.append(
                InputSample(
                    sample_name=f"{group_name.lower()}{i + 1}",
                    sample_id=current_id,
                    grouping_var=group_name,
                )
            )
            current_id += 1
    return samples


def make_queue_input(
    samples: list[InputSample],
    randomization: Literal["no", "random", "blocked"] = "no",
    container_id: int = 99999,
) -> QueueInput:
    """Create QueueInput with minimal boilerplate."""
    params = QueueParameters(
        tech_area="Proteomics",
        instrument="ASTRAL_1",
        sampler="Vanquish.vial",
        output_format="xcalibur",
        queue_pattern="noqc",
        polarity=["pos"],
        date="20260116",
        user="test",
        randomization=randomization,
    )
    return QueueInput(
        parameters=params,
        sample_groups=[SampleGroup(container_id=container_id, samples=samples)],
    )


def make_multi_group_input(
    groups: list[tuple[int, list[InputSample]]],
    randomization: Literal["no", "random", "blocked"] = "no",
) -> QueueInput:
    """Create QueueInput with multiple sample groups.

    Args:
        groups: List of (container_id, samples) tuples
        randomization: Randomization mode
    """
    params = QueueParameters(
        tech_area="Proteomics",
        instrument="ASTRAL_1",
        sampler="Vanquish.vial",
        output_format="xcalibur",
        queue_pattern="noqc",
        polarity=["pos"],
        date="20260116",
        user="test",
        randomization=randomization,
    )
    return QueueInput(
        parameters=params,
        sample_groups=[SampleGroup(container_id=cid, samples=s) for cid, s in groups],
    )


def get_group_ids(queue_input: QueueInput, group_idx: int = 0) -> list[int]:
    """Extract sample IDs from a specific group."""
    return [s.sample_id for s in queue_input.sample_groups[group_idx].samples]


# =============================================================================
# Tests
# =============================================================================


class TestQueueRandomizationCopyBehavior:
    """Tests that queue_randomization returns a proper deep copy."""

    def test_returns_different_objects(self):
        """Result is a distinct object tree from original."""
        original = make_queue_input(make_samples(5), randomization="random")

        py_random.seed(42)
        result = queue_randomization(original)

        assert result is not original
        assert result.sample_groups is not original.sample_groups
        assert result.sample_groups[0] is not original.sample_groups[0]
        assert result.sample_groups[0].samples is not original.sample_groups[0].samples

    def test_original_unchanged_after_randomization(self):
        """Original queue_input is not modified."""
        original = make_queue_input(make_samples(5), randomization="random")
        original_ids = get_group_ids(original)

        py_random.seed(42)
        queue_randomization(original)

        assert get_group_ids(original) == original_ids


class TestNoRandomization:
    """Tests for randomization='no' mode."""

    def test_preserves_order_single_group(self):
        """'no' mode returns copy with unchanged order."""
        samples = make_samples(5)
        original_ids = [s.sample_id for s in samples]
        queue_input = make_queue_input(samples, randomization="no")

        result = queue_randomization(queue_input)

        assert get_group_ids(result) == original_ids

    def test_preserves_order_multiple_groups(self):
        """'no' mode preserves order in all groups."""
        samples1 = make_samples(5, id_offset=10000)
        samples2 = make_samples(5, id_offset=20000)
        queue_input = make_multi_group_input(
            [(1, samples1), (2, samples2)],
            randomization="no",
        )
        original_ids1 = [s.sample_id for s in samples1]
        original_ids2 = [s.sample_id for s in samples2]

        result = queue_randomization(queue_input)

        assert get_group_ids(result, 0) == original_ids1
        assert get_group_ids(result, 1) == original_ids2


class TestRandomShuffle:
    """Tests for randomization='random' mode."""

    def test_shuffles_single_group(self):
        """'random' mode shuffles samples."""
        py_random.seed(42)
        samples = make_samples(10)
        original_ids = [s.sample_id for s in samples]
        queue_input = make_queue_input(samples, randomization="random")

        result = queue_randomization(queue_input)
        result_ids = get_group_ids(result)

        assert result_ids != original_ids, "Samples should be shuffled"
        assert set(result_ids) == set(original_ids), "Same samples, different order"

    def test_shuffles_each_group_independently(self):
        """'random' mode shuffles each group independently."""
        py_random.seed(42)
        samples1 = make_samples(10, id_offset=10000)
        samples2 = make_samples(10, id_offset=20000)
        queue_input = make_multi_group_input(
            [(1, samples1), (2, samples2)],
            randomization="random",
        )
        original_ids1 = [s.sample_id for s in samples1]
        original_ids2 = [s.sample_id for s in samples2]

        result = queue_randomization(queue_input)
        result_ids1 = get_group_ids(result, 0)
        result_ids2 = get_group_ids(result, 1)

        # Both should be shuffled
        assert set(result_ids1) == set(original_ids1)
        assert set(result_ids2) == set(original_ids2)
        # At least one should have different order
        assert result_ids1 != original_ids1 or result_ids2 != original_ids2


class TestBlockedRandomization:
    """Tests for randomization='blocked' (RCBD) mode."""

    def test_equal_groups_creates_balanced_blocks(self):
        """RCBD with equal group sizes creates balanced blocks."""
        py_random.seed(42)
        # 3 groups of 2 samples each
        samples = make_grouped_samples({"A": 2, "B": 2, "C": 2})
        queue_input = make_queue_input(samples, randomization="blocked")

        result = queue_randomization(queue_input)
        result_ids = get_group_ids(result)

        # Block 1: one from each group (ids 1, 3, 5)
        # Block 2: one from each group (ids 2, 4, 6)
        block1 = set(result_ids[:3])
        block2 = set(result_ids[3:])

        assert block1 == {1, 3, 5}, f"Block 1 got {block1}"
        assert block2 == {2, 4, 6}, f"Block 2 got {block2}"

    def test_unequal_groups_handles_incomplete_blocks(self):
        """RCBD with unequal sizes creates incomplete later blocks."""
        py_random.seed(42)
        # A has 3, B has 2, C has 1
        samples = make_grouped_samples({"A": 3, "B": 2, "C": 1})
        queue_input = make_queue_input(samples, randomization="blocked")

        result = queue_randomization(queue_input)
        result_ids = get_group_ids(result)

        # Block 1: a1, b1, c1 (ids 1, 4, 6)
        # Block 2: a2, b2 (ids 2, 5)
        # Block 3: a3 (id 3)
        block1 = set(result_ids[:3])
        block2 = set(result_ids[3:5])
        block3 = set(result_ids[5:])

        assert block1 == {1, 4, 6}, f"Block 1 got {block1}"
        assert block2 == {2, 5}, f"Block 2 got {block2}"
        assert block3 == {3}, f"Block 3 got {block3}"

    def test_no_grouping_var_falls_back_to_shuffle(self):
        """RCBD with no grouping_var falls back to simple shuffle."""
        py_random.seed(42)
        samples = make_samples(5)  # No grouping_var
        original_ids = [s.sample_id for s in samples]
        queue_input = make_queue_input(samples, randomization="blocked")

        result = queue_randomization(queue_input)
        result_ids = get_group_ids(result)

        assert set(result_ids) == set(original_ids)
        assert result_ids != original_ids, "Should be shuffled"

    def test_multiple_sample_groups_with_rcbd(self):
        """RCBD applies independently to each sample group."""
        py_random.seed(42)
        # Group 1: 2 grouping vars, 2 samples each
        samples1 = make_grouped_samples({"A": 2, "B": 2}, id_start=1)
        # Group 2: 3 grouping vars, 1 sample each
        samples2 = make_grouped_samples({"X": 1, "Y": 1, "Z": 1}, id_start=100)

        queue_input = make_multi_group_input(
            [(1, samples1), (2, samples2)],
            randomization="blocked",
        )

        result = queue_randomization(queue_input)
        result_ids1 = get_group_ids(result, 0)
        result_ids2 = get_group_ids(result, 1)

        # Group 1: 2 blocks of 2
        # Block 1: ids 1, 3; Block 2: ids 2, 4
        block1_g1 = set(result_ids1[:2])
        block2_g1 = set(result_ids1[2:])
        assert block1_g1 == {1, 3}
        assert block2_g1 == {2, 4}

        # Group 2: 1 block of 3 (all samples)
        assert set(result_ids2) == {100, 101, 102}

    def test_multiple_groups_mixed_with_and_without_grouping_var(self):
        """RCBD handles mix of groups with/without grouping_var."""
        py_random.seed(42)
        # Group 1: has grouping_var
        samples1 = make_grouped_samples({"A": 2, "B": 2}, id_start=1)
        # Group 2: no grouping_var (should shuffle)
        samples2 = make_samples(5, id_offset=100)
        original_ids2 = [s.sample_id for s in samples2]

        queue_input = make_multi_group_input(
            [(1, samples1), (2, samples2)],
            randomization="blocked",
        )

        result = queue_randomization(queue_input)
        result_ids1 = get_group_ids(result, 0)
        result_ids2 = get_group_ids(result, 1)

        # Group 1: RCBD applied
        block1_g1 = set(result_ids1[:2])
        block2_g1 = set(result_ids1[2:])
        assert block1_g1 == {1, 3}
        assert block2_g1 == {2, 4}

        # Group 2: shuffled (no grouping_var)
        assert set(result_ids2) == set(original_ids2)
        assert result_ids2 != original_ids2, "Should be shuffled"
