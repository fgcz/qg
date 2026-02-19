"""Tests for queue structure computation functions."""

from pathlib import Path

import pytest
from loguru import logger

from qg.config_models.loader import qg_configuration
from qg.config_models.structure import QueuePattern, SamplesConfig
from qg.queue_structure import (
    _compute_extended_positions,
    _compute_middle_block_positions,
    build_multi_container_queue_structure,
)


def _compute_queue_counts(num_samples: int, pattern: QueuePattern) -> dict[str, int]:
    """Compute queue slot counts without building the structure.

    Args:
        num_samples: Number of user samples
        pattern: Queue pattern configuration

    Returns:
        Dict with queue structure counts including extended blocks
    """
    num_middle_blocks = (num_samples - 1) // pattern.run_QC_after_n_samples if num_samples > 0 else 0

    # Calculate extended blocks (replaces every Nth middle)
    multiplier = pattern.middle_extended_frequency_multiplier or 0
    if multiplier > 0 and pattern.middle_extended:
        num_extended_blocks = num_middle_blocks // multiplier
        num_regular_middle = num_middle_blocks - num_extended_blocks
    else:
        num_extended_blocks = 0
        num_regular_middle = num_middle_blocks

    start_qcs = len(pattern.start)
    middle_qcs = num_regular_middle * len(pattern.middle)
    extended_qcs = num_extended_blocks * len(pattern.middle_extended or [])
    end_qcs = len(pattern.end)

    total_qcs = start_qcs + middle_qcs + extended_qcs + end_qcs

    counts = {
        "start_qcs": start_qcs,
        "user_samples": num_samples,
        "middle_blocks": num_middle_blocks,
        "regular_middle_blocks": num_regular_middle,
        "middle_qcs": middle_qcs,
        "extended_blocks": num_extended_blocks,
        "extended_qcs": extended_qcs,
        "end_qcs": end_qcs,
        "total_qcs": total_qcs,
        "total": total_qcs + num_samples,
    }
    logger.debug("Queue counts: {}", counts)
    return counts


# =============================================================================
# Fixtures
# =============================================================================


@pytest.fixture
def all_patterns() -> dict[str, dict[str, QueuePattern]]:
    """Load all queue patterns from config."""
    qg_configuration.cache_clear()
    config_dir = Path(__file__).parent.parent / "qg_configs"
    config = qg_configuration(config_dir)
    return config.queue_patterns.patterns


# =============================================================================
# Test Data - Generated from configs
# =============================================================================

SAMPLE_COUNTS = [8, 16, 32, 56, 106, 212]

# Generate pattern keys from config file
_config_dir = Path(__file__).parent.parent / "qg_configs"
_config = qg_configuration(_config_dir)
_patterns_config = _config.queue_patterns

ALL_PATTERN_KEYS = [
    (tech, pattern_name) for tech, patterns in _patterns_config.patterns.items() for pattern_name in patterns.keys()
]

# Patterns with middle_extended
EXTENDED_PATTERN_KEYS = [
    (tech, pattern_name)
    for tech, patterns in _patterns_config.patterns.items()
    for pattern_name, pattern in patterns.items()
    if pattern.middle_extended is not None
]


# =============================================================================
# Test compute_middle_block_positions
# =============================================================================


class TestComputeMiddleBlockPositions:
    """Tests for compute_middle_block_positions function."""

    @pytest.mark.parametrize(
        "num_samples,frequency,expected",
        [
            (0, 10, []),
            (1, 10, []),
            (5, 10, []),
            (10, 10, []),  # exactly at frequency, no middle
            (11, 10, [9]),  # one over
            (20, 10, [9]),  # exactly 2x, still one block
            (21, 10, [9, 19]),  # two blocks
            (16, 16, []),
            (17, 16, [15]),
            (32, 16, [15]),
            (33, 16, [15, 31]),
            (8, 8, []),
            (9, 8, [7]),
            (16, 8, [7]),
            (17, 8, [7, 15]),
        ],
    )
    def test_positions(self, num_samples: int, frequency: int, expected: list[int]) -> None:
        assert _compute_middle_block_positions(num_samples, frequency) == expected


# =============================================================================
# Test compute_extended_positions
# =============================================================================


class TestComputeExtendedPositions:
    """Tests for compute_extended_positions function."""

    @pytest.mark.parametrize(
        "num_blocks,multiplier,expected",
        [
            # No multiplier or zero
            (5, 0, set()),
            (5, -1, set()),
            # Multiplier 2: blocks 2,4,6... (1-indexed) -> indices 1,3,5... (0-indexed)
            (1, 2, set()),  # only 1 block, no extended
            (2, 2, {1}),  # blocks 1,2 -> block 2 extended -> index 1
            (3, 2, {1}),  # blocks 1,2,3 -> block 2 extended -> index 1
            (4, 2, {1, 3}),  # blocks 1,2,3,4 -> blocks 2,4 extended -> indices 1,3
            (6, 2, {1, 3, 5}),  # blocks 2,4,6 extended
            # Multiplier 3: blocks 3,6,9... (1-indexed) -> indices 2,5,8... (0-indexed)
            (2, 3, set()),
            (3, 3, {2}),
            (6, 3, {2, 5}),
            (9, 3, {2, 5, 8}),
            # Multiplier 1: every block is extended
            (5, 1, {0, 1, 2, 3, 4}),
        ],
    )
    def test_extended_positions(self, num_blocks: int, multiplier: int, expected: set[int]) -> None:
        assert _compute_extended_positions(num_blocks, multiplier) == expected


# =============================================================================
# Test compute_queue_counts - All Patterns
# =============================================================================


class TestComputeQueueCounts:
    """Tests for compute_queue_counts with all patterns and sample counts."""

    @pytest.mark.parametrize("tech,pattern_name", ALL_PATTERN_KEYS)
    @pytest.mark.parametrize("num_samples", SAMPLE_COUNTS)
    def test_queue_counts(
        self,
        all_patterns: dict[str, dict[str, QueuePattern]],
        tech: str,
        pattern_name: str,
        num_samples: int,
    ) -> None:
        """Test queue counts for all pattern/sample combinations."""
        pattern = all_patterns[tech][pattern_name]
        result = _compute_queue_counts(num_samples, pattern)

        # Compute expected values
        freq = pattern.run_QC_after_n_samples
        num_middle_blocks = (num_samples - 1) // freq if num_samples > 0 else 0

        # Extended blocks calculation
        multiplier = pattern.middle_extended_frequency_multiplier or 0
        if multiplier > 0 and pattern.middle_extended:
            num_extended = num_middle_blocks // multiplier
            num_regular = num_middle_blocks - num_extended
        else:
            num_extended = 0
            num_regular = num_middle_blocks

        start_len = len(pattern.start)
        middle_len = len(pattern.middle)
        extended_len = len(pattern.middle_extended or [])
        end_len = len(pattern.end)

        expected_middle_qcs = num_regular * middle_len
        expected_extended_qcs = num_extended * extended_len
        expected_total_qcs = start_len + expected_middle_qcs + expected_extended_qcs + end_len

        # Assertions
        assert result["start_qcs"] == start_len
        assert result["middle_blocks"] == num_middle_blocks
        assert result["regular_middle_blocks"] == num_regular
        assert result["middle_qcs"] == expected_middle_qcs
        assert result["extended_blocks"] == num_extended
        assert result["extended_qcs"] == expected_extended_qcs
        assert result["end_qcs"] == end_len
        assert result["user_samples"] == num_samples
        assert result["total_qcs"] == expected_total_qcs
        assert result["total"] == expected_total_qcs + num_samples


# =============================================================================
# Test Extended Patterns Specifically
# =============================================================================


class TestComputeQueueCountsExtended:
    """Tests specifically for patterns with middle_extended."""

    @pytest.mark.parametrize("tech,pattern_name", EXTENDED_PATTERN_KEYS)
    def test_has_extended_config(
        self,
        all_patterns: dict[str, dict[str, QueuePattern]],
        tech: str,
        pattern_name: str,
    ) -> None:
        """Verify extended patterns have the required config."""
        pattern = all_patterns[tech][pattern_name]
        assert pattern.middle_extended is not None
        assert len(pattern.middle_extended) > 0
        assert pattern.middle_extended_frequency_multiplier is not None
        assert pattern.middle_extended_frequency_multiplier > 0

    @pytest.mark.parametrize("tech,pattern_name", EXTENDED_PATTERN_KEYS)
    @pytest.mark.parametrize("num_samples", SAMPLE_COUNTS)
    def test_extended_blocks_computed(
        self,
        all_patterns: dict[str, dict[str, QueuePattern]],
        tech: str,
        pattern_name: str,
        num_samples: int,
    ) -> None:
        """Verify extended blocks are correctly computed for Metabolomics/Lipidomics."""
        pattern = all_patterns[tech][pattern_name]
        result = _compute_queue_counts(num_samples, pattern)

        freq = pattern.run_QC_after_n_samples
        num_middle_blocks = (num_samples - 1) // freq if num_samples > 0 else 0
        multiplier = pattern.middle_extended_frequency_multiplier

        expected_extended = num_middle_blocks // multiplier if multiplier else 0

        assert result["extended_blocks"] == expected_extended
        assert result["regular_middle_blocks"] == num_middle_blocks - expected_extended
        assert result["middle_blocks"] == num_middle_blocks


# =============================================================================
# Test Edge Cases
# =============================================================================


class TestComputeQueueCountsEdgeCases:
    """Test edge cases for compute_queue_counts."""

    @pytest.mark.parametrize("tech,pattern_name", ALL_PATTERN_KEYS)
    @pytest.mark.parametrize("num_samples", [0, 1])
    def test_zero_and_one_sample(
        self,
        all_patterns: dict[str, dict[str, QueuePattern]],
        tech: str,
        pattern_name: str,
        num_samples: int,
    ) -> None:
        """Test with zero or one sample - should have no middle blocks."""
        pattern = all_patterns[tech][pattern_name]
        result = _compute_queue_counts(num_samples, pattern)

        assert result["user_samples"] == num_samples
        assert result["middle_blocks"] == 0
        assert result["middle_qcs"] == 0
        assert result["extended_blocks"] == 0
        assert result["extended_qcs"] == 0

    @pytest.mark.parametrize("tech,pattern_name", ALL_PATTERN_KEYS)
    def test_exactly_at_frequency(
        self,
        all_patterns: dict[str, dict[str, QueuePattern]],
        tech: str,
        pattern_name: str,
    ) -> None:
        """Test when num_samples equals frequency (no middle block)."""
        pattern = all_patterns[tech][pattern_name]
        freq = pattern.run_QC_after_n_samples
        result = _compute_queue_counts(freq, pattern)

        # (freq-1)//freq = 0
        assert result["middle_blocks"] == 0
        assert result["extended_blocks"] == 0

    @pytest.mark.parametrize("tech,pattern_name", ALL_PATTERN_KEYS)
    def test_one_over_frequency(
        self,
        all_patterns: dict[str, dict[str, QueuePattern]],
        tech: str,
        pattern_name: str,
    ) -> None:
        """Test when num_samples is frequency + 1 (one middle block)."""
        pattern = all_patterns[tech][pattern_name]
        freq = pattern.run_QC_after_n_samples
        result = _compute_queue_counts(freq + 1, pattern)

        # (freq+1-1)//freq = 1
        assert result["middle_blocks"] == 1

        # With 1 block: extended only if multiplier is 1
        multiplier = pattern.middle_extended_frequency_multiplier or 0
        if multiplier == 1 and pattern.middle_extended:
            assert result["extended_blocks"] == 1
            assert result["regular_middle_blocks"] == 0
        else:
            assert result["extended_blocks"] == 0
            assert result["regular_middle_blocks"] == 1


# =============================================================================
# Test Consistency
# =============================================================================


class TestComputeQueueCountsConsistency:
    """Test internal consistency of compute_queue_counts results."""

    @pytest.mark.parametrize("tech,pattern_name", ALL_PATTERN_KEYS)
    @pytest.mark.parametrize("num_samples", SAMPLE_COUNTS)
    def test_totals_are_consistent(
        self,
        all_patterns: dict[str, dict[str, QueuePattern]],
        tech: str,
        pattern_name: str,
        num_samples: int,
    ) -> None:
        """Verify totals equal sum of components."""
        pattern = all_patterns[tech][pattern_name]
        result = _compute_queue_counts(num_samples, pattern)

        # total = start + user + middle + extended + end
        assert result["total"] == (
            result["start_qcs"]
            + result["user_samples"]
            + result["middle_qcs"]
            + result["extended_qcs"]
            + result["end_qcs"]
        )

        # total_qcs = start + middle + extended + end
        assert result["total_qcs"] == (
            result["start_qcs"] + result["middle_qcs"] + result["extended_qcs"] + result["end_qcs"]
        )

        # middle_blocks = regular + extended
        assert result["middle_blocks"] == (result["regular_middle_blocks"] + result["extended_blocks"])


# =============================================================================
# Test build_multi_container_queue_structure (single container)
# =============================================================================


def _build_structure(num_samples: int, pattern: QueuePattern) -> list[str]:
    """Helper: build single-container structure and extract sample_ids."""
    slot_entries = build_multi_container_queue_structure([(0, num_samples)], pattern, SamplesConfig.DEFAULT_SAMPLE_ID)
    return [s.sample_id for s in slot_entries]


class TestBuildQueueStructure:
    """Tests for build_multi_container_queue_structure with single container."""

    @pytest.mark.parametrize("tech,pattern_name", ALL_PATTERN_KEYS)
    @pytest.mark.parametrize("num_samples", SAMPLE_COUNTS)
    def test_structure_length_matches_counts(
        self,
        all_patterns: dict[str, dict[str, QueuePattern]],
        tech: str,
        pattern_name: str,
        num_samples: int,
    ) -> None:
        """Verify structure length matches computed total."""
        pattern = all_patterns[tech][pattern_name]
        structure = _build_structure(num_samples, pattern)
        counts = _compute_queue_counts(num_samples, pattern)

        assert len(structure) == counts["total"]

    @pytest.mark.parametrize("tech,pattern_name", ALL_PATTERN_KEYS)
    @pytest.mark.parametrize("num_samples", SAMPLE_COUNTS)
    def test_structure_user_count_matches(
        self,
        all_patterns: dict[str, dict[str, QueuePattern]],
        tech: str,
        pattern_name: str,
        num_samples: int,
    ) -> None:
        """Verify structure has correct number of user slots."""
        pattern = all_patterns[tech][pattern_name]
        structure = _build_structure(num_samples, pattern)

        user_count = sum(1 for s in structure if s == SamplesConfig.DEFAULT_SAMPLE_ID)
        assert user_count == num_samples

    @pytest.mark.parametrize("tech,pattern_name", ALL_PATTERN_KEYS)
    def test_structure_starts_with_pattern_start(
        self,
        all_patterns: dict[str, dict[str, QueuePattern]],
        tech: str,
        pattern_name: str,
    ) -> None:
        """Verify structure starts with pattern.start."""
        pattern = all_patterns[tech][pattern_name]
        structure = _build_structure(10, pattern)

        start_len = len(pattern.start)
        assert structure[:start_len] == pattern.start

    @pytest.mark.parametrize("tech,pattern_name", ALL_PATTERN_KEYS)
    def test_structure_ends_with_pattern_end(
        self,
        all_patterns: dict[str, dict[str, QueuePattern]],
        tech: str,
        pattern_name: str,
    ) -> None:
        """Verify structure ends with pattern.end."""
        pattern = all_patterns[tech][pattern_name]
        structure = _build_structure(10, pattern)

        end_len = len(pattern.end)
        if end_len == 0:
            # No end pattern to check
            return
        assert structure[-end_len:] == pattern.end

    def test_structure_with_zero_samples(
        self,
        all_patterns: dict[str, dict[str, QueuePattern]],
    ) -> None:
        """Verify structure with 0 samples is just start + end."""
        pattern = all_patterns["Proteomics"]["standard"]
        structure = _build_structure(0, pattern)

        expected = pattern.start + pattern.end
        assert structure == expected
