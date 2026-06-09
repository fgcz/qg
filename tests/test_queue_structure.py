"""Tests for queue structure computation functions."""

from itertools import groupby
from pathlib import Path

import pytest

from qg.config_models.loader import qg_configuration
from qg.config_models.structure import QueuePattern, SamplesConfig
from qg.queue_structure import build_multi_container_queue_structure

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

# Patterns without middle_extended (regular block-count arithmetic)
NON_EXTENDED_PATTERN_KEYS = [key for key in ALL_PATTERN_KEYS if key not in set(EXTENDED_PATTERN_KEYS)]


# =============================================================================
# Helper
# =============================================================================


def _build_structure(num_samples: int, pattern: QueuePattern) -> list[str]:
    """Helper: build single-container structure and extract sample_ids."""
    slot_entries = build_multi_container_queue_structure([(0, num_samples)], pattern, SamplesConfig.DEFAULT_SAMPLE_ID)
    return [s.sample_id for s in slot_entries]


def _decompose_body(structure: list[str], pattern: QueuePattern) -> tuple[list[int], list[list[str]]]:
    """Strip the start/end blocks, then split the body into user-sample run
    lengths and the QC blocks that separate them (in order)."""
    end = len(structure) - len(pattern.end) if pattern.end else len(structure)
    body = structure[len(pattern.start) : end]
    is_user = lambda sample_id: sample_id == SamplesConfig.DEFAULT_SAMPLE_ID  # noqa: E731
    runs = [len(list(g)) for user, g in groupby(body, key=is_user) if user]
    blocks = [list(g) for user, g in groupby(body, key=is_user) if not user]
    return runs, blocks


# =============================================================================
# Test build_multi_container_queue_structure (single container)
# =============================================================================


class TestBuildQueueStructure:
    """Tests for build_multi_container_queue_structure with single container."""

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

    # ------------------------------------------------------------------
    # Middle block tests through the public API
    # ------------------------------------------------------------------

    @pytest.mark.parametrize("tech,pattern_name", ALL_PATTERN_KEYS)
    def test_middle_blocks_inserted_at_correct_positions(
        self,
        all_patterns: dict[str, dict[str, QueuePattern]],
        tech: str,
        pattern_name: str,
    ) -> None:
        """Middle QC IDs appear after every N-th user sample."""
        pattern = all_patterns[tech][pattern_name]
        if not pattern.middle:
            return

        freq = pattern.run_QC_after_n_samples
        # Use enough samples to guarantee at least one middle block
        num_samples = freq + 1
        structure = _build_structure(num_samples, pattern)

        # Walk past start block, then find the first middle block
        start_len = len(pattern.start)
        body = structure[start_len:]
        # Strip end block
        end_len = len(pattern.end)
        if end_len > 0:
            body = body[:-end_len]

        # Count user samples before first non-user slot in body
        user_count_before_first_middle = 0
        for s in body:
            if s == SamplesConfig.DEFAULT_SAMPLE_ID:
                user_count_before_first_middle += 1
            else:
                break

        assert user_count_before_first_middle == freq

    @pytest.mark.parametrize("tech,pattern_name", EXTENDED_PATTERN_KEYS)
    def test_extended_blocks_replace_middle(
        self,
        all_patterns: dict[str, dict[str, QueuePattern]],
        tech: str,
        pattern_name: str,
    ) -> None:
        """For patterns with middle_extended, extended QC IDs appear at the right frequency."""
        pattern = all_patterns[tech][pattern_name]
        freq = pattern.run_QC_after_n_samples
        multiplier = pattern.middle_extended_frequency_multiplier or 0
        assert multiplier > 0
        assert pattern.middle_extended is not None

        # Use enough samples to get at least multiplier middle blocks
        num_samples = freq * multiplier + 1
        structure = _build_structure(num_samples, pattern)

        # Collect all QC block sequences between user sample runs
        extended_ids = set(pattern.middle_extended)
        middle_ids = set(pattern.middle)

        # Find QC blocks that are purely extended (not in regular middle)
        # Extended-only IDs are those in middle_extended but not in middle
        extended_only = extended_ids - middle_ids

        if extended_only:
            # At least one extended-only ID should appear in the structure
            assert any(s in extended_only for s in structure), (
                f"Expected extended QC IDs {extended_only} in structure for pattern {tech}.{pattern_name}"
            )

    @pytest.mark.parametrize("tech,pattern_name", ALL_PATTERN_KEYS)
    @pytest.mark.parametrize("num_samples", [0, 1])
    def test_no_middle_blocks_for_small_sample_count(
        self,
        all_patterns: dict[str, dict[str, QueuePattern]],
        tech: str,
        pattern_name: str,
        num_samples: int,
    ) -> None:
        """0 or 1 samples should produce no middle QC blocks."""
        pattern = all_patterns[tech][pattern_name]
        structure = _build_structure(num_samples, pattern)

        expected = pattern.start + [SamplesConfig.DEFAULT_SAMPLE_ID] * num_samples + pattern.end
        assert structure == expected

    def test_structure_with_no_groups_emits_qc_bookends(
        self,
        all_patterns: dict[str, dict[str, QueuePattern]],
    ) -> None:
        """Verify groups=[] still emits start+end QC blocks (QC-only queue)."""
        pattern = all_patterns["Proteomics"]["standard"]
        slot_entries = build_multi_container_queue_structure([], pattern, SamplesConfig.DEFAULT_SAMPLE_ID)

        sample_ids = [s.sample_id for s in slot_entries]
        assert sample_ids == pattern.start + pattern.end

    # ------------------------------------------------------------------
    # Block-count / spacing boundaries (public-API coverage of the
    # (n-1)//freq arithmetic, incl. the exactly-at-frequency off-by-one)
    # ------------------------------------------------------------------

    @pytest.mark.parametrize("tech,pattern_name", ALL_PATTERN_KEYS)
    @pytest.mark.parametrize("n_blocks", [1, 2, 3])
    def test_middle_block_count_at_frequency_boundaries(
        self,
        all_patterns: dict[str, dict[str, QueuePattern]],
        tech: str,
        pattern_name: str,
        n_blocks: int,
    ) -> None:
        """A middle block appears once per `freq` user samples: `k*freq` samples
        yield `k-1` blocks, and one more sample (`k*freq + 1`) yields `k`."""
        pattern = all_patterns[tech][pattern_name]
        if not pattern.middle:
            return  # patterns with an empty middle block never interleave QC
        freq = pattern.run_QC_after_n_samples

        # Exactly at the frequency boundary: no new block yet.
        _, blocks_at = _decompose_body(_build_structure(n_blocks * freq, pattern), pattern)
        assert len(blocks_at) == n_blocks - 1

        # One sample over the boundary: exactly one more block, and every block
        # except the trailing remainder is preceded by exactly `freq` samples.
        runs_over, blocks_over = _decompose_body(_build_structure(n_blocks * freq + 1, pattern), pattern)
        assert len(blocks_over) == n_blocks
        assert all(run == freq for run in runs_over[:-1])
        assert sum(runs_over) == n_blocks * freq + 1

    @pytest.mark.parametrize("tech,pattern_name", NON_EXTENDED_PATTERN_KEYS)
    @pytest.mark.parametrize("num_samples", SAMPLE_COUNTS)
    def test_total_length_non_extended(
        self,
        all_patterns: dict[str, dict[str, QueuePattern]],
        tech: str,
        pattern_name: str,
        num_samples: int,
    ) -> None:
        """Total slot count = start + users + (n-1)//freq middle blocks + end."""
        pattern = all_patterns[tech][pattern_name]
        freq = pattern.run_QC_after_n_samples
        num_blocks = (num_samples - 1) // freq if num_samples > 0 else 0

        expected_len = len(pattern.start) + num_samples + num_blocks * len(pattern.middle) + len(pattern.end)
        assert len(_build_structure(num_samples, pattern)) == expected_len

    @pytest.mark.parametrize("tech,pattern_name", EXTENDED_PATTERN_KEYS)
    def test_extended_blocks_at_correct_indices(
        self,
        all_patterns: dict[str, dict[str, QueuePattern]],
        tech: str,
        pattern_name: str,
    ) -> None:
        """Every `multiplier`-th middle block (1-indexed) is the extended block;
        the others are the regular middle block."""
        pattern = all_patterns[tech][pattern_name]
        freq = pattern.run_QC_after_n_samples
        multiplier = pattern.middle_extended_frequency_multiplier
        assert multiplier and multiplier > 0

        n_blocks = 2 * multiplier  # enough to see the extended cadence repeat
        _, blocks = _decompose_body(_build_structure(freq * n_blocks + 1, pattern), pattern)
        assert len(blocks) == n_blocks

        for i, block in enumerate(blocks):
            if (i + 1) % multiplier == 0:
                assert block == pattern.middle_extended, f"block {i} should be extended"
            else:
                assert block == pattern.middle, f"block {i} should be regular middle"

    # ------------------------------------------------------------------
    # Extended-block cadence, via synthetic patterns through the public API.
    # Real configs only use multiplier=2, so the config-driven test above can
    # never exercise other multipliers. Rather than re-list the multipliers
    # that happen to exist today (which would leave the next new multiplier
    # uncovered, just as multiplier=2-only did), sweep a range and assert the
    # documented contract: the extended block replaces every `multiplier`-th
    # middle block (1-indexed). This decouples cadence coverage from the configs.
    # ------------------------------------------------------------------

    @staticmethod
    def _synthetic_extended_pattern(multiplier: int | None) -> QueuePattern:
        """A pattern whose middle ('M') and extended ('E') blocks are
        distinguishable, with one middle-block boundary per user sample
        (freq=1), so N+1 samples yield exactly N middle blocks."""
        return QueuePattern(
            description="synthetic",
            run_QC_after_n_samples=1,
            start=[],
            middle=["M"],
            end=[],
            middle_extended=["E"],
            middle_extended_frequency_multiplier=multiplier,
        )

    @pytest.mark.parametrize("multiplier", [1, 2, 3, 4, 5, 7])
    @pytest.mark.parametrize("num_blocks", [1, 3, 8, 15])
    def test_extended_cadence_via_public_api(self, multiplier: int, num_blocks: int) -> None:
        """For any multiplier >= 1, the extended block replaces every
        `multiplier`-th middle block (1-indexed), verified end-to-end through
        build_multi_container_queue_structure."""
        pattern = self._synthetic_extended_pattern(multiplier)
        _, blocks = _decompose_body(_build_structure(num_blocks + 1, pattern), pattern)

        assert len(blocks) == num_blocks
        extended_indices = {i for i, block in enumerate(blocks) if block == ["E"]}
        expected = {i for i in range(num_blocks) if (i + 1) % multiplier == 0}
        assert extended_indices == expected

    @pytest.mark.parametrize("multiplier", [None, 0, -1])
    def test_extended_disabled_when_multiplier_not_positive(self, multiplier: int | None) -> None:
        """A non-positive (or absent) multiplier leaves middle_extended unused:
        every middle block is the regular block."""
        pattern = self._synthetic_extended_pattern(multiplier)
        _, blocks = _decompose_body(_build_structure(6, pattern), pattern)

        assert blocks  # sanity: middle blocks were actually produced
        assert all(block == ["M"] for block in blocks)
