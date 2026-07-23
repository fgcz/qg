"""Queue structure building functions.

Pure functions for building the queue structure (slot sequence) without
any sample data or position assignment.
"""

from __future__ import annotations

from dataclasses import dataclass

from loguru import logger

from qg.config_models.structure import QueuePattern


@dataclass(frozen=True)
class SlotEntry:
    """A slot in the queue structure with container context."""

    sample_id: str  # DEFAULT_SAMPLE_ID for user samples, qc_id for QC
    container_id: int  # Which container this slot belongs to


def _compute_middle_block_positions(num_samples: int, qc_frequency: int) -> list[int]:
    """Compute user sample indices after which middle QC blocks are inserted.

    Args:
        num_samples: Number of user samples
        qc_frequency: Insert middle QC after every N samples

    Returns:
        List of user indices (0-based) after which to insert middle QC
    """
    if num_samples <= 1:
        return []
    # Middle QC after indices: F-1, 2F-1, 3F-1, ... but not after last sample
    num_blocks = (num_samples - 1) // qc_frequency
    positions = [qc_frequency * (i + 1) - 1 for i in range(num_blocks)]
    logger.trace("Middle blocks after sample indices: {}", positions)
    return positions


def _compute_extended_positions(num_middle_blocks: int, multiplier: int) -> set[int]:
    """Compute which middle blocks should use middle_extended.

    Uses 1-based counting: with multiplier=2, blocks 2,4,6... get extended.
    Returns 0-based indices for internal use.

    Args:
        num_middle_blocks: Total number of middle blocks
        multiplier: Every Nth block (1-indexed) uses middle_extended

    Returns:
        Set of 0-based indices where middle_extended should be used
    """
    if multiplier <= 0:
        return set()
    # Block N (1-indexed) is extended when N % multiplier == 0
    # In 0-indexed: index i is extended when (i+1) % multiplier == 0
    positions = {i for i in range(num_middle_blocks) if (i + 1) % multiplier == 0}
    logger.trace("Extended block indices (0-based): {}", positions)
    return positions


def _entries(sample_ids: list[str], container_id: int) -> list[SlotEntry]:
    """Attach container context to a block of sample IDs."""
    return [SlotEntry(sample_id=sample_id, container_id=container_id) for sample_id in sample_ids]


def _build_group_entries(
    container_id: int,
    num_samples: int,
    pattern: QueuePattern,
    default_sample_id: str,
    run_qc_after_n: int,
) -> list[SlotEntry]:
    """Build one container's user samples and periodic middle-QC blocks."""
    middle_positions = set(_compute_middle_block_positions(num_samples, run_qc_after_n))
    extended_positions = _compute_extended_positions(
        len(middle_positions),
        pattern.middle_extended_frequency_multiplier or 0,
    )

    result: list[SlotEntry] = []
    middle_block_idx = 0
    for sample_idx in range(num_samples):
        result.append(SlotEntry(sample_id=default_sample_id, container_id=container_id))
        if sample_idx not in middle_positions:
            continue
        middle = (
            pattern.middle_extended
            if middle_block_idx in extended_positions and pattern.middle_extended
            else pattern.middle
        )
        result.extend(_entries(middle, container_id))
        middle_block_idx += 1
    return result


def build_multi_container_queue_structure(
    groups: list[tuple[int, int]],  # (container_id, num_samples)
    pattern: QueuePattern,
    default_sample_id: str,
    qc_frequency_override: int | None = None,
) -> list[SlotEntry]:
    """Build queue structure for multiple groups with separation blocks.

    The structure is:
        start -> [group1 samples + middles] -> separation ->
                 [group2 samples + middles] -> separation ->
                 ... -> [groupN samples + middles] -> end

    Args:
        groups: List of (container_id, num_samples) tuples
        pattern: Queue pattern configuration
        qc_frequency_override: If set, overrides pattern.run_QC_after_n_samples

    Returns:
        List of SlotEntry with container context
    """
    if not groups:
        return _entries(pattern.start + pattern.end, 0)

    # Apply QC frequency override if specified
    run_qc_after_n = qc_frequency_override if qc_frequency_override is not None else pattern.run_QC_after_n_samples

    separation_block = pattern.effective_separation
    first_container_id = groups[0][0]
    structure = _entries(pattern.start, first_container_id)

    logger.debug(
        "Building multi-group structure: {} groups, pattern '{}', separation={}",
        len(groups),
        pattern.description,
        separation_block,
    )

    # Process each group
    for group_idx, (container_id, num_samples) in enumerate(groups):
        # Insert separation block before each group (except first)
        # Separation block belongs to the group being finished (previous group)
        if group_idx > 0 and separation_block:
            prev_container_id = groups[group_idx - 1][0]
            structure.extend(_entries(separation_block, prev_container_id))

        # Build group structure (user samples + middle QCs)
        if num_samples > 0:
            structure.extend(
                _build_group_entries(
                    container_id,
                    num_samples,
                    pattern,
                    default_sample_id,
                    run_qc_after_n,
                )
            )

    # End block - use last group's container_id
    last_container_id = groups[-1][0]
    structure.extend(_entries(pattern.end, last_container_id))

    user_count = sum(1 for s in structure if s.sample_id == default_sample_id)
    qc_count = len(structure) - user_count
    logger.debug(
        "Multi-group structure built: {} total slots ({} QC, {} user across {} groups)",
        len(structure),
        qc_count,
        user_count,
        len(groups),
    )

    return structure
