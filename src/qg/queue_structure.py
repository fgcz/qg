"""Queue structure building functions.

Pure functions for building the queue structure (slot sequence) without
any sample data or position assignment.
"""

from loguru import logger

from qg.config_models import QueuePattern


def compute_queue_counts(num_samples: int, pattern: QueuePattern) -> dict[str, int]:
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


def compute_middle_block_positions(num_samples: int, qc_frequency: int) -> list[int]:
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


def compute_extended_positions(num_middle_blocks: int, multiplier: int) -> set[int]:
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


def build_queue_structure(num_samples: int, pattern: QueuePattern) -> list[str]:
    """Build pure queue structure as list of sample_ids.

    Args:
        num_samples: Number of user samples
        pattern: Queue pattern configuration

    Returns:
        List of sample_ids like: ["QC03dia", "QC01", "default", "default", "clean", ...]
        - QC slots: actual sample_id (e.g., "QC01", "blank", "pooledQC")
        - User slots: "default"
    """
    logger.debug(
        "Building queue structure: {} samples, pattern '{}' (QC every {} samples)",
        num_samples,
        pattern.description,
        pattern.run_QC_after_n_samples,
    )

    structure: list[str] = []

    # Start
    structure.extend(pattern.start)

    # User samples with middle QCs
    if num_samples > 0:
        middle_positions = set(
            compute_middle_block_positions(num_samples, pattern.run_QC_after_n_samples)
        )
        extended_positions = compute_extended_positions(
            len(middle_positions), pattern.middle_extended_frequency_multiplier or 0
        )

        middle_block_idx = 0
        for i in range(num_samples):
            structure.append("default")
            if i in middle_positions:
                if middle_block_idx in extended_positions and pattern.middle_extended:
                    structure.extend(pattern.middle_extended)
                else:
                    structure.extend(pattern.middle)
                middle_block_idx += 1

    # End
    structure.extend(pattern.end)

    qc_count = len(structure) - num_samples
    logger.debug(
        "Queue structure built: {} total slots ({} QC, {} user)",
        len(structure),
        qc_count,
        num_samples,
    )
    logger.trace("Structure: {}", structure)

    return structure
