"""Sample randomization for queue generation."""

from __future__ import annotations

import random

from qg.params_models import InputSample, QueueInput, SampleGroup


def queue_randomization(queue_input: QueueInput) -> QueueInput:
    """Create a copy of queue_input with randomized samples.

    Based on queue_input.parameters.randomization:
    - "no": return copy unchanged
    - "random": shuffle samples within each group
    - "blocked": randomized complete block design (RCBD)

    Args:
        queue_input: The original QueueInput object.

    Returns:
        A deep copy of queue_input with samples randomized.
    """
    result = queue_input.model_copy(deep=True)

    mode = result.parameters.randomization
    if mode == "random":
        _shuffle_samples(result.sample_groups)
    elif mode == "blocked":
        _block_randomize_samples(result.sample_groups)
    # "no" - return unchanged copy

    return result


def _shuffle_samples(sample_groups: list[SampleGroup]) -> None:
    """Shuffle samples within each SampleGroup in place."""
    for group in sample_groups:
        random.shuffle(group.samples)


def _block_randomize_samples(sample_groups: list[SampleGroup]) -> None:
    """Apply randomized complete block design (RCBD) to samples within each SampleGroup.

    For each group:
    1. Group samples by their grouping_var
    2. Create blocks where each block contains one sample from each grouping_var group
    3. Shuffle the order within each block
    4. Flatten blocks back to the sample list

    If no samples have grouping_var set, falls back to simple shuffle.
    """
    for group in sample_groups:
        samples = group.samples

        # Check if any sample has grouping_var set
        has_grouping = any(s.grouping_var is not None for s in samples)
        if not has_grouping:
            # Fall back to simple shuffle if no grouping_var
            random.shuffle(samples)
            continue

        # Group samples by grouping_var
        groups_dict: dict[str | None, list[InputSample]] = {}
        for sample in samples:
            key = sample.grouping_var
            if key not in groups_dict:
                groups_dict[key] = []
            groups_dict[key].append(sample)

        # Find the maximum group size (determines number of blocks)
        max_size = max(len(g) for g in groups_dict.values())

        # Create blocks: each block contains one sample from each grouping_var group
        blocks: list[list[InputSample]] = []
        for block_idx in range(max_size):
            block: list[InputSample] = []
            for grp_samples in groups_dict.values():
                if block_idx < len(grp_samples):
                    block.append(grp_samples[block_idx])
            # Shuffle within block
            random.shuffle(block)
            blocks.append(block)

        # Flatten blocks back to list
        group.samples = [sample for block in blocks for sample in block]
