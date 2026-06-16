"""Sample randomization for PlateQueue (new models).

Randomization respects boundaries:
- Within plates only (not across plates)
- Within container batches only (not across containers)

Cells are grouped by (plate_id, container_id) and shuffled within each group.
"""

from __future__ import annotations

import random
import secrets
from typing import Literal

from qg.params_models import PlateCell, PlateQueue


def draw_seed() -> int:
    """Draw a fresh 32-bit RNG seed from OS entropy.

    Independent of the global ``random`` state, so a drawn seed is reproducible
    only via itself (record it to reproduce the run).
    """
    return secrets.randbits(32)


def randomize_plate_queue(
    queue: PlateQueue,
    mode: Literal["no", "random", "blocked", "blocked_uniform"],
    rng: random.Random | None = None,
) -> PlateQueue:
    """Randomize cells within (plate_id, container_id) groups.

    Args:
        queue: PlateQueue to randomize.
        mode: "no" (unchanged), "random" (shuffle), "blocked" (RCBD),
            "blocked_uniform" (group-uniform interleave).
        rng: Random source to draw from. Defaults to the process-global ``random``
            module; pass a ``random.Random(seed)`` instance for reproducible runs.

    Returns:
        New PlateQueue with randomized cells (plates/batches unchanged).
        Group order is preserved (e.g., if plate 2 comes before plate 1 in input,
        it will also come before plate 1 in output).
    """
    if mode == "no":
        return queue

    # Default to the global module so callers (and existing tests) that seed via
    # random.seed(...) keep working; QueueGenerator passes an explicit instance.
    if rng is None:
        rng = random

    # Group cells by (plate_id, container_id), preserving original group order
    def group_key(cell: PlateCell) -> tuple[int, int]:
        return (cell.plate_id, cell.sample.container_id)

    # Build groups in order of first occurrence (not sorted by key)
    grouped: dict[tuple[int, int], list[PlateCell]] = {}
    for cell in queue.cells:
        key = group_key(cell)
        if key not in grouped:
            grouped[key] = []
        grouped[key].append(cell)

    # Randomize within each group, preserving group order
    randomized_cells: list[PlateCell] = []
    for cells in grouped.values():
        if mode == "random":
            shuffled = _shuffle(cells, rng)
        elif mode == "blocked":
            shuffled = _block_randomize(cells, rng)
        else:  # blocked_uniform
            shuffled = _uniform_block_randomize(cells, rng)
        randomized_cells.extend(shuffled)

    return PlateQueue(batches=queue.batches, plates=queue.plates, cells=randomized_cells)


def _shuffle(cells: list[PlateCell], rng: random.Random) -> list[PlateCell]:
    """Simple shuffle, returns new list."""
    result = cells.copy()
    rng.shuffle(result)
    return result


def _block_randomize(cells: list[PlateCell], rng: random.Random) -> list[PlateCell]:
    """Randomized complete block design (RCBD).

    Groups cells by grouping_var, creates blocks with one from each group,
    shuffles within blocks.
    """
    # Check if any sample has grouping_var
    has_grouping = any(c.sample.grouping_var is not None for c in cells)
    if not has_grouping:
        return _shuffle(cells, rng)

    # Group by grouping_var
    by_group: dict[str | None, list[PlateCell]] = {}
    for cell in cells:
        key = cell.sample.grouping_var
        if key not in by_group:
            by_group[key] = []
        by_group[key].append(cell)

    # Shuffle within each group before assigning to blocks
    for group_cells in by_group.values():
        rng.shuffle(group_cells)

    # Create blocks
    max_size = max(len(g) for g in by_group.values())
    blocks: list[list[PlateCell]] = []
    for block_idx in range(max_size):
        block: list[PlateCell] = []
        for group_cells in by_group.values():
            if block_idx < len(group_cells):
                block.append(group_cells[block_idx])
        rng.shuffle(block)
        blocks.append(block)

    return [cell for block in blocks for cell in block]


def _uniform_block_randomize(cells: list[PlateCell], rng: random.Random) -> list[PlateCell]:
    """Group-uniform interleave for unbalanced ``grouping_var`` counts.

    Unlike RCBD (``_block_randomize``), which front-loads complete blocks and
    leaves a majority-only tail, this spreads every group evenly across the whole
    run. It uses fair-share ("most-behind") selection: at each output slot it emits
    a sample from a group whose ``emitted / total`` ratio is currently lowest.

    The order *among* the groups tied for most-behind is randomized at each step,
    so the group sequence within each round is shuffled rather than fixed -- the
    same within-round randomization RCBD applies (a group may occasionally repeat
    across a round boundary, exactly as in RCBD). Sample *identity* within each
    group is also randomized (each group is shuffled first). Spread is unaffected:
    only equally-behind groups are reordered, so every group still lands evenly
    across the run (eta^2 ~ 0). With equal group sizes this reduces to RCBD (one
    sample of each group per block, in a per-block-random order); for
    ``A x 5, B x 3, C x 2`` it still interleaves as ``A B C A B A C A B A`` up to
    tie ordering, rather than ``ABC ABC AB A A``.

    Given a seeded ``rng`` the result is reproducible.
    """
    has_grouping = any(c.sample.grouping_var is not None for c in cells)
    if not has_grouping:
        return _shuffle(cells, rng)

    # Group by grouping_var, preserving first-seen order
    by_group: dict[str | None, list[PlateCell]] = {}
    for cell in cells:
        by_group.setdefault(cell.sample.grouping_var, []).append(cell)

    # Randomize identity within each group
    for group_cells in by_group.values():
        rng.shuffle(group_cells)

    order = list(by_group.keys())
    totals = {key: len(by_group[key]) for key in order}
    emitted = dict.fromkeys(order, 0)

    result: list[PlateCell] = []
    for _ in range(sum(totals.values())):
        remaining = [key for key in order if emitted[key] < totals[key]]
        ratios = {key: emitted[key] / totals[key] for key in remaining}
        min_ratio = min(ratios.values())
        candidates = [key for key in remaining if ratios[key] == min_ratio]
        choice = rng.choice(candidates)
        result.append(by_group[choice][emitted[choice]])
        emitted[choice] += 1

    return result
