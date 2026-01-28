"""Sample randomization for PlateQueue (new models).

Randomization respects boundaries:
- Within plates only (not across plates)
- Within container batches only (not across containers)

Cells are grouped by (plate_id, container_id) and shuffled within each group.
"""

from __future__ import annotations

import random
from itertools import groupby
from typing import Literal

from qg.params_models import PlateCell, PlateQueue


def randomize_plate_queue(
    queue: PlateQueue,
    mode: Literal["no", "random", "blocked"],
) -> PlateQueue:
    """Randomize cells within (plate_id, container_id) groups.

    Args:
        queue: PlateQueue to randomize.
        mode: "no" (unchanged), "random" (shuffle), "blocked" (RCBD).

    Returns:
        New PlateQueue with randomized cells (plates/batches unchanged).
    """
    if mode == "no":
        return queue

    # Group cells by (plate_id, container_id)
    def group_key(cell: PlateCell) -> tuple[int, int]:
        return (cell.plate_id, cell.sample.container_id)

    sorted_cells = sorted(queue.cells, key=group_key)
    grouped = {k: list(v) for k, v in groupby(sorted_cells, key=group_key)}

    # Randomize within each group
    randomized_cells: list[PlateCell] = []
    for cells in grouped.values():
        if mode == "random":
            shuffled = _shuffle(cells)
        else:  # blocked
            shuffled = _block_randomize(cells)
        randomized_cells.extend(shuffled)

    return PlateQueue(batches=queue.batches, plates=queue.plates, cells=randomized_cells)


def _shuffle(cells: list[PlateCell]) -> list[PlateCell]:
    """Simple shuffle, returns new list."""
    result = cells.copy()
    random.shuffle(result)
    return result


def _block_randomize(cells: list[PlateCell]) -> list[PlateCell]:
    """Randomized complete block design (RCBD).

    Groups cells by grouping_var, creates blocks with one from each group,
    shuffles within blocks.
    """
    # Check if any sample has grouping_var
    has_grouping = any(c.sample.grouping_var is not None for c in cells)
    if not has_grouping:
        return _shuffle(cells)

    # Group by grouping_var
    by_group: dict[str | None, list[PlateCell]] = {}
    for cell in cells:
        key = cell.sample.grouping_var
        if key not in by_group:
            by_group[key] = []
        by_group[key].append(cell)

    # Create blocks
    max_size = max(len(g) for g in by_group.values())
    blocks: list[list[PlateCell]] = []
    for block_idx in range(max_size):
        block: list[PlateCell] = []
        for group_cells in by_group.values():
            if block_idx < len(group_cells):
                block.append(group_cells[block_idx])
        random.shuffle(block)
        blocks.append(block)

    return [cell for block in blocks for cell in block]
