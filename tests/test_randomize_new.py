"""Tests for qg.randomize_new module."""

from __future__ import annotations

import random as py_random

from qg.params_models_new import ContainerBatch, Plate, PlateCell, PlateQueue, VialSample
from qg.randomize_new import randomize_plate_queue


def make_plate_queue(
    n_samples: int,
    plate_id: int = 1,
    container_id: int = 99999,
    id_offset: int = 10000,
) -> PlateQueue:
    """Create PlateQueue with n samples on one plate/container."""
    samples = [
        VialSample(sample_name=f"sample_{i}", sample_id=id_offset + i, container_id=container_id)
        for i in range(1, n_samples + 1)
    ]
    cells = [
        PlateCell(sample=s, position=i + 1, grid_position=f"A{i + 1}", plate_id=plate_id) for i, s in enumerate(samples)
    ]
    return PlateQueue(
        batches={container_id: ContainerBatch(container_id=container_id)},
        plates={plate_id: Plate(plate_id=plate_id, tray="Y", nr_samples=n_samples)},
        cells=cells,
    )


def make_multi_plate_queue(
    groups: list[tuple[int, int, int]],  # (plate_id, container_id, n_samples)
    id_start: int = 1,
) -> PlateQueue:
    """Create PlateQueue with multiple (plate_id, container_id) groups."""
    batches: dict[int, ContainerBatch] = {}
    plates: dict[int, Plate] = {}
    cells: list[PlateCell] = []
    current_id = id_start

    for plate_id, container_id, n_samples in groups:
        if container_id not in batches:
            batches[container_id] = ContainerBatch(container_id=container_id)
        if plate_id not in plates:
            plates[plate_id] = Plate(plate_id=plate_id, tray=f"P{plate_id}", nr_samples=0)

        for i in range(n_samples):
            sample = VialSample(
                sample_name=f"s_{current_id}",
                sample_id=current_id,
                container_id=container_id,
            )
            cells.append(
                PlateCell(
                    sample=sample,
                    position=i + 1,
                    grid_position=f"A{i + 1}",
                    plate_id=plate_id,
                )
            )
            current_id += 1

    # Update sample counts
    for plate_id in plates:
        plates[plate_id] = Plate(
            plate_id=plate_id,
            tray=plates[plate_id].tray,
            nr_samples=sum(1 for c in cells if c.plate_id == plate_id),
        )

    return PlateQueue(batches=batches, plates=plates, cells=cells)


def make_grouped_plate_queue(
    groups_spec: dict[str, int],  # grouping_var -> count
    plate_id: int = 1,
    container_id: int = 99999,
    id_start: int = 1,
) -> PlateQueue:
    """Create PlateQueue with samples having grouping_var."""
    cells: list[PlateCell] = []
    current_id = id_start
    pos = 1

    for group_name, count in groups_spec.items():
        for i in range(count):
            sample = VialSample(
                sample_name=f"{group_name.lower()}{i + 1}",
                sample_id=current_id,
                container_id=container_id,
                grouping_var=group_name,
            )
            cells.append(
                PlateCell(
                    sample=sample,
                    position=pos,
                    grid_position=f"A{pos}",
                    plate_id=plate_id,
                )
            )
            current_id += 1
            pos += 1

    return PlateQueue(
        batches={container_id: ContainerBatch(container_id=container_id)},
        plates={plate_id: Plate(plate_id=plate_id, tray="Y", nr_samples=len(cells))},
        cells=cells,
    )


def get_sample_ids(queue: PlateQueue) -> list[int]:
    """Extract sample IDs from PlateQueue."""
    return [c.sample.sample_id for c in queue.cells]


class TestNoRandomization:
    def test_preserves_order(self):
        queue = make_plate_queue(5)
        original_ids = get_sample_ids(queue)

        result = randomize_plate_queue(queue, "no")

        assert get_sample_ids(result) == original_ids

    def test_returns_same_object(self):
        queue = make_plate_queue(5)

        result = randomize_plate_queue(queue, "no")

        assert result is queue


class TestRandomShuffle:
    def test_shuffles_samples(self):
        py_random.seed(42)
        queue = make_plate_queue(10)
        original_ids = get_sample_ids(queue)

        result = randomize_plate_queue(queue, "random")
        result_ids = get_sample_ids(result)

        assert result_ids != original_ids
        assert set(result_ids) == set(original_ids)

    def test_shuffles_within_plate_container_groups(self):
        """Samples on different plates/containers are NOT mixed."""
        py_random.seed(42)
        # Two groups: (plate=1, container=1) and (plate=2, container=2)
        queue = make_multi_plate_queue(
            [
                (1, 1, 5),  # ids 1-5
                (2, 2, 5),  # ids 6-10
            ]
        )

        result = randomize_plate_queue(queue, "random")

        # Get ids by (plate_id, container_id)
        group1_ids = [c.sample.sample_id for c in result.cells if c.plate_id == 1]
        group2_ids = [c.sample.sample_id for c in result.cells if c.plate_id == 2]

        # Each group should contain only its original samples
        assert set(group1_ids) == {1, 2, 3, 4, 5}
        assert set(group2_ids) == {6, 7, 8, 9, 10}


class TestBlockedRandomization:
    def test_equal_groups_creates_balanced_blocks(self):
        py_random.seed(42)
        queue = make_grouped_plate_queue({"A": 2, "B": 2, "C": 2})

        result = randomize_plate_queue(queue, "blocked")
        result_ids = get_sample_ids(result)

        # Block 1: one from each group (ids 1, 3, 5)
        # Block 2: one from each group (ids 2, 4, 6)
        block1 = set(result_ids[:3])
        block2 = set(result_ids[3:])

        assert block1 == {1, 3, 5}
        assert block2 == {2, 4, 6}

    def test_unequal_groups_handles_incomplete_blocks(self):
        py_random.seed(42)
        queue = make_grouped_plate_queue({"A": 3, "B": 2, "C": 1})

        result = randomize_plate_queue(queue, "blocked")
        result_ids = get_sample_ids(result)

        block1 = set(result_ids[:3])
        block2 = set(result_ids[3:5])
        block3 = set(result_ids[5:])

        assert block1 == {1, 4, 6}
        assert block2 == {2, 5}
        assert block3 == {3}

    def test_no_grouping_var_falls_back_to_shuffle(self):
        py_random.seed(42)
        queue = make_plate_queue(5)  # No grouping_var
        original_ids = get_sample_ids(queue)

        result = randomize_plate_queue(queue, "blocked")
        result_ids = get_sample_ids(result)

        assert set(result_ids) == set(original_ids)
        assert result_ids != original_ids


class TestBoundaryRespect:
    """Tests that randomization respects plate AND container boundaries."""

    def test_same_plate_different_containers(self):
        """Samples on same plate but different containers are NOT mixed."""
        py_random.seed(42)
        queue = make_multi_plate_queue(
            [
                (1, 100, 5),  # plate 1, container 100, ids 1-5
                (1, 200, 5),  # plate 1, container 200, ids 6-10
            ]
        )

        result = randomize_plate_queue(queue, "random")

        group1_ids = [c.sample.sample_id for c in result.cells if c.sample.container_id == 100]
        group2_ids = [c.sample.sample_id for c in result.cells if c.sample.container_id == 200]

        assert set(group1_ids) == {1, 2, 3, 4, 5}
        assert set(group2_ids) == {6, 7, 8, 9, 10}

    def test_same_container_different_plates(self):
        """Samples in same container but different plates are NOT mixed."""
        py_random.seed(42)
        queue = make_multi_plate_queue(
            [
                (1, 100, 5),  # plate 1, container 100, ids 1-5
                (2, 100, 5),  # plate 2, container 100, ids 6-10
            ]
        )

        result = randomize_plate_queue(queue, "random")

        group1_ids = [c.sample.sample_id for c in result.cells if c.plate_id == 1]
        group2_ids = [c.sample.sample_id for c in result.cells if c.plate_id == 2]

        assert set(group1_ids) == {1, 2, 3, 4, 5}
        assert set(group2_ids) == {6, 7, 8, 9, 10}
