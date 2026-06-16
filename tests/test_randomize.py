"""Tests for qg.randomize module."""

from __future__ import annotations

import random as py_random

import pytest

from qg.params_models import ContainerBatch, Plate, PlateCell, PlateQueue, VialSample
from qg.randomize import draw_seed, randomize_plate_queue


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
        # A: ids 1,2  B: ids 3,4  C: ids 5,6
        queue = make_grouped_plate_queue({"A": 2, "B": 2, "C": 2})

        result = randomize_plate_queue(queue, "blocked")
        result_ids = get_sample_ids(result)

        # Each block has exactly one sample from each group
        group_ids = {"A": {1, 2}, "B": {3, 4}, "C": {5, 6}}
        block1 = set(result_ids[:3])
        block2 = set(result_ids[3:])

        for group_name, ids in group_ids.items():
            assert len(block1 & ids) == 1, f"Block 1 should have exactly 1 from group {group_name}"
            assert len(block2 & ids) == 1, f"Block 2 should have exactly 1 from group {group_name}"

        assert set(result_ids) == {1, 2, 3, 4, 5, 6}

    def test_unequal_groups_handles_incomplete_blocks(self):
        py_random.seed(42)
        # A: ids 1,2,3  B: ids 4,5  C: ids 6
        queue = make_grouped_plate_queue({"A": 3, "B": 2, "C": 1})

        result = randomize_plate_queue(queue, "blocked")
        result_ids = get_sample_ids(result)

        group_ids = {"A": {1, 2, 3}, "B": {4, 5}, "C": {6}}

        # Block 1: one from each group (3 samples)
        block1 = set(result_ids[:3])
        assert len(block1 & group_ids["A"]) == 1
        assert len(block1 & group_ids["B"]) == 1
        assert len(block1 & group_ids["C"]) == 1

        # Block 2: one from A, one from B (2 samples, C exhausted)
        block2 = set(result_ids[3:5])
        assert len(block2 & group_ids["A"]) == 1
        assert len(block2 & group_ids["B"]) == 1

        # Block 3: one from A (1 sample, B and C exhausted)
        block3 = set(result_ids[5:])
        assert len(block3 & group_ids["A"]) == 1

        assert set(result_ids) == {1, 2, 3, 4, 5, 6}

    def test_single_group_shuffles_order(self):
        """Single grouping_var group: blocked mode should still shuffle order."""
        py_random.seed(42)
        queue = make_grouped_plate_queue({"A": 10})
        original_ids = get_sample_ids(queue)

        result = randomize_plate_queue(queue, "blocked")
        result_ids = get_sample_ids(result)

        assert set(result_ids) == set(original_ids), "All sample IDs must be preserved"
        assert result_ids != original_ids, "Single group should be shuffled, not left in input order"

    def test_no_grouping_var_falls_back_to_shuffle(self):
        py_random.seed(42)
        queue = make_plate_queue(5)  # No grouping_var
        original_ids = get_sample_ids(queue)

        result = randomize_plate_queue(queue, "blocked")
        result_ids = get_sample_ids(result)

        assert set(result_ids) == set(original_ids)
        assert result_ids != original_ids


class TestUniformBlockedRandomization:
    """blocked_uniform: spread groups evenly via fair-share interleave."""

    @staticmethod
    def _labels(queue: PlateQueue) -> list[str | None]:
        return [c.sample.grouping_var for c in queue.cells]

    def test_unequal_groups_spread_evenly(self):
        """A x 5, B x 3, C x 2 -> every group spread across the run, not front-loaded."""
        py_random.seed(42)
        queue = make_grouped_plate_queue({"A": 5, "B": 3, "C": 2})

        result = randomize_plate_queue(queue, "blocked_uniform")
        labels = self._labels(result)

        # All samples preserved exactly once, with the right per-group counts.
        assert set(get_sample_ids(result)) == set(range(1, 11))
        assert labels.count("A") == 5
        assert labels.count("B") == 3
        assert labels.count("C") == 2
        # Fair share spreads each group across the whole run: the smallest group's
        # two members land in different halves, and the largest reaches both halves.
        c_positions = [i for i, lab in enumerate(labels) if lab == "C"]
        a_positions = [i for i, lab in enumerate(labels) if lab == "A"]
        assert c_positions[0] < 5 <= c_positions[1], labels
        assert a_positions[0] < 5 <= a_positions[-1], labels

    def test_no_majority_only_tail(self):
        """The largest group must not be exhausted only at the end (vs. blocked)."""
        py_random.seed(1)
        queue = make_grouped_plate_queue({"A": 6, "B": 2})

        labels = self._labels(randomize_plate_queue(queue, "blocked_uniform"))

        # B is interleaved (one per half), not stranded in the first slots like RCBD.
        b_positions = [i for i, lab in enumerate(labels) if lab == "B"]
        assert b_positions[0] < 4 <= b_positions[1], labels
        assert labels.count("A") == 6
        assert labels.count("B") == 2

    def test_equal_groups_match_blocked(self):
        """Equal counts reduce to RCBD: one of each group per block."""
        py_random.seed(42)
        queue = make_grouped_plate_queue({"A": 2, "B": 2, "C": 2})

        labels = self._labels(randomize_plate_queue(queue, "blocked_uniform"))

        assert set(labels[:3]) == {"A", "B", "C"}
        assert set(labels[3:]) == {"A", "B", "C"}

    def test_identity_shuffled_within_groups(self):
        """Sample identity within a group is randomized (so is the per-round order)."""
        py_random.seed(7)
        queue = make_grouped_plate_queue({"A": 5, "B": 3, "C": 2})

        result = randomize_plate_queue(queue, "blocked_uniform")

        # All A samples (ids 1-5) preserved exactly once, in some shuffled order.
        a_ids = [c.sample.sample_id for c in result.cells if c.sample.grouping_var == "A"]
        assert set(a_ids) == {1, 2, 3, 4, 5}

    def test_within_round_order_varies_across_seeds(self):
        """Equal groups: per-round group order is randomized, not a fixed repeat."""
        seqs = []
        for seed in (1, 7):
            queue = make_grouped_plate_queue({"A": 4, "B": 4, "C": 4, "D": 4})
            labels = self._labels(randomize_plate_queue(queue, "blocked_uniform", py_random.Random(seed)))
            # Each consecutive block of four is a full permutation of the groups:
            # fair-share spread is preserved and no group clusters.
            for start in range(0, 16, 4):
                assert set(labels[start : start + 4]) == {"A", "B", "C", "D"}, labels
            seqs.append(labels)

        # Two seeds produce different within-round orderings (no longer deterministic).
        assert seqs[0] != seqs[1]

    def test_single_group_shuffles_order(self):
        py_random.seed(42)
        queue = make_grouped_plate_queue({"A": 10})
        original_ids = get_sample_ids(queue)

        result_ids = get_sample_ids(randomize_plate_queue(queue, "blocked_uniform"))

        assert set(result_ids) == set(original_ids)
        assert result_ids != original_ids

    def test_no_grouping_var_falls_back_to_shuffle(self):
        py_random.seed(42)
        queue = make_plate_queue(5)
        original_ids = get_sample_ids(queue)

        result_ids = get_sample_ids(randomize_plate_queue(queue, "blocked_uniform"))

        assert set(result_ids) == set(original_ids)
        assert result_ids != original_ids

    def test_respects_plate_container_boundaries(self):
        py_random.seed(42)
        queue = make_multi_plate_queue([(1, 100, 5), (2, 200, 5)])

        result = randomize_plate_queue(queue, "blocked_uniform")

        group1 = {c.sample.sample_id for c in result.cells if c.plate_id == 1}
        group2 = {c.sample.sample_id for c in result.cells if c.plate_id == 2}
        assert group1 == {1, 2, 3, 4, 5}
        assert group2 == {6, 7, 8, 9, 10}


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

    def test_plate_order_preserved(self):
        """Plate order from B-Fabric input is preserved (plate 2 before plate 1)."""
        py_random.seed(42)
        # Input order: plate 2 first, then plate 1 (simulating B-Fabric order)
        queue = make_multi_plate_queue(
            [
                (2, 100, 5),  # plate 2, container 100, ids 1-5
                (1, 200, 5),  # plate 1, container 200, ids 6-10
            ]
        )
        input_plate_order = [c.plate_id for c in queue.cells]
        assert input_plate_order == [2, 2, 2, 2, 2, 1, 1, 1, 1, 1], "Test setup: plate 2 before plate 1"

        result = randomize_plate_queue(queue, "random")

        # Plate order must be preserved: first 5 are plate 2, next 5 are plate 1
        output_plate_order = [c.plate_id for c in result.cells]
        assert output_plate_order[:5] == [2, 2, 2, 2, 2], "Plate 2 samples should come first"
        assert output_plate_order[5:] == [1, 1, 1, 1, 1], "Plate 1 samples should come second"

    def test_plate_order_preserved_blocked(self):
        """Plate order preserved with blocked randomization too."""
        py_random.seed(42)
        # Create cells manually to set grouping_var and preserve plate 2 before plate 1
        cells = []
        batches = {100: ContainerBatch(container_id=100), 200: ContainerBatch(container_id=200)}
        plates = {1: Plate(plate_id=1, tray="Y", nr_samples=4), 2: Plate(plate_id=2, tray="R", nr_samples=4)}

        # Plate 2 first (container 100)
        for i, group in enumerate(["A", "A", "B", "B"]):
            s = VialSample(sample_name=f"p2_{group}{i}", sample_id=i + 1, container_id=100, grouping_var=group)
            cells.append(PlateCell(sample=s, position=i, grid_position=f"A{i}", plate_id=2))
        # Plate 1 second (container 200)
        for i, group in enumerate(["A", "A", "B", "B"]):
            s = VialSample(sample_name=f"p1_{group}{i}", sample_id=i + 5, container_id=200, grouping_var=group)
            cells.append(PlateCell(sample=s, position=i, grid_position=f"A{i}", plate_id=1))

        queue = PlateQueue(batches=batches, plates=plates, cells=cells)

        result = randomize_plate_queue(queue, "blocked")

        output_plate_order = [c.plate_id for c in result.cells]
        assert output_plate_order[:4] == [2, 2, 2, 2], "Plate 2 samples should come first"
        assert output_plate_order[4:] == [1, 1, 1, 1], "Plate 1 samples should come second"


class TestSeedReproducibility:
    """An explicit random.Random(seed) makes randomized modes reproducible."""

    @pytest.mark.parametrize("mode", ["random", "blocked", "blocked_uniform"])
    def test_same_seed_same_order(self, mode):
        queue = make_grouped_plate_queue({"A": 5, "B": 3, "C": 2})

        first = randomize_plate_queue(queue, mode, py_random.Random(123))
        second = randomize_plate_queue(queue, mode, py_random.Random(123))

        assert get_sample_ids(first) == get_sample_ids(second)

    @pytest.mark.parametrize("mode", ["random", "blocked", "blocked_uniform"])
    def test_different_seeds_differ(self, mode):
        # Large input so two seeds almost surely yield a different order.
        queue = make_grouped_plate_queue({"A": 20, "B": 20})

        first = randomize_plate_queue(queue, mode, py_random.Random(1))
        second = randomize_plate_queue(queue, mode, py_random.Random(2))

        assert get_sample_ids(first) != get_sample_ids(second)
        assert set(get_sample_ids(first)) == set(get_sample_ids(second))

    def test_seed_independent_of_global_state(self):
        # A seeded instance ignores the global RNG: perturbing random.seed between
        # calls must not change the result.
        queue = make_plate_queue(15)

        py_random.seed(0)
        first = randomize_plate_queue(queue, "random", py_random.Random(77))
        py_random.seed(999)
        second = randomize_plate_queue(queue, "random", py_random.Random(77))

        assert get_sample_ids(first) == get_sample_ids(second)


class TestDrawSeed:
    def test_returns_32bit_int(self):
        seeds = {draw_seed() for _ in range(50)}
        assert all(isinstance(s, int) and 0 <= s < 2**32 for s in seeds)
        # Overwhelmingly likely to be distinct; guards against a constant return.
        assert len(seeds) > 1
