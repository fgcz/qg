"""Tests for the public physical-positioning stage."""

from __future__ import annotations

import json
import os
import subprocess
import sys

import pytest

from qg.generator import QueueGenerator
from qg.params_models import PlateQueueInput, PositionedQueueInput
from qg.positioning import position_queue

from .helpers import make_queue_input


def test_vial_input_becomes_positioned_without_mutating_source():
    source = make_queue_input(num_samples=5)
    before = source.model_dump_json()

    positioned = position_queue(source)

    assert isinstance(positioned, PositionedQueueInput)
    assert len(positioned.queue.cells) == 5
    assert source.model_dump_json() == before
    assert positioned.qg_version == source.qg_version
    assert positioned.resolved_config == source.resolved_config


def test_plate_input_is_validated_into_positioned_input():
    source = make_queue_input(num_samples=3)
    assigned = position_queue(source)
    plate_source = PlateQueueInput(
        parameters=source.parameters,
        queue=assigned.queue,
        qg_version=source.qg_version,
        resolved_config=source.resolved_config,
    )

    positioned = position_queue(plate_source)

    assert positioned.queue == assigned.queue


def test_queue_generator_rejects_unpositioned_input():
    with pytest.raises(TypeError, match="PositionedQueueInput"):
        QueueGenerator(make_queue_input(num_samples=1))  # type: ignore[arg-type]


def test_generated_plate_ids_are_stable_across_processes(tmp_path):
    source_path = tmp_path / "source.json"
    source_path.write_text(make_queue_input(num_samples=60).model_dump_json())
    code = (
        "import json,sys; "
        "from qg.params_models import read_queue_input; "
        "from qg.positioning import position_queue; "
        "q=position_queue(read_queue_input(sys.argv[1])); "
        "print(json.dumps(sorted(q.queue.plates)))"
    )

    results = []
    for hash_seed in ("1", "987654"):
        env = os.environ.copy()
        env["PYTHONHASHSEED"] = hash_seed
        result = subprocess.run(
            [sys.executable, "-c", code, str(source_path)],
            capture_output=True,
            check=True,
            text=True,
            env=env,
        )
        results.append(json.loads(result.stdout))

    assert results == [[1, 2], [1, 2]]
