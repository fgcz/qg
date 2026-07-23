"""Tests for the public physical-positioning stage."""

from __future__ import annotations

import json
import os
import subprocess
import sys
from pathlib import Path

import pytest

from qg.config_models.loader import qg_configuration
from qg.generator import QueueGenerator
from qg.params_models import PlateQueueInput, PositionedQueueInput

from .helpers import make_queue_input

CONFIG_DIR = Path(__file__).parent.parent / "qg_configs"


@pytest.fixture
def config():
    qg_configuration.cache_clear()
    return qg_configuration(CONFIG_DIR)


def test_vial_input_becomes_positioned_without_mutating_source(config):
    source = make_queue_input(config=config, num_samples=5)
    before = source.model_dump_json()

    positioned = source.position_queue()

    assert isinstance(positioned, PositionedQueueInput)
    assert len(positioned.queue.cells) == 5
    assert source.model_dump_json() == before
    assert positioned.qg_version == source.qg_version
    assert positioned.resolved_config == source.resolved_config


def test_plate_input_is_validated_into_positioned_input(config):
    source = make_queue_input(config=config, num_samples=3)
    assigned = source.position_queue()
    plate_source = PlateQueueInput(
        parameters=source.parameters,
        queue=assigned.queue,
        qg_version=source.qg_version,
        resolved_config=source.resolved_config,
    )

    positioned = plate_source.position_queue()

    assert positioned.queue == assigned.queue


def test_queue_generator_rejects_unpositioned_input(config):
    with pytest.raises(TypeError, match="PositionedQueueInput"):
        QueueGenerator(config, make_queue_input(config=config, num_samples=1))  # type: ignore[arg-type]


def test_positioned_input_rejects_plate_without_tray(config):
    source = make_queue_input(config=config, num_samples=1)
    data = source.position_queue().model_dump()
    data["queue"]["plates"][1]["tray"] = None

    with pytest.raises(ValueError, match="missing a tray"):
        PositionedQueueInput.model_validate(data)


def test_generated_plate_ids_are_stable_across_processes(config, tmp_path):
    source_path = tmp_path / "source.json"
    source_path.write_text(make_queue_input(config=config, num_samples=60).model_dump_json())
    code = (
        "import json,sys; "
        "from qg.params_models import read_queue_input; "
        "q=read_queue_input(sys.argv[1]).position_queue(); "
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
