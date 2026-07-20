"""Tests for CLI entry points."""

import json
import subprocess
import sys
from pathlib import Path

import pytest

from qg.config_models.loader import qg_configuration
from qg.params_models import PlateQueueInput, PositionedQueueInput

from .helpers import make_queue_input

CONFIG_DIR = Path(__file__).parent.parent / "qg_configs"


def _cli(name: str) -> Path:
    """Resolve an entry point from the active Python environment."""
    return Path(sys.executable).with_name(name)


@pytest.fixture
def config():
    qg_configuration.cache_clear()
    return qg_configuration(CONFIG_DIR)


def test_validate_config_cli():
    """Test qg-validate CLI passes with valid configs."""
    result = subprocess.run(
        [_cli("qg-validate")],
        capture_output=True,
        text=True,
    )

    assert result.returncode == 0, f"CLI failed: {result.stderr}"
    # Validation output goes to stderr via loguru
    assert "All validations passed" in result.stderr


def test_generate_queues_cli(config, tmp_path):
    """Test qg CLI generates CSV output to file."""
    queue_input = make_queue_input(num_samples=5)
    input_file = tmp_path / "input.json"
    input_file.write_text(queue_input.model_dump_json(indent=2))

    output_file = tmp_path / "output.csv"

    result = subprocess.run(
        [_cli("qg"), str(input_file), "-o", str(output_file)],
        capture_output=True,
        text=True,
    )

    assert result.returncode == 0, f"CLI failed: {result.stderr}"
    assert output_file.exists()
    content = output_file.read_text()

    # Check that output contains expected columns from config
    output_format = config.output_formats.get_format(queue_input.parameters.output_format)
    expected_columns = list(output_format.columns.keys())
    lines = content.split("\n")
    # xcalibur_csv writer prepends a preamble line; find header by looking for first expected column
    header_line = next(line for line in lines if expected_columns[0] in line)
    for col in expected_columns:
        assert col in header_line, f"Missing column '{col}' in output"


def test_generate_queues_cli_stdout(config, tmp_path):
    """Test qg CLI outputs to stdout when no -o given."""
    queue_input = make_queue_input(num_samples=5)
    input_file = tmp_path / "input.json"
    input_file.write_text(queue_input.model_dump_json(indent=2))

    result = subprocess.run(
        [_cli("qg"), str(input_file)],
        capture_output=True,
        text=True,
    )

    assert result.returncode == 0, f"CLI failed: {result.stderr}"

    # Check that output contains expected columns from config
    output_format = config.output_formats.get_format(queue_input.parameters.output_format)
    expected_columns = list(output_format.columns.keys())
    lines = result.stdout.split("\n")
    header_line = next(line for line in lines if expected_columns[0] in line)
    for col in expected_columns:
        assert col in header_line, f"Missing column '{col}' in output"


def test_generate_queues_cli_seed_reproducible(config, tmp_path):
    """A seed set in the input JSON makes a randomized run reproducible across invocations."""
    queue_input = make_queue_input(num_samples=10)
    queue_input.parameters.randomization = "random"
    queue_input.parameters.seed = 123
    input_file = tmp_path / "input.json"
    input_file.write_text(queue_input.model_dump_json(indent=2))

    def run(out_name: str) -> str:
        out = tmp_path / out_name
        result = subprocess.run(
            [_cli("qg"), str(input_file), "-o", str(out)],
            capture_output=True,
            text=True,
        )
        assert result.returncode == 0, f"CLI failed: {result.stderr}"
        return out.read_text()

    assert run("a.csv") == run("b.csv")


def test_generate_queues_cli_missing_input_file(tmp_path):
    """qg exits non-zero when the input JSON file does not exist."""
    result = subprocess.run(
        [_cli("qg"), str(tmp_path / "nope.json")],
        capture_output=True,
        text=True,
    )
    assert result.returncode != 0


def test_generate_queues_cli_malformed_json(tmp_path):
    """qg exits non-zero when the input file is not valid JSON."""
    bad = tmp_path / "bad.json"
    bad.write_text("{not valid json")
    result = subprocess.run(
        [_cli("qg"), str(bad)],
        capture_output=True,
        text=True,
    )
    assert result.returncode != 0


def test_generate_queues_cli_unknown_instrument(tmp_path):
    """qg exits non-zero when the input names an instrument absent from the config."""
    queue_input = make_queue_input(num_samples=3)
    queue_input.parameters.instrument = "BOGUS_99"
    input_file = tmp_path / "input.json"
    input_file.write_text(queue_input.model_dump_json(indent=2))

    result = subprocess.run(
        [_cli("qg"), str(input_file)],
        capture_output=True,
        text=True,
    )
    assert result.returncode != 0


def test_app_local_launcher_invokes_marimo(mocker):
    """qg-app-local launches the local notebook via `marimo run` (no B-Fabric)."""
    from qg.apps import launcher_local

    run = mocker.patch.object(launcher_local.subprocess, "run")
    launcher_local.launch_local_app()

    run.assert_called_once()
    cmd = run.call_args.args[0]
    assert "marimo" in cmd and "run" in cmd
    notebook = Path(cmd[-1])
    assert notebook.name == "queue_app_local.py"
    assert notebook.exists()


@pytest.mark.bfabric
def test_find_projects_cli_help():
    """qg-find-projects (qg[bfabric]) imports and prints help with exit 0."""
    result = subprocess.run(
        [_cli("qg-find-projects"), "--help"],
        capture_output=True,
        text=True,
    )
    assert result.returncode == 0, f"CLI failed: {result.stderr}"
    assert "Usage:" in result.stdout


@pytest.mark.bfabric
def test_refresh_cache_cli_help():
    """qg-refresh-cache (qg[bfabric]) imports and prints help with exit 0."""
    result = subprocess.run(
        [_cli("qg-refresh-cache"), "--help"],
        capture_output=True,
        text=True,
    )
    assert result.returncode == 0, f"CLI failed: {result.stderr}"
    assert "Usage:" in result.stdout


def test_generate_queues_cli_hystar_xml(config, tmp_path):
    """Test qg CLI generates XML output for hystar format."""
    queue_input = make_queue_input(num_samples=3, output_format="hystar")
    input_file = tmp_path / "input.json"
    input_file.write_text(queue_input.model_dump_json(indent=2))

    output_file = tmp_path / "output.xml"

    result = subprocess.run(
        [_cli("qg"), str(input_file), "-o", str(output_file)],
        capture_output=True,
        text=True,
    )

    assert result.returncode == 0, f"CLI failed: {result.stderr}"
    assert output_file.exists()
    content = output_file.read_text()

    # Check XML structure
    assert "<?xml version=" in content
    assert "<SampleTable>" in content
    assert "</SampleTable>" in content
    assert "<Sample " in content

    # Check hystar-specific attributes
    assert 'Position="S' in content  # Position starts with S
    assert "SampleID=" in content
    assert "DataPath=" in content
    assert "ResultDatafile=" in content
    assert 'SampleComment=""' in content
    assert "ACQEND_EXECUTE=" in content


def test_assign_positions_cli_outputs_positioned_json(tmp_path):
    queue_input = make_queue_input(num_samples=5)
    input_file = tmp_path / "input.json"
    input_file.write_text(queue_input.model_dump_json(indent=2))
    output_file = tmp_path / "positioned.json"

    result = subprocess.run(
        [_cli("qg-assign-positions"), str(input_file), "-o", str(output_file)],
        capture_output=True,
        text=True,
    )

    assert result.returncode == 0, result.stderr
    positioned = PositionedQueueInput.model_validate_json(output_file.read_text())
    assert len(positioned.queue.cells) == 5
    assert positioned.qg_version == queue_input.qg_version


def test_assign_positions_cli_outputs_positioned_json_to_stdout(tmp_path):
    queue_input = make_queue_input(num_samples=3)
    input_file = tmp_path / "input.json"
    input_file.write_text(queue_input.model_dump_json(indent=2))

    result = subprocess.run(
        [_cli("qg-assign-positions"), str(input_file)],
        capture_output=True,
        text=True,
    )

    assert result.returncode == 0, result.stderr
    positioned = PositionedQueueInput.model_validate_json(result.stdout)
    assert len(positioned.queue.cells) == 3


def test_assign_positions_cli_validates_plate_input(tmp_path):
    source = make_queue_input(num_samples=4)
    expected = source.position_queue()
    plate_input = PlateQueueInput(
        parameters=source.parameters,
        queue=expected.queue,
        qg_version=source.qg_version,
        resolved_config=source.resolved_config,
    )
    input_file = tmp_path / "plate.json"
    input_file.write_text(plate_input.model_dump_json(indent=2))

    result = subprocess.run(
        [_cli("qg-assign-positions"), str(input_file)],
        capture_output=True,
        text=True,
    )

    assert result.returncode == 0, result.stderr
    assert PositionedQueueInput.model_validate_json(result.stdout) == expected


def test_positioned_cli_output_generates_same_vendor_queue(tmp_path):
    source = make_queue_input(num_samples=8)
    source_path = tmp_path / "source.json"
    positioned_path = tmp_path / "positioned.json"
    direct_output = tmp_path / "direct.csv"
    staged_output = tmp_path / "staged.csv"
    source_path.write_text(source.model_dump_json(indent=2))

    commands = [
        [_cli("qg-assign-positions"), str(source_path), "-o", str(positioned_path)],
        [_cli("qg"), str(source_path), "-o", str(direct_output)],
        [_cli("qg"), str(positioned_path), "-o", str(staged_output)],
    ]
    for command in commands:
        result = subprocess.run(command, capture_output=True, text=True)
        assert result.returncode == 0, result.stderr

    assert staged_output.read_bytes() == direct_output.read_bytes()


def test_assign_positions_cli_rejects_missing_provenance(tmp_path):
    data = make_queue_input(num_samples=2).model_dump(mode="json")
    data.pop("qg_version")
    data.pop("resolved_config")
    input_file = tmp_path / "unstamped.json"
    input_file.write_text(json.dumps(data))

    result = subprocess.run(
        [_cli("qg-assign-positions"), str(input_file)],
        capture_output=True,
        text=True,
    )

    assert result.returncode != 0
    assert "qg_version" in result.stderr
    assert "resolved_config" in result.stderr
