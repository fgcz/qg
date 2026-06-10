"""Tests for CLI entry points."""

import subprocess
from pathlib import Path

import pytest

from qg.config_models.loader import qg_configuration

from .helpers import make_queue_input

CONFIG_DIR = Path(__file__).parent.parent / "qg_configs"


@pytest.fixture
def config():
    qg_configuration.cache_clear()
    return qg_configuration(CONFIG_DIR)


def test_validate_config_cli():
    """Test qg-validate CLI passes with valid configs."""
    result = subprocess.run(
        ["uv", "run", "qg-validate"],
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
        ["uv", "run", "qg", str(input_file), "-o", str(output_file), "-c", str(CONFIG_DIR)],
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
        ["uv", "run", "qg", str(input_file), "-c", str(CONFIG_DIR)],
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
            ["uv", "run", "qg", str(input_file), "-o", str(out), "-c", str(CONFIG_DIR)],
            capture_output=True,
            text=True,
        )
        assert result.returncode == 0, f"CLI failed: {result.stderr}"
        return out.read_text()

    assert run("a.csv") == run("b.csv")


def test_generate_queues_cli_missing_input_file(tmp_path):
    """qg exits non-zero when the input JSON file does not exist."""
    result = subprocess.run(
        ["uv", "run", "qg", str(tmp_path / "nope.json"), "-c", str(CONFIG_DIR)],
        capture_output=True,
        text=True,
    )
    assert result.returncode != 0


def test_generate_queues_cli_malformed_json(tmp_path):
    """qg exits non-zero when the input file is not valid JSON."""
    bad = tmp_path / "bad.json"
    bad.write_text("{not valid json")
    result = subprocess.run(
        ["uv", "run", "qg", str(bad), "-c", str(CONFIG_DIR)],
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
        ["uv", "run", "qg", str(input_file), "-c", str(CONFIG_DIR)],
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
        ["uv", "run", "qg-find-projects", "--help"],
        capture_output=True,
        text=True,
    )
    assert result.returncode == 0, f"CLI failed: {result.stderr}"
    assert "Usage:" in result.stdout


@pytest.mark.bfabric
def test_refresh_cache_cli_help():
    """qg-refresh-cache (qg[bfabric]) imports and prints help with exit 0."""
    result = subprocess.run(
        ["uv", "run", "qg-refresh-cache", "--help"],
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
        ["uv", "run", "qg", str(input_file), "-o", str(output_file), "-c", str(CONFIG_DIR)],
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
