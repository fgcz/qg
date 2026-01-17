"""Tests for CLI entry points."""

import json
import subprocess
from pathlib import Path

import pytest

from qg.config import load_all_configs
from qg.params_simulator import simulate_params


@pytest.fixture
def configs():
    return load_all_configs(Path("qg_configs"))


@pytest.fixture
def projects_json(tmp_path):
    """Create minimal projects JSON for qg-params testing."""
    data = [
        {
            "id": 1000,
            "name": "Test Project",
            "technology": ["Proteomics"],
            "order": [{"id": 12345, "sample_count": 5, "plate_count": 0}],
        }
    ]
    path = tmp_path / "projects.json"
    path.write_text(json.dumps(data))
    return path


def test_validate_config_cli():
    """Test qg-validate CLI passes with valid configs."""
    result = subprocess.run(
        ["uv", "run", "qg-validate"],
        capture_output=True,
        text=True,
    )

    assert result.returncode == 0, f"CLI failed: {result.stderr}"
    assert "All validations passed" in result.stdout


def test_generate_params_dry_run(projects_json, tmp_path):
    """Test qg-params --dry-run shows what would be generated."""
    result = subprocess.run(
        ["uv", "run", "qg-params", "--projects", str(projects_json), "--dry-run", "--limit", "1"],
        capture_output=True,
        text=True,
    )

    assert result.returncode == 0, f"CLI failed: {result.stderr}"
    assert "Would create" in result.stdout or "valid combinations" in result.stdout


def test_generate_params_creates_json(projects_json, tmp_path):
    """Test qg-params generates JSON files, then use them with qg."""
    output_dir = tmp_path / "params"

    # Generate params (dry-run to avoid B-Fabric, but verify CLI works)
    result = subprocess.run(
        [
            "uv", "run", "qg-params",
            "--projects", str(projects_json),
            "--output-dir", str(output_dir),
            "--dry-run",
            "--limit", "1",
        ],
        capture_output=True,
        text=True,
    )

    assert result.returncode == 0, f"CLI failed: {result.stderr}"


def test_generate_queues_cli(configs, tmp_path):
    """Test qg CLI generates CSV output to file."""
    # Generate input using simulator
    queue_input = simulate_params(
        num_samples=5,
        configs=configs,
        technology="Proteomics",
        instrument="ASTRAL_1",
        sampler="Vanquish.vial",
        queue_pattern="standard",
    )
    input_file = tmp_path / "input.json"
    input_file.write_text(queue_input.model_dump_json(indent=2))

    output_file = tmp_path / "output.csv"

    result = subprocess.run(
        ["uv", "run", "qg", str(input_file), "-o", str(output_file)],
        capture_output=True,
        text=True,
    )

    assert result.returncode == 0, f"CLI failed: {result.stderr}"
    assert output_file.exists()
    content = output_file.read_text()

    # Check that output contains expected columns from config
    output_format = configs.output_formats.get_format(queue_input.parameters.output_format)
    expected_columns = list(output_format.columns.keys())
    header_line = content.split("\n")[0]
    for col in expected_columns:
        assert col in header_line, f"Missing column '{col}' in output"


def test_generate_queues_cli_stdout(configs, tmp_path):
    """Test qg CLI outputs to stdout when no -o given."""
    queue_input = simulate_params(
        num_samples=5,
        configs=configs,
        technology="Proteomics",
        instrument="ASTRAL_1",
        sampler="Vanquish.vial",
        queue_pattern="standard",
    )
    input_file = tmp_path / "input.json"
    input_file.write_text(queue_input.model_dump_json(indent=2))

    result = subprocess.run(
        ["uv", "run", "qg", str(input_file)],
        capture_output=True,
        text=True,
    )

    assert result.returncode == 0, f"CLI failed: {result.stderr}"

    # Check that output contains expected columns from config
    output_format = configs.output_formats.get_format(queue_input.parameters.output_format)
    expected_columns = list(output_format.columns.keys())
    header_line = result.stdout.split("\n")[0]
    for col in expected_columns:
        assert col in header_line, f"Missing column '{col}' in output"
