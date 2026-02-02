"""Tests for CLI entry points."""

import subprocess
from pathlib import Path

import pytest

from qg.config_models_new.loader import qg_configuration

from .helpers import make_queue_input

CONFIG_DIR = Path(__file__).parent.parent / "qg_configs_new"


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
    header_line = content.split("\n")[0]
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
    header_line = result.stdout.split("\n")[0]
    for col in expected_columns:
        assert col in header_line, f"Missing column '{col}' in output"
