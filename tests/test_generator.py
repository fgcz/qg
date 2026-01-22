"""Tests for QueueGenerator output formats."""

from pathlib import Path

import polars as pl
import pytest

from qg.config import ConfigBundle, qg_config
from qg.generator import QueueGenerator
from qg.params_models import InputSample, QueueInput, QueueParameters, SampleGroup

CONFIG_DIR = Path(__file__).parent.parent / "qg_configs"


@pytest.fixture
def configs():
    """Load configs for all tests."""
    return qg_config(CONFIG_DIR)


@pytest.fixture
def ui_configs():
    """Load UI configs for all tests."""
    return qg_config(CONFIG_DIR)


def make_samples(n: int) -> list[InputSample]:
    """Create a list of n InputSamples."""
    return [InputSample(sample_name=f"sample_{i}", sample_id=10000 + i) for i in range(1, n + 1)]


def make_queue_input(params: QueueParameters, samples: list[InputSample], container_id: int = 99999) -> QueueInput:
    """Create QueueInput from params and samples."""
    return QueueInput(
        parameters=params,
        sample_groups=[SampleGroup(container_id=container_id, samples=samples)],
    )


def find_combination_for_format(configs: ConfigBundle, ui_configs, output_format: str) -> tuple[str, str, str]:
    """Find a valid (tech_area, instrument, sampler) for the given output format."""
    for combo in ui_configs.combinations.combinations:
        if combo.output_format == output_format:
            # Look up tech_area for this instrument
            for instr in configs.instruments.instruments:
                if instr.instrument == combo.instrument:
                    return instr.tech_area, combo.instrument, combo.sampler
    raise ValueError(f"No combination found for output_format={output_format}")


def get_expected_columns(configs: ConfigBundle, output_format: str) -> list[str]:
    """Get expected output columns for a format (only those that map to existing fields)."""
    fmt = configs.output_formats.get_format(output_format)
    if not fmt:
        raise ValueError(f"Unknown output format: {output_format}")
    # Only include columns that map to fields in QueueRow (or derived like position)
    queue_row_fields = {
        "run_number",
        "sample_type",
        "sample_id",
        "sample_name",
        "position",
        "tray",
        "grid_position",
        "inj_vol",
        "method",
        "file_name",
        "polarity",
        "data_path",
        "container_id",
    }
    return [col for col, field in fmt.columns.items() if field in queue_row_fields]


class TestOutputFormats:
    """Tests for QueueGenerator output formats - columns and row counts."""

    @pytest.mark.parametrize("output_format", ["xcalibur", "chronos", "hystar"])
    def test_output_format_has_expected_columns(self, configs, ui_configs, output_format: str):
        """Each output format produces a DataFrame with the configured columns."""
        tech_area, instrument, sampler = find_combination_for_format(configs, ui_configs, output_format)
        expected_columns = get_expected_columns(configs, output_format)
        samples = make_samples(1)

        params = QueueParameters(
            tech_area=tech_area,
            instrument=instrument,
            sampler=sampler,
            output_format=output_format,
            queue_pattern="noqc",
            polarity=["pos"],
            date="20260116",
            user="test",
        )
        queue_input = make_queue_input(params, samples)

        generator = QueueGenerator(configs, queue_input)
        result = generator.generate()

        assert isinstance(result, pl.DataFrame)
        missing = set(expected_columns) - set(result.columns)
        assert not missing, f"Missing columns for {output_format}: {missing}"

    @pytest.mark.parametrize("output_format", ["xcalibur", "chronos", "hystar"])
    def test_output_format_single_sample_row_count(self, configs, ui_configs, output_format: str):
        """Single sample with noqc pattern produces exactly 1 row."""
        tech_area, instrument, sampler = find_combination_for_format(configs, ui_configs, output_format)
        samples = make_samples(1)

        params = QueueParameters(
            tech_area=tech_area,
            instrument=instrument,
            sampler=sampler,
            output_format=output_format,
            queue_pattern="noqc",
            polarity=["pos"],
            date="20260116",
            user="test",
        )
        queue_input = make_queue_input(params, samples)

        generator = QueueGenerator(configs, queue_input)
        result = generator.generate()

        # All combinations found use Proteomics (no polarity), so expect 1 row
        assert len(result) == 1, f"{output_format}: expected 1 row, got {len(result)}"


class TestNoQCPattern:
    """Tests for noqc queue pattern - output should only contain user samples."""

    @pytest.mark.parametrize(
        "tech_area,instrument,sampler,expected_multiplier",
        [
            pytest.param("Proteomics", "ASTRAL_1", "Vanquish.vial", 1, id="Proteomics"),
            pytest.param("Metabolomics", "EXPLORIS_3", "Vanquish.vial", 2, id="Metabolomics"),
            pytest.param("Lipidomics", "EXPLORIS_3", "Vanquish.vial", 2, id="Lipidomics"),
        ],
    )
    @pytest.mark.parametrize("num_samples", [1, 5])
    def test_noqc_row_count(
        self,
        configs,
        tech_area: str,
        instrument: str,
        sampler: str,
        expected_multiplier: int,
        num_samples: int,
    ):
        """noqc pattern returns num_samples * polarity_multiplier rows."""
        samples = make_samples(num_samples)
        # Set polarity explicitly: proteomics=pos only, others=pos+neg
        polarity = ["pos", "neg"] if expected_multiplier == 2 else ["pos"]
        params = QueueParameters(
            tech_area=tech_area,
            instrument=instrument,
            sampler=sampler,
            output_format="xcalibur",
            queue_pattern="noqc",
            polarity=polarity,
            date="20260116",
            user="test",
        )
        queue_input = make_queue_input(params, samples)

        generator = QueueGenerator(configs, queue_input)
        result = generator.generate()

        expected = num_samples * expected_multiplier
        assert len(result) == expected, (
            f"{tech_area} noqc {num_samples} samples: expected {expected}, got {len(result)}"
        )


class TestQCOnlyPattern:
    """Tests for qc_only queue pattern - start/end QCs with user samples in middle."""

    @pytest.mark.parametrize("num_samples", [0, 5, 10])
    def test_qc_only_row_count(self, configs, num_samples: int):
        """qc_only pattern returns N + 6 rows (3 start + N samples + 3 end)."""
        samples = make_samples(num_samples)
        params = QueueParameters(
            tech_area="Proteomics",
            instrument="ASTRAL_1",
            sampler="Vanquish.vial",
            output_format="xcalibur",
            queue_pattern="qc_only",
            polarity=["pos"],
            date="20260116",
            user="test",
        )
        queue_input = make_queue_input(params, samples)

        generator = QueueGenerator(configs, queue_input)
        result = generator.generate()

        expected = num_samples + 6
        assert len(result) == expected, f"qc_only {num_samples} samples: expected {expected}, got {len(result)}"


class TestMetabolomicsBlankPattern:
    """Tests for Metabolomics.blank pattern with polarity expansion."""

    @pytest.mark.parametrize("num_samples", [0, 5, 10])
    def test_blank_row_count_with_polarity(self, configs, num_samples: int):
        """blank pattern returns (N + 6) * 2 rows (3 start + N samples + 3 end) * 2 polarities."""
        samples = make_samples(num_samples)
        params = QueueParameters(
            tech_area="Metabolomics",
            instrument="EXPLORIS_3",
            sampler="Vanquish.vial",
            output_format="xcalibur",
            queue_pattern="blank",
            polarity=["pos", "neg"],
            date="20260116",
            user="test",
        )
        queue_input = make_queue_input(params, samples)

        generator = QueueGenerator(configs, queue_input)
        result = generator.generate()

        expected = (num_samples + 6) * 2  # pos + neg polarity
        assert len(result) == expected, (
            f"Metabolomics blank {num_samples} samples: expected {expected}, got {len(result)}"
        )
