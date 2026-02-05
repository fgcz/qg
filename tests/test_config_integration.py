"""Integration tests for config loading and validation."""

from pathlib import Path

import pytest

from qg.config_models.loader import QGConfiguration, qg_configuration

CONFIG_DIR = Path(__file__).parent.parent / "qg_configs"


# =============================================================================
# Fixtures
# =============================================================================


@pytest.fixture
def config() -> QGConfiguration:
    """Load configs via qg_configuration()."""
    qg_configuration.cache_clear()
    return qg_configuration(CONFIG_DIR)


# =============================================================================
# Test QGConfiguration Contents
# =============================================================================


class TestConfigSamples:
    """Tests for samples in QGConfiguration."""

    def test_samples_loaded(self, config: QGConfiguration) -> None:
        assert config.samples is not None
        assert len(config.samples.samples) > 0

    def test_samples_have_required_fields(self, config: QGConfiguration) -> None:
        for sample in config.samples.samples:
            assert sample.tech_area
            assert sample.sample_id
            assert sample.inj_vol > 0
            assert sample.file_name_template

    def test_each_tech_area_has_default(self, config: QGConfiguration) -> None:
        technologies = {s.tech_area for s in config.samples.samples}
        for tech in technologies:
            default = config.samples.get_sample(tech, "default")
            assert default is not None, f"Technology '{tech}' missing default sample"


class TestConfigInstruments:
    """Tests for instruments in QGConfiguration."""

    def test_instruments_loaded(self, config: QGConfiguration) -> None:
        assert config.instruments is not None
        assert len(config.instruments.instruments) > 0

    def test_instruments_have_methods_files(self, config: QGConfiguration) -> None:
        for instr in config.instruments.instruments:
            assert instr.methods_file.startswith("methods/")
            assert instr.methods_file.endswith("_methods.csv")

    def test_instruments_to_table(self, config: QGConfiguration) -> None:
        df = config.instruments.to_table()
        assert df is not None
        assert len(df) > 0


class TestConfigQueuePatterns:
    """Tests for queue_patterns in QGConfiguration."""

    def test_queue_patterns_loaded(self, config: QGConfiguration) -> None:
        assert config.queue_patterns is not None

    def test_patterns_have_technologies(self, config: QGConfiguration) -> None:
        technologies = config.queue_patterns.get_technologies()
        assert len(technologies) > 0

    def test_get_pattern_returns_pattern(self, config: QGConfiguration) -> None:
        tech = config.queue_patterns.get_technologies()[0]
        pattern_name = list(config.queue_patterns.get_patterns_for_tech_area(tech).keys())[0]
        pattern = config.queue_patterns.get_pattern(tech, pattern_name)
        assert pattern is not None
        assert pattern.run_QC_after_n_samples > 0

    def test_get_pattern_raises_for_unknown(self, config: QGConfiguration) -> None:
        tech = config.queue_patterns.get_technologies()[0]
        with pytest.raises(KeyError):
            config.queue_patterns.get_pattern("unknown_tech", "standard")
        with pytest.raises(KeyError):
            config.queue_patterns.get_pattern(tech, "nonexistent")


class TestConfigQCLayoutsGrid:
    """Tests for qc_layouts_grid in QGConfiguration."""

    def test_qc_layouts_grid_loaded(self, config: QGConfiguration) -> None:
        assert config.qc_layouts_grid is not None
        assert len(config.qc_layouts_grid.samples) > 0

    def test_get_samples_returns_dict(self, config: QGConfiguration) -> None:
        # Get first sample to extract tech_area, qc_layout_name, plate_layout
        first = config.qc_layouts_grid.samples[0]
        samples = config.qc_layouts_grid.get_samples(first.tech_area, first.qc_layout_name, first.plate_layout)
        assert samples is not None
        assert len(samples) > 0


class TestConfigOutputFormats:
    """Tests for output_formats in QGConfiguration."""

    def test_output_formats_loaded(self, config: QGConfiguration) -> None:
        assert config.output_formats is not None

    def test_formats_have_names(self, config: QGConfiguration) -> None:
        names = config.output_formats.get_format_names()
        assert len(names) > 0
        assert "xcalibur" in names

    def test_get_format_returns_format(self, config: QGConfiguration) -> None:
        fmt = config.output_formats.get_format("xcalibur")
        assert fmt is not None
        assert fmt.file_extension
        assert len(fmt.columns) > 0

    def test_get_format_raises_for_unknown(self, config: QGConfiguration) -> None:
        with pytest.raises(KeyError):
            config.output_formats.get_format("nonexistent")


class TestConfigSamplers:
    """Tests for samplers in QGConfiguration."""

    def test_samplers_loaded(self, config: QGConfiguration) -> None:
        assert config.samplers is not None

    def test_samplers_have_names(self, config: QGConfiguration) -> None:
        names = config.samplers.get_sampler_names()
        assert len(names) > 0
        assert "Vanquish" in names


class TestMethodResolution:
    """Tests for method resolution fallback logic."""

    def test_qc_sample_uses_fallback_to_default(self, config: QGConfiguration) -> None:
        """QC sample without specific method falls back to 'default' sample_type."""
        methods = config.methods.get_methods("Proteomics", "ASTRAL_1")
        # QC01 has no explicit method in ASTRAL_1_methods.csv
        path = methods.get_method_path("QC01", "pos", "")
        # Should fallback to default's method
        assert path != "", "QC01 should fallback to default method"

    def test_default_method_returns_path(self, config: QGConfiguration) -> None:
        """Default sample_type returns method path."""
        methods = config.methods.get_methods("Proteomics", "ASTRAL_1")
        path = methods.get_method_path("default", "pos", "DIA_60min")
        assert "DIA_60min" in path

    def test_explicit_qc_method_returns_specific_path(self, config: QGConfiguration) -> None:
        """QC sample with explicit method returns that specific method."""
        methods = config.methods.get_methods("Proteomics", "ASTRAL_1")
        # QC03dda has explicit method in ASTRAL_1_methods.csv
        path = methods.get_method_path("QC03dda", "pos", "")
        assert "DDA_60min" in path

    def test_nonexistent_method_returns_empty(self, config: QGConfiguration) -> None:
        """Nonexistent sample_type with no default fallback returns empty."""
        methods = config.methods.get_methods("Proteomics", "ASTRAL_1")
        # Request a method_name that doesn't exist for default
        path = methods.get_method_path("default", "pos", "NONEXISTENT_METHOD")
        assert path == ""


class TestConfigInstrumentConfigs:
    """Tests for instrument_configs (UI) in QGConfiguration."""

    def test_instrument_configs_loaded(self, config: QGConfiguration) -> None:
        assert config.instrument_configs is not None
        assert len(config.instrument_configs.configs) > 0

    def test_instrument_configs_have_valid_samplers(self, config: QGConfiguration) -> None:
        for ic in config.instrument_configs.configs:
            # Sampler should be base name (e.g., "Vanquish", not "Vanquish.vial")
            assert "." not in ic.sampler


# =============================================================================
# Test Validation
# =============================================================================


class TestQgConfigurationValidation:
    """Tests for validation during qg_configuration() loading."""

    def test_qg_configuration_validates_successfully(self) -> None:
        """qg_configuration() runs validation and returns config on success."""
        qg_configuration.cache_clear()
        config = qg_configuration(CONFIG_DIR)
        assert config is not None
        assert config.samples is not None
        assert config.instruments is not None


# =============================================================================
# Test Cross-References
# =============================================================================


class TestCrossReferences:
    """Tests for cross-reference integrity between configs."""

    def test_queue_patterns_reference_valid_samples(self, config: QGConfiguration) -> None:
        """All sample_ids in queue_patterns should exist in samples.csv."""
        for tech in config.queue_patterns.get_technologies():
            pattern_refs = config.queue_patterns.get_all_sample_ids(tech)
            valid_ids = {s.sample_id for s in config.samples.get_by_tech_area(tech)}
            unknown = pattern_refs - valid_ids
            assert not unknown, f"Unknown samples in {tech} patterns: {unknown}"

    def test_instrument_configs_reference_valid_instruments(self, config: QGConfiguration) -> None:
        """All instruments in instrument_configs should exist in instruments.csv."""
        valid_names = {i.instrument for i in config.instruments.instruments}
        config_names = {c.instrument for c in config.instrument_configs.configs}
        unknown = config_names - valid_names
        assert not unknown, f"Unknown instruments in instrument_configs: {unknown}"

    def test_instrument_configs_reference_valid_output_formats(self, config: QGConfiguration) -> None:
        """All output_formats in instrument_configs should exist in output_formats.toml."""
        valid_formats = set(config.output_formats.get_format_names())
        config_formats = {c.output_format for c in config.instrument_configs.configs}
        unknown = config_formats - valid_formats
        assert not unknown, f"Unknown output formats in instrument_configs: {unknown}"


# =============================================================================
# Test QueueParameters.create() Factory
# =============================================================================


class TestQueueParametersCreate:
    """Tests for QueueParameters.create() factory method validation."""

    def test_create_with_valid_params(self, config: QGConfiguration) -> None:
        """Factory creates QueueParameters with valid config references."""
        from qg.params_models import QueueParameters

        params = QueueParameters.create(
            config,
            tech_area="Proteomics",
            instrument="ASTRAL_1",
            sampler="Vanquish",
            output_format="xcalibur",
            queue_pattern="standard",
            queue_type="Vial",
            plate_layout="Vanquish_54",
            polarity=["pos"],
            date="20260122",
            user="testuser",
        )
        assert params.tech_area == "Proteomics"
        assert params.instrument == "ASTRAL_1"

    def test_create_rejects_invalid_pattern(self, config: QGConfiguration) -> None:
        """Factory raises KeyError for non-existent pattern."""
        from qg.params_models import QueueParameters

        with pytest.raises(KeyError, match="Pattern 'nonexistent' not found"):
            QueueParameters.create(
                config,
                tech_area="Proteomics",
                instrument="ASTRAL_1",
                sampler="Vanquish",
                output_format="xcalibur",
                queue_pattern="nonexistent",
                queue_type="Vial",
                plate_layout="Vanquish_54",
                polarity=["pos"],
                date="20260122",
            )

    def test_create_rejects_invalid_output_format(self, config: QGConfiguration) -> None:
        """Factory raises KeyError for non-existent output format."""
        from qg.params_models import QueueParameters

        with pytest.raises(KeyError, match="Output format 'nonexistent' not found"):
            QueueParameters.create(
                config,
                tech_area="Proteomics",
                instrument="ASTRAL_1",
                sampler="Vanquish",
                output_format="nonexistent",
                queue_pattern="standard",
                queue_type="Vial",
                plate_layout="Vanquish_54",
                polarity=["pos"],
                date="20260122",
            )

    def test_create_rejects_invalid_instrument(self, config: QGConfiguration) -> None:
        """Factory raises KeyError for non-existent instrument."""
        from qg.params_models import QueueParameters

        with pytest.raises(KeyError, match="Instrument 'FAKE_INSTRUMENT' not found"):
            QueueParameters.create(
                config,
                tech_area="Proteomics",
                instrument="FAKE_INSTRUMENT",
                sampler="Vanquish",
                output_format="xcalibur",
                queue_pattern="standard",
                queue_type="Vial",
                plate_layout="Vanquish_54",
                polarity=["pos"],
                date="20260122",
            )

    def test_create_rejects_invalid_sampler(self, config: QGConfiguration) -> None:
        """Factory raises KeyError for non-existent sampler."""
        from qg.params_models import QueueParameters

        with pytest.raises(KeyError, match="Sampler 'FakeSampler' not found"):
            QueueParameters.create(
                config,
                tech_area="Proteomics",
                instrument="ASTRAL_1",
                sampler="FakeSampler",
                output_format="xcalibur",
                queue_pattern="standard",
                queue_type="Vial",
                plate_layout="Vanquish_54",
                polarity=["pos"],
                date="20260122",
            )

    def test_create_rejects_invalid_plate_layout(self, config: QGConfiguration) -> None:
        """Factory raises ValueError for invalid plate_layout/sampler combo."""
        from qg.params_models import QueueParameters

        with pytest.raises(ValueError, match="not valid for"):
            QueueParameters.create(
                config,
                tech_area="Proteomics",
                instrument="ASTRAL_1",
                sampler="Vanquish",
                output_format="xcalibur",
                queue_pattern="standard",
                queue_type="Vial",
                plate_layout="Evosep_96",  # Invalid for Vanquish
                polarity=["pos"],
                date="20260122",
            )


class TestQueueInputCreate:
    """Tests for VialQueueInput / PlateQueueInput construction."""

    def test_plate_cell_requires_grid_position(self) -> None:
        """PlateCell requires grid_position (enforced by Pydantic)."""
        from pydantic import ValidationError

        from qg.params_models import PlateCell, VialSample

        sample = VialSample(sample_name="S1", sample_id=1001, container_id=12345)
        with pytest.raises(ValidationError):
            PlateCell(sample=sample, position=1, plate_id=1)  # missing grid_position

    def test_plate_queue_input_with_positions_succeeds(self) -> None:
        """PlateQueueInput succeeds when cells have grid_position."""
        from qg.params_models import (
            ContainerBatch,
            Plate,
            PlateCell,
            PlateQueue,
            PlateQueueInput,
            QueueParameters,
            VialSample,
        )

        params = QueueParameters(
            tech_area="Proteomics",
            instrument="ASTRAL_1",
            sampler="Vanquish",
            output_format="xcalibur",
            queue_pattern="standard",
            queue_type="Plate",
            plate_layout="Vanquish_96",
            polarity=["pos"],
            date="20260122",
        )
        sample = VialSample(sample_name="S1", sample_id=1001, container_id=12345)
        plate_id = 1
        queue = PlateQueue(
            batches={12345: ContainerBatch(container_id=12345)},
            plates={plate_id: Plate(plate_id=plate_id, tray="Y", nr_samples=1)},
            cells=[PlateCell(sample=sample, position=1, grid_position="A1", plate_id=plate_id)],
        )
        queue_input = PlateQueueInput(parameters=params, queue=queue)
        assert len(queue_input.queue.cells) == 1

    def test_vial_queue_input_without_positions_succeeds(self) -> None:
        """VialQueueInput succeeds without grid_position (positions are generated)."""
        from qg.params_models import ContainerBatch, QueueParameters, VialQueue, VialQueueInput, VialSample

        params = QueueParameters(
            tech_area="Proteomics",
            instrument="ASTRAL_1",
            sampler="Vanquish",
            output_format="xcalibur",
            queue_pattern="standard",
            queue_type="Vial",
            plate_layout="Vanquish_54",
            polarity=["pos"],
            date="20260122",
        )
        samples = [
            VialSample(sample_name="S1", sample_id=1001, container_id=12345),
            VialSample(sample_name="S2", sample_id=1002, container_id=12345),
        ]
        queue = VialQueue(
            batches={12345: ContainerBatch(container_id=12345)},
            samples=samples,
        )
        queue_input = VialQueueInput(parameters=params, queue=queue)
        assert len(queue_input.queue.samples) == 2
