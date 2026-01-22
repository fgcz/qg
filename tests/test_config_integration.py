"""Integration tests for config loading and validation."""

from pathlib import Path

import pytest

from qg.config import ConfigBundle, qg_config

# =============================================================================
# Fixtures
# =============================================================================


@pytest.fixture
def config_bundle() -> ConfigBundle:
    """Load configs via qg_config()."""
    qg_config.cache_clear()  # Clear LRU cache for test isolation
    config_dir = Path(__file__).parent.parent / "qg_configs"
    return qg_config(config_dir)


# =============================================================================
# Test ConfigBundle Contents
# =============================================================================


class TestConfigBundleSamples:
    """Tests for samples in ConfigBundle."""

    def test_samples_loaded(self, config_bundle: ConfigBundle) -> None:
        assert config_bundle.samples is not None
        assert len(config_bundle.samples.samples) > 0

    def test_samples_have_required_fields(self, config_bundle: ConfigBundle) -> None:
        for sample in config_bundle.samples.samples:
            assert sample.tech_area
            assert sample.sample_id
            assert sample.inj_vol > 0
            assert sample.file_name_template

    def test_each_tech_area_has_default(self, config_bundle: ConfigBundle) -> None:
        technologies = {s.tech_area for s in config_bundle.samples.samples}
        for tech in technologies:
            default = config_bundle.samples.get_sample(tech, "default")
            assert default is not None, f"Technology '{tech}' missing default sample"


class TestConfigBundleInstruments:
    """Tests for instruments in ConfigBundle."""

    def test_instruments_loaded(self, config_bundle: ConfigBundle) -> None:
        assert config_bundle.instruments is not None
        assert len(config_bundle.instruments.instruments) > 0

    def test_instruments_have_methods_files(self, config_bundle: ConfigBundle) -> None:
        for instr in config_bundle.instruments.instruments:
            assert instr.methods_file.startswith("methods/")
            assert instr.methods_file.endswith("_methods.csv")

    def test_instruments_to_table(self, config_bundle: ConfigBundle) -> None:
        df = config_bundle.instruments.to_table()
        assert df is not None
        assert len(df) > 0


class TestConfigBundleQueuePatterns:
    """Tests for queue_patterns in ConfigBundle."""

    def test_queue_patterns_loaded(self, config_bundle: ConfigBundle) -> None:
        assert config_bundle.queue_patterns is not None

    def test_patterns_have_technologies(self, config_bundle: ConfigBundle) -> None:
        technologies = config_bundle.queue_patterns.get_technologies()
        assert len(technologies) > 0

    def test_get_pattern_returns_pattern(self, config_bundle: ConfigBundle) -> None:
        tech = config_bundle.queue_patterns.get_technologies()[0]
        pattern_name = list(config_bundle.queue_patterns.get_patterns_for_tech_area(tech).keys())[0]
        pattern = config_bundle.queue_patterns.get_pattern(tech, pattern_name)
        assert pattern is not None
        assert pattern.run_QC_after_n_samples > 0

    def test_get_pattern_returns_none_for_unknown(self, config_bundle: ConfigBundle) -> None:
        tech = config_bundle.queue_patterns.get_technologies()[0]
        assert config_bundle.queue_patterns.get_pattern("unknown_tech", "standard") is None
        assert config_bundle.queue_patterns.get_pattern(tech, "nonexistent") is None


class TestConfigBundleQCLayouts:
    """Tests for qc_layouts in ConfigBundle."""

    def test_qc_layouts_loaded(self, config_bundle: ConfigBundle) -> None:
        assert config_bundle.qc_layouts is not None

    def test_layouts_have_technologies(self, config_bundle: ConfigBundle) -> None:
        technologies = config_bundle.qc_layouts.get_technologies()
        assert len(technologies) > 0

    def test_get_layout_exact_match(self, config_bundle: ConfigBundle) -> None:
        tech = config_bundle.qc_layouts.get_technologies()[0]
        sampler = config_bundle.qc_layouts.get_samplers_for_tech_area(tech)[0]
        layout = config_bundle.qc_layouts.get_layout(tech, sampler)
        assert layout is not None
        assert len(layout) > 0


class TestConfigBundleOutputFormats:
    """Tests for output_formats in ConfigBundle."""

    def test_output_formats_loaded(self, config_bundle: ConfigBundle) -> None:
        assert config_bundle.output_formats is not None

    def test_formats_have_names(self, config_bundle: ConfigBundle) -> None:
        names = config_bundle.output_formats.get_format_names()
        assert len(names) > 0
        assert "xcalibur" in names

    def test_get_format_returns_format(self, config_bundle: ConfigBundle) -> None:
        fmt = config_bundle.output_formats.get_format("xcalibur")
        assert fmt is not None
        assert fmt.file_extension
        assert len(fmt.columns) > 0

    def test_get_format_returns_none_for_unknown(self, config_bundle: ConfigBundle) -> None:
        assert config_bundle.output_formats.get_format("nonexistent") is None


class TestConfigBundleSamplers:
    """Tests for samplers in ConfigBundle."""

    def test_samplers_loaded(self, config_bundle: ConfigBundle) -> None:
        assert config_bundle.samplers is not None

    def test_samplers_have_names(self, config_bundle: ConfigBundle) -> None:
        names = config_bundle.samplers.get_sampler_names()
        assert len(names) > 0
        assert "Vanquish" in names


class TestConfigBundleCombinations:
    """Tests for combinations in ConfigBundle."""

    def test_combinations_loaded(self, config_bundle: ConfigBundle) -> None:
        assert config_bundle.combinations is not None
        assert len(config_bundle.combinations.combinations) > 0

    def test_combinations_have_valid_samplers(self, config_bundle: ConfigBundle) -> None:
        for combo in config_bundle.combinations.combinations:
            assert "." in combo.sampler  # Should be Sampler.container format


class TestConfigBundleInstrumentPatterns:
    """Tests for instrument_patterns in ConfigBundle."""

    def test_instrument_patterns_loaded(self, config_bundle: ConfigBundle) -> None:
        assert config_bundle.instrument_patterns is not None
        assert len(config_bundle.instrument_patterns.patterns) > 0


# =============================================================================
# Test Validation
# =============================================================================


class TestQgConfigValidation:
    """Tests for validation during qg_config() loading.

    Note: Detailed validation output tests are in test_cli.py via subprocess.
    """

    def test_qg_config_validates_successfully(self) -> None:
        """qg_config() runs validation and returns bundle on success."""
        qg_config.cache_clear()
        config_dir = Path(__file__).parent.parent / "qg_configs"
        # Should not raise - validation passes
        bundle = qg_config(config_dir)
        assert bundle is not None
        assert bundle.samples is not None
        assert bundle.instruments is not None


# =============================================================================
# Test Cross-References
# =============================================================================


class TestCrossReferences:
    """Tests for cross-reference integrity between configs."""

    def test_queue_patterns_reference_valid_samples(self, config_bundle: ConfigBundle) -> None:
        """All sample_ids in queue_patterns should exist in samples.csv."""
        for tech in config_bundle.queue_patterns.get_technologies():
            pattern_refs = config_bundle.queue_patterns.get_all_sample_refs(tech)
            valid_ids = {s.sample_id for s in config_bundle.samples.get_by_tech_area(tech)}
            unknown = pattern_refs - valid_ids
            assert not unknown, f"Unknown samples in {tech} patterns: {unknown}"

    def test_qc_layouts_reference_valid_samples(self, config_bundle: ConfigBundle) -> None:
        """All sample_ids in qc_layouts should exist in samples.csv."""
        for tech in config_bundle.qc_layouts.get_technologies():
            valid_ids = {s.sample_id for s in config_bundle.samples.get_by_tech_area(tech)}
            for sampler_key in config_bundle.qc_layouts.get_samplers_for_tech_area(tech):
                layout = config_bundle.qc_layouts.get_layout(tech, sampler_key)
                if layout:
                    layout_ids = set(layout.keys())
                    unknown = layout_ids - valid_ids
                    assert not unknown, f"Unknown samples in {tech}.{sampler_key}: {unknown}"

    def test_instrument_patterns_reference_valid_instruments(self, config_bundle: ConfigBundle) -> None:
        """All instruments in instrument_patterns should exist in instruments.csv."""
        valid = {(i.tech_area, i.instrument) for i in config_bundle.instruments.instruments}
        pattern_refs = {
            (p.tech_area, p.instrument) for p in config_bundle.instrument_patterns.patterns
        }
        unknown = pattern_refs - valid
        assert not unknown, f"Unknown instruments in patterns: {unknown}"

    def test_combinations_reference_valid_instruments(self, config_bundle: ConfigBundle) -> None:
        """All instruments in combinations should exist in instruments.csv."""
        valid_names = {i.instrument for i in config_bundle.instruments.instruments}
        combo_names = {c.instrument for c in config_bundle.combinations.combinations}
        unknown = combo_names - valid_names
        assert not unknown, f"Unknown instruments in combinations: {unknown}"

    def test_combinations_reference_valid_output_formats(self, config_bundle: ConfigBundle) -> None:
        """All output_formats in combinations should exist in output_formats.toml."""
        valid_formats = set(config_bundle.output_formats.get_format_names())
        combo_formats = {c.output_format for c in config_bundle.combinations.combinations}
        unknown = combo_formats - valid_formats
        assert not unknown, f"Unknown output formats in combinations: {unknown}"


# =============================================================================
# Test QueueParameters.create() Factory
# =============================================================================


class TestQueueParametersCreate:
    """Tests for QueueParameters.create() factory method validation."""

    def test_create_with_valid_params(self, config_bundle: ConfigBundle) -> None:
        """Factory creates QueueParameters with valid config references."""
        from qg.params_models import QueueParameters

        params = QueueParameters.create(
            config_bundle,
            tech_area="Proteomics",
            instrument="ASTRAL_1",
            sampler="Vanquish.vial",
            output_format="xcalibur",
            queue_pattern="standard",
            polarity=["pos"],
            date="20260122",
            user="testuser",
        )
        assert params.tech_area == "Proteomics"
        assert params.instrument == "ASTRAL_1"

    def test_create_rejects_invalid_pattern(self, config_bundle: ConfigBundle) -> None:
        """Factory raises ValueError for non-existent pattern."""
        from qg.params_models import QueueParameters

        with pytest.raises(ValueError, match="Pattern 'nonexistent' not found"):
            QueueParameters.create(
                config_bundle,
                tech_area="Proteomics",
                instrument="ASTRAL_1",
                sampler="Vanquish.vial",
                output_format="xcalibur",
                queue_pattern="nonexistent",
                polarity=["pos"],
                date="20260122",
            )

    def test_create_rejects_invalid_sampler(self, config_bundle: ConfigBundle) -> None:
        """Factory raises ValueError for non-existent QC layout."""
        from qg.params_models import QueueParameters

        with pytest.raises(ValueError, match="QC layout not found"):
            QueueParameters.create(
                config_bundle,
                tech_area="Proteomics",
                instrument="ASTRAL_1",
                sampler="NonexistentSampler.vial",
                output_format="xcalibur",
                queue_pattern="standard",
                polarity=["pos"],
                date="20260122",
            )

    def test_create_rejects_invalid_output_format(self, config_bundle: ConfigBundle) -> None:
        """Factory raises ValueError for non-existent output format."""
        from qg.params_models import QueueParameters

        with pytest.raises(ValueError, match="Output format 'nonexistent' not found"):
            QueueParameters.create(
                config_bundle,
                tech_area="Proteomics",
                instrument="ASTRAL_1",
                sampler="Vanquish.vial",
                output_format="nonexistent",
                queue_pattern="standard",
                polarity=["pos"],
                date="20260122",
            )

    def test_create_rejects_invalid_instrument(self, config_bundle: ConfigBundle) -> None:
        """Factory raises ValueError for non-existent instrument."""
        from qg.params_models import QueueParameters

        with pytest.raises(ValueError, match="Instrument 'FAKE_INSTRUMENT' not found"):
            QueueParameters.create(
                config_bundle,
                tech_area="Proteomics",
                instrument="FAKE_INSTRUMENT",
                sampler="Vanquish.vial",
                output_format="xcalibur",
                queue_pattern="standard",
                polarity=["pos"],
                date="20260122",
            )
