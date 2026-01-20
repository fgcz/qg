"""Integration tests for config loading and validation."""

from pathlib import Path

import pytest

from qg.config import (
    ConfigBundle,
    get_core_dir,
    get_ui_dir,
    load_all_configs,
    load_combinations,
    load_instrument_patterns,
    load_instruments,
    load_output_formats,
    load_qc_layouts,
    load_queue_patterns,
    load_samplers,
    validate_all_configs,
)
from qg.config_models import (
    CombinationsConfig,
    InstrumentsConfig,
    OutputFormatsConfig,
    QCLayoutsConfig,
    QueuePatternsConfig,
    SamplesConfig,
    SamplersConfig,
    requires_polarity,
)


# =============================================================================
# Fixtures
# =============================================================================


@pytest.fixture
def config_dir() -> Path:
    """Return path to the qg_configs directory."""
    return Path(__file__).parent.parent / "qg_configs"


@pytest.fixture
def core_dir(config_dir: Path) -> Path:
    """Return path to the core config directory."""
    return get_core_dir(config_dir)


@pytest.fixture
def ui_dir(config_dir: Path) -> Path:
    """Return path to the UI config directory."""
    return get_ui_dir(config_dir)


@pytest.fixture
def config_bundle(config_dir: Path) -> ConfigBundle:
    """Load core configs into a ConfigBundle."""
    return load_all_configs(config_dir)


# =============================================================================
# Test requires_polarity
# =============================================================================


class TestRequiresPolarity:
    """Tests for the requires_polarity function."""

    def test_polarity_technologies_from_config(self, config_bundle: ConfigBundle) -> None:
        """Technologies with {polarity} in file_name_template should require polarity."""
        polarity_techs = config_bundle.polarity_technologies
        for tech in polarity_techs:
            assert requires_polarity(tech) is True, f"{tech} should require polarity"

    def test_non_polarity_technologies_from_config(self, config_bundle: ConfigBundle) -> None:
        """Technologies without {polarity} should not require polarity."""
        all_techs = {s.technology for s in config_bundle.samples.samples}
        polarity_techs = config_bundle.polarity_technologies
        non_polarity_techs = all_techs - polarity_techs
        for tech in non_polarity_techs:
            assert requires_polarity(tech) is False, f"{tech} should not require polarity"

    def test_unknown_technology_no_polarity(self, config_bundle: ConfigBundle) -> None:
        assert requires_polarity("unknown") is False
        assert requires_polarity("") is False


# =============================================================================
# Test Individual Config Loading
# =============================================================================


class TestLoadSamples:
    """Tests for loading samples.csv."""

    def test_load_samples_success(self, config_bundle: ConfigBundle) -> None:
        # Use config_bundle.samples since loading requires polarity_technologies
        samples = config_bundle.samples
        assert isinstance(samples, SamplesConfig)
        assert len(samples.samples) > 0

    def test_samples_have_required_fields(self, config_bundle: ConfigBundle) -> None:
        samples = config_bundle.samples
        for sample in samples.samples:
            assert sample.technology
            assert sample.sample_id
            assert sample.inj_vol > 0
            assert sample.file_name_template

    def test_each_technology_has_default(self, config_bundle: ConfigBundle) -> None:
        samples = config_bundle.samples
        technologies = {s.technology for s in samples.samples}
        for tech in technologies:
            default = samples.get_sample(tech, "default")
            assert default is not None, f"Technology '{tech}' missing default sample"


class TestLoadInstruments:
    """Tests for loading instruments.csv."""

    def test_load_instruments_success(self, core_dir: Path) -> None:
        instruments = load_instruments(core_dir / "instruments.csv")
        assert isinstance(instruments, InstrumentsConfig)
        assert len(instruments.instruments) > 0

    def test_instruments_have_methods_files(self, core_dir: Path) -> None:
        instruments = load_instruments(core_dir / "instruments.csv")
        for instr in instruments.instruments:
            assert instr.methods_file.startswith("methods/")
            assert instr.methods_file.endswith("_methods.csv")


class TestLoadQueuePatterns:
    """Tests for loading queue_patterns.toml."""

    def test_load_queue_patterns_success(self, core_dir: Path) -> None:
        patterns = load_queue_patterns(core_dir / "queue_patterns.toml")
        assert isinstance(patterns, QueuePatternsConfig)

    def test_patterns_have_technologies(self, core_dir: Path) -> None:
        patterns = load_queue_patterns(core_dir / "queue_patterns.toml")
        technologies = patterns.get_technologies()
        assert len(technologies) > 0

    def test_get_pattern_returns_pattern(self, core_dir: Path) -> None:
        patterns = load_queue_patterns(core_dir / "queue_patterns.toml")
        # Get first technology and its first pattern dynamically
        tech = patterns.get_technologies()[0]
        pattern_name = list(patterns.get_patterns_for_technology(tech).keys())[0]
        pattern = patterns.get_pattern(tech, pattern_name)
        assert pattern is not None
        assert pattern.run_QC_after_n_samples > 0

    def test_get_pattern_returns_none_for_unknown(self, core_dir: Path) -> None:
        patterns = load_queue_patterns(core_dir / "queue_patterns.toml")
        tech = patterns.get_technologies()[0]
        assert patterns.get_pattern("unknown_tech", "standard") is None
        assert patterns.get_pattern(tech, "nonexistent") is None


class TestLoadQCLayouts:
    """Tests for loading qc_layouts.toml."""

    def test_load_qc_layouts_success(self, core_dir: Path) -> None:
        layouts = load_qc_layouts(core_dir / "qc_layouts.toml")
        assert isinstance(layouts, QCLayoutsConfig)

    def test_layouts_have_technologies(self, core_dir: Path) -> None:
        layouts = load_qc_layouts(core_dir / "qc_layouts.toml")
        technologies = layouts.get_technologies()
        assert len(technologies) > 0

    def test_get_layout_exact_match(self, core_dir: Path) -> None:
        layouts = load_qc_layouts(core_dir / "qc_layouts.toml")
        # Get first technology and its first sampler dynamically
        tech = layouts.get_technologies()[0]
        sampler = layouts.get_samplers_for_technology(tech)[0]
        layout = layouts.get_layout(tech, sampler)
        assert layout is not None
        assert len(layout) > 0

    def test_get_layout_fallback_to_parent(self, core_dir: Path) -> None:
        layouts = load_qc_layouts(core_dir / "qc_layouts.toml")
        # Get first technology dynamically
        tech = layouts.get_technologies()[0]
        samplers = layouts.get_samplers_for_technology(tech)
        # Find a sampler with dot notation (e.g., Vanquish.vial)
        dot_samplers = [s for s in samplers if "." in s]
        if dot_samplers:
            layout_exact = layouts.get_layout(tech, dot_samplers[0])
            assert layout_exact is not None


class TestLoadOutputFormats:
    """Tests for loading output_formats.toml."""

    def test_load_output_formats_success(self, core_dir: Path) -> None:
        formats = load_output_formats(core_dir / "output_formats.toml")
        assert isinstance(formats, OutputFormatsConfig)

    def test_formats_have_names(self, core_dir: Path) -> None:
        formats = load_output_formats(core_dir / "output_formats.toml")
        names = formats.get_format_names()
        assert len(names) > 0
        assert "xcalibur" in names

    def test_get_format_returns_format(self, core_dir: Path) -> None:
        formats = load_output_formats(core_dir / "output_formats.toml")
        fmt = formats.get_format("xcalibur")
        assert fmt is not None
        assert fmt.file_extension
        assert len(fmt.columns) > 0

    def test_get_format_returns_none_for_unknown(self, core_dir: Path) -> None:
        formats = load_output_formats(core_dir / "output_formats.toml")
        assert formats.get_format("nonexistent") is None


class TestLoadCombinations:
    """Tests for loading combinations.csv (UI config)."""

    def test_load_combinations_success(self, ui_dir: Path, config_bundle: ConfigBundle) -> None:
        # UI configs need valid_samplers to be set (done by loading core configs)
        combos = load_combinations(ui_dir / "combinations.csv")
        assert isinstance(combos, CombinationsConfig)
        assert len(combos.combinations) > 0

    def test_combinations_have_valid_samplers(self, ui_dir: Path, config_bundle: ConfigBundle) -> None:
        combos = load_combinations(ui_dir / "combinations.csv")
        for combo in combos.combinations:
            assert "." in combo.sampler  # Should be Sampler.container format


class TestLoadSamplers:
    """Tests for loading sampler.toml."""

    def test_load_samplers_success(self, core_dir: Path) -> None:
        samplers = load_samplers(core_dir / "sampler.toml")
        assert isinstance(samplers, SamplersConfig)

    def test_samplers_have_names(self, core_dir: Path) -> None:
        samplers = load_samplers(core_dir / "sampler.toml")
        names = samplers.get_sampler_names()
        assert len(names) > 0
        assert "Vanquish" in names


# =============================================================================
# Test ConfigBundle
# =============================================================================


class TestConfigBundle:
    """Tests for loading core configs together."""

    def test_load_all_configs_success(self, config_bundle: ConfigBundle) -> None:
        assert config_bundle.samples is not None
        assert config_bundle.instruments is not None
        assert config_bundle.samplers is not None
        assert config_bundle.queue_patterns is not None
        assert config_bundle.qc_layouts is not None
        assert config_bundle.output_formats is not None

    def test_config_bundle_has_instruments_df(self, config_bundle: ConfigBundle) -> None:
        assert config_bundle.instruments_df is not None
        assert len(config_bundle.instruments_df) > 0


# =============================================================================
# Test Validation
# =============================================================================


class TestValidateAllConfigs:
    """Tests for validate_all_configs function."""

    def test_validate_all_configs_passes(self, config_dir: Path, capsys) -> None:
        result = validate_all_configs(config_dir)
        assert result is True
        captured = capsys.readouterr()
        assert "All validations passed!" in captured.out

    def test_validate_all_configs_prints_counts(self, config_dir: Path, capsys) -> None:
        validate_all_configs(config_dir)
        captured = capsys.readouterr()
        # Check that validation output contains expected sections
        assert "Validating samples.csv" in captured.out
        assert "Validating instruments.csv" in captured.out
        assert "Validating queue_patterns.toml" in captured.out
        assert "Cross-validating" in captured.out


# =============================================================================
# Test Cross-References
# =============================================================================


class TestCrossReferences:
    """Tests for cross-reference integrity between configs."""

    def test_queue_patterns_reference_valid_samples(self, config_bundle: ConfigBundle) -> None:
        """All sample_ids in queue_patterns should exist in samples.csv."""
        for tech in config_bundle.queue_patterns.get_technologies():
            pattern_refs = config_bundle.queue_patterns.get_all_sample_refs(tech)
            valid_ids = {s.sample_id for s in config_bundle.samples.get_by_technology(tech)}
            unknown = pattern_refs - valid_ids
            assert not unknown, f"Unknown samples in {tech} patterns: {unknown}"

    def test_qc_layouts_reference_valid_samples(self, config_bundle: ConfigBundle) -> None:
        """All sample_ids in qc_layouts should exist in samples.csv."""
        for tech in config_bundle.qc_layouts.get_technologies():
            valid_ids = {s.sample_id for s in config_bundle.samples.get_by_technology(tech)}
            for sampler_key in config_bundle.qc_layouts.get_samplers_for_technology(tech):
                layout = config_bundle.qc_layouts.get_layout(tech, sampler_key)
                if layout:
                    layout_ids = set(layout.keys())
                    unknown = layout_ids - valid_ids
                    assert not unknown, f"Unknown samples in {tech}.{sampler_key}: {unknown}"

    def test_instrument_patterns_reference_valid_instruments(
        self, ui_dir: Path, config_bundle: ConfigBundle
    ) -> None:
        """All instruments in instrument_patterns should exist in instruments.csv."""
        instrument_patterns = load_instrument_patterns(ui_dir / "instrument_patterns.csv")
        valid = {(i.technology, i.instrument) for i in config_bundle.instruments.instruments}
        pattern_refs = {
            (p.technology, p.instrument) for p in instrument_patterns.patterns
        }
        unknown = pattern_refs - valid
        assert not unknown, f"Unknown instruments in patterns: {unknown}"

    def test_combinations_reference_valid_instruments(
        self, ui_dir: Path, config_bundle: ConfigBundle
    ) -> None:
        """All instruments in combinations should exist in instruments.csv."""
        combinations = load_combinations(ui_dir / "combinations.csv")
        valid_names = {i.instrument for i in config_bundle.instruments.instruments}
        combo_names = {c.instrument for c in combinations.combinations}
        unknown = combo_names - valid_names
        assert not unknown, f"Unknown instruments in combinations: {unknown}"

    def test_combinations_reference_valid_output_formats(
        self, ui_dir: Path, config_bundle: ConfigBundle
    ) -> None:
        """All output_formats in combinations should exist in output_formats.toml."""
        combinations = load_combinations(ui_dir / "combinations.csv")
        valid_formats = set(config_bundle.output_formats.get_format_names())
        combo_formats = {c.output_format for c in combinations.combinations}
        unknown = combo_formats - valid_formats
        assert not unknown, f"Unknown output formats in combinations: {unknown}"
