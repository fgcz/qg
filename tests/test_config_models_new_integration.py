"""Integration tests for new config models - loading real configs from qg_configs_new."""

import tomllib
from pathlib import Path

import polars as pl

# Config root
CONFIG_ROOT = Path(__file__).parent.parent / "qg_configs_new"


# =============================================================================
# Test config_model_formatting_new
# =============================================================================


class TestFormattingConfigs:
    """Test loading formatting configs."""

    def test_load_instruments(self):
        """Load instruments.csv into InstrumentsConfig."""
        from qg.config_model_formatting_new import InstrumentsConfig

        path = CONFIG_ROOT / "core" / "formatting" / "instruments.csv"
        df = pl.read_csv(path)
        config = InstrumentsConfig.from_table(df)

        assert len(config.instruments) > 0
        # Check a known instrument exists
        instr = config.get_instrument("Proteomics", "ASTRAL_1")
        assert instr is not None
        assert instr.methods_file.startswith("methods/")

    def test_load_samples(self):
        """Load samples.csv into SamplesConfig."""
        from qg.config_model_formatting_new import SamplesConfig

        path = CONFIG_ROOT / "core" / "formatting" / "samples.csv"
        df = pl.read_csv(path)
        config = SamplesConfig.from_table(df)

        assert len(config.samples) > 0
        # Check default sample exists for each tech_area
        for tech in ["Proteomics", "Metabolomics", "Lipidomics"]:
            sample = config.get_sample(tech, "default")
            assert sample is not None, f"Missing default sample for {tech}"

    def test_load_output_formats(self):
        """Load output_formats.toml into OutputFormatsConfig."""
        from qg.config_model_formatting_new import OutputFormatsConfig

        path = CONFIG_ROOT / "core" / "formatting" / "output_formats.toml"
        with open(path, "rb") as f:
            data = tomllib.load(f)
        config = OutputFormatsConfig.from_dict(data)

        assert len(config.formats) > 0
        # Check known formats exist
        for fmt in ["xcalibur", "chronos", "hystar"]:
            assert config.get_format(fmt) is not None, f"Missing format {fmt}"


# =============================================================================
# Test config_model_structure_new
# =============================================================================


class TestStructureConfigs:
    """Test loading structure configs."""

    def test_load_queue_patterns(self):
        """Load queue_patterns.toml into QueuePatternsConfig."""
        from qg.config_model_structure_new import QueuePatternsConfig

        path = CONFIG_ROOT / "core" / "structure" / "queue_patterns.toml"
        with open(path, "rb") as f:
            data = tomllib.load(f)
        config = QueuePatternsConfig.from_dict(data)

        # Check technologies exist
        techs = config.get_technologies()
        assert "Proteomics" in techs
        assert "Metabolomics" in techs
        assert "Lipidomics" in techs

        # Check standard pattern exists
        pattern = config.get_pattern("Proteomics", "standard")
        assert pattern is not None
        assert pattern.qc_layout_name == "standard"
        assert pattern.run_QC_after_n_samples > 0


# =============================================================================
# Test config_model_position_new
# =============================================================================


class TestPositionConfigs:
    """Test loading position configs."""

    def test_load_plate_layouts(self):
        """Load plate_layouts.toml into PlateLayoutsConfig."""
        from qg.config_model_position_new import PlateLayoutsConfig

        path = CONFIG_ROOT / "core" / "position" / "plate_layouts.toml"
        with open(path, "rb") as f:
            data = tomllib.load(f)
        config = PlateLayoutsConfig.from_dict(data)

        # Check known layouts exist
        for name in ["Vanquish_54", "Vanquish_96", "MClass_48", "Evosep_96"]:
            layout = config.get_layout(name)
            assert layout is not None, f"Missing layout {name}"
            assert layout.capacity > 0

    def test_load_samplers(self):
        """Load sampler.toml into SamplersConfig."""
        from qg.config_model_position_new import SamplersConfig

        path = CONFIG_ROOT / "core" / "position" / "sampler.toml"
        with open(path, "rb") as f:
            data = tomllib.load(f)
        config = SamplersConfig.from_dict(data)

        # Check known samplers exist
        for name in ["Vanquish", "MClass", "Evosep"]:
            sampler = config.get_sampler(name)
            assert sampler is not None, f"Missing sampler {name}"
            assert len(sampler.trays) > 0
            assert sampler.position_fun in ["string_concat", "int_add"]

    def test_load_sampler_plate_layouts(self):
        """Load sampler_plate_layouts.csv into SamplerPlateLayoutsConfig."""
        from qg.config_model_position_new import SamplerPlateLayoutsConfig

        path = CONFIG_ROOT / "core" / "position" / "sampler_plate_layouts.csv"
        df = pl.read_csv(path)
        config = SamplerPlateLayoutsConfig.from_table(df)

        assert len(config.mappings) > 0
        # Check Vanquish has both Vial and Plate
        queue_types = config.get_queue_types_for_sampler("Vanquish")
        assert "Vial" in queue_types
        assert "Plate" in queue_types

    def test_load_qc_layouts_grid(self):
        """Load qc_layouts_grid.csv into QCLayoutsGridConfig."""
        from qg.config_model_position_new import QCLayoutsGridConfig

        path = CONFIG_ROOT / "core" / "position" / "qc_layouts_grid.csv"
        df = pl.read_csv(path, comment_prefix="#")
        config = QCLayoutsGridConfig.from_table(df)

        assert len(config.samples) > 0
        # Check Proteomics standard layout has QC01
        samples = config.get_samples("Proteomics", "standard", "Vanquish_54")
        sample_ids = {s.sample_id for s in samples}
        assert "QC01" in sample_ids

    def test_load_qc_layouts_evosep(self):
        """Load qc_layouts_evosep.csv into QCLayoutsEvosepConfig."""
        from qg.config_model_position_new import QCLayoutsEvosepConfig

        path = CONFIG_ROOT / "core" / "position" / "qc_layouts_evosep.csv"
        df = pl.read_csv(path, comment_prefix="#")
        config = QCLayoutsEvosepConfig.from_table(df)

        assert len(config.samples) > 0
        # Check Proteomics Evosep layout exists
        samples = config.get_samples("Proteomics", "standard", "Evosep_96")
        assert len(samples) > 0


# =============================================================================
# Test config_models_methods_new
# =============================================================================


class TestMethodsConfigs:
    """Test loading methods configs."""

    def test_load_single_methods_file(self):
        """Load a single methods CSV into MethodsForInstrument."""
        from qg.config_models_methods_new import MethodsForInstrument

        path = CONFIG_ROOT / "core" / "methods" / "proteomics" / "ASTRAL_1_methods.csv"
        df = pl.read_csv(path)
        methods = MethodsForInstrument.from_table(df)

        assert len(methods.methods) > 0
        # Check default method exists
        method = methods.get_method("default", "pos")
        assert method is not None

    def test_load_all_methods(self):
        """Load all methods into MethodsConfig."""
        from qg.config_models_methods_new import MethodsConfig

        methods_root = CONFIG_ROOT / "core" / "methods"
        tables: dict[tuple[str, str], pl.DataFrame] = {}

        for tech_dir in methods_root.iterdir():
            if not tech_dir.is_dir():
                continue
            tech_area = tech_dir.name.capitalize()  # proteomics -> Proteomics
            for csv_file in tech_dir.glob("*_methods.csv"):
                instrument = csv_file.stem.replace("_methods", "")
                df = pl.read_csv(csv_file)
                tables[(tech_area, instrument)] = df

        config = MethodsConfig.from_tables(tables)

        # Check some methods loaded
        astral_methods = config.get_methods("Proteomics", "ASTRAL_1")
        assert astral_methods is not None
        assert len(astral_methods.methods) > 0


# =============================================================================
# Test config_model_ui_new
# =============================================================================


class TestUIConfigs:
    """Test loading UI configs."""

    def test_load_instrument_config(self):
        """Load instrument_config.csv into InstrumentConfigsConfig."""
        from qg.config_model_ui_new import InstrumentConfigsConfig

        path = CONFIG_ROOT / "ui" / "instrument_config.csv"
        df = pl.read_csv(path)
        config = InstrumentConfigsConfig.from_table(df)

        assert len(config.configs) > 0

        # Check instruments by tech_area
        proteomics_instruments = config.get_instruments("Proteomics")
        assert "ASTRAL_1" in proteomics_instruments

        # Check samplers for instrument
        samplers = config.get_samplers_for_instrument("ASTRAL_1")
        assert len(samplers) > 0

        # Check specific config
        cfg = config.get_config("ASTRAL_1", "Vanquish")
        assert cfg is not None
        assert cfg.output_format == "xcalibur"
        assert cfg.default_pattern == "standard"
