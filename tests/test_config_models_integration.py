"""Integration tests for config models - loading real configs."""

import tomllib
from pathlib import Path

import polars as pl

# Config root
CONFIG_ROOT = Path(__file__).parent.parent / "qg_configs"


# =============================================================================
# Test formatting configs
# =============================================================================


class TestFormattingConfigs:
    """Test loading formatting configs."""

    def test_load_instruments(self):
        """Load instruments.csv into InstrumentsConfig."""
        from qg.config_models.formatting import InstrumentsConfig

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
        from qg.config_models.structure import SamplesConfig

        path = CONFIG_ROOT / "core" / "structure" / "samples.csv"
        df = pl.read_csv(path)
        config = SamplesConfig.from_table(df)

        assert len(config.samples) > 0
        # Check default sample exists for each tech_area
        for tech in ["Proteomics", "Metabolomics", "Lipidomics"]:
            sample = config.get_sample(tech, "default")
            assert sample is not None, f"Missing default sample for {tech}"

    def test_sample_type_and_levels_round_trip(self):
        """`sample_type` and `levels` columns load from CSV with expected defaults."""
        from qg.config_models.structure import SamplesConfig

        path = CONFIG_ROOT / "core" / "structure" / "samples.csv"
        df = pl.read_csv(path)
        config = SamplesConfig.from_table(df)

        # Every default sample is `unknown` with no level.
        for tech in ("Proteomics", "Metabolomics", "Lipidomics", "Testing"):
            default = config.get_sample(tech, "default")
            assert default.sample_type == "unknown"
            assert default.level is None

        # Spot-check categorisations
        assert config.get_sample("Proteomics", "QC01").sample_type == "qc"
        assert config.get_sample("Proteomics", "clean").sample_type == "blank"
        assert config.get_sample("Metabolomics", "blank").sample_type == "blank"
        assert config.get_sample("Metabolomics", "pooledQCDil1").sample_type == "qc"
        assert config.get_sample("Lipidomics", "EquiSPLASH").sample_type == "qc"
        assert config.get_sample("Testing", "clean").sample_type == "blank"

    def test_load_output_formats(self):
        """Load output_formats.toml into OutputFormatsConfig."""
        from qg.config_models.formatting import OutputFormatsConfig

        path = CONFIG_ROOT / "core" / "formatting" / "output_formats.toml"
        with open(path, "rb") as f:
            data = tomllib.load(f)
        config = OutputFormatsConfig.from_dict(data)

        assert len(config.formats) > 0
        # Check known formats exist
        for fmt in ["xcalibur", "chronos", "hystar"]:
            assert config.get_format(fmt) is not None, f"Missing format {fmt}"

    def test_xcalibur_sii_has_metabolomics_overlay(self):
        """xcalibur_sii defines a Metabolomics column overlay that adds Sample Type / Levels."""
        from qg.config_models.formatting import OutputFormatsConfig

        path = CONFIG_ROOT / "core" / "formatting" / "output_formats.toml"
        with open(path, "rb") as f:
            data = tomllib.load(f)
        config = OutputFormatsConfig.from_dict(data)

        fmt = config.get_format("xcalibur_sii")
        assert "Metabolomics" in fmt.columns_by_tech
        # Overlay must NOT pollute the base.
        assert "Sample Type" not in fmt.columns
        assert "Level" not in fmt.columns
        # Merged Metabolomics view contains both base and overlay columns.
        meta_cols = fmt.columns_for("Metabolomics")
        assert meta_cols["File Name"] == "file_name"
        assert meta_cols["Sample Type"] == "sample_type"
        assert meta_cols["Level"] == "level"
        # Other technologies fall through to the base.
        assert fmt.columns_for("Proteomics") == fmt.columns

    def test_output_formats_dict_roundtrip_preserves_per_tech_overlay(self):
        """to_dict -> from_dict must preserve `columns.<TechArea>` overlays.

        Regression: the config editor displays `tomli_w.dumps(cfg.to_dict())`
        and re-parses on save. A previous to_dict emitted `columns_by_tech` as
        a sibling of `columns`, which from_dict ignores -- so saving via the
        editor silently dropped every per-tech overlay.
        """
        from qg.config_models.formatting import OutputFormatsConfig

        path = CONFIG_ROOT / "core" / "formatting" / "output_formats.toml"
        with open(path, "rb") as f:
            data = tomllib.load(f)

        loaded = OutputFormatsConfig.from_dict(data)
        roundtripped = OutputFormatsConfig.from_dict(loaded.to_dict())

        original = loaded.get_format("xcalibur_sii")
        again = roundtripped.get_format("xcalibur_sii")
        assert again.columns == original.columns
        assert again.columns_by_tech == original.columns_by_tech
        assert again.columns_for("Metabolomics") == original.columns_for("Metabolomics")


# =============================================================================
# Test structure configs
# =============================================================================


class TestStructureConfigs:
    """Test loading structure configs."""

    def test_load_queue_patterns(self):
        """Load queue_patterns.toml into QueuePatternsConfig."""
        from qg.config_models.structure import QueuePatternsConfig

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
        assert pattern.run_QC_after_n_samples > 0


# =============================================================================
# Test position configs
# =============================================================================


class TestPositionConfigs:
    """Test loading position configs."""

    def test_load_plate_layouts(self):
        """Load plate_layouts.toml into PlateLayoutsConfig."""
        from qg.config_models.positions import PlateLayoutsConfig

        path = CONFIG_ROOT / "core" / "position" / "plate_layouts.toml"
        with open(path, "rb") as f:
            data = tomllib.load(f)
        config = PlateLayoutsConfig.from_dict(data)

        # Check known layouts exist
        for name in ["Vanquish_54", "Plate_96", "MClass_48"]:
            layout = config.get_layout(name)
            assert layout is not None, f"Missing layout {name}"
            assert layout.capacity > 0

    def test_load_samplers(self):
        """Load sampler.toml into SamplersConfig."""
        from qg.config_models.positions import SamplersConfig

        path = CONFIG_ROOT / "core" / "position" / "sampler.toml"
        with open(path, "rb") as f:
            data = tomllib.load(f)
        config = SamplersConfig.from_dict(data)

        # Check known samplers exist
        for name in ["Vanquish", "MClass", "Evosep"]:
            sampler = config.get_sampler(name)
            assert sampler is not None, f"Missing sampler {name}"
            assert len(sampler.trays) > 0
            assert sampler.position_fun == "string_concat"

    def test_load_sampler_plate_layouts(self):
        """Load sampler_plate_layouts.csv into SamplerPlateLayoutsConfig."""
        from qg.config_models.positions import SamplerPlateLayoutsConfig

        path = CONFIG_ROOT / "core" / "position" / "sampler_plate_layouts.csv"
        df = pl.read_csv(path)
        config = SamplerPlateLayoutsConfig.from_table(df)

        assert len(config.mappings) > 0
        # Check Vanquish has both Vial and Plate
        queue_types = config.get_queue_types_for_sampler("Vanquish")
        assert "Vial" in queue_types
        assert "Plate" in queue_types

    def test_load_qc_layouts_well(self):
        """Load qc_layouts_well.csv into QCLayoutsWellConfig."""
        from qg.config_models.positions import QCLayoutsWellConfig

        path = CONFIG_ROOT / "core" / "position" / "qc_layouts_well.csv"
        df = pl.read_csv(path, comment_prefix="#")
        config = QCLayoutsWellConfig.from_table(df)

        assert len(config.samples) > 0
        # Check Proteomics standard layout has QC01
        samples = config.get_samples("Proteomics", "standard", "Vanquish_54")
        sample_ids = {s.sample_id for s in samples}
        assert "QC01" in sample_ids

    def test_noqc_layout_available_for_metabolomics_and_lipidomics(self):
        """`noqc` is a selectable QC layout for Metabolomics and Lipidomics; absent for Proteomics."""
        from qg.config_models.positions import QCLayoutsWellConfig

        path = CONFIG_ROOT / "core" / "position" / "qc_layouts_well.csv"
        df = pl.read_csv(path, comment_prefix="#")
        config = QCLayoutsWellConfig.from_table(df)

        for tech in ("Metabolomics", "Lipidomics"):
            for plate_layout in ("Vanquish_54", "Plate_96"):
                assert "noqc" in config.get_layout_names(tech, plate_layout), (
                    f"noqc should be selectable for {tech}/{plate_layout}"
                )
                # Empty layout: no real sample positions are exposed.
                assert config.get_sample_ids(tech, "noqc", plate_layout) == set()

        # Proteomics is intentionally excluded from the noqc option.
        for plate_layout in ("Vanquish_54", "Plate_96"):
            assert "noqc" not in config.get_layout_names("Proteomics", plate_layout)

    def test_load_qc_layouts_tip(self):
        """Load qc_layouts_tip.csv into QCLayoutsTipConfig."""
        from qg.config_models.positions import QCLayoutsTipConfig

        path = CONFIG_ROOT / "core" / "position" / "qc_layouts_tip.csv"
        df = pl.read_csv(path, comment_prefix="#")
        config = QCLayoutsTipConfig.from_table(df)

        assert len(config.samples) > 0
        # Check Testing standard tip layout exists
        samples = config.get_samples("Testing", "standard", "Plate_96")
        assert len(samples) > 0


# =============================================================================
# Test methods configs
# =============================================================================


class TestMethodsConfigs:
    """Test loading methods configs."""

    def test_load_single_methods_file(self):
        """Load a single methods CSV into MethodsForInstrument."""
        from qg.config_models.methods import MethodsForInstrument

        path = CONFIG_ROOT / "core" / "methods" / "Proteomics" / "ASTRAL_1_methods.csv"
        df = pl.read_csv(path)
        config_path = Path("core/methods/Proteomics/ASTRAL_1_methods.csv")
        methods = MethodsForInstrument.from_table(df, config_path=config_path)

        assert len(methods.methods) > 0
        # Check default method exists
        method = methods.get_method("default", "pos")
        assert method is not None

    def test_load_all_methods(self):
        """Load all methods into MethodsConfig."""
        from qg.config_models.methods import MethodsConfig

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
# Test UI configs
# =============================================================================


class TestUIConfigs:
    """Test loading UI configs."""

    def test_load_instrument_config(self):
        """Load instrument_config.csv into InstrumentConfigsConfig."""
        from qg.config_models.ui import InstrumentConfigsConfig

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
        assert cfg.output_format == "xcalibur_sii"
        assert cfg.default_pattern == "standard"
