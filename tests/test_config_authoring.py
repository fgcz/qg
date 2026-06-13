"""End-to-end tests that build a brand-new technology config from scratch.

These tests author a complete, minimal ``Testing_v3`` configuration tree in a
temp directory (no committed ``qg_configs/`` rows are involved) and prove it is
loadable, valid, and usable for queue generation through both the Python API and
the ``qg`` CLI. They double as the executable mirror of the
``qg-config-authoring`` skill: the file contents below are exactly what the skill
teaches an author to write.
"""

import subprocess
from pathlib import Path

import pytest

from qg.config_models.loader import qg_configuration
from qg.generator import QueueGenerator

from .helpers import make_queue_input

# A complete, minimal Testing_v3 configuration: one technology, one instrument
# (TESTING_V3_1), the reused Vanquish well sampler, one output format
# (xcalibur), one new 12-well plate layout (TestingV3_12), three sample types,
# and one QC pattern. Every QC sample the pattern references (QCv3) has a
# reserved position in the QC layout — the consistency rule the loader enforces.
TESTING_V3_FILES: dict[str, str] = {
    "core/formatting/instruments.csv": (
        "tech_area,instrument,methods_file,path_template\n"
        r"Testing_v3,TESTING_V3_1,methods/Testing_v3/TESTING_V3_1_methods.csv,"
        r"D:\Data2San\p{container}\Testing_v3\TESTING_V3_1\{user}_{date}"
        "\n"
    ),
    "core/formatting/output_formats.toml": (
        "[xcalibur]\n"
        'description = "Thermo XCalibur format"\n'
        'file_extension = ".csv"\n'
        'writer = "xcalibur_csv"\n'
        'position_format = "{tray}:{grid_position}"\n'
        'grid_position_format = "{row},{col}"\n'
        'grid_position_conversion = "identity"\n'
        "\n"
        "[xcalibur.columns]\n"
        '"File Name" = "file_name"\n'
        'Path = "data_path"\n'
        'Position = "position"\n'
        '"Inj Vol" = "inj_vol"\n'
        '"L3 Laboratory" = "literal:FGCZ"\n'
        '"Sample ID" = "sample_id"\n'
        '"Sample Name" = "sample_name"\n'
        '"Instrument Method" = "method"\n'
    ),
    "core/position/plate_layouts.toml": ('[TestingV3_12]\nrows = ["A", "B"]\ncols = [1, 2, 3, 4, 5, 6]\n'),
    "core/position/sampler.toml": (
        "[Vanquish]\n"
        'description = "Thermo Vanquish autosampler"\n'
        'sampler_type = "well"\n'
        'trays = ["Y", "R", "G", "B"]\n'
        'position_fun = "string_concat"\n'
    ),
    "core/position/sampler_plate_layouts.csv": ("sampler,plate_layout,queue_type\nVanquish,TestingV3_12,Vial\n"),
    "core/position/qc_layouts_well.csv": (
        "tech_area,qc_layout_name,plate_layout,sample_id,tray,row,col\nTesting_v3,v3std,TestingV3_12,QCv3,B,B,6\n"
    ),
    # Vanquish is a well sampler, so no tip layouts — but the loader reads this
    # file unconditionally, so it must exist (header only is valid).
    "core/position/qc_layouts_tip.csv": (
        "tech_area,qc_layout_name,plate_layout,sample_id,tray,position_start,position_end\n"
    ),
    "core/structure/samples.csv": (
        "tech_area,sample_id,sample_name,sample_type,level,description,inj_vol,file_name_template\n"
        'Testing_v3,default,"",Unknown,,Default Testing_v3 user sample,2.0,'
        "{date}_{run}_C{container}_S{sample_id}_{sample_name}\n"
        "Testing_v3,QCv3,QCv3,QC,,Testing_v3 QC sample,2.0,"
        "{date}_{run}_C{container}_{sample_name}\n"
        "Testing_v3,cleanv3,cleanv3,Blank,,Testing_v3 column cleanup blank,2.0,"
        "{date}_{run}_C{container}_{sample_name}\n"
    ),
    "core/structure/queue_patterns.toml": (
        "[Testing_v3.simple]\n"
        'description = "Testing_v3 simple pattern: QC at start and end"\n'
        "run_QC_after_n_samples = 4\n"
        'start = ["QCv3"]\n'
        "middle = []\n"
        'end = ["QCv3"]\n'
    ),
    "core/methods/Testing_v3/TESTING_V3_1_methods.csv": (
        "sample_type,polarity,method_name,method_path\n"
        "default,pos,DDA,C:\\Xcalibur\\methods\\\n"
        "QCv3,pos,DDA,C:\\Xcalibur\\methods\\__autoQC\\QCv3\n"
    ),
    "ui/instrument_config.csv": (
        "tech_area,instrument,sampler,output_format,default_pattern\nTesting_v3,TESTING_V3_1,Vanquish,xcalibur,simple\n"
    ),
    "ui/tech_area_defaults.toml": (
        '[Testing_v3]\ndefault_user = ""\ndefault_polarities = ["pos"]\nbfabric_areas = []\n'
    ),
}


def write_testing_v3_config(root: Path) -> Path:
    """Write a complete, minimal Testing_v3 config tree under ``root``.

    Returns the config directory (``root``), suitable for ``qg_configuration``.
    """
    for rel_path, content in TESTING_V3_FILES.items():
        path = root / rel_path
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content)
    return root


def make_testing_v3_input(num_samples: int = 5):
    """Build a VialQueueInput targeting the from-scratch Testing_v3 config."""
    return make_queue_input(
        num_samples=num_samples,
        tech_area="Testing_v3",
        instrument="TESTING_V3_1",
        sampler="Vanquish",
        queue_pattern="simple",
        queue_type="Vial",
        plate_layout="TestingV3_12",
        qc_layout_name="v3std",
        output_format="xcalibur",
    )


@pytest.fixture
def v3_config_dir(tmp_path: Path) -> Path:
    """A from-scratch Testing_v3 config dir, with the loader cache cleared.

    ``qg_configuration`` is ``@lru_cache(maxsize=1)``, so clear before and after
    to keep this temp config from poisoning (or being poisoned by) other tests.
    """
    qg_configuration.cache_clear()
    config_dir = write_testing_v3_config(tmp_path / "qg_configs")
    yield config_dir
    qg_configuration.cache_clear()


def test_testing_v3_config_loads(v3_config_dir: Path) -> None:
    """The from-scratch config loads and cross-validation passes."""
    cfg = qg_configuration(v3_config_dir)

    assert "Testing_v3" in {s.tech_area for s in cfg.samples.samples}
    assert "Testing_v3" in {c.tech_area for c in cfg.instrument_configs.configs}
    assert "simple" in cfg.queue_patterns.get_patterns_for_tech_area("Testing_v3")


def test_testing_v3_validates_via_cli(v3_config_dir: Path) -> None:
    """`qg-validate -c <dir>` accepts the from-scratch config (fresh subprocess)."""
    result = subprocess.run(
        ["uv", "run", "qg-validate", "-c", str(v3_config_dir)],
        capture_output=True,
        text=True,
    )
    assert result.returncode == 0, f"CLI failed: {result.stderr}"


def test_testing_v3_generates_via_python(v3_config_dir: Path) -> None:
    """QueueGenerator produces a queue: 5 user rows bracketed by QCv3 injections."""
    cfg = qg_configuration(v3_config_dir)
    queue_input = make_testing_v3_input(num_samples=5)

    df = QueueGenerator(cfg, queue_input).generate()

    names = [str(n) for n in df["Sample Name"].to_list()]
    user_rows = [n for n in names if n.startswith("Sample_")]
    qc_rows = [n for n in names if n == "QCv3"]
    assert len(user_rows) == 5
    assert len(qc_rows) >= 2  # start + end
    assert len(df) == len(user_rows) + len(qc_rows)


def test_testing_v3_generates_via_cli(v3_config_dir: Path, tmp_path: Path) -> None:
    """`qg <input> -o <out> -c <dir>` generates a CSV for the from-scratch config."""
    queue_input = make_testing_v3_input(num_samples=5)
    input_file = tmp_path / "input.json"
    input_file.write_text(queue_input.model_dump_json(indent=2))
    output_file = tmp_path / "output.csv"

    result = subprocess.run(
        ["uv", "run", "qg", str(input_file), "-o", str(output_file), "-c", str(v3_config_dir)],
        capture_output=True,
        text=True,
    )

    assert result.returncode == 0, f"CLI failed: {result.stderr}"
    assert output_file.exists()
    assert "QCv3" in output_file.read_text()
