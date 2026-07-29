"""Tests for self-contained params: ResolvedConfig extraction and regeneration.

The headline guarantee: an exported params JSON with an embedded ``resolved_config``
regenerates the queue byte-for-byte from that snapshot alone, without ``qg_configs/``.
"""

import json
import subprocess
import sys
from pathlib import Path

import pytest
from pydantic import ValidationError

from qg import __version__
from qg.apps.integrations.example_params import list_example_params, read_example_params
from qg.config_models.loader import qg_configuration
from qg.generator import QueueGenerator
from qg.params_models import (
    ContainerBatch,
    QueueParameters,
    VialQueue,
    VialQueueInput,
    read_queue_input,
)

sys.path.insert(0, str(Path(__file__).parent))
from helpers import make_queue_input, make_samples  # noqa: E402

CONFIG_DIR = Path(__file__).parent.parent / "qg_configs"


def _cli(name: str) -> Path:
    """Resolve an entry point from the active Python environment."""
    return Path(sys.executable).with_name(name)


@pytest.fixture
def config():
    qg_configuration.cache_clear()
    return qg_configuration(CONFIG_DIR)


def _vial_input(config, params: QueueParameters, n: int = 6) -> VialQueueInput:
    container_id = 70001
    batches = {container_id: ContainerBatch(container_id=container_id, container_name="P")}
    return VialQueueInput(
        parameters=params,
        queue=VialQueue(batches=batches, samples=make_samples(n, container_id)),
        qg_version=__version__,
        resolved_config=config.subset_for(params),
    )


def _regen_matches(queue_input) -> bool:
    """Generate twice from the self-contained positioned representation."""
    positioned = queue_input.position_queue()
    base_config = positioned.resolved_config.to_configuration()
    base = QueueGenerator(base_config, positioned).write()
    restored = type(positioned).model_validate_json(positioned.model_dump_json())
    restored_config = restored.resolved_config.to_configuration()
    repro = QueueGenerator(restored_config, restored).write()
    return repro == base


# --------------------------------------------------------------------------- #
# Phase 1 — extraction is minimal and round-trips through JSON                 #
# --------------------------------------------------------------------------- #


def test_subset_for_captures_minimal_set(config):
    qi = make_queue_input(config=config, num_samples=12)
    rc = config.subset_for(qi.parameters)

    # Only the fragments the run uses, not the whole config.
    assert rc.instrument.instrument == "ASTRAL_1"
    assert rc.output_format_name == "xcalibur"
    assert rc.sampler.name == "Vanquish"
    assert rc.queue_pattern_name == "standard"
    # samples = default + pattern/QC-layout references for this tech only.
    sample_ids = {s.sample_id for s in rc.samples}
    assert "default" in sample_ids
    assert sample_ids >= rc.queue_pattern.get_all_sample_ids()
    assert all(s.tech_area == "Proteomics" for s in rc.samples)
    # Vanquish is a well sampler: tip rows stay empty.
    assert rc.qc_samples_tip == []
    assert rc.methods  # instrument methods captured


def test_roundtrip_json_preserves_resolved_config(config, tmp_path):
    stamped = make_queue_input(num_samples=10, config=config)

    path = tmp_path / "params.json"
    path.write_text(json.dumps(stamped.model_dump(mode="json"), indent=2))
    loaded = read_queue_input(path)

    assert loaded.qg_version == stamped.qg_version
    assert loaded.resolved_config == stamped.resolved_config


def test_load_without_required_provenance_fails(config, tmp_path):
    data = make_queue_input(config=config, num_samples=5).model_dump(mode="json")
    data.pop("qg_version", None)
    data.pop("resolved_config", None)
    path = tmp_path / "legacy.json"
    path.write_text(json.dumps(data))

    with pytest.raises(ValidationError):
        read_queue_input(path)


# --------------------------------------------------------------------------- #
# Phase 2 — self-contained regeneration                                        #
# --------------------------------------------------------------------------- #


def test_regenerates_byte_identical_proteomics(config):
    assert _regen_matches(make_queue_input(config=config, num_samples=12))


def test_randomized_reproducible_from_embedded(config):
    qi = make_queue_input(groups=[(111, 6), (222, 4), (333, 2)], config=config)
    qi.parameters.randomization = "blocked_uniform"
    qi.parameters.seed = 424242
    assert _regen_matches(qi)


def test_lipidomics_dual_polarity_self_contained(config):
    # Pick a real Lipidomics vial combination from the config overview.
    overview = config.to_overview_table()
    lipid = [
        r
        for r in overview.iter_rows(named=True)
        if r["tech_area"] == "Lipidomics" and r["queue_type"] == "Vial" and r["pattern_name"] and r["qc_layout_name"]
    ]
    if not lipid:
        pytest.skip("no Lipidomics vial combination configured")
    row = lipid[0]
    params = QueueParameters(
        tech_area=row["tech_area"],
        instrument=row["instrument"],
        sampler=row["sampler"],
        output_format=row["output_format"],
        queue_pattern=row["pattern_name"],
        queue_type="Vial",
        plate_layout=row["plate_layout"],
        qc_layout_name=row["qc_layout_name"],
        polarity=["pos", "neg"],
        date="20260101",
        user="t",
    )
    assert _regen_matches(_vial_input(config, params))


def test_every_vial_combination_regenerates_byte_identical(config):
    """Sweep all supported vial combos — proves to_configuration() across all techs,
    instruments, samplers (well + tip), patterns and QC layouts."""
    overview = config.to_overview_table()
    checked = 0
    for row in overview.iter_rows(named=True):
        if row["queue_type"] != "Vial" or not row["pattern_name"] or not row["qc_layout_name"]:
            continue
        tech = row["tech_area"]
        params = QueueParameters(
            tech_area=tech,
            instrument=row["instrument"],
            sampler=row["sampler"],
            output_format=row["output_format"],
            queue_pattern=row["pattern_name"],
            queue_type="Vial",
            plate_layout=row["plate_layout"],
            qc_layout_name=row["qc_layout_name"],
            # Real per-tech polarity so the queue is non-empty (an empty polarity
            # list expands to zero injections and would mask a regeneration bug).
            polarity=config.tech_area_defaults.get_default_polarities(tech) or ["pos"],
            date="20260101",
            user="t",
        )
        qi = _vial_input(config, params)
        assert QueueGenerator(config, qi.position_queue()).build_rows().to_table().shape[0] > 0, (
            f"empty queue for {row}"
        )
        assert _regen_matches(qi), f"mismatch for {row}"
        checked += 1
    assert checked > 0, "no vial combinations found to check"


# --------------------------------------------------------------------------- #
# Drift detection                                                              #
# --------------------------------------------------------------------------- #


def test_no_false_drift_against_same_config(config):
    qi = make_queue_input(config=config, num_samples=8)
    rc = config.subset_for(qi.parameters)
    assert rc.differs_from(config, qi.parameters) is False


def test_drift_detected_when_config_changes(config):
    qi = make_queue_input(config=config, num_samples=8)
    rc = config.subset_for(qi.parameters)

    # A config whose pattern content changed under the same name. Replace with a
    # modified *copy* so the embedded snapshot's own pattern object is untouched.
    tweaked = rc.to_configuration()
    orig = tweaked.queue_patterns.patterns["Proteomics"]["standard"]
    tweaked.queue_patterns.patterns["Proteomics"]["standard"] = orig.model_copy(
        update={"run_QC_after_n_samples": orig.run_QC_after_n_samples + 1}
    )

    assert rc.differs_from(tweaked, qi.parameters) is True
    # The embedded snapshot still reproduces the ORIGINAL queue, immune to the drift.
    assert _regen_matches(qi)


def test_generator_uses_the_injected_config(config):
    qi = make_queue_input(config=config, num_samples=3)
    positioned = qi.position_queue()
    injected = positioned.resolved_config.model_copy(deep=True).to_configuration()
    injected.output_formats.get_format("xcalibur").columns["Config Source"] = "literal:injected"

    assert "Config Source" not in positioned.resolved_config.output_format.columns
    generated = QueueGenerator(injected, positioned).generate()

    assert generated["Config Source"].to_list() == ["injected"] * len(generated)


# --------------------------------------------------------------------------- #
# CLI — reproduces without a config dir                                        #
# --------------------------------------------------------------------------- #


def test_cli_reproduces_without_config_dir(config, tmp_path):
    qi = make_queue_input(num_samples=8, config=config)
    params_json = tmp_path / "params.json"
    params_json.write_text(json.dumps(qi.model_dump(mode="json"), indent=2))

    out = tmp_path / "queue.csv"

    result = subprocess.run(
        [_cli("qg"), str(params_json), "-o", str(out)],
        capture_output=True,
        text=True,
    )
    assert result.returncode == 0, result.stderr
    positioned = qi.position_queue()
    snapshot_config = positioned.resolved_config.to_configuration()
    assert out.read_text() == QueueGenerator(snapshot_config, positioned).write()


# --------------------------------------------------------------------------- #
# Phase 3 — bundled self-contained example params (must not rot)               #
# --------------------------------------------------------------------------- #


def test_bundled_example_params_are_self_contained():
    entries = list_example_params()
    assert {e.id for e in entries} == {"repro_proteomics_12", "lipidomics_standard"}
    for entry in entries:
        _, qi = read_example_params(entry.id)
        positioned = qi.position_queue()
        snapshot_config = positioned.resolved_config.to_configuration()
        rows = QueueGenerator(snapshot_config, positioned).build_rows().to_table()
        assert rows.shape[0] > 0, f"{entry.id} generated an empty queue"


@pytest.mark.parametrize(
    ("example_id", "expected_rows"),
    [("repro_proteomics_12", 20), ("lipidomics_standard", 60)],
)
def test_bundled_example_row_counts(example_id, expected_rows):
    _, qi = read_example_params(example_id)
    positioned = qi.position_queue()
    snapshot_config = positioned.resolved_config.to_configuration()
    rows = QueueGenerator(snapshot_config, positioned).build_rows().to_table()
    assert rows.shape[0] == expected_rows


def test_bundled_example_params_match_live_config(config):
    """The shipped snapshots must stay in sync with qg_configs/ — regenerate on drift."""
    for entry in list_example_params():
        _, qi = read_example_params(entry.id)
        assert qi.resolved_config.differs_from(config, qi.parameters) is False, (
            f"{entry.id} has drifted from qg_configs/; regenerate the fixture"
        )
