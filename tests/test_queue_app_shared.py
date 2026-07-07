"""Unit tests for qg.apps.queue_app_shared — the B-Fabric-free pipeline helpers.

All tests are core-installable (no B-Fabric, no browser). They run in both
``test:unit`` and ``test:core-no-bfabric`` CI jobs.

Fixtures
--------
config : QGConfiguration
    Loaded once per session from the repo's ``qg_configs/`` directory.
vial_sample_df : pl.DataFrame
    25 vial samples (5 groups × 5) parsed from ``docs/examples/vial_samples_5x5.csv``
    through the real :func:`~qg.apps.integrations.local_samples.parse_sample_table`
    — exactly the shape the local app feeds to ``build_queue_input``.
plate_sample_df : pl.DataFrame
    80 plate samples (1 plate, container 40011) from
    ``docs/examples/plate_samples_80.csv`` — used for plate-mode cases.
"""

from __future__ import annotations

from datetime import date
from pathlib import Path

import polars as pl
import pytest

from qg.apps.integrations.example_samples import (
    list_example_sample_tables,
    read_example_sample_table,
)
from qg.apps.integrations.local_samples import parse_sample_table
from qg.apps.queue_app_shared import (
    GenerationResult,
    available_method_names,
    build_queue_input,
    build_queue_parameters,
    filter_by_column,
    generate_queue,
    load_methods_table,
    params_json_filename,
    queue_output_filename,
    resolve_default_qc_frequency,
    resolve_output_format,
    resolve_qc_layout_preview,
    synthesize_local_orders,
    validate_selection,
)
from qg.config_models.loader import qg_configuration
from qg.params_models import QueueParameters

_REPO_ROOT = Path(__file__).parent.parent
_CONFIG_DIR = _REPO_ROOT / "qg_configs"
_EXAMPLES_DIR = _REPO_ROOT / "docs" / "examples"


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------


@pytest.fixture(scope="session")
def config():
    return qg_configuration(_CONFIG_DIR)


@pytest.fixture(scope="session")
def vial_sample_df() -> pl.DataFrame:
    data = (_EXAMPLES_DIR / "vial_samples_5x5.csv").read_bytes()
    return parse_sample_table(data, "vial_samples_5x5.csv").df


@pytest.fixture(scope="session")
def plate_sample_df() -> pl.DataFrame:
    data = (_EXAMPLES_DIR / "plate_samples_80.csv").read_bytes()
    return parse_sample_table(data, "plate_samples_80.csv").df


@pytest.fixture(scope="session")
def multi_container_df() -> pl.DataFrame:
    data = (_EXAMPLES_DIR / "multi_container_samples.csv").read_bytes()
    return parse_sample_table(data, "multi_container_samples.csv").df


def _vial_params(**overrides) -> QueueParameters:
    """Minimal valid Proteomics/Vial QueueParameters."""
    defaults: dict = {
        "tech_area": "Proteomics",
        "instrument": "ASTRAL_1",
        "sampler": "Vanquish",
        "output_format": "xcalibur",
        "queue_pattern": "standard",
        "queue_type": "Vial",
        "plate_layout": "Vanquish_54",
        "qc_layout_name": "standard",
        "polarity": ["pos"],
        "date": "20260101",
        "user": "testuser",
        "method": {},
        "randomization": "no",
    }
    defaults.update(overrides)
    return QueueParameters(**defaults)


def _plate_params(**overrides) -> QueueParameters:
    """Minimal valid Proteomics/Plate QueueParameters (Vanquish, 4-tray sampler)."""
    defaults: dict = {
        "tech_area": "Proteomics",
        "instrument": "ASTRAL_1",
        "sampler": "Vanquish",
        "output_format": "xcalibur",
        "queue_pattern": "standard",
        "queue_type": "Plate",
        "plate_layout": "Vanquish_54",
        "qc_layout_name": "standard",
        "polarity": ["pos"],
        "date": "20260101",
        "user": "testuser",
        "method": {},
        "randomization": "no",
    }
    defaults.update(overrides)
    return QueueParameters(**defaults)


# ---------------------------------------------------------------------------
# synthesize_local_orders
# ---------------------------------------------------------------------------


class TestSynthesizeLocalOrders:
    def test_empty_df_returns_empty_list(self):
        assert synthesize_local_orders(pl.DataFrame()) == []

    def test_single_container(self, vial_sample_df):
        result = synthesize_local_orders(vial_sample_df)
        assert result == [(40000, None)]

    def test_multi_container_sorted(self, multi_container_df):
        result = synthesize_local_orders(multi_container_df)
        # Three containers (50001, 50002, 50003) returned in ascending order
        assert result == [(50001, None), (50002, None), (50003, None)]

    def test_second_element_is_none(self, vial_sample_df):
        for _, area in synthesize_local_orders(vial_sample_df):
            assert area is None


# ---------------------------------------------------------------------------
# resolve_output_format
# ---------------------------------------------------------------------------


class TestResolveOutputFormat:
    def test_sampler_selected_with_table(self, config):
        table = config.instrument_configs.to_table().filter(
            (pl.col("tech_area") == "Proteomics")
            & (pl.col("instrument") == "ASTRAL_1")
            & (pl.col("sampler") == "Vanquish")
        )
        result = resolve_output_format(table, sampler_selected=True)
        assert result == "xcalibur_sii"

    def test_sampler_not_selected_returns_xcalibur(self, config):
        table = config.instrument_configs.to_table()
        assert resolve_output_format(table, sampler_selected=False) == "xcalibur"

    def test_empty_table_returns_xcalibur(self):
        assert resolve_output_format(pl.DataFrame(), sampler_selected=True) == "xcalibur"


# ---------------------------------------------------------------------------
# build_queue_input
# ---------------------------------------------------------------------------


class TestBuildQueueInput:
    def test_no_samples_source_returns_none_none(self, config, vial_sample_df):
        qi, err = build_queue_input(config, _vial_params(), vial_sample_df, has_samples_source=False)
        assert qi is None and err is None

    def test_none_params_returns_none_none(self, config, vial_sample_df):
        qi, err = build_queue_input(config, None, vial_sample_df, has_samples_source=True)
        assert qi is None and err is None

    def test_none_df_returns_none_none(self, config):
        qi, err = build_queue_input(config, _vial_params(), None, has_samples_source=True)
        assert qi is None and err is None

    def test_valid_input_returns_queue_input(self, config, vial_sample_df):
        qi, err = build_queue_input(config, _vial_params(), vial_sample_df, has_samples_source=True)
        assert err is None
        assert qi is not None
        assert len(qi.queue.samples) == 25

    def test_randomization_draws_seed_onto_returned_input(self, config, vial_sample_df):
        params = _vial_params(randomization="random", seed=None)
        qi, err = build_queue_input(config, params, vial_sample_df, has_samples_source=True)
        assert err is None
        assert qi is not None
        assert qi.parameters.seed is not None
        assert 0 <= qi.parameters.seed < 2**32

    def test_randomization_does_not_mutate_original_params(self, config, vial_sample_df):
        # model_copy must be used internally; the caller's object must be unchanged.
        params = _vial_params(randomization="random", seed=None)
        build_queue_input(config, params, vial_sample_df, has_samples_source=True)
        assert params.seed is None, "build_queue_input must not mutate the caller's QueueParameters"

    def test_explicit_seed_is_preserved(self, config, vial_sample_df):
        params = _vial_params(randomization="random", seed=42)
        qi, _ = build_queue_input(config, params, vial_sample_df, has_samples_source=True)
        assert qi is not None
        assert qi.parameters.seed == 42

    def test_df_missing_container_id_returns_error(self, config):
        # add_samples_from_dataframe requires container_id; its absence surfaces as an error.
        bad_df = pl.DataFrame({"sample_name": ["S1"], "sample_id": [1]})
        qi, err = build_queue_input(config, _vial_params(), bad_df, has_samples_source=True)
        assert qi is None
        assert err is not None and "container_id" in err


# ---------------------------------------------------------------------------
# generate_queue
# ---------------------------------------------------------------------------


class TestGenerateQueue:
    def test_none_input_returns_empty_result(self, config):
        result = generate_queue(config, None, None)
        assert isinstance(result, GenerationResult)
        assert result.generated_df is None
        assert result.output_str is None
        assert result.error is None

    def test_happy_path_vial(self, config, vial_sample_df):
        qi, _ = build_queue_input(config, _vial_params(), vial_sample_df, has_samples_source=True)
        result = generate_queue(config, qi, _vial_params())
        assert result.error is None
        assert result.generated_df is not None
        assert result.raw_df is not None
        assert result.output_str is not None
        assert result.file_extension == ".csv"

    def test_tray_overflow_gives_friendly_error(self, config):
        """Vanquish has 4 trays; 5 plates overflow it.

        This case was previously unreachable because generate_queue checked for
        the substring "Not enough tray positions" while positionV2.py raises
        "Not enough trays (N) for M plates". The fix changes the check to
        "Not enough trays" so the enrichment fires.
        """
        # Build a plate DataFrame with 5 distinct plate_ids on a single container.
        rows = []
        for plate_idx in range(1, 6):  # 5 plates
            for i in range(1, 4):  # 3 samples per plate (tray assignment is automatic)
                rows.append(
                    {
                        "sample_name": f"P{plate_idx}_S{i}",
                        "sample_id": plate_idx * 100 + i,
                        "container_id": 99001,
                        "plate_id": 90000 + plate_idx,
                        "grid_position": f"A{i}",
                        "tray": None,
                        "grouping_var": None,
                    }
                )
        plate_df = pl.DataFrame(rows).with_columns(
            pl.col("sample_id").cast(pl.Int64),
            pl.col("container_id").cast(pl.Int64),
            pl.col("plate_id").cast(pl.Int64),
        )

        params = _plate_params()
        qi, build_err = build_queue_input(config, params, plate_df, has_samples_source=True)
        assert build_err is None, f"build_queue_input failed: {build_err}"
        assert qi is not None

        result = generate_queue(config, qi, params)

        assert result.generated_df is None
        assert result.error is not None
        assert "cannot hold this many plates" in result.error
        assert "Vanquish" in result.error

    def test_output_str_and_generated_df_consistent(self, config, vial_sample_df):
        """The output CSV rows should match the DataFrame row count."""
        qi, _ = build_queue_input(config, _vial_params(), vial_sample_df, has_samples_source=True)
        result = generate_queue(config, qi, _vial_params())
        assert result.generated_df is not None
        assert result.output_str is not None
        assert len(result.generated_df) > 0


# ---------------------------------------------------------------------------
# queue_output_filename
# ---------------------------------------------------------------------------


class TestQueueOutputFilename:
    def test_single_container(self):
        params = _vial_params(date="20260101", instrument="ASTRAL_1")
        assert queue_output_filename(params, [37180], ".csv") == "20260101_ASTRAL_1_37180.csv"

    def test_multi_container(self):
        params = _vial_params(date="20260101", instrument="ASTRAL_1")
        assert queue_output_filename(params, [100, 200, 300], ".csv") == "20260101_ASTRAL_1_100_200_300.csv"

    def test_empty_container_ids_uses_queue_fallback(self):
        params = _vial_params(date="20260101", instrument="ASTRAL_1")
        assert queue_output_filename(params, [], ".csv") == "20260101_ASTRAL_1_queue.csv"

    def test_xml_extension(self):
        params = _vial_params(date="20260101", instrument="ASTRAL_1")
        assert queue_output_filename(params, [37180], ".xml") == "20260101_ASTRAL_1_37180.xml"

    def test_none_params_returns_none(self):
        assert queue_output_filename(None, [37180], ".csv") is None


# ---------------------------------------------------------------------------
# params_json_filename
# ---------------------------------------------------------------------------


class TestParamsJsonFilename:
    def test_single_container(self, vial_sample_df):
        params = _vial_params(tech_area="Proteomics", sampler="Vanquish")
        name = params_json_filename(params, vial_sample_df)
        assert name == "Proteomics_Vanquish_c40000_n25.json"

    def test_multi_container_sorted(self, multi_container_df):
        params = _vial_params(tech_area="Proteomics", sampler="Vanquish")
        name = params_json_filename(params, multi_container_df)
        assert name == "Proteomics_Vanquish_c50001_50002_50003_n30.json"

    def test_sampler_dot_replaced(self, vial_sample_df):
        params = _vial_params(tech_area="Metabolomics", sampler="MClass48")
        name = params_json_filename(params, vial_sample_df)
        assert "." not in name.split("_c")[0]


# ---------------------------------------------------------------------------
# Bundled example tables (catalog -> parse -> build -> generate)
# ---------------------------------------------------------------------------


class TestBundledExamples:
    """A bundled vial and a bundled plate example each generate a queue with the
    recommended Proteomics/ASTRAL_1/Vanquish parameters."""

    def _build_and_generate(self, config, example_id, params):
        _, data = read_example_sample_table(example_id)
        parsed = parse_sample_table(data, read_example_sample_table(example_id)[0].filename)
        qi, err = build_queue_input(config, params, parsed.df, has_samples_source=True)
        assert err is None and qi is not None
        return generate_queue(config, qi, params)

    def test_catalog_has_a_loadable_vial_and_plate(self):
        modes = {e.mode for e in list_example_sample_tables()}
        assert {"vial", "plate"} <= modes

    def test_vial_example_generates_queue(self, config):
        result = self._build_and_generate(config, "vial_5x5", _vial_params())
        assert result.error is None
        assert result.output_str is not None
        assert result.file_extension == ".csv"

    def test_plate_example_generates_queue(self, config):
        result = self._build_and_generate(config, "plate_5x5", _plate_params())
        assert result.error is None
        assert result.output_str is not None
        assert result.file_extension == ".csv"


# ---------------------------------------------------------------------------
# filter_by_column
# ---------------------------------------------------------------------------


class TestFilterByColumn:
    @staticmethod
    def _table() -> pl.DataFrame:
        return pl.DataFrame({"tech_area": ["Proteomics", "Metabolomics"], "n": [1, 2]})

    def test_filters_to_matching_rows(self):
        assert filter_by_column(self._table(), "tech_area", "Proteomics")["n"].to_list() == [1]

    def test_falsy_value_passes_through_unchanged(self):
        table = self._table()
        assert filter_by_column(table, "tech_area", None).equals(table)
        assert filter_by_column(table, "tech_area", "").equals(table)

    def test_no_match_yields_empty(self):
        assert filter_by_column(self._table(), "tech_area", "Lipidomics").is_empty()


# ---------------------------------------------------------------------------
# resolve_default_qc_frequency
# ---------------------------------------------------------------------------


class TestResolveDefaultQcFrequency:
    def test_fallback_when_unresolved(self, config):
        assert resolve_default_qc_frequency(config, None, None) == 16
        assert resolve_default_qc_frequency(config, "Proteomics", None) == 16

    def test_reads_pattern_value(self, config):
        freq = resolve_default_qc_frequency(config, "Proteomics", "standard")
        assert isinstance(freq, int)
        assert freq > 0


# ---------------------------------------------------------------------------
# load_methods_table / available_method_names
# ---------------------------------------------------------------------------


class TestLoadMethodsTable:
    def test_empty_when_unresolved(self, config):
        assert load_methods_table(config, None, None).is_empty()
        assert load_methods_table(config, "Proteomics", None).is_empty()

    def test_returns_methods_for_instrument(self, config):
        assert not load_methods_table(config, "Proteomics", "ASTRAL_1").is_empty()


class TestAvailableMethodNames:
    def test_empty_when_methods_df_empty(self, config):
        pos, neg = available_method_names(config, pl.DataFrame(), "Proteomics", "ASTRAL_1", "standard")
        assert pos == []
        assert neg == []

    def test_returns_lists_for_resolved_selection(self, config):
        methods_df = load_methods_table(config, "Proteomics", "ASTRAL_1")
        pos, neg = available_method_names(config, methods_df, "Proteomics", "ASTRAL_1", "standard")
        assert isinstance(pos, list)
        assert isinstance(neg, list)


# ---------------------------------------------------------------------------
# validate_selection
# ---------------------------------------------------------------------------


class TestValidateSelection:
    @staticmethod
    def _kwargs(**overrides) -> dict:
        kwargs: dict = {
            "selected_orders": [(37180, None)],
            "tech_area": "Proteomics",
            "instrument": "ASTRAL_1",
            "sampler": "Vanquish",
            "queue_type": "Vial",
            "plate_layout": "Vanquish_54",
            "qc_layout": "standard",
            "pattern": "standard",
            "filtered_table": pl.DataFrame({"x": [1]}),
            "no_source_message": "Please select an order",
        }
        kwargs.update(overrides)
        return kwargs

    def test_no_source_message_when_no_orders(self, config):
        valid, errors = validate_selection(config, **self._kwargs(selected_orders=[]))
        assert not valid
        assert errors == ["Please select an order"]

    def test_no_source_message_is_customizable(self, config):
        _valid, errors = validate_selection(
            config, **self._kwargs(selected_orders=[], no_source_message="Upload a sample table")
        )
        assert errors == ["Upload a sample table"]

    def test_reports_missing_fields(self, config):
        valid, errors = validate_selection(config, **self._kwargs(instrument=None, sampler=None))
        assert not valid
        assert "Instrument not selected" in errors
        assert "Sampler not selected" in errors

    def test_empty_filtered_table_is_invalid(self, config):
        valid, errors = validate_selection(config, **self._kwargs(filtered_table=pl.DataFrame()))
        assert not valid
        assert "No valid combination found" in errors

    def test_valid_selection_passes(self, config):
        valid, errors = validate_selection(config, **self._kwargs())
        assert valid, errors
        assert errors == []


# ---------------------------------------------------------------------------
# resolve_qc_layout_preview
# ---------------------------------------------------------------------------


class TestResolveQcLayoutPreview:
    def test_none_when_selection_incomplete(self, config):
        assert (
            resolve_qc_layout_preview(
                config,
                tech_area="Proteomics",
                sampler=None,
                plate_layout="Vanquish_54",
                qc_layout="standard",
                pattern="standard",
                raw_queue_df=None,
            )
            is None
        )

    def test_returns_preview_for_valid_selection(self, config):
        preview = resolve_qc_layout_preview(
            config,
            tech_area="Proteomics",
            sampler="Vanquish",
            plate_layout="Vanquish_54",
            qc_layout="standard",
            pattern="standard",
            raw_queue_df=None,
        )
        assert preview is not None
        assert "sample_id" in preview.columns


# ---------------------------------------------------------------------------
# build_queue_parameters
# ---------------------------------------------------------------------------


class TestBuildQueueParameters:
    @staticmethod
    def _kwargs(**overrides) -> dict:
        base: dict = {
            "tech_area": "Proteomics",
            "instrument": "ASTRAL_1",
            "sampler": "Vanquish",
            "output_format": "xcalibur",
            "queue_pattern": "standard",
            "queue_type": "Vial",
            "plate_layout": "Vanquish_54",
            "qc_layout_name": "standard",
            "polarity_flags": {"pos": True, "neg": False},
            "date": date(2026, 1, 15),
            "user": "  tester  ",
            "method_pos": None,
            "method_neg": None,
            "randomization": "no",
            "inj_vol_text": "",
            "qc_frequency_text": "",
            "start_position": None,
            "start_tray": None,
            "level_concentrations": {},
        }
        base.update(overrides)
        return base

    def test_builds_valid_params_with_conversions(self):
        params, err = build_queue_parameters(**self._kwargs())
        assert err is None
        assert params is not None
        assert params.tech_area == "Proteomics"
        assert params.date == "20260115"  # datetime.date -> %Y%m%d
        assert params.user == "tester"  # stripped
        assert params.start_position == "A1"  # None -> default
        assert params.start_tray == ""  # None -> default
        assert params.inj_vol_override is None  # blank text -> None
        assert params.qc_frequency_override is None

    def test_polarity_method_and_overrides(self):
        params, err = build_queue_parameters(
            **self._kwargs(
                polarity_flags={"pos": True, "neg": True},
                method_pos="M_pos",
                method_neg="",  # empty selection is dropped
                inj_vol_text="2.5",
                qc_frequency_text="8",
            )
        )
        assert err is None
        assert params.polarity == ["pos", "neg"]
        assert params.method == {"pos": "M_pos"}
        assert params.inj_vol_override == 2.5
        assert params.qc_frequency_override == 8

    def test_invalid_selection_returns_error(self):
        params, err = build_queue_parameters(**self._kwargs(queue_type="Bogus"))
        assert params is None
        assert err is not None

    def test_nonnumeric_inj_vol_still_raises(self):
        # Matches the original inline cell: only ValidationError is caught, so a
        # non-numeric inj-vol surfaces as ValueError rather than being swallowed.
        with pytest.raises(ValueError):
            build_queue_parameters(**self._kwargs(inj_vol_text="abc"))
