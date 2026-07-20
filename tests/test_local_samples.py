"""Tests for the local CSV/XLSX sample parser (qg.apps.integrations.local_samples)."""

import io
from pathlib import Path

import polars as pl
import pytest

from qg.apps import queue_app_shared as shared
from qg.apps.integrations.local_samples import parse_sample_table
from qg.config_models.loader import qg_configuration
from qg.params_models import QueueParameters


def _csv(text: str) -> bytes:
    return text.encode("utf-8")


def _xlsx(df: pl.DataFrame) -> bytes:
    buf = io.BytesIO()
    df.write_excel(buf)
    return buf.getvalue()


class TestVial:
    def test_minimal_vial_csv(self):
        data = _csv("sample_name,sample_id,container_id\nS1,101,37180\nS2,102,37180\n")
        parsed = parse_sample_table(data, "samples.csv")

        assert parsed.mode == "vial"
        assert parsed.df.columns == ["sample_name", "sample_id", "container_id"]
        assert parsed.df["sample_id"].dtype == pl.Int64
        assert parsed.df.height == 2

    def test_optional_columns_kept(self):
        data = _csv("sample_name,sample_id,container_id,tube_id,grouping_var\nS1,1,5,37180/1,treated\n")
        parsed = parse_sample_table(data, "s.csv")
        assert set(parsed.df.columns) == {"sample_name", "sample_id", "container_id", "tube_id", "grouping_var"}

    def test_display_name_aliases(self):
        data = _csv("Sample Name,Sample ID,Container ID,Tube ID\nS1,1,5,5/1\n")
        parsed = parse_sample_table(data, "s.csv")
        assert parsed.mode == "vial"
        assert {"sample_name", "sample_id", "container_id", "tube_id"} == set(parsed.df.columns)

    def test_bfabric_export_aliases(self):
        data = _csv("name,id,container_id,groupingvar_name\nS1,1,5,g1\n")
        parsed = parse_sample_table(data, "s.csv")
        assert {"sample_name", "sample_id", "container_id", "grouping_var"} == set(parsed.df.columns)

    def test_mixed_case_non_aliased_headers_lowercased(self):
        # Headers not in the alias table (e.g. underscore + caps) fall back to a
        # lower-cased name so they still match their canonical column.
        data = _csv("Sample_Name,Sample_ID,Container_ID\nS1,1,5\n")
        parsed = parse_sample_table(data, "s.csv")
        assert parsed.mode == "vial"
        assert {"sample_name", "sample_id", "container_id"} == set(parsed.df.columns)

    def test_unknown_columns_dropped(self):
        data = _csv("sample_name,sample_id,container_id,notes\nS1,1,5,ignore-me\n")
        parsed = parse_sample_table(data, "s.csv")
        assert "notes" not in parsed.df.columns


class TestPlate:
    def test_minimal_plate_csv(self):
        data = _csv(
            "sample_name,sample_id,container_id,plate_id,grid_position,tray\nS1,1,5,900,A1,Y\nS2,2,5,900,A2,Y\n"
        )
        parsed = parse_sample_table(data, "plate.csv")

        assert parsed.mode == "plate"
        assert {"plate_id", "grid_position", "tray"} <= set(parsed.df.columns)
        assert parsed.df["plate_id"].dtype == pl.Int64

    def test_plate_without_tray_gets_null_tray_column(self):
        # The builder's plate path requires a `tray` column; parser supplies a null one.
        data = _csv("sample_name,sample_id,container_id,plate_id,grid_position\nS1,1,5,900,A1\n")
        parsed = parse_sample_table(data, "plate.csv")
        assert parsed.mode == "plate"
        assert "tray" in parsed.df.columns
        assert parsed.df["tray"].null_count() == 1


class TestErrors:
    def test_missing_required_vial_column(self):
        data = _csv("sample_name,sample_id\nS1,1\n")  # no container_id
        with pytest.raises(ValueError, match="missing required column.*container_id"):
            parse_sample_table(data, "s.csv")

    def test_duplicate_sample_id(self):
        data = _csv("sample_name,sample_id,container_id\nS1,1,5\nS2,1,5\n")
        with pytest.raises(ValueError, match="Duplicate sample_id"):
            parse_sample_table(data, "s.csv")

    def test_non_integer_sample_id(self):
        data = _csv("sample_name,sample_id,container_id\nS1,abc,5\n")
        with pytest.raises(ValueError, match="sample_id.*whole numbers"):
            parse_sample_table(data, "s.csv")

    def test_unsupported_extension(self):
        with pytest.raises(ValueError, match="Unsupported file type"):
            parse_sample_table(b"whatever", "samples.txt")

    def test_unreadable_csv(self):
        # A valid extension but bytes that the reader cannot parse as a table.
        with pytest.raises(ValueError, match="Could not read"):
            parse_sample_table(b"\x00\x01\x02 not,a,valid\x00csv", "broken.xlsx")


class TestExcel:
    def test_reads_xlsx_round_trip(self):
        df = pl.DataFrame({"sample_name": ["S1", "S2"], "sample_id": [1, 2], "container_id": [5, 5]})
        parsed = parse_sample_table(_xlsx(df), "samples.xlsx")
        assert parsed.mode == "vial"
        assert parsed.df.height == 2
        assert parsed.df["sample_id"].to_list() == [1, 2]


class TestExamples:
    def test_vial_samples_80_groups_span_generated_trays(self):
        path = Path(__file__).parents[1] / "docs/examples/vial_samples_80.csv"
        parsed = parse_sample_table(path.read_bytes(), path.name)
        params = QueueParameters(
            tech_area="Proteomics",
            instrument="ASTRAL_1",
            sampler="Vanquish",
            output_format="xcalibur_sii",
            queue_pattern="standard",
            queue_type="Vial",
            plate_layout="Vanquish_54",
            qc_layout_name="standard",
            polarity=["pos"],
            date="20260610",
            user="test",
            inj_vol_override=1,
            start_position="A1",
            start_tray="Y",
        )
        config = qg_configuration()
        queue_input, error = shared.build_queue_input(
            config,
            params,
            parsed.df,
            has_samples_source=True,
            provenance_instance=None,
        )
        assert error is None

        result = shared.generate_queue(queue_input, params)
        assert result.error is None

        user_rows = result.raw_df.filter(pl.col("slot_kind") == "user")
        tray_counts = user_rows.group_by("grouping_var").agg(pl.col("tray").n_unique().alias("n_trays"))
        assert tray_counts["n_trays"].to_list() == [2] * 8
