"""Tests for HyStar XML writer."""

from pathlib import Path

import polars as pl

from qg.generator import write_queue
from qg.hystar_xml_writer import write_hystar_xml


def test_write_hystar_xml(tmp_path: Path) -> None:
    """Test basic XML generation."""
    df = pl.DataFrame(
        {
            "Position": ["S1-A1", "S1-B1", "S5-A1"],
            "SampleID": ["20240828_001_sample1", "20240828_002_sample2", "20240828_003_clean"],
            "Volume": [1, 1, 1],
            "DataPath": [r"D:\Data\project"] * 3,
            "SuperMethod": ["", "", ""],
        }
    )

    output_path = tmp_path / "test_queue.xml"
    write_hystar_xml(df, output_path)

    content = output_path.read_text()

    # Check XML structure (ElementTree uses single quotes in declaration)
    assert "<?xml version='1.0' encoding='utf-8'?>" in content
    assert "<SampleTable>" in content
    assert "</SampleTable>" in content
    assert '<Sample Position="S1-A1"' in content

    # Check derived fields
    assert 'SampleComment=""' in content
    assert r'ResultDatafile="D:\Data\project\20240828_001_sample1.d"' in content
    assert r'ACQEND_EXECUTE="C:\FGCZ\Biobeamer\biobeamer.bat"' in content


def test_write_hystar_xml_custom_acqend(tmp_path: Path) -> None:
    """Test custom ACQEND_EXECUTE path."""
    df = pl.DataFrame(
        {
            "Position": ["S1-A1"],
            "SampleID": ["sample1"],
            "Volume": [1],
            "DataPath": [r"D:\Data"],
            "SuperMethod": [""],
        }
    )

    output_path = tmp_path / "test_queue.xml"
    write_hystar_xml(df, output_path, acqend_execute=r"D:\Scripts\custom.bat")

    content = output_path.read_text()
    assert r'ACQEND_EXECUTE="D:\Scripts\custom.bat"' in content


def test_write_queue_hystar_returns_xml() -> None:
    """Test write_queue returns XML for hystar format."""
    df = pl.DataFrame(
        {
            "Position": ["S1-A1"],
            "SampleID": ["sample1"],
            "Volume": [1],
            "DataPath": [r"D:\Data"],
            "SuperMethod": [""],
        }
    )

    content = write_queue(df, "hystar")

    assert "<SampleTable>" in content
    assert '<Sample Position="S1-A1"' in content


def test_write_queue_xcalibur_returns_csv() -> None:
    """Test write_queue returns CSV for non-hystar formats."""
    df = pl.DataFrame(
        {
            "Position": ["Y:A1"],
            "File Name": ["sample1"],
        }
    )

    content = write_queue(df, "xcalibur")

    assert "Position" in content
    assert "Y:A1" in content
    assert "<" not in content  # Not XML
