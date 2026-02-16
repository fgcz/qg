"""Tests for HyStar XML writer and write_queue dispatcher."""

from pathlib import Path

import polars as pl

from qg.config_models.formatting import OutputFormat
from qg.generator import write_queue
from qg.hystar_xml_writer import write_hystar_xml


def _make_format(writer: str = "csv", file_extension: str = ".csv") -> OutputFormat:
    return OutputFormat(
        description="test",
        file_extension=file_extension,
        writer=writer,
        position_format="{tray}:{grid_position}",
        columns={},
    )


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
    """Test write_queue returns XML for hystar_xml writer."""
    df = pl.DataFrame(
        {
            "Position": ["S1-A1"],
            "SampleID": ["sample1"],
            "Volume": [1],
            "DataPath": [r"D:\Data"],
            "SuperMethod": [""],
        }
    )

    content = write_queue(df, _make_format(writer="hystar_xml", file_extension=".xml"))

    assert "<SampleTable>" in content
    assert '<Sample Position="S1-A1"' in content


def test_write_queue_csv_returns_plain_csv() -> None:
    """Test write_queue returns plain CSV for csv writer."""
    df = pl.DataFrame(
        {
            "Position": ["Y:A1"],
            "File Name": ["sample1"],
        }
    )

    content = write_queue(df, _make_format(writer="csv"))

    assert "Position" in content
    assert "Y:A1" in content
    assert "<" not in content  # Not XML
    assert "Bracket Type" not in content  # No preamble


def test_write_queue_xcalibur_csv_has_preamble() -> None:
    """Xcalibur CSV writer prepends Bracket Type preamble with correct comma count."""
    df = pl.DataFrame(
        {
            "File Name": ["s1"],
            "Path": ["p"],
            "Position": ["1:F,7"],
            "Inj Vol": [1],
            "L3 Laboratory": ["FGCZ"],
            "Sample ID": ["123"],
            "Sample Name": ["sample1"],
            "Instrument Method": [r"C:\Xcalibur\methods\\"],
        }
    )

    content = write_queue(df, _make_format(writer="xcalibur_csv"))

    lines = content.split("\n")
    # 8 columns → "Bracket Type=4" + 7 commas
    assert lines[0] == "Bracket Type=4,,,,,,,"
    assert lines[1].startswith("File Name,")
