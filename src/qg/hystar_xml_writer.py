"""HyStar XML writer for TimsTOF instruments."""

from __future__ import annotations

from pathlib import Path
from xml.etree import ElementTree as ET

import polars as pl


def write_hystar_xml(
    df: pl.DataFrame,
    path: Path | str,
    acqend_execute: str = r"C:\FGCZ\Biobeamer\biobeamer.bat",
) -> None:
    """Write DataFrame as HyStar XML sequence file.

    Args:
        df: DataFrame with columns: Position, SampleID, Volume, DataPath, SuperMethod
        path: Output file path
        acqend_execute: Post-acquisition script path

    The DataFrame should already be formatted via output_formats.toml [hystar].
    This function adds derived fields:
        - SampleComment: empty string
        - ResultDatafile: {DataPath}\\{SampleID}.d
        - ACQEND_EXECUTE: configurable script path
    """
    root = ET.Element("SampleTable")

    for row in df.iter_rows(named=True):
        sample_id = str(row.get("SampleID", ""))
        data_path = str(row.get("DataPath", ""))

        attrs = {
            "Position": str(row.get("Position", "")),
            "SampleID": sample_id,
            "SampleComment": "",
            "Volume": str(row.get("Volume", "1")),
            "DataPath": data_path,
            "SuperMethod": str(row.get("SuperMethod", "")),
            "ResultDatafile": f"{data_path}\\{sample_id}.d",
            "ACQEND_EXECUTE": acqend_execute,
        }
        ET.SubElement(root, "Sample", **attrs)

    tree = ET.ElementTree(root)
    ET.indent(tree, space="  ")
    tree.write(path, encoding="utf-8", xml_declaration=True)
