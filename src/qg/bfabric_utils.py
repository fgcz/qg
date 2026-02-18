"""Utilities for loading B-Fabric data into typed DataFrames."""

from pathlib import Path

import polars as pl
from bfabric import Bfabric
from loguru import logger

from qg.sample_rows import PlateSampleRow, VialSampleRow

# =============================================================================
# Public API
# =============================================================================


class BfabricHelper:
    def __init__(self, client: Bfabric) -> None:
        self.client = client

    def get_samples(
        self,
        container_id: int,
        container_type: str,
        plate_ids: list[int] | None = None,
        dump_dir: Path | None = None,
    ) -> pl.DataFrame:
        """Load samples from B-Fabric as a typed DataFrame.

        Args:
            container_id: Container ID.
            container_type: "Vials" or "Plates".
            plate_ids: Filter to specific plates (only for Plates type).
            dump_dir: If set, write the DataFrame to a CSV in this directory.

        Returns:
            DataFrame with VialSampleRow or PlateSampleRow schema.
        """
        if container_type == "Plates":
            plates = self.get_plates(container_id)
            rows = self._load_plate_samples(plates, container_id, plate_ids)
        else:
            rows = self._load_vial_samples(container_id)

        if not rows:
            return pl.DataFrame()
        df = pl.DataFrame([r.model_dump() for r in rows])

        if dump_dir is not None:
            dump_dir = Path(dump_dir)
            dump_dir.mkdir(parents=True, exist_ok=True)
            path = dump_dir / f"samples_{container_id}_{container_type}.csv"
            df.write_csv(path)
            logger.info("Dumped {} samples to {}", len(df), path)

        return df

    def get_plates(self, container_id: int) -> dict:
        """Query plates for a container."""
        return self.client.reader.query("plate", {"containerid": container_id})

    def _load_vial_samples(self, container_id: int) -> list[VialSampleRow]:
        """Load samples for a vial container."""
        df = self.client.read("sample", {"containerid": container_id}, max_results=None).to_polars(flatten=True)
        return [
            VialSampleRow(
                sample_name=row["name"],
                sample_id=row["id"],
                tube_id=row.get("tubeid"),
                container_id=container_id,
                grouping_var=row.get("groupingvar_name"),
            )
            for row in df.iter_rows(named=True)
        ]

    @staticmethod
    def _load_plate_samples(
        plates: dict,
        container_id: int,
        plate_ids: list[int] | None = None,
    ) -> list[PlateSampleRow]:
        """Load samples from plates."""
        rows: list[PlateSampleRow] = []
        for uri, plate in plates.items():
            plate_id = uri.components.entity_id
            if plate_ids and plate_id not in plate_ids:
                continue
            for sample in plate.refs.sample:
                _gv = sample.get("groupingvar")
                if isinstance(_gv, dict):
                    _gv = _gv.get("name")
                rows.append(
                    PlateSampleRow(
                        sample_name=sample["name"],
                        sample_id=sample["id"],
                        container_id=container_id,
                        position=sample.get("_position"),
                        grid_position=sample.get("_gridposition"),
                        plate_id=plate_id,
                        grouping_var=_gv,
                    )
                )
        return rows
