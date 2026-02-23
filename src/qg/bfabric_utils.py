"""Utilities for loading B-Fabric data into typed DataFrames."""

from pathlib import Path

import polars as pl
from bfabric import Bfabric
from bfabric_rest_proxy.feeder_operations.create_workunit import (
    CreateWorkunitParams,
    create_workunit,
)
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
        filename_prefix: str | None = None,
    ) -> pl.DataFrame:
        """Load samples from B-Fabric as a typed DataFrame.

        Args:
            container_id: Container ID.
            container_type: "Vials" or "Plates".
            plate_ids: Filter to specific plates (only for Plates type).
            dump_dir: If set, write the DataFrame to a CSV in this directory.
            filename_prefix: If set, prepend to the dump filename.

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
            _base = f"samples_{container_id}_{container_type}.csv"
            _name = f"{filename_prefix}_{_base}" if filename_prefix else _base
            path = dump_dir / _name
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


# =============================================================================
# Feeder uploaders
# =============================================================================


class BfabricFeederUploader:
    """Real uploader using B-Fabric feeder client."""

    def __init__(self, user_client: Bfabric, feeder_client) -> None:
        self._user_client = user_client
        self._feeder_client = feeder_client

    def upload(self, params: CreateWorkunitParams) -> str:
        result = create_workunit(
            user_client=self._user_client,
            feeder_client=self._feeder_client,
            params=params,
        )
        return f"Created [Workunit {result.id}]({result.uri})"


class MockFeederUploader:
    """Mock uploader for local testing."""

    def upload(self, params: CreateWorkunitParams) -> str:
        return (
            f"**[Mock]** Would create workunit in container {params.container_id} "
            f"with {len(params.resources)} resources."
        )


def make_feeder_uploader(user_client: Bfabric, feeder_client) -> BfabricFeederUploader | MockFeederUploader:
    """Return a real uploader if feeder_client is available, otherwise a mock."""
    if feeder_client is not None:
        return BfabricFeederUploader(user_client, feeder_client)
    return MockFeederUploader()
