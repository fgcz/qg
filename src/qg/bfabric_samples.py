"""Retrieve B-Fabric samples as typed vial or plate tables."""

from __future__ import annotations

from collections.abc import Mapping, Sequence
from enum import StrEnum
from pathlib import Path
from typing import Any, NamedTuple, cast

import polars as pl
from bfabric import Bfabric
from bfabric.entities import Plate
from bfabric.entities.core.uri import EntityUri
from loguru import logger

from qg.artifacts import build_timestamp
from qg.sample_rows import (
    PlateSampleRow,
    PlateSampleTable,
    VialSampleRow,
    VialSampleTable,
)

STORAGE_PLATE_TYPE = "Storage"


class SampleSource(StrEnum):
    """B-Fabric records that determine which samples enter a queue."""

    ORDER_ITEMS = "Order items"
    CONTAINER = "All container samples"


class OrderItemRefs(NamedTuple):
    """Sample and plate identifiers referenced by one order's line items."""

    sample_ids: frozenset[int]
    plate_ids: frozenset[int]

    @property
    def is_empty(self) -> bool:
        """Return whether the order has no line items."""
        return not self.sample_ids and not self.plate_ids


class ContainerComposition(NamedTuple):
    """Whether selected containers contribute plate samples, vials, or both."""

    has_plates: bool
    has_vials: bool


def _is_storage_plate(plate: Plate) -> bool:
    """Return whether a plate is a non-injectable B-Fabric storage plate."""
    plate_type = str(plate.get("type") or "").strip().casefold()
    return plate_type == STORAGE_PLATE_TYPE.casefold()


def _plate_id(uri: EntityUri) -> int:
    return uri.components.entity_id


def _referenced_sample_ids(plates: Mapping[EntityUri, Plate]) -> set[int]:
    """Return plate sample IDs without resolving referenced entities."""
    sample_ids: set[int] = set()
    for plate in plates.values():
        sample_uris = plate.refs.uris["sample"]
        if isinstance(sample_uris, EntityUri):
            raise TypeError("Plate sample references must be plural")
        sample_ids.update(uri.components.entity_id for uri in sample_uris)
    return sample_ids


class BfabricHelper:
    """Load and classify B-Fabric samples for queue generation."""

    def __init__(
        self,
        client: Bfabric,
        *,
        restrict_to_container_id: int | None = None,
    ) -> None:
        self.client = client
        self._restrict_to_container_id = restrict_to_container_id

    def get_order_items(self, container_id: int) -> OrderItemRefs:
        """Return sample and plate references from an order's billable items.

        A B-Fabric order item must reference exactly one sample or one plate.
        Projects and orders without line items return empty reference sets.
        """
        result = self.client.read("orderitem", {"orderid": container_id}, max_results=None)
        table = result.to_polars(flatten=True)
        if table.is_empty():
            return OrderItemRefs(frozenset(), frozenset())

        sample_ids: set[int] = set()
        plate_ids: set[int] = set()
        for row in table.iter_rows(named=True):
            sample_id = row.get("sample_id")
            plate_id = row.get("plate_id")
            if (sample_id is None) == (plate_id is None):
                item_id = row.get("id", "unknown")
                raise ValueError(f"Order item {item_id} must reference exactly one sample or plate")
            if sample_id is not None:
                sample_ids.add(int(sample_id))
            else:
                assert plate_id is not None
                plate_ids.add(int(plate_id))
        return OrderItemRefs(frozenset(sample_ids), frozenset(plate_ids))

    def get_plates(
        self,
        container_id: int,
        *,
        source: SampleSource,
    ) -> dict[EntityUri, Plate]:
        """Return injectable plates that can contribute samples under ``source``."""
        plates = self._get_container_plates(container_id)
        if source is SampleSource.CONTAINER:
            return plates

        refs = self.get_order_items(container_id)
        if refs.is_empty:
            return plates

        return {
            uri: plate
            for uri, plate in plates.items()
            if _plate_id(uri) in refs.plate_ids or bool(_referenced_sample_ids({uri: plate}) & set(refs.sample_ids))
        }

    def get_container_composition(
        self,
        container_ids: Sequence[int],
        *,
        source: SampleSource,
    ) -> ContainerComposition:
        """Classify the queue types contributed by the selected source.

        All container samples are deliberately presented as vials. Order-item
        samples retain their B-Fabric plate/vial placement.
        """
        if source is SampleSource.CONTAINER:
            return ContainerComposition(
                has_plates=False,
                has_vials=any(self._fetch_container_sample_ids(container_id) for container_id in container_ids),
            )

        has_plates = False
        has_vials = False
        for container_id in container_ids:
            plates = self._get_container_plates(container_id)
            all_sample_ids = self._fetch_container_sample_ids(container_id)
            if self.get_order_items(container_id).is_empty:
                has_vials = has_vials or bool(all_sample_ids)
                continue
            included_ids = self._included_sample_ids(
                container_id,
                source=source,
                plates=plates,
                container_sample_ids=all_sample_ids,
            )
            plate_sample_ids = _referenced_sample_ids(plates)
            has_plates = has_plates or bool(included_ids & plate_sample_ids)
            has_vials = has_vials or bool(included_ids - plate_sample_ids)
        return ContainerComposition(has_plates=has_plates, has_vials=has_vials)

    def get_vial_samples(
        self,
        container_ids: Sequence[int],
        *,
        source: SampleSource,
        dump_dir: Path | None = None,
    ) -> VialSampleTable:
        """Load off-plate samples from all selected containers."""
        tables = [self._load_vial_samples(container_id, source=source) for container_id in container_ids]
        result = VialSampleTable.concat(tables)
        self._dump(result.table, dump_dir, mode="vials")
        return result

    def get_plate_samples(
        self,
        container_ids: Sequence[int],
        *,
        plate_ids: Mapping[int, frozenset[int]],
        source: SampleSource,
        dump_dir: Path | None = None,
    ) -> PlateSampleTable:
        """Load plate-resident samples from all selected containers."""
        tables = [
            self._load_plate_samples(
                container_id,
                selected_plate_ids=plate_ids.get(container_id),
                source=source,
            )
            for container_id in container_ids
        ]
        result = PlateSampleTable.concat(tables)
        self._dump(result.table, dump_dir, mode="plates")
        return result

    def _get_container_plates(self, container_id: int) -> dict[EntityUri, Plate]:
        plates = self.client.reader.query(
            "plate",
            {"containerid": container_id},
            expected_type=Plate,
        )
        return {uri: plate for uri, plate in plates.items() if not _is_storage_plate(plate)}

    def _read_container_samples(self, container_id: int) -> pl.DataFrame:
        return self.client.read("sample", {"containerid": container_id}, max_results=None).to_polars(flatten=True)

    def _fetch_container_sample_ids(self, container_id: int) -> set[int]:
        table = self._read_container_samples(container_id)
        if table.is_empty():
            return set()
        return set(table["id"].to_list())

    def _included_sample_ids(
        self,
        container_id: int,
        *,
        source: SampleSource,
        plates: Mapping[EntityUri, Plate],
        container_sample_ids: set[int],
    ) -> set[int]:
        if source is SampleSource.CONTAINER:
            return container_sample_ids

        refs = self.get_order_items(container_id)
        if refs.is_empty:
            return container_sample_ids

        referenced_plates = {uri: plate for uri, plate in plates.items() if _plate_id(uri) in refs.plate_ids}
        ordered_ids = set(refs.sample_ids) | _referenced_sample_ids(referenced_plates)
        return ordered_ids & container_sample_ids

    def _load_vial_samples(
        self,
        container_id: int,
        *,
        source: SampleSource,
    ) -> VialSampleTable:
        plates = self._get_container_plates(container_id)
        refs = self.get_order_items(container_id) if source is SampleSource.ORDER_ITEMS else None
        on_plate = _referenced_sample_ids(plates) if refs is not None and not refs.is_empty else set()
        table = self._read_container_samples(container_id)
        if table.is_empty():
            return VialSampleTable.from_rows([])
        included_ids = self._included_sample_ids(
            container_id,
            source=source,
            plates=plates,
            container_sample_ids=set(table["id"].to_list()),
        )
        rows = [
            VialSampleRow(
                sample_name=row["name"],
                sample_id=row["id"],
                tube_id=row.get("tubeid"),
                container_id=container_id,
                grouping_var=row.get("groupingvar_name"),
                sample_type=row.get("type"),
            )
            for row in table.iter_rows(named=True)
            if row["id"] in included_ids and row["id"] not in on_plate
        ]
        return VialSampleTable.from_rows(rows)

    def _load_plate_samples(
        self,
        container_id: int,
        *,
        selected_plate_ids: frozenset[int] | None,
        source: SampleSource,
    ) -> PlateSampleTable:
        plates = self.get_plates(container_id, source=source)
        refs = (
            self.get_order_items(container_id)
            if source is SampleSource.ORDER_ITEMS
            else OrderItemRefs(frozenset(), frozenset())
        )
        source_ids: set[int] | None = None
        if source is SampleSource.ORDER_ITEMS and not refs.is_empty:
            referenced_plates = {uri: plate for uri, plate in plates.items() if _plate_id(uri) in refs.plate_ids}
            source_ids = set(refs.sample_ids) | _referenced_sample_ids(referenced_plates)

        allowed_ids = (
            self._fetch_container_sample_ids(container_id) if self._restrict_to_container_id is not None else None
        )
        rows: list[PlateSampleRow] = []
        for uri, plate in plates.items():
            plate_id = _plate_id(uri)
            if selected_plate_ids is not None and plate_id not in selected_plate_ids:
                continue
            samples = cast(Sequence[dict[str, Any]], plate.refs.sample)
            for sample in samples:
                sample_id = int(sample["id"])
                if source_ids is not None and sample_id not in source_ids:
                    continue
                if allowed_ids is not None and sample_id not in allowed_ids:
                    continue
                grouping_var = sample.get("groupingvar")
                if isinstance(grouping_var, dict):
                    grouping_var = grouping_var.get("name")
                rows.append(
                    PlateSampleRow(
                        sample_name=sample["name"],
                        sample_id=sample_id,
                        container_id=container_id,
                        grid_position=sample["_gridposition"],
                        plate_id=plate_id,
                        grouping_var=grouping_var,
                        sample_type=sample.get("type"),
                    )
                )
        return PlateSampleTable.from_rows(rows)

    @staticmethod
    def _dump(
        table: pl.DataFrame,
        dump_dir: Path | None,
        *,
        mode: str,
    ) -> None:
        if dump_dir is None or table.is_empty():
            return
        dump_dir.mkdir(parents=True, exist_ok=True)
        path = dump_dir / f"{build_timestamp()}_samples_{mode}.csv"
        table.write_csv(path)
        logger.info("Dumped {} samples to {}", len(table), path)
