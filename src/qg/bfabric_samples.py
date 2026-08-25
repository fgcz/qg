"""Retrieve B-Fabric samples as typed vial or plate tables."""

from __future__ import annotations

from collections.abc import Mapping, Sequence
from dataclasses import dataclass
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


@dataclass(frozen=True, slots=True)
class _ContainerSamples:
    container_id: int
    table: pl.DataFrame


@dataclass(frozen=True, slots=True)
class _OrderContainerSamples:
    container_id: int
    table: pl.DataFrame
    plates: Mapping[EntityUri, Plate]
    order_items: OrderItemRefs


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


def _sample_ids(table: pl.DataFrame) -> set[int]:
    if table.is_empty():
        return set()
    return set(table["id"].to_list())


def _vial_table(
    container: _ContainerSamples,
    *,
    included_ids: set[int],
    on_plate: set[int],
) -> VialSampleTable:
    rows = [
        VialSampleRow(
            sample_name=row["name"],
            sample_id=row["id"],
            tube_id=row.get("tubeid"),
            container_id=container.container_id,
            grouping_var=row.get("groupingvar_name"),
            sample_type=row.get("type"),
        )
        for row in container.table.iter_rows(named=True)
        if row["id"] in included_ids and row["id"] not in on_plate
    ]
    return VialSampleTable.from_rows(rows)


def _dump(table: pl.DataFrame, dump_dir: Path | None, *, mode: str) -> None:
    if dump_dir is None or table.is_empty():
        return
    dump_dir.mkdir(parents=True, exist_ok=True)
    path = dump_dir / f"{build_timestamp()}_samples_{mode}.csv"
    table.write_csv(path)
    logger.info("Dumped {} samples to {}", len(table), path)


@dataclass(frozen=True, slots=True)
class AllContainerSamplesSelection:
    """Loaded container samples presented as a Vial queue."""

    containers: tuple[_ContainerSamples, ...]

    @property
    def plates(self) -> dict[int, Mapping[EntityUri, Plate]]:
        """Return no selectable plates because this source presents all rows as vials."""
        return {container.container_id: {} for container in self.containers}

    @property
    def composition(self) -> ContainerComposition:
        """Return the Vial-only composition of the loaded containers."""
        return ContainerComposition(
            has_plates=False,
            has_vials=any(not container.table.is_empty() for container in self.containers),
        )

    @property
    def fallback_container_ids(self) -> tuple[int, ...]:
        """Return no fallbacks because this source explicitly requests container samples."""
        return ()

    def vial_samples(self, *, dump_dir: Path | None = None) -> VialSampleTable:
        """Return every loaded container sample as a vial."""
        tables = [
            _vial_table(container, included_ids=_sample_ids(container.table), on_plate=set())
            for container in self.containers
        ]
        result = VialSampleTable.concat(tables)
        _dump(result.table, dump_dir, mode="vials")
        return result

    def plate_samples(
        self,
        *,
        plate_ids: Mapping[int, frozenset[int]],  # noqa: ARG002
        dump_dir: Path | None = None,
    ) -> PlateSampleTable:
        """Return no plate rows because this source always presents samples as vials."""
        result = PlateSampleTable.from_rows([])
        _dump(result.table, dump_dir, mode="plates")
        return result


@dataclass(frozen=True, slots=True)
class OrderItemSamplesSelection:
    """Loaded order-item samples with their physical Plate/Vial placement."""

    containers: tuple[_OrderContainerSamples, ...]
    restrict_plate_samples_to_container: bool

    @staticmethod
    def _included_ids(container: _OrderContainerSamples) -> set[int]:
        container_ids = _sample_ids(container.table)
        if container.order_items.is_empty:
            return container_ids
        referenced_plates = {
            uri: plate for uri, plate in container.plates.items() if _plate_id(uri) in container.order_items.plate_ids
        }
        ordered_ids = set(container.order_items.sample_ids) | _referenced_sample_ids(referenced_plates)
        return ordered_ids & container_ids

    @staticmethod
    def _selectable_plates(container: _OrderContainerSamples) -> dict[EntityUri, Plate]:
        if container.order_items.is_empty:
            return dict(container.plates)
        return {
            uri: plate
            for uri, plate in container.plates.items()
            if _plate_id(uri) in container.order_items.plate_ids
            or bool(_referenced_sample_ids({uri: plate}) & set(container.order_items.sample_ids))
        }

    @property
    def plates(self) -> dict[int, Mapping[EntityUri, Plate]]:
        """Return selectable order-item plates by container."""
        return {container.container_id: self._selectable_plates(container) for container in self.containers}

    @property
    def composition(self) -> ContainerComposition:
        """Return whether the loaded order items contribute plates, vials, or both."""
        has_plates = False
        has_vials = False
        for container in self.containers:
            included_ids = self._included_ids(container)
            plate_ids = _referenced_sample_ids(container.plates)
            has_plates = has_plates or bool(included_ids & plate_ids)
            has_vials = has_vials or bool(included_ids - plate_ids)
        return ContainerComposition(has_plates=has_plates, has_vials=has_vials)

    @property
    def fallback_container_ids(self) -> tuple[int, ...]:
        """Return containers whose empty order items fall back to container samples."""
        return tuple(container.container_id for container in self.containers if container.order_items.is_empty)

    def vial_samples(self, *, dump_dir: Path | None = None) -> VialSampleTable:
        """Return included off-plate samples as vials."""
        tables = [
            _vial_table(
                _ContainerSamples(container.container_id, container.table),
                included_ids=self._included_ids(container),
                on_plate=_referenced_sample_ids(container.plates),
            )
            for container in self.containers
        ]
        result = VialSampleTable.concat(tables)
        _dump(result.table, dump_dir, mode="vials")
        return result

    def plate_samples(
        self,
        *,
        plate_ids: Mapping[int, frozenset[int]],
        dump_dir: Path | None = None,
    ) -> PlateSampleTable:
        """Return included plate-resident samples, optionally narrowed by plate ID."""
        tables: list[PlateSampleTable] = []
        for container in self.containers:
            rows: list[PlateSampleRow] = []
            included_ids = self._included_ids(container)
            allowed_ids = _sample_ids(container.table) if self.restrict_plate_samples_to_container else None
            selected_plate_ids = plate_ids.get(container.container_id)
            for uri, plate in self._selectable_plates(container).items():
                plate_id = _plate_id(uri)
                if selected_plate_ids is not None and plate_id not in selected_plate_ids:
                    continue
                samples = cast(Sequence[dict[str, Any]], plate.refs.sample)
                for sample in samples:
                    sample_id = int(sample["id"])
                    if sample_id not in included_ids:
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
                            container_id=container.container_id,
                            grid_position=sample["_gridposition"],
                            plate_id=plate_id,
                            grouping_var=grouping_var,
                            sample_type=sample.get("type"),
                        )
                    )
            tables.append(PlateSampleTable.from_rows(rows))
        result = PlateSampleTable.concat(tables)
        _dump(result.table, dump_dir, mode="plates")
        return result


BfabricSampleSelection = AllContainerSamplesSelection | OrderItemSamplesSelection


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

    def _read_order_items(self, container_id: int) -> OrderItemRefs:
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

    def load_sample_selection(
        self,
        container_ids: Sequence[int],
        *,
        source: SampleSource,
    ) -> BfabricSampleSelection:
        """Load one bounded selection for network-free sample derivation."""
        if source is SampleSource.CONTAINER:
            return AllContainerSamplesSelection(
                tuple(
                    _ContainerSamples(container_id, self._read_container_samples(container_id))
                    for container_id in container_ids
                )
            )
        return OrderItemSamplesSelection(
            containers=tuple(
                _OrderContainerSamples(
                    container_id=container_id,
                    table=self._read_container_samples(container_id),
                    plates=self._get_container_plates(container_id),
                    order_items=self._read_order_items(container_id),
                )
                for container_id in container_ids
            ),
            restrict_plate_samples_to_container=self._restrict_to_container_id is not None,
        )

    def _get_container_plates(self, container_id: int) -> dict[EntityUri, Plate]:
        plates = self.client.reader.query(
            "plate",
            {"containerid": container_id},
            expected_type=Plate,
        )
        return {uri: plate for uri, plate in plates.items() if not _is_storage_plate(plate)}

    def _read_container_samples(self, container_id: int) -> pl.DataFrame:
        return self.client.read("sample", {"containerid": container_id}, max_results=None).to_polars(flatten=True)
