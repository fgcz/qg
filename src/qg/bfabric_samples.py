"""Retrieve B-Fabric samples as typed vial or plate tables."""

from __future__ import annotations

from collections import Counter
from collections.abc import Collection, Mapping, Sequence
from dataclasses import dataclass, replace
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


NO_ORDER_ITEMS = OrderItemRefs(frozenset(), frozenset())


class ContainerComposition(NamedTuple):
    """Whether selected containers contribute plate samples, vials, or both."""

    has_plates: bool
    has_vials: bool


class SamplePlacement(NamedTuple):
    """How many admitted samples sit where, and how many entered through lineage."""

    on_plate: int
    in_storage: int
    loose: int
    derived: int


@dataclass(frozen=True, slots=True)
class _ContainerSelection:
    """One container's samples and plates with the sample IDs the source admits."""

    container_id: int
    table: pl.DataFrame
    plates: Mapping[EntityUri, Plate]
    storage_plates: Mapping[EntityUri, Plate]
    included_ids: frozenset[int]
    derived_ids: frozenset[int]
    generations: Mapping[int, int]
    ordered_plate_ids: frozenset[int]
    fell_back: bool


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


def _parent_ids(table: pl.DataFrame) -> dict[int, list[int]]:
    """Return each container sample's parent sample IDs from the ``parent`` relation."""
    if table.is_empty() or "parent" not in table.columns:
        return {}
    return {
        row["id"]: [int(parent["id"]) for parent in row["parent"] or ()]
        for row in table.select("id", "parent").iter_rows(named=True)
    }


def _generations(table: pl.DataFrame) -> dict[int, int]:
    """Return each container sample's generation.

    A sample without a parent in the container is generation 0; a sample is one
    generation below its deepest in-container parent.
    """
    parent_ids = _parent_ids(table)
    container_ids = _sample_ids(table)
    generations: dict[int, int] = {}
    pending = {sid: [p for p in parent_ids.get(sid, []) if p in container_ids] for sid in container_ids}
    while pending:
        resolved = {
            sid: (1 + max(generations[p] for p in parents)) if parents else 0
            for sid, parents in pending.items()
            if all(p in generations for p in parents)
        }
        if not resolved:
            raise ValueError(f"Sample lineage contains a cycle among samples {sorted(pending)}")
        generations.update(resolved)
        for sid in resolved:
            del pending[sid]
    return generations


def _with_descendants(sample_ids: set[int], parent_ids: Mapping[int, Sequence[int]]) -> set[int]:
    """Extend a sample set with every sample derived, over any number of steps, from a member."""
    included = set(sample_ids)
    while True:
        derived = {
            sample_id
            for sample_id, parents in parent_ids.items()
            if sample_id not in included and not included.isdisjoint(parents)
        }
        if not derived:
            return included
        included |= derived


def _ordered_sample_ids(
    table: pl.DataFrame,
    plates: Mapping[EntityUri, Plate],
    order_items: OrderItemRefs,
) -> tuple[frozenset[int], frozenset[int]]:
    """Return the admitted container samples and the subset that entered through lineage.

    Order items admit the samples they reference, the residents of the plates they
    reference, and every sample derived from those. Without order items every
    container sample is admitted and nothing counts as derived.
    """
    container_ids = _sample_ids(table)
    if order_items.is_empty:
        return frozenset(container_ids), frozenset()
    referenced_plates = {uri: plate for uri, plate in plates.items() if _plate_id(uri) in order_items.plate_ids}
    direct_ids = set(order_items.sample_ids) | _referenced_sample_ids(referenced_plates)
    included_ids = _with_descendants(direct_ids, _parent_ids(table)) & container_ids
    return frozenset(included_ids), frozenset(included_ids - direct_ids)


def _vial_table(
    container: _ContainerSelection,
    *,
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
        if row["id"] in container.included_ids and row["id"] not in on_plate
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
class BfabricSampleSelection:
    """Loaded container samples with their physical Plate/Vial placement."""

    containers: tuple[_ContainerSelection, ...]
    restrict_plate_samples_to_container: bool

    @staticmethod
    def _selectable_plates(container: _ContainerSelection) -> dict[EntityUri, Plate]:
        return {
            uri: plate
            for uri, plate in container.plates.items()
            if _plate_id(uri) in container.ordered_plate_ids
            or not container.included_ids.isdisjoint(_referenced_sample_ids({uri: plate}))
        }

    @property
    def plates(self) -> dict[int, Mapping[EntityUri, Plate]]:
        """Return selectable plates by container."""
        return {container.container_id: self._selectable_plates(container) for container in self.containers}

    @property
    def composition(self) -> ContainerComposition:
        """Return whether the loaded samples contribute plates, vials, or both."""
        has_plates = False
        has_vials = False
        for container in self.containers:
            plate_ids = _referenced_sample_ids(container.plates)
            has_plates = has_plates or bool(container.included_ids & plate_ids)
            has_vials = has_vials or bool(container.included_ids - plate_ids)
        return ContainerComposition(has_plates=has_plates, has_vials=has_vials)

    @property
    def generation_counts(self) -> dict[int, int]:
        """Return how many admitted samples each generation holds, ordered by generation."""
        counts: Counter[int] = Counter()
        for container in self.containers:
            counts.update(container.generations[sample_id] for sample_id in container.included_ids)
        return dict(sorted(counts.items()))

    def restricted_to_generations(self, generations: Collection[int]) -> BfabricSampleSelection:
        """Return a selection admitting only samples of the given generations."""
        keep = set(generations)
        return replace(
            self,
            containers=tuple(
                replace(
                    container,
                    included_ids=frozenset(s for s in container.included_ids if container.generations[s] in keep),
                    derived_ids=frozenset(s for s in container.derived_ids if container.generations[s] in keep),
                )
                for container in self.containers
            ),
        )

    @property
    def placement(self) -> SamplePlacement:
        """Return where the admitted samples sit and how many entered through lineage."""
        on_plate = in_storage = loose = derived = 0
        for container in self.containers:
            plate_ids = container.included_ids & _referenced_sample_ids(container.plates)
            storage_ids = (container.included_ids & _referenced_sample_ids(container.storage_plates)) - plate_ids
            on_plate += len(plate_ids)
            in_storage += len(storage_ids)
            loose += len(container.included_ids - plate_ids - storage_ids)
            derived += len(container.derived_ids)
        return SamplePlacement(on_plate=on_plate, in_storage=in_storage, loose=loose, derived=derived)

    @property
    def fallback_container_ids(self) -> tuple[int, ...]:
        """Return containers whose empty order items fall back to container samples."""
        return tuple(container.container_id for container in self.containers if container.fell_back)

    def vial_samples(self, *, dump_dir: Path | None = None) -> VialSampleTable:
        """Return included off-plate samples as vials."""
        tables = [
            _vial_table(container, on_plate=_referenced_sample_ids(container.plates)) for container in self.containers
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
            allowed_ids = _sample_ids(container.table) if self.restrict_plate_samples_to_container else None
            selected_plate_ids = plate_ids.get(container.container_id)
            for uri, plate in self._selectable_plates(container).items():
                plate_id = _plate_id(uri)
                if selected_plate_ids is not None and plate_id not in selected_plate_ids:
                    continue
                samples = cast(Sequence[dict[str, Any]], plate.refs.sample)
                for sample in samples:
                    sample_id = int(sample["id"])
                    if sample_id not in container.included_ids:
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
        return BfabricSampleSelection(
            containers=tuple(self._load_container(container_id, source) for container_id in container_ids),
            restrict_plate_samples_to_container=self._restrict_to_container_id is not None,
        )

    def _load_container(self, container_id: int, source: SampleSource) -> _ContainerSelection:
        table = self._read_container_samples(container_id)
        all_plates = self._read_container_plates(container_id)
        plates = {uri: plate for uri, plate in all_plates.items() if not _is_storage_plate(plate)}
        storage_plates = {uri: plate for uri, plate in all_plates.items() if _is_storage_plate(plate)}
        order_items = self._read_order_items(container_id) if source is SampleSource.ORDER_ITEMS else NO_ORDER_ITEMS
        included_ids, derived_ids = _ordered_sample_ids(table, plates, order_items)
        return _ContainerSelection(
            container_id=container_id,
            table=table,
            plates=plates,
            storage_plates=storage_plates,
            included_ids=included_ids,
            derived_ids=derived_ids,
            generations=_generations(table),
            ordered_plate_ids=order_items.plate_ids,
            fell_back=source is SampleSource.ORDER_ITEMS and order_items.is_empty,
        )

    def _read_container_plates(self, container_id: int) -> dict[EntityUri, Plate]:
        return self.client.reader.query("plate", {"containerid": container_id}, expected_type=Plate)

    def _read_container_samples(self, container_id: int) -> pl.DataFrame:
        return self.client.read(
            "sample",
            {"containerid": container_id, "includeparents": True},
            max_results=None,
        ).to_polars(flatten=True)
