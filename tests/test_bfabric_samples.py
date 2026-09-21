"""Tests for B-Fabric order-item and container sample retrieval."""

from unittest.mock import MagicMock

import polars as pl
import pytest

pytest.importorskip("bfabric")

from bfabric.entities.core.uri import EntityUri  # noqa: E402

from qg.bfabric_samples import (  # noqa: E402
    BfabricHelper,
    BfabricSampleSelection,
    ContainerComposition,
    SamplePlacement,
    SampleSource,
)
from qg.sample_rows import PlateSampleRow, PlateSampleTable, VialSampleRow, VialSampleTable  # noqa: E402

pytestmark = pytest.mark.bfabric

_INSTANCE = "https://fgcz-bfabric.uzh.ch/bfabric/"


class _ReadResult:
    def __init__(self, rows: list[dict]) -> None:
        self._rows = rows

    def __len__(self) -> int:
        return len(self._rows)

    def to_polars(self, *, flatten: bool = False) -> pl.DataFrame:
        if not self._rows:
            return pl.DataFrame()
        rows = []
        for row in self._rows:
            flat = {}
            for key, value in row.items():
                if flatten and isinstance(value, dict):
                    flat.update({f"{key}_{nested}": item for nested, item in value.items()})
                else:
                    flat[key] = value
            rows.append(flat)
        return pl.DataFrame(rows, infer_schema_length=None)


class _Refs:
    def __init__(self, samples: list[dict]) -> None:
        self.sample = samples
        self.uris = {"sample": [EntityUri.from_components(_INSTANCE, "sample", sample["id"]) for sample in samples]}


class _Plate:
    def __init__(self, samples: list[dict], plate_type: str | None = None) -> None:
        self.refs = _Refs(samples)
        self._type = plate_type

    def get(self, key: str, default: object = None) -> object:
        return self._type if key == "type" else default


def _uri(entity: str, entity_id: int) -> EntityUri:
    return EntityUri.from_components(_INSTANCE, entity, entity_id)


def _sample(
    sample_id: int,
    *,
    name: str | None = None,
    grid_position: str | None = None,
    sample_type: str | None = "Biological Sample",
    parent_id: int | None = None,
) -> dict:
    row = {
        "id": sample_id,
        "name": name or f"S{sample_id}",
        "tubeid": f"T{sample_id}",
        "groupingvar": None,
    }
    if grid_position is not None:
        row["_gridposition"] = grid_position
    if sample_type is not None:
        row["type"] = sample_type
    if parent_id is not None:
        row["parent"] = [{"classname": "sample", "id": parent_id}]
    return row


def _client(
    *,
    samples: dict[int, list[dict]],
    order_items: dict[int, list[dict]] | None = None,
    plates: dict[int, dict[EntityUri, _Plate]] | None = None,
) -> MagicMock:
    client = MagicMock()
    order_items = order_items or {}
    plates = plates or {}

    def read(endpoint: str, query: dict, max_results: int | None = None) -> _ReadResult:  # noqa: ARG001
        if endpoint == "sample":
            return _ReadResult(samples.get(int(query["containerid"]), []))
        if endpoint == "orderitem":
            return _ReadResult(order_items.get(int(query["orderid"]), []))
        raise AssertionError(f"Unexpected endpoint: {endpoint}")

    client.read.side_effect = read
    client.reader.query.side_effect = lambda endpoint, query, expected_type: plates.get(int(query["containerid"]), {})
    return client


def test_sample_tables_apply_declared_schema_to_all_null_optional_fields() -> None:
    vial = VialSampleTable.from_rows([VialSampleRow(sample_name="S", sample_id=1, container_id=10)])
    plate = PlateSampleTable.from_rows(
        [
            PlateSampleRow(
                sample_name="S",
                sample_id=1,
                container_id=10,
                grid_position="A1",
                plate_id=20,
            )
        ]
    )

    assert vial.table.schema["tube_id"] == pl.String
    assert vial.table.schema["grouping_var"] == pl.String
    assert vial.table.schema["sample_type"] == pl.String
    assert plate.table.schema["tray"] == pl.String
    assert plate.table.schema["sample_type"] == pl.String
    assert set(VialSampleTable.from_rows([]).table.columns) == set(VialSampleRow.model_fields)
    assert set(PlateSampleTable.from_rows([]).table.columns) == set(PlateSampleRow.model_fields)


def test_storage_plates_are_excluded_and_their_samples_become_vials() -> None:
    client = _client(
        samples={10: [_sample(1)]},
        plates={
            10: {
                _uri("plate", 20): _Plate(
                    [_sample(1, grid_position="A1")],
                    plate_type="storage",
                )
            }
        },
    )
    helper = BfabricHelper(client)
    selection = helper.load_sample_selection([10], source=SampleSource.ORDER_ITEMS)

    assert selection.plates == {10: {}}
    assert selection.composition == ContainerComposition(has_plates=False, has_vials=True)
    assert selection.plate_samples(plate_ids={}).table.is_empty()


def test_container_source_keeps_plate_placement() -> None:
    client = _client(
        samples={10: [_sample(1), _sample(2)]},
        plates={10: {_uri("plate", 20): _Plate([_sample(1, grid_position="A1")])}},
    )
    selection = BfabricHelper(client).load_sample_selection([10], source=SampleSource.CONTAINER)

    assert selection.composition == ContainerComposition(has_plates=True, has_vials=True)
    assert {uri.components.entity_id for uri in selection.plates[10]} == {20}
    assert selection.vial_samples().table["sample_id"].to_list() == [2]
    assert selection.plate_samples(plate_ids={}).table["sample_id"].to_list() == [1]
    assert selection.fallback_container_ids == ()


def test_get_order_items_resolves_sample_and_plate_references() -> None:
    client = _client(
        samples={10: [_sample(101), _sample(5), _sample(6)]},
        order_items={
            10: [
                {"id": 1, "sample": {"id": 101}},
                {"id": 2, "plate": {"id": 202}},
            ]
        },
        plates={10: {_uri("plate", 202): _Plate([_sample(5, grid_position="A1")])}},
    )

    selection = BfabricHelper(client).load_sample_selection([10], source=SampleSource.ORDER_ITEMS)
    assert isinstance(selection, BfabricSampleSelection)
    container = selection.containers[0]

    assert container.included_ids == frozenset({101, 5})
    assert container.ordered_plate_ids == frozenset({202})
    assert not container.fell_back


@pytest.mark.parametrize(
    "item",
    [
        {"id": 1},
        {"id": 1, "sample": {"id": 101}, "plate": {"id": 202}},
    ],
)
def test_get_order_items_rejects_malformed_reference_shape(item: dict) -> None:
    helper = BfabricHelper(_client(samples={}, order_items={10: [item]}))

    with pytest.raises(ValueError, match="exactly one sample or plate"):
        helper.load_sample_selection([10], source=SampleSource.ORDER_ITEMS)


def test_order_item_source_filters_vials_and_preserves_missing_sample_type() -> None:
    samples = {
        10: [
            _sample(1, sample_type=None),
            _sample(2, sample_type="Quality Control Sample"),
        ]
    }
    client = _client(
        samples=samples,
        order_items={10: [{"id": 1, "sample": {"id": 1}}]},
    )
    helper = BfabricHelper(client)

    ordered = helper.load_sample_selection([10], source=SampleSource.ORDER_ITEMS).vial_samples().table
    container = helper.load_sample_selection([10], source=SampleSource.CONTAINER).vial_samples().table

    assert ordered["sample_id"].to_list() == [1]
    assert ordered["sample_type"].to_list() == [None]
    assert container["sample_id"].to_list() == [1, 2]


def test_empty_order_items_fallback_preserves_plates() -> None:
    client = _client(
        samples={10: [_sample(1), _sample(2)]},  # 1 on plate, 1 off-plate
        plates={10: {_uri("plate", 20): _Plate([_sample(1, grid_position="A1")])}},
    )
    helper = BfabricHelper(client)
    selection = helper.load_sample_selection([10], source=SampleSource.ORDER_ITEMS)

    assert selection.composition == ContainerComposition(has_plates=True, has_vials=True)

    # The plate-resident sample is NOT flattened into vials on the fallback.
    vials = selection.vial_samples().table
    assert vials["sample_id"].to_list() == [2]
    assert "grid_position" not in vials.columns

    # It IS available as a plate sample with its placement preserved.
    plates = selection.plate_samples(plate_ids={}).table
    assert plates["sample_id"].to_list() == [1]
    assert plates["grid_position"].to_list() == ["A1"]
    assert plates["plate_id"].to_list() == [20]

    # The explicit "all container samples" source yields the same placement
    # without reporting a fallback.
    container_selection = helper.load_sample_selection([10], source=SampleSource.CONTAINER)
    assert container_selection.vial_samples().table["sample_id"].to_list() == [2]
    assert container_selection.plate_samples(plate_ids={}).table["sample_id"].to_list() == [1]
    assert selection.fallback_container_ids == (10,)
    assert container_selection.fallback_container_ids == ()


def test_empty_order_items_fallback_offers_plates_when_plates_present() -> None:
    client = _client(
        samples={10: [_sample(1)]},
        plates={10: {_uri("plate", 20): _Plate([_sample(1, grid_position="A1")])}},
    )
    selection = BfabricHelper(client).load_sample_selection([10], source=SampleSource.ORDER_ITEMS)
    assert selection.composition == ContainerComposition(has_plates=True, has_vials=False)


def test_order_item_source_offers_referenced_and_sample_containing_plates() -> None:
    plates = {
        10: {
            _uri("plate", 20): _Plate([_sample(1, grid_position="A1")]),
            _uri("plate", 21): _Plate([_sample(2, grid_position="A1")]),
            _uri("plate", 22): _Plate([_sample(3, grid_position="A1")]),
        }
    }
    client = _client(
        samples={10: [_sample(1), _sample(2), _sample(3)]},
        order_items={
            10: [
                {"id": 1, "sample": {"id": 1}},
                {"id": 2, "plate": {"id": 21}},
            ]
        },
        plates=plates,
    )
    selection = BfabricHelper(client).load_sample_selection([10], source=SampleSource.ORDER_ITEMS)
    selected = selection.plates[10]

    assert {uri.components.entity_id for uri in selected} == {20, 21}


def test_plate_loading_combines_sample_and_plate_order_items() -> None:
    plate_rows = {
        _uri("plate", 20): _Plate([_sample(1, grid_position="A1")]),
        _uri("plate", 21): _Plate([_sample(2, grid_position="A1"), _sample(3, grid_position="A2")]),
        _uri("plate", 22): _Plate([_sample(4, grid_position="A1")]),
    }
    client = _client(
        samples={10: [_sample(i) for i in range(1, 5)]},
        order_items={
            10: [
                {"id": 1, "sample": {"id": 1}},
                {"id": 2, "plate": {"id": 21}},
            ]
        },
        plates={10: plate_rows},
    )
    selection = BfabricHelper(client).load_sample_selection([10], source=SampleSource.ORDER_ITEMS)
    all_ordered = selection.plate_samples(plate_ids={}).table
    picked = selection.plate_samples(plate_ids={10: frozenset({21})}).table

    assert all_ordered["sample_id"].to_list() == [1, 2, 3]
    assert picked["sample_id"].to_list() == [2, 3]


def test_composition_ors_vials_and_plates_across_containers() -> None:
    client = _client(
        samples={10: [_sample(1)], 11: [_sample(2)]},
        order_items={
            10: [{"id": 1, "sample": {"id": 1}}],
            11: [{"id": 2, "sample": {"id": 2}}],
        },
        plates={11: {_uri("plate", 21): _Plate([_sample(2, grid_position="A1")])}},
    )

    selection = BfabricHelper(client).load_sample_selection([10, 11], source=SampleSource.ORDER_ITEMS)

    assert selection.composition == ContainerComposition(has_plates=True, has_vials=True)


def test_restricted_plate_loading_intersects_order_items_with_container_samples() -> None:
    client = _client(
        samples={10: [_sample(1)]},
        order_items={10: [{"id": 1, "plate": {"id": 20}}]},
        plates={
            10: {
                _uri("plate", 20): _Plate(
                    [
                        _sample(1, grid_position="A1"),
                        _sample(99, grid_position="A2"),
                    ]
                )
            }
        },
    )
    selection = BfabricHelper(client, restrict_to_container_id=10).load_sample_selection(
        [10], source=SampleSource.ORDER_ITEMS
    )
    table = selection.plate_samples(plate_ids={}).table

    assert table["sample_id"].to_list() == [1]


def test_plate_selection_mapping_only_narrows_keyed_container() -> None:
    client = _client(
        samples={10: [_sample(1), _sample(2)], 11: [_sample(3)]},
        plates={
            10: {
                _uri("plate", 20): _Plate([_sample(1, grid_position="A1")]),
                _uri("plate", 21): _Plate([_sample(2, grid_position="A1")]),
            },
            11: {_uri("plate", 30): _Plate([_sample(3, grid_position="A1")])},
        },
    )

    selection = BfabricHelper(client).load_sample_selection([11, 10], source=SampleSource.ORDER_ITEMS)
    table = selection.plate_samples(plate_ids={10: frozenset({20})}).table

    assert table["sample_id"].to_list() == [1, 3]


def test_order_item_selection_reads_each_endpoint_once_per_container() -> None:
    client = _client(
        samples={10: [_sample(1)], 11: [_sample(2)]},
        order_items={
            10: [{"id": 1, "sample": {"id": 1}}],
            11: [{"id": 2, "sample": {"id": 2}}],
        },
        plates={11: {_uri("plate", 21): _Plate([_sample(2, grid_position="A1")])}},
    )

    selection = BfabricHelper(client).load_sample_selection([10, 11], source=SampleSource.ORDER_ITEMS)

    assert client.read.call_count == 4  # one sample + one order-item read per container
    assert client.reader.query.call_count == 2  # one plate query per container

    _ = selection.plates
    _ = selection.composition
    _ = selection.fallback_container_ids
    selection.vial_samples()
    selection.plate_samples(plate_ids={})

    assert client.read.call_count == 4
    assert client.reader.query.call_count == 2


def test_container_selection_reads_samples_and_plates_but_no_order_items() -> None:
    client = _client(
        samples={10: [_sample(1)]},
        plates={10: {_uri("plate", 20): _Plate([_sample(1, grid_position="A1")])}},
    )

    selection = BfabricHelper(client).load_sample_selection([10], source=SampleSource.CONTAINER)

    assert client.read.call_count == 1
    assert client.read.call_args.args[0] == "sample"
    assert client.reader.query.call_count == 1

    _ = selection.plates
    _ = selection.composition
    _ = selection.fallback_container_ids
    selection.vial_samples()
    selection.plate_samples(plate_ids={})

    assert client.read.call_count == 1
    assert client.reader.query.call_count == 1


def test_container_samples_are_read_with_their_parents() -> None:
    client = _client(samples={10: [_sample(1)]})

    BfabricHelper(client).load_sample_selection([10], source=SampleSource.CONTAINER)

    assert client.read.call_args.args[1] == {"containerid": 10, "includeparents": True}


def test_order_items_admit_derived_samples_across_generations() -> None:
    """A child plate is selectable when its samples derive from ordered samples.

    Sample 1 is ordered; 2 derives from 1 and 3 from 2. Sample 9 is an unordered
    container sample and 8 derives from it, so neither enters the queue.
    """
    client = _client(
        samples={
            10: [
                _sample(1),
                _sample(2, parent_id=1),
                _sample(3, parent_id=2),
                _sample(9),
                _sample(8, parent_id=9),
            ]
        },
        order_items={10: [{"id": 1, "sample": {"id": 1}}]},
        plates={
            10: {
                _uri("plate", 20): _Plate([_sample(2, grid_position="A1")]),
                _uri("plate", 21): _Plate([_sample(8, grid_position="A1")]),
            }
        },
    )
    selection = BfabricHelper(client).load_sample_selection([10], source=SampleSource.ORDER_ITEMS)

    assert selection.containers[0].included_ids == frozenset({1, 2, 3})
    assert {uri.components.entity_id for uri in selection.plates[10]} == {20}
    assert selection.composition == ContainerComposition(has_plates=True, has_vials=True)
    assert selection.vial_samples().table["sample_id"].to_list() == [1, 3]
    assert selection.plate_samples(plate_ids={}).table["sample_id"].to_list() == [2]


def test_derived_samples_of_an_ordered_plate_are_admitted() -> None:
    client = _client(
        samples={10: [_sample(1), _sample(2, parent_id=1)]},
        order_items={10: [{"id": 1, "plate": {"id": 20}}]},
        plates={10: {_uri("plate", 20): _Plate([_sample(1, grid_position="A1")])}},
    )
    selection = BfabricHelper(client).load_sample_selection([10], source=SampleSource.ORDER_ITEMS)

    assert selection.containers[0].included_ids == frozenset({1, 2})
    assert selection.vial_samples().table["sample_id"].to_list() == [2]


def test_derived_sample_with_parent_outside_the_container_stays_excluded() -> None:
    client = _client(
        samples={10: [_sample(1), _sample(2, parent_id=999)]},
        order_items={10: [{"id": 1, "sample": {"id": 1}}]},
    )
    selection = BfabricHelper(client).load_sample_selection([10], source=SampleSource.ORDER_ITEMS)

    assert selection.containers[0].included_ids == frozenset({1})


def test_placement_counts_injection_plate_storage_box_loose_and_derived_samples() -> None:
    """Ordered samples 1, 2 sit in a storage box; child 3 is on an injection plate; child 4 is loose."""
    client = _client(
        samples={
            10: [
                _sample(1),
                _sample(2),
                _sample(3, parent_id=1),
                _sample(4, parent_id=2),
                _sample(9),
            ]
        },
        order_items={10: [{"id": 1, "sample": {"id": 1}}, {"id": 2, "sample": {"id": 2}}]},
        plates={
            10: {
                _uri("plate", 20): _Plate(
                    [_sample(1, grid_position="A1"), _sample(2, grid_position="A2")], plate_type="Storage"
                ),
                _uri("plate", 21): _Plate([_sample(3, grid_position="A1")]),
            }
        },
    )
    helper = BfabricHelper(client)

    ordered = helper.load_sample_selection([10], source=SampleSource.ORDER_ITEMS)
    everything = helper.load_sample_selection([10], source=SampleSource.CONTAINER)

    assert ordered.placement == SamplePlacement(on_plate=1, in_storage=2, loose=1, derived=2)
    assert everything.placement == SamplePlacement(on_plate=1, in_storage=2, loose=2, derived=0)
    assert ordered.vial_samples().table["sample_id"].to_list() == [1, 2, 4]


def test_generation_counts_and_restriction_follow_the_lineage() -> None:
    """1 and 9 are originals; 2 derives from 1, 3 from 2; 8 derives from 9."""
    client = _client(
        samples={
            10: [
                _sample(1),
                _sample(2, parent_id=1),
                _sample(3, parent_id=2),
                _sample(9),
                _sample(8, parent_id=9),
            ]
        },
        order_items={10: [{"id": 1, "sample": {"id": 1}}]},
        plates={10: {_uri("plate", 20): _Plate([_sample(2, grid_position="A1")])}},
    )
    helper = BfabricHelper(client)

    ordered = helper.load_sample_selection([10], source=SampleSource.ORDER_ITEMS)
    everything = helper.load_sample_selection([10], source=SampleSource.CONTAINER)

    assert ordered.generation_counts == {0: 1, 1: 1, 2: 1}
    assert everything.generation_counts == {0: 2, 1: 2, 2: 1}

    children_only = ordered.restricted_to_generations({1})
    assert children_only.plate_samples(plate_ids={}).table["sample_id"].to_list() == [2]
    assert children_only.vial_samples().table.is_empty()
    assert children_only.placement.derived == 1
    assert ordered.restricted_to_generations(()).vial_samples().table.is_empty()


def test_generation_of_a_pooled_sample_follows_its_deepest_parent() -> None:
    rows = [_sample(1), _sample(2, parent_id=1), _sample(3)]
    rows[2]["parent"] = [{"classname": "sample", "id": 1}, {"classname": "sample", "id": 2}]
    selection = BfabricHelper(_client(samples={10: rows})).load_sample_selection([10], source=SampleSource.CONTAINER)

    assert selection.containers[0].generations == {1: 0, 2: 1, 3: 2}


def test_lineage_cycle_is_rejected() -> None:
    rows = [_sample(1, parent_id=2), _sample(2, parent_id=1)]

    with pytest.raises(ValueError, match="cycle"):
        BfabricHelper(_client(samples={10: rows})).load_sample_selection([10], source=SampleSource.CONTAINER)
