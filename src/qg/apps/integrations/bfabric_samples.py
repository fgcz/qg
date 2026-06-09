"""B-Fabric sample source for the portal queue app.

Wraps the ``BfabricHelper`` calls that the portal notebook's source cells used to
inline, so the cells stay thin and the logic is reusable/testable. The B-Fabric
client is passed in (these functions do not import B-Fabric at module top); the
order browser/cache widgets remain in the notebook because they are marimo UI.
"""

from __future__ import annotations

from pathlib import Path
from typing import Any

import polars as pl

from qg.artifacts import build_timestamp


def container_composition(
    bfabric: Any, selected_orders: list[tuple[int, Any]]
) -> tuple[dict[int, Any], bool | None, bool | None]:
    """Per-container plate entities plus a vials/plates classification.

    Returns ``(all_plates, has_plates, has_vials)``; the two flags are ``None``
    until an order is selected (matching the portal's prior behavior).
    """
    all_plates: dict[int, Any] = {}
    has_plates: bool | None = None
    has_vials: bool | None = None
    if selected_orders:
        has_plates = False
        has_vials = False
        for container_id, *_ in selected_orders:
            all_plates[container_id] = bfabric.get_plates(container_id)
            comp = bfabric.get_container_composition(container_id)
            has_plates = has_plates or comp.has_plates
            has_vials = has_vials or comp.has_vials
    return all_plates, has_plates, has_vials


def load_samples(
    bfabric: Any,
    selected_orders: list[tuple[int, Any]],
    container_type: str,
    first_plate_ids: list[int] | None,
    dump_dir: Path,
) -> pl.DataFrame:
    """Load and concatenate samples for every selected order.

    ``first_plate_ids`` filters only the first order's plates (the portal's
    current single-order plate-selection behavior). Returns an empty frame when
    no orders are selected.
    """
    frames: list[pl.DataFrame] = []
    if selected_orders:
        ts = build_timestamp()
        for container_id, *_ in selected_orders:
            plate_ids = first_plate_ids if container_id == selected_orders[0][0] and first_plate_ids else None
            df = bfabric.get_samples(container_id, container_type, plate_ids, dump_dir=dump_dir, filename_prefix=ts)
            if not df.is_empty():
                frames.append(df.with_columns(pl.lit(container_id).alias("container_id")))
    if frames:
        return pl.concat(frames, how="vertical_relaxed").sort(["container_id", "sample_id"])
    return pl.DataFrame()
