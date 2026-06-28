"""Catalog of bundled self-contained example parameter files (full reproducible runs).

Unlike the sample-table examples (which carry only samples and still need the
sidebar configured), each entry here is a *complete, self-contained* params JSON —
parameters + samples + an embedded ``resolved_config`` — that regenerates a
specific queue on its own via ``qg <file>.json``, independent of ``qg_configs/``.

B-Fabric-free. Reads the packaged JSONs under ``qg.examples.params`` via
:mod:`importlib.resources` so they load from an installed wheel, and parses them
with the unchanged :func:`qg.params_models.parse_queue_input`.
"""

from __future__ import annotations

from dataclasses import dataclass
from importlib.resources import files

from qg.params_models import QueueInput, parse_queue_input

_PACKAGE = "qg.examples.params"


@dataclass(frozen=True)
class ExampleParams:
    """Metadata for one bundled self-contained example params file."""

    id: str
    label: str
    filename: str
    description: str


_CATALOG: tuple[ExampleParams, ...] = (
    ExampleParams(
        id="repro_proteomics_12",
        label="Proteomics — 12-sample reproducibility run",
        filename="repro_proteomics_12.json",
        description=(
            "Self-contained 12-sample, three-group proteomics run (blocked_uniform). "
            "Regenerates byte-for-byte from this file alone."
        ),
    ),
    ExampleParams(
        id="lipidomics_standard",
        label="Lipidomics — dual-polarity standard run",
        filename="lipidomics_standard.json",
        description=(
            "Self-contained lipidomics run (EXPLORIS_4, dual polarity, Lipidomics.standard). "
            "Reproduces the worked-example acquisition queue."
        ),
    ),
)

_BY_ID = {e.id: e for e in _CATALOG}


def list_example_params() -> list[ExampleParams]:
    """Return the bundled example params files, in display order."""
    return list(_CATALOG)


def read_example_params(example_id: str) -> tuple[ExampleParams, QueueInput]:
    """Return the catalog entry and parsed ``QueueInput`` for ``example_id``.

    Raises:
        KeyError: ``example_id`` is not in the catalog.
    """
    try:
        entry = _BY_ID[example_id]
    except KeyError as exc:
        raise KeyError(f"Unknown example params id: {example_id!r}") from exc
    data = (files(_PACKAGE) / entry.filename).read_bytes()
    return entry, parse_queue_input(data)
