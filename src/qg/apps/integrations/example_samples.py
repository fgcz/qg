"""Catalog of bundled example sample tables for the standalone local app.

B-Fabric-free. Reads the packaged tables under ``qg.examples.sample_tables`` via
:mod:`importlib.resources` so they load from an installed wheel, and returns raw
bytes + filename so the caller can run them through the unchanged
:func:`qg.apps.integrations.local_samples.parse_sample_table`.

The ``mode`` field is a *cross-check* only: ``parse_sample_table`` derives the
real mode from the table shape. Tests assert the two agree; app logic must not
branch on ``ExampleSampleTable.mode``.
"""

from __future__ import annotations

from dataclasses import dataclass
from importlib.resources import files

_PACKAGE = "qg.examples.sample_tables"


@dataclass(frozen=True)
class ExampleSampleTable:
    """Metadata for one bundled example sample table."""

    id: str
    label: str
    filename: str
    mode: str  # "vial" | "plate" — cross-check only (parse_sample_table is authority)
    description: str
    recommended_settings: dict[str, str]


_PROTEOMICS_VANQUISH = {
    "tech_area": "Proteomics",
    "instrument": "ASTRAL_1",
    "sampler": "Vanquish",
}

_CATALOG: tuple[ExampleSampleTable, ...] = (
    ExampleSampleTable(
        id="vial_5x5",
        label="Vial — 25 samples (5×5)",
        filename="vial_samples_5x5.csv",
        mode="vial",
        description="Small single-container vial run; the simplest starting point.",
        recommended_settings={**_PROTEOMICS_VANQUISH, "queue_type": "Vial"},
    ),
    ExampleSampleTable(
        id="plate_5x5",
        label="Plate — 25 samples (5×5)",
        filename="plate_samples_5x5.csv",
        mode="plate",
        description="Small single-plate run; the simplest plate starting point.",
        recommended_settings={**_PROTEOMICS_VANQUISH, "queue_type": "Plate"},
    ),
    ExampleSampleTable(
        id="vial_80",
        label="Vial — 80 samples",
        filename="vial_samples_80.csv",
        mode="vial",
        description="Larger vial run for randomization and tray-spanning examples.",
        recommended_settings={**_PROTEOMICS_VANQUISH, "queue_type": "Vial"},
    ),
    ExampleSampleTable(
        id="plate_80",
        label="Plate — 80 samples",
        filename="plate_samples_80.csv",
        mode="plate",
        description="Larger plate run for randomization and plate visualization.",
        recommended_settings={**_PROTEOMICS_VANQUISH, "queue_type": "Plate"},
    ),
    ExampleSampleTable(
        id="multi_project",
        label="Vial — 3 projects (multi-container)",
        filename="multi_container_samples.csv",
        mode="vial",
        description="Three project/order groups in one vial upload.",
        recommended_settings={**_PROTEOMICS_VANQUISH, "queue_type": "Vial"},
    ),
)

_BY_ID = {e.id: e for e in _CATALOG}


def list_example_sample_tables() -> list[ExampleSampleTable]:
    """Return the bundled example sample tables, in display order."""
    return list(_CATALOG)


def read_example_sample_table(example_id: str) -> tuple[ExampleSampleTable, bytes]:
    """Return the catalog entry and raw bytes for ``example_id``.

    Args:
        example_id: One of the ids from :func:`list_example_sample_tables`.

    Returns:
        ``(entry, data)`` where ``data`` is the raw file bytes, ready to pass to
        ``parse_sample_table(data, entry.filename)``.

    Raises:
        KeyError: ``example_id`` is not in the catalog.
    """
    try:
        entry = _BY_ID[example_id]
    except KeyError as exc:
        raise KeyError(f"Unknown example sample table id: {example_id!r}") from exc
    data = (files(_PACKAGE) / entry.filename).read_bytes()
    return entry, data
