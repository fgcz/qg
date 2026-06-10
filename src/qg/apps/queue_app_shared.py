"""B-Fabric-free pipeline helpers shared by the portal and local queue apps.

Both ``queue_app.py`` (FGCZ portal) and ``queue_app_local.py`` (standalone upload)
are thin marimo notebooks that declare their own cells; the reusable logic lives
here so it is written once and is unit-testable. Nothing in this module imports
B-Fabric — only ``marimo`` (for the download controls) and the source-neutral core
(``QueueBuilder`` / ``QueueGenerator``).

The notebooks agree on a variable-name contract so the shared cells and the GUI
tests are identical across both: ``sample_df`` -> ``queue_input`` ->
(``generated_queue_df``, ``raw_queue_df``, ``queue_output_str``) ->
``queue_output_filename``.
"""

from __future__ import annotations

import json
from dataclasses import dataclass
from typing import TYPE_CHECKING

import marimo as mo
import polars as pl
from loguru import logger

from qg.generator import QueueGenerator, format_table, write_queue
from qg.params_models import QueueParameters
from qg.queue_builder import QueueBuilder
from qg.randomize import draw_seed

if TYPE_CHECKING:
    from qg.config_models.loader import QGConfiguration
    from qg.params_models import QueueInput


def synthesize_local_orders(full_samples_df: pl.DataFrame) -> list[tuple[int, None]]:
    """Derive the ``selected_orders`` contract from an uploaded sample table.

    Replaces the B-Fabric order browser in the local app: sorts the unique
    ``container_id`` values from the uploaded DataFrame into ascending order and
    returns them as ``(container_id, None)`` tuples — the same shape
    ``selected_orders`` takes in the portal (where the second element is the
    B-Fabric tech-area object).

    Args:
        full_samples_df: Normalized sample DataFrame with a ``container_id`` column.
            An empty frame (no upload yet) produces an empty list.

    Returns:
        Sorted list of ``(container_id, None)`` tuples, one per distinct container.
    """
    if full_samples_df.is_empty():
        return []
    return [(int(c), None) for c in full_samples_df["container_id"].unique().sort().to_list()]


def resolve_output_format(filtered_table: pl.DataFrame, sampler_selected: bool) -> str:
    """Output format implied by the instrument+sampler row (fallback ``xcalibur``)."""
    if sampler_selected and not filtered_table.is_empty():
        formats = filtered_table["output_format"].unique().to_list()
        return formats[0] if formats else "xcalibur"
    return "xcalibur"


def build_queue_input(
    config: QGConfiguration,
    queue_parameters: QueueParameters | None,
    sample_df: pl.DataFrame | None,
    *,
    has_samples_source: bool,
    provenance_instance: str | None = None,
) -> tuple[QueueInput | None, str | None]:
    """Build a ``QueueInput`` once, shared by the Parameters tab and generation.

    Resolves the randomization seed up front for randomized modes so the Parameters
    tab, the params-JSON download, and (portal) the workunit all record the same
    reproducible seed. ``provenance_instance`` is stamped onto
    ``QueueParameters.bfabric_instance`` when given (portal mode); local mode leaves
    it ``None``.

    Args:
        has_samples_source: whether a sample source is present at all (portal:
            ``selected_orders`` truthy; local: a file was uploaded). Guards building
            before the user has provided any input.

    Returns:
        ``(queue_input, error_message)`` — exactly one is non-None on a real attempt.
    """
    if not (queue_parameters and has_samples_source and sample_df is not None):
        return None, None
    try:
        params = queue_parameters
        if params.randomization != "no" and params.seed is None:
            params = params.model_copy(update={"seed": draw_seed()})
        builder = QueueBuilder(config).with_parameters(params)
        if provenance_instance is not None:
            builder = builder.with_bfabric_instance(provenance_instance)
        if not sample_df.is_empty():
            builder = builder.add_samples_from_dataframe(sample_df)
        return builder.build(), None
    except ValueError as exc:
        return None, str(exc)


@dataclass
class GenerationResult:
    """Outcome of a single ``build_rows()`` pass (called exactly once)."""

    generated_df: pl.DataFrame | None = None
    raw_df: pl.DataFrame | None = None
    output_str: str | None = None
    file_extension: str = ".csv"
    error: str | None = None


def generate_queue(
    config: QGConfiguration, queue_input: QueueInput | None, queue_parameters: QueueParameters | None
) -> GenerationResult:
    """Run generation once so preview and downloaded file are identical.

    Randomization is non-deterministic per build, so ``build_rows()`` must be
    called a single time and its result reused for both the preview table and the
    downloaded content.
    """
    result = GenerationResult()
    if queue_input is None:
        return result
    try:
        generator = QueueGenerator(config, queue_input)
        queue_rows = generator.build_rows()
        result.raw_df = queue_rows.to_table()
        result.generated_df = format_table(
            queue_rows, generator.output_format, generator.plate_layout, generator.tech_area
        )
        result.output_str = write_queue(result.generated_df, generator.output_format)
        result.file_extension = generator.file_extension
    except ValueError as exc:
        logger.exception("Queue generation failed")
        err = str(exc)
        if "Not enough trays" in err:
            sampler = queue_parameters.sampler if queue_parameters else "sampler"
            result.error = (
                f"{err}\n\n"
                f"The {sampler} sampler cannot hold this many plates simultaneously. "
                f"Either select fewer plates, or use a sampler with more tray positions "
                f"(e.g., Vanquish has 4 trays)."
            )
        else:
            result.error = err
    return result


def queue_output_filename(
    queue_parameters: QueueParameters | None, container_ids: list[int], file_extension: str
) -> str | None:
    """`<date>_<instrument>_<container ids>.<ext>` — the downloaded queue file name."""
    if queue_parameters is None:
        return None
    ids_str = "_".join(str(c) for c in container_ids) if container_ids else "queue"
    return f"{queue_parameters.date}_{queue_parameters.instrument}_{ids_str}{file_extension}"


def params_json_filename(queue_parameters: QueueParameters, sample_df: pl.DataFrame) -> str:
    """`<tech>_<sampler>_c<container ids>_n<count>.json` — the params download name."""
    ids_str = "_".join(str(c) for c in sample_df["container_id"].unique().sort().to_list())
    sampler = queue_parameters.sampler.replace(".", "_")
    return f"{queue_parameters.tech_area}_{sampler}_c{ids_str}_n{len(sample_df)}.json"


def params_download_button(queue_input: QueueInput, filename: str) -> mo.Html:
    """A `Download Params JSON` button for the resolved ``queue_input``."""
    data = json.dumps(queue_input.model_dump(mode="json"), indent=2)
    return mo.download(data=data.encode("utf-8"), filename=filename, label="Download Params JSON")


def queue_download_button(queue_output_str: str, filename: str, *, disabled: bool = False) -> mo.Html:
    """A `Download Queue File` button. ``disabled`` lets the portal gate it behind upload."""
    return mo.download(
        data=lambda: queue_output_str.encode("utf-8"),
        filename=filename,
        label="Download Queue File",
        disabled=disabled,
    )
