"""Scenario T13 — Selecting two orders aggregates samples from both containers.

The download step is implemented inline rather than via the helper: ``mo.download``
renders an HTML anchor, not a button — the shared ``download_queue`` helper uses
``get_by_role("button", ...)`` which doesn't match. We instead locate the anchor
and capture the suggested filename via ``page.expect_download``;
``info.value.suggested_filename`` gives the server-side filename (the helper's
return value is only the tmp-dir path Playwright writes the bytes to, with no
relation to the declared filename).

The filename embedding both container IDs is implemented at
``queue_app.py:1276``:
    ``_ids_str = "_".join(str(c) for c in _container_ids)``

Note on the ``plates_37181.json`` fixture: it previously stored well positions
as strings ("B1".."B4"), which caused ``PlateSampleRow(position: int)`` to
raise ``ValidationError`` as soon as the second container loaded — making the
entire multi-container flow unreachable. The fixture was corrected to
sequential integer positions (13..16 — the well indices of B1..B4 on a
row-major 96-well plate) so plate samples from 37181 can be ingested
alongside those from 37180.
"""

from __future__ import annotations

from pathlib import Path

import polars as pl
from playwright.sync_api import Page, expect
from pytest_bdd import given, parsers, scenarios, then, when

from tests.gui._helpers import (
    open_app,
    select_order,
    set_dropdown,
)

scenarios("features/multi_container.feature")


# ----------------------------------------------------------------------------
# Step shared state
# ----------------------------------------------------------------------------


class _State:
    """Per-scenario mutable bag (pytest-bdd lacks a built-in scenario scope)."""

    def __init__(self) -> None:
        self.suggested_filename: str | None = None
        self.downloaded_path: Path | None = None


# ----------------------------------------------------------------------------
# Given / When / Then
# ----------------------------------------------------------------------------


@given("the queue app is open as an employee", target_fixture="state")
def _open(page: Page, queue_app_url: str) -> _State:
    open_app(page, queue_app_url)
    return _State()


@when(parsers.parse('I set "{label}" to "{value}"'))
def _set(page: Page, label: str, value: str) -> None:
    set_dropdown(page, label, value)


@when(parsers.parse("I select order {container_id:d}"))
def _select_order_step(page: Page, container_id: int) -> None:
    select_order(page, container_id)


@when("I upload to B-Fabric")
def _upload(page: Page) -> None:
    # Wait for cells to settle before clicking — marimo re-renders multiple
    # times as the form options propagate, which can detach the button DOM
    # mid-click. Give the network/render loop a moment to quiesce, then locate
    # the button via its run-button test-id (stable across re-renders).
    page.wait_for_load_state("networkidle", timeout=15_000)
    upload_btn = page.get_by_role("button", name="Upload to B-Fabric")
    expect(upload_btn).to_be_visible(timeout=15_000)
    expect(upload_btn).to_be_enabled(timeout=15_000)
    upload_btn.click()


@then("I download the queue CSV")
def _download(page: Page, state: _State) -> None:
    # ``mo.download`` renders an HTML anchor (``<a href="..." download="...">``),
    # not a button — so ``page.get_by_role("button", name="Download Queue File")``
    # (and therefore ``_helpers.download_queue``) doesn't match. We click the
    # anchor inline and capture the suggested filename from the Playwright
    # download event, which mirrors the server-side ``Content-Disposition``
    # / anchor ``download`` attribute.
    page.wait_for_load_state("networkidle", timeout=15_000)
    download_link = page.get_by_role("link", name="Download Queue File")
    expect(download_link).to_be_visible(timeout=15_000)
    with page.expect_download(timeout=15_000) as info:
        download_link.click()
    download = info.value
    state.suggested_filename = download.suggested_filename
    assert state.suggested_filename, "Playwright did not report a suggested filename"
    path = download.path()
    assert path is not None, "Playwright did not materialize the download"
    state.downloaded_path = Path(path)


@then(parsers.parse('the downloaded filename contains "{needle}"'))
def _filename_contains(state: _State, needle: str) -> None:
    assert state.suggested_filename is not None, "Download step did not run"
    assert needle in state.suggested_filename, (
        f"Expected '{needle}' in downloaded filename, got: {state.suggested_filename!r}"
    )


def _load_queue_df(state: _State) -> pl.DataFrame:
    """Load the downloaded Xcalibur CSV into a polars DataFrame.

    Xcalibur queues have a "Bracket Type=4" preamble line above the real header,
    so we skip it via ``skip_rows=1``.
    """
    assert state.downloaded_path is not None, "Download step did not run"
    return pl.read_csv(state.downloaded_path, skip_rows=1)


@then(parsers.parse("the queue contains all {n:d} samples from container {container_id:d}"))
def _contains_samples(state: _State, n: int, container_id: int) -> None:
    df = _load_queue_df(state)
    # Samples from a container have File Name embedding ``_C{container_id}_S<sample_id>_<name>``
    # (see queue_app.py file_name_template) — both container ID and a sample-name prefix
    # are required for a row to count.
    prefix = "B_" if container_id == 37181 else "Sample_"
    matches = df.filter(
        pl.col("File Name").str.contains(f"_C{container_id}_") & pl.col("Sample Name").str.starts_with(prefix)
    )
    assert matches.height == n, (
        f"Expected {n} sample rows from container {container_id}; "
        f"found {matches.height}. File Name column:\n{df['File Name'].to_list()}"
    )


@then("samples from container 37181 appear after samples from container 37180")
def _37181_after_37180(state: _State) -> None:
    df = _load_queue_df(state)
    file_names = df["File Name"].to_list()
    last_37180 = max(i for i, fn in enumerate(file_names) if "_C37180_S" in fn)
    first_37181 = min(i for i, fn in enumerate(file_names) if "_C37181_S" in fn)
    assert last_37180 < first_37181, (
        f"Expected all 37180 samples before any 37181 sample; "
        f"last 37180 at row {last_37180}, first 37181 at row {first_37181}"
    )
