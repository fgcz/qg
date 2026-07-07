"""Scenario T19 — unchecking a sample in the selection table excludes it from the queue.

The samples_table (``queue_app.py:955-962``) is a multi-select mo.ui.table with
``initial_selection`` set to every row. Unticking a row removes the sample from
the downstream sample list; this test confirms the change reaches the generator.
"""

from __future__ import annotations

from pathlib import Path

import polars as pl
from playwright.sync_api import Page, expect
from pytest_bdd import given, parsers, scenarios, then, when

from tests.gui._helpers import open_app, select_order, set_dropdown

scenarios("features/sample_exclusion.feature")


class _State:
    def __init__(self) -> None:
        self.downloaded_path: Path | None = None


@given("the queue app is open as an employee", target_fixture="state")
def _open(page: Page, queue_app_url: str) -> _State:
    open_app(page, queue_app_url)
    return _State()


@when(parsers.parse('I set "{label}" to "{value}"'))
def _set(page: Page, label: str, value: str) -> None:
    set_dropdown(page, label, value)


@when(parsers.parse("I select order {container_id:d}"))
def _select_order(page: Page, container_id: int) -> None:
    select_order(page, container_id)


@when(parsers.parse('I uncheck sample "{sample_name}" from the selection table'))
def _uncheck_sample(page: Page, sample_name: str) -> None:
    """Toggle off the row whose Sample Name cell holds ``sample_name``.

    samples_table renders multiple <table> elements; the selection table is
    unique in containing the "Uncheck to exclude samples" label. We locate it
    via the marimo-table data-label, then pick the row by its Sample Name cell.
    """
    # Wait for the samples table to be present (it renders after order selection
    # propagates through the cells).
    # samples_table is in the DOM but inside the "✎ Edit Samples" tab panel
    # (queue_app.py:1537), which renders with ``display:none`` until that tab is
    # active. ``mo.ui.radio`` doesn't expose a Playwright-friendly clickable
    # surface that reliably switches the tab — locating by label and by
    # marimo-radio both bind to elements that don't propagate the click.
    # Workaround: dispatch the click event directly on the checkbox via JS;
    # marimo's selection-state listener fires from the event regardless of
    # CSS visibility.
    # Locate the samples_table by its row + column count (12 samples × 8 cols);
    # the other <marimo-table> elements on the page have different totals.
    samples_table = page.locator('marimo-table[data-total-rows="12"][data-total-columns="8"]').first
    expect(samples_table).to_be_attached(timeout=15_000)
    # samples_df columns are snake_case (sample_name, sample_id, tube_id, ...)
    # per BfabricHelper._load_plate_samples (bfabric_utils.py:162).
    cell = samples_table.locator("td[data-cell-id$='_sample_name']", has_text=sample_name).first
    expect(cell).to_be_attached(timeout=10_000)
    checkbox = cell.locator("xpath=ancestor::tr[1]").locator("[data-testid='select-row-checkbox']")
    checkbox.dispatch_event("click")


@when("I upload to B-Fabric")
def _upload(page: Page) -> None:
    page.wait_for_load_state("networkidle", timeout=15_000)
    btn = page.get_by_role("button", name="Upload to B-Fabric")
    expect(btn).to_be_enabled(timeout=15_000)
    btn.click()


@then("I download the queue CSV")
def _download(page: Page, state: _State) -> None:
    page.wait_for_load_state("networkidle", timeout=15_000)
    link = page.get_by_role("link", name="Download Queue File")
    expect(link).to_be_visible(timeout=15_000)
    with page.expect_download(timeout=15_000) as info:
        link.click()
    path = info.value.path()
    assert path is not None, "Playwright did not materialize the download"
    state.downloaded_path = Path(path)


@then(parsers.parse('"{sample_name}" is absent from the queue'))
def _absent(state: _State, sample_name: str) -> None:
    assert state.downloaded_path is not None
    df = pl.read_csv(state.downloaded_path, skip_rows=1)
    matches = df.filter(pl.col("Sample Name") == sample_name)
    assert matches.height == 0, (
        f"Expected {sample_name!r} to be absent from the queue but found {matches.height} row(s)"
    )


@then(parsers.parse("{n:d} user samples are present"))
def _user_count(state: _State, n: int) -> None:
    assert state.downloaded_path is not None
    df = pl.read_csv(state.downloaded_path, skip_rows=1)
    user_rows = df.filter(pl.col("Sample Name").str.starts_with("Sample_"))
    assert user_rows.height == n, (
        f"Expected {n} user-sample rows; got {user_rows.height}. Sample Names: {user_rows['Sample Name'].to_list()}"
    )
