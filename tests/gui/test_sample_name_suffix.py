"""Scenario — appending a batch suffix to every sample name.

The suffix dropdown (``queue_app.py`` ``name_suffix``) lives directly in the
"Sample Editor" subtab of the "✎ Edit Samples" tab. Reaching it requires
switching to the outer tab, then the inner "Sample Editor" subtab.
"""

from __future__ import annotations

from pathlib import Path

import polars as pl
from playwright.sync_api import Page, expect
from pytest_bdd import given, parsers, scenarios, then, when

from tests.gui._helpers import open_app, select_order, set_dropdown

scenarios("features/sample_name_suffix.feature")


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


@when(parsers.parse('I append the "{suffix}" sample name suffix'))
def _append_suffix(page: Page, suffix: str) -> None:
    """Switch to the editor, set the suffix, then switch back to Queue Preview.

    The "Upload to B-Fabric" button lives in the Queue Preview tab panel
    (queue_app.py:1344), which is display:none while any other outer tab is
    active. Restoring the default tab afterward keeps the rest of the scenario
    (upload, download) working exactly like ``test_sample_exclusion.py``.
    """
    page.locator("label", has_text="✎ Edit Samples").first.click()
    page.locator("label", has_text="Sample Editor").first.click()
    select = page.locator("select", has=page.locator("option", has_text=suffix))
    expect(select.first).to_be_visible(timeout=10_000)
    select.first.select_option(label=suffix)
    # The suffix must reach the tables, not just the output: the Sample Selection
    # table (``samples_table``) is rebuilt from the suffixed source, so its
    # ``sample_name`` cells now carry ``_<suffix>``. Assert attached (not visible):
    # marimo keeps every tab panel in the DOM via ``display:none``, so a visibility
    # assertion is unreliable here (see test_sample_exclusion.py for the same note).
    table_cell = page.locator("td[data-cell-id$='_sample_name']", has_text=f"_{suffix}").first
    expect(table_cell).to_be_attached(timeout=10_000)
    page.locator("label", has_text="Queue Preview").first.click()


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


@then(parsers.parse('every user sample name ends with "{suffix}"'))
def _user_names_suffixed(state: _State, suffix: str) -> None:
    assert state.downloaded_path is not None
    df = pl.read_csv(state.downloaded_path, skip_rows=1)
    user_rows = df.filter(pl.col("Sample Name").str.starts_with("Sample_"))
    assert user_rows.height > 0, "Expected at least one user-sample row"
    unsuffixed = user_rows.filter(~pl.col("Sample Name").str.ends_with(suffix))
    assert unsuffixed.height == 0, f"Rows missing the suffix: {unsuffixed['Sample Name'].to_list()}"


@then(parsers.parse('no sample name ends with "{doubled_suffix}"'))
def _no_double_suffix(state: _State, doubled_suffix: str) -> None:
    assert state.downloaded_path is not None
    df = pl.read_csv(state.downloaded_path, skip_rows=1)
    doubled = df.filter(pl.col("Sample Name").str.ends_with(doubled_suffix))
    assert doubled.height == 0, f"Suffix applied twice: {doubled['Sample Name'].to_list()}"


@then("QC sample names are unchanged")
def _qc_unchanged(state: _State) -> None:
    assert state.downloaded_path is not None
    df = pl.read_csv(state.downloaded_path, skip_rows=1)
    qc_rows = df.filter(~pl.col("Sample Name").str.starts_with("Sample_"))
    assert qc_rows.height > 0, "Expected at least one QC/blank row"
    suffixed = qc_rows.filter(pl.col("Sample Name").str.ends_with("_enriched"))
    assert suffixed.height == 0, f"QC rows unexpectedly suffixed: {suffixed['Sample Name'].to_list()}"
