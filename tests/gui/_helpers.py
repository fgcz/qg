"""Reusable Playwright helpers for queue-app GUI scenarios.

Step implementations should be one-liners that delegate here. Locators are
scoped to ``role=complementary`` (the sidebar) because marimo renders each form
widget in both the sidebar and the main output area; without scoping every
``get_by_label`` would be ambiguous.

Reference for the queue app's interaction surface lives in
``src/qg/apps/queue_app.py`` — line numbers cited inline below.
"""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any

from playwright.sync_api import Locator, Page, expect

# ----------------------------------------------------------------------------
# Locator scopes
# ----------------------------------------------------------------------------


def sidebar(page: Page) -> Locator:
    """Sidebar where all form widgets live (queue_app.py:786 `mo.sidebar(...)`)."""
    return page.get_by_role("complementary")


def main(page: Page) -> Locator:
    """Main output area — preview tables, queue preview, callouts (non-sidebar)."""
    return (
        page.locator("body").locator(":not([role='complementary']) > *").first
    )  # rough; tests should usually use page directly


# ----------------------------------------------------------------------------
# App bootstrap
# ----------------------------------------------------------------------------


def open_app(page: Page, queue_app_url: str, *, ready_timeout_ms: int = 30_000) -> None:
    """Navigate to the app and wait for the sidebar Tech Area dropdown to be visible."""
    page.goto(queue_app_url)
    expect(sidebar(page).get_by_label("Tech Area")).to_be_visible(timeout=ready_timeout_ms)


# ----------------------------------------------------------------------------
# Generic widget interaction
# ----------------------------------------------------------------------------


def set_dropdown(page: Page, label: str, value: str) -> None:
    """Set a sidebar dropdown identified by its `label=` to `value`."""
    sidebar(page).get_by_label(label).select_option(value)


def expect_dropdown_options(page: Page, label: str, *values: str) -> None:
    """Assert each value appears in the named sidebar dropdown's option list."""
    options = sidebar(page).get_by_label(label).locator("option")
    for v in values:
        expect(options.filter(has_text=v)).to_have_count(1)


def expect_dropdown_missing_option(page: Page, label: str, value: str) -> None:
    """Assert the named sidebar dropdown is present but offers no option matching ``value``."""
    select = sidebar(page).get_by_label(label)
    expect(select).to_be_visible()
    expect(select.locator("option").filter(has_text=value)).to_have_count(0)


def expect_dropdown_hidden(page: Page, label: str) -> None:
    """Assert the named sidebar dropdown is not present (e.g. conditional widget hidden)."""
    expect(sidebar(page).get_by_label(label)).to_have_count(0)


def expect_dropdown_visible(page: Page, label: str) -> None:
    expect(sidebar(page).get_by_label(label)).to_be_visible()


def expect_dropdown_value(page: Page, label: str, value: str) -> None:
    """Assert the named sidebar dropdown's currently-selected option text is ``value``.

    Matches the selected ``<option>``'s text rather than the ``<select>`` value
    attribute: marimo's option values are positional keys, not the human label.
    Retries on a timeout to ride out marimo's reactive settle on first render.
    """
    select = sidebar(page).get_by_label(label)
    expect(select).to_be_visible()
    expect(select.locator("option:checked").first).to_have_text(value, timeout=10_000)


def click_button(page: Page, label: str) -> None:
    """Click a button (or run_button) by its accessible name. Searches the whole page."""
    page.get_by_role("button", name=label).click()


# ----------------------------------------------------------------------------
# Order selection (employee project table)
# ----------------------------------------------------------------------------


def select_order(page: Page, container_id: int) -> None:
    """Tick the row for ``container_id`` in the employee project table.

    The project table is ``mo.ui.table(selection="multi")`` (queue_app.py:835).
    Three marimo-specific gotchas this helper handles:

    1. The table's ``label=`` kwarg renders as a sibling ``<label>`` element,
       not as an ``aria-label``. We identify the project table among the
       multiple page tables by its unique ``"Container Name"`` ``<th>``.
    2. Integer columns render with thousands separators (``"37,180"``, not
       ``"37180"``). We match the ``Container ID`` ``<td>`` by its
       ``data-cell-id="..._Container ID"`` attribute and use the formatted form
       in the ``has_text`` filter.
    3. Row "checkboxes" are ``<button role="checkbox"
       data-testid="select-row-checkbox">`` (Radix UI), not ``<input
       type="checkbox">``. We click the button.
    """
    formatted = f"{container_id:,}"
    project_table = page.locator("table").filter(has=page.locator("th", has_text="Container Name")).first
    expect(project_table).to_be_visible(timeout=15_000)
    cell = project_table.locator("td[data-cell-id$='_Container ID']", has_text=formatted).first
    expect(cell).to_be_visible(timeout=10_000)
    row = cell.locator("xpath=ancestor::tr[1]")
    row.locator("[data-testid='select-row-checkbox']").click()


# ----------------------------------------------------------------------------
# Queue generation + download
# ----------------------------------------------------------------------------


def upload_to_bfabric(page: Page) -> None:
    """Click the 'Upload to B-Fabric' run button (queue_app.py:1285).

    Required before 'Download Queue File' becomes enabled (the download button
    is disabled while `upload_result is None`, queue_app.py:1330).
    """
    click_button(page, "Upload to B-Fabric")


def _expect_download(page: Page, label: str, *, timeout_ms: int) -> Any:
    """Click the named download control and return the Playwright Download object.

    `mo.download(...)` renders as an ``<a href="" download="...">`` anchor, not a
    button. `get_by_role("button", name=...)` returns 0 matches; the link role
    works.
    """
    with page.expect_download(timeout=timeout_ms) as info:
        page.get_by_role("link", name=label).click()
    return info.value


def download_queue(page: Page, *, download_timeout_ms: int = 15_000) -> Path:
    """Click 'Download Queue File' and return the path to the saved file.

    Assumes upload_to_bfabric() has been called (the button is disabled otherwise).
    """
    download = _expect_download(page, "Download Queue File", timeout_ms=download_timeout_ms)
    path = download.path()
    if path is None:
        raise RuntimeError("download.path() returned None — Playwright failed to materialize the file")
    return Path(path)


def download_params_json(page: Page, *, download_timeout_ms: int = 15_000) -> dict[str, Any]:
    """Click the Params JSON download button and return the parsed dict.

    The control is labelled 'Download Params JSON' (queue_app.py:1143). Like the
    queue-file download it renders as an anchor, not a button.
    """
    download = _expect_download(page, "Download Params JSON", timeout_ms=download_timeout_ms)
    return json.loads(Path(download.path()).read_text())


def get_workunit_message(page: Page) -> str:
    """Return the text of the upload-result markdown after clicking Upload.

    `MockFeederUploader.upload()` returns a string like
    ``'**[Mock]** Would create workunit in container 37180 with N resources.'``
    which is rendered via ``mo.md(upload_result)`` at queue_app.py:1340.
    """
    # The message follows the upload button row; pick the first non-empty paragraph
    # below it. Strict scoping isn't needed because the message text is unique.
    return page.get_by_text("Would create workunit", exact=False).inner_text()


# ----------------------------------------------------------------------------
# Callouts
# ----------------------------------------------------------------------------


def _assert_callout(page: Page, kind: str, text: str) -> None:
    """Assert a marimo callout of the given ``kind`` containing ``text`` is visible.

    Marimo's ``build_stateless_plugin`` (web_component.py:114) wraps every arg
    as ``data-{name}='<json-encoded value>'``. So ``mo.callout(..., kind="warn")``
    renders as ``<marimo-callout-output data-kind='&quot;warn&quot;' ...>``, not
    ``kind="warn"``. We use a substring match on ``data-kind`` to skip past the
    JSON quoting, which differentiates a real warn/danger callout from any text
    that merely happens to contain the same words elsewhere on the page.
    """
    callout = page.locator(f"marimo-callout-output[data-kind*='{kind}']").filter(has_text=text).first
    expect(callout).to_be_visible()


def assert_warn(page: Page, text: str) -> None:
    """Assert a yellow warn callout containing ``text`` is visible."""
    _assert_callout(page, "warn", text)


def assert_danger(page: Page, text: str) -> None:
    """Assert a red danger callout containing ``text`` is visible."""
    _assert_callout(page, "danger", text)
