"""Scenario T21 — Valid Combinations tab renders the master reference table.

The Valid Combinations tab (queue_app.py:1356-1415) shows a polars DataFrame of
all valid (tech_area, instrument, sampler, ...) tuples with a checkmark column
highlighting rows matching the current sidebar selection. The header markdown
``"{n}/{total} combinations match | Filters: ..."`` (line 1411) is what we
assert on — its presence proves the entire cell ran without error.
"""

from __future__ import annotations

from playwright.sync_api import Page, expect
from pytest_bdd import given, parsers, scenarios, then, when

from tests.gui._helpers import open_app

scenarios("features/valid_combinations_tab.feature")


@given("the queue app is open as an employee")
def _open(page: Page, queue_app_url: str) -> None:
    open_app(page, queue_app_url)


@when(parsers.parse('I switch to the "{tab_name}" tab'))
def _switch_tab(page: Page, tab_name: str) -> None:
    # mo.ui.radio renders the tab options as <label> elements next to <input>
    # radios. Clicking the label routes via the for-attribute binding.
    page.locator("label", has_text=tab_name).first.click()


@then("the combinations table is visible")
def _table_visible(page: Page) -> None:
    # The Valid Combinations content is the only place that surfaces the
    # word "combinations match" anywhere on the page (queue_app.py:1411).
    expect(page.get_by_text("combinations match", exact=False).first).to_be_visible(timeout=15_000)


@then("the table reports how many combinations match the current selection")
def _reports_match_count(page: Page) -> None:
    text = page.get_by_text("combinations match", exact=False).first.inner_text()
    # Format: "N/M combinations match | Filters: ..."
    assert "/" in text and "combinations match" in text, text
    nm = text.split(" combinations match")[0]
    matched, total = nm.split("/")
    assert matched.isdigit() and total.isdigit(), text
    assert int(total) > 0, "master combinations table is empty"
