"""Scenario T24 — Visualizations tab shows the plate layout and acquisition timeline.

Once a queue has been generated for a layout with row/column geometry, the
Visualizations tab renders two sub-views: a Plotly plate map (colored by sample
category by default, under the heading ``"**Plate layout** — color = sample
type, ..."``) annotated with a group↔plate-position balance score, and an
acquisition-order timeline annotated with a group↔queue-position score.

Before a queue exists the tab shows a placeholder instead; this scenario drives
the full happy-path configuration first (Proteomics / ASTRAL_1 / Vanquish plate
order 37180) so the figure branch is the one exercised. Asserting on the unique
heading and score text proves the cells ran end-to-end without error.
"""

from __future__ import annotations

from playwright.sync_api import Page, expect
from pytest_bdd import given, parsers, scenarios, then, when

from tests.gui._helpers import (
    open_app,
    select_order,
    set_dropdown,
    upload_to_bfabric,
)

scenarios("features/show_plate.feature")


@given("the queue app is open as an employee")
def _open(page: Page, queue_app_url: str) -> None:
    open_app(page, queue_app_url)


@when(parsers.parse('I set "{label}" to "{value}"'))
def _set(page: Page, label: str, value: str) -> None:
    set_dropdown(page, label, value)


@when(parsers.parse("I select order {container_id:d}"))
def _select_order(page: Page, container_id: int) -> None:
    select_order(page, container_id)


@when("I upload to B-Fabric")
def _upload(page: Page) -> None:
    upload_to_bfabric(page)


@when(parsers.parse('I switch to the "{tab_name}" tab'))
def _switch_tab(page: Page, tab_name: str) -> None:
    # mo.ui.radio renders each tab option as a <label>; clicking it routes via
    # the for-attribute binding (mirrors the Valid Combinations tab test).
    page.locator("label", has_text=tab_name).first.click()


@then("the plate layout visualization is visible")
def _plate_visible(page: Page) -> None:
    # The "color = sample type" phrasing is unique to the Plate Layout heading;
    # its presence means the figure branch rendered rather than the
    # "Generate a queue..." placeholder or the no-geometry callout.
    expect(page.get_by_text("color = sample type", exact=False).first).to_be_visible(timeout=15_000)


@then("the plate balance score is shown")
def _plate_score_visible(page: Page) -> None:
    # The group↔plate-position score line is unique to the Plate Layout sub-view.
    expect(page.get_by_text("Group ↔ plate position", exact=False).first).to_be_visible(timeout=15_000)


@then("the acquisition timeline with its balance score is visible")
def _timeline_visible(page: Page) -> None:
    # The group↔queue-position score line is unique to the Acquisition Timeline sub-view.
    expect(page.get_by_text("Group ↔ queue position", exact=False).first).to_be_visible(timeout=15_000)
