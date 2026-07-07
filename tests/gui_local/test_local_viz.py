"""Scenario: Local app Visualizations tab renders after a plate queue is generated.

Closes the coverage gap that the plate/timeline view builders (now
``queue_app_shared.render_plate_layout_view`` / ``render_timeline_view``) were
GUI-tested only on the portal app (T24). This drives ``queue_app_local.py``
end-to-end: load the bundled plate example, configure a Proteomics/Vanquish
plate queue, switch to the Visualizations tab, and assert the plate map and
both balance scores render (proving the shared view builders ran without error
through the local wrappers).

No B-Fabric dependency — the app and this test run on a core ``pip install qg``.
"""

from __future__ import annotations

import pytest
from playwright.sync_api import Page, expect
from pytest_bdd import given, parsers, scenarios, then, when

from qg.apps.integrations.example_samples import list_example_sample_tables

# App-agnostic helpers from the portal suite; the local app shares the same
# sidebar/dropdown patterns (see test_local_happy_path.py for the same reuse).
from tests.gui._helpers import set_dropdown, sidebar

pytestmark = pytest.mark.local_gui

scenarios("features/local_viz.feature")


@given("the local queue app is open")
def _open(page: Page, local_app_url: str) -> None:
    page.goto(local_app_url)
    expect(page.get_by_text("Local Queue Generator", exact=False).first).to_be_visible(timeout=30_000)


@when("I load the bundled plate example")
def _load_plate_example(page: Page) -> None:
    label = next(e.label for e in list_example_sample_tables() if e.mode == "plate")
    page.get_by_label("load a bundled example").select_option(label=label)


@when(parsers.parse('I set "{label}" to "{value}"'))
def _set_dropdown(page: Page, label: str, value: str) -> None:
    set_dropdown(page, label, value)


@when(parsers.parse('I set the date to "{iso_date}"'))
def _set_date(page: Page, iso_date: str) -> None:
    sidebar(page).locator('input[type="date"]').first.fill(iso_date)


@when(parsers.parse('I switch to the "{tab_name}" tab'))
def _switch_tab(page: Page, tab_name: str) -> None:
    # mo.ui.radio renders each tab option as a <label>; clicking routes via the
    # for-attribute binding (mirrors the portal show_plate / valid-combinations tests).
    page.locator("label", has_text=tab_name).first.click()


@then("the plate layout visualization is visible")
def _plate_visible(page: Page) -> None:
    # "color = sample type" is unique to the Plate Layout heading; its presence
    # means the figure branch rendered, not the placeholder or no-geometry callout.
    expect(page.get_by_text("color = sample type", exact=False).first).to_be_visible(timeout=20_000)


@then("the plate balance score is shown")
def _plate_score_visible(page: Page) -> None:
    expect(page.get_by_text("Group ↔ plate position", exact=False).first).to_be_visible(timeout=15_000)


@then("the acquisition timeline with its balance score is visible")
def _timeline_visible(page: Page) -> None:
    expect(page.get_by_text("Group ↔ queue position", exact=False).first).to_be_visible(timeout=15_000)
