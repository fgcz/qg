"""Scenario T4 — Plate-mode tray-capacity validation.

Two scenarios cover both directions of the validation in queue_app.py:508-523:

* Container 37180 has a single plate, Vanquish has 4 trays → no warning.
* Container 37200 has 5 plates, Vanquish has 4 trays → the validation panel
  raises a warn callout containing "trays but N plates selected".
"""

from __future__ import annotations

from playwright.sync_api import Page, expect
from pytest_bdd import given, parsers, scenarios, then, when

from tests.gui import _helpers as H

scenarios("features/tray_capacity.feature")


@given("the queue app is open as an employee")
def _open_app(page: Page, queue_app_url: str) -> None:
    H.open_app(page, queue_app_url)


@when(parsers.parse('I set "{label}" to "{value}"'))
def _set(page: Page, label: str, value: str) -> None:
    H.set_dropdown(page, label, value)


@when(parsers.parse("I select order {container_id:d}"))
def _select_order(page: Page, container_id: int) -> None:
    H.select_order(page, container_id)


@then("the validation panel does not warn about tray capacity")
def _no_tray_capacity_warning(page: Page) -> None:
    # The warning text from queue_app.py:522 is
    # f"{sampler} has {num_trays} trays but {num_plates} plates selected".
    # "trays but" is a stable substring unique to that callout.
    expect(page.get_by_text("trays but", exact=False)).to_have_count(0)


@then("the validation panel warns about tray capacity")
def _warns_about_tray_capacity(page: Page) -> None:
    H.assert_warn(page, "trays but")
