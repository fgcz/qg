"""GUI scenarios — Start Position is vial-only; Start Tray shows in both modes.

The conditionals live in ``queue_app.py``:

- Start Position dropdown (``label="& position"``): created only when
  ``queue_type_field.value == "Vial"``.
- Start Tray dropdown (``label="Start: tray"``): created whenever a sampler is
  selected, in both vial and plate modes — in plate mode it relocates the
  user's plate off the default tray to sidestep QC-layout collisions.

The Gherkin describes them in domain terms — "starting tray" and "starting
well" — rather than the literal widget labels, so the test survives a label
rename without churning every feature file.

Order 37182 is vial-only (no plates JSON fixture). Order 37180 has a plate.
"""

from __future__ import annotations

from playwright.sync_api import Page
from pytest_bdd import given, parsers, scenarios, then, when

from tests.gui import _helpers as H

scenarios("features/vial_mode.feature")

_START_TRAY_LABEL = "Start: tray"
_START_POSITION_LABEL = "& position"


@given("the queue app is open as an employee")
def _open_app(page: Page, queue_app_url: str) -> None:
    H.open_app(page, queue_app_url)


@when(parsers.parse('I set "{label}" to "{value}"'))
def _set_selector(page: Page, label: str, value: str) -> None:
    H.set_dropdown(page, label, value)


@when(parsers.parse("I select order {container_id:d}"))
def _select_order(page: Page, container_id: int) -> None:
    H.select_order(page, container_id)


@then("the user is prompted for the starting tray")
def _start_tray_visible(page: Page) -> None:
    H.expect_dropdown_visible(page, _START_TRAY_LABEL)


@then("the user is prompted for the starting well")
def _start_position_visible(page: Page) -> None:
    H.expect_dropdown_visible(page, _START_POSITION_LABEL)


@then("the user is not prompted for the starting tray")
def _start_tray_hidden(page: Page) -> None:
    H.expect_dropdown_hidden(page, _START_TRAY_LABEL)


@then("the user is not prompted for the starting well")
def _start_position_hidden(page: Page) -> None:
    H.expect_dropdown_hidden(page, _START_POSITION_LABEL)
