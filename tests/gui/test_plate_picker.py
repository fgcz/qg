"""GUI scenarios — the plate picker shows only in Plate mode; a neutral note
flags mixed (plate+vial) orders.

Gating lives in ``queue_app.py`` (the picker display cell requires
``queue_type_field.value == "Plate"``); the note is ``make_mixed_order_note`` in
``queue_app_shared.py``. Fixtures (see ``tests/gui/AGENTS.md``): 37183 is the mixed
container (plate 50003 + off-plate samples, Metabolomics), 37180 is plate-only,
37182 is vial-only.
"""

from __future__ import annotations

from playwright.sync_api import Page
from pytest_bdd import given, parsers, scenarios, then, when

from tests.gui import _helpers as H

scenarios("features/plate_picker.feature")


@given("the queue app is open as an employee")
def _open_app(page: Page, queue_app_url: str) -> None:
    H.open_app(page, queue_app_url)


@when(parsers.parse('I set "{label}" to "{value}"'))
def _set_selector(page: Page, label: str, value: str) -> None:
    H.set_dropdown(page, label, value)


@when(parsers.parse("I select order {container_id:d}"))
def _select_order(page: Page, container_id: int) -> None:
    H.select_order(page, container_id)


@then("the plate picker is shown")
def _picker_shown(page: Page) -> None:
    H.expect_plate_picker_visible(page)


@then("the plate picker is not shown")
def _picker_hidden(page: Page) -> None:
    H.expect_plate_picker_hidden(page)


@then("the mixed-order note is shown")
def _note_shown(page: Page) -> None:
    H.expect_mixed_order_note_visible(page)


@then("the mixed-order note is not shown")
def _note_hidden(page: Page) -> None:
    H.expect_mixed_order_note_hidden(page)
