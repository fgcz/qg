"""GUI scenarios for the non-employee pinned-container workflow.

When ``is_employee=False``, ``queue_app.py`` short-circuits the project browser:

- The "Refresh Projects" run button is ``None`` (queue_app.py:799).
- The project table is ``None`` (queue_app.py:842) — the "Select orders
  (multi-select)" widget is not rendered.
- ``selected_orders = [(entity_id, None)]`` (queue_app.py:854) — the pinned
  container is auto-selected, so samples auto-load and tech / instrument /
  sampler configuration still works normally.
"""

from __future__ import annotations

from collections.abc import Callable

from playwright.sync_api import Page, expect
from pytest_bdd import given, parsers, scenarios, then, when

from tests.gui import _helpers as H

scenarios("features/non_employee.feature")


@given("a non-employee session pinned to container 37180")
def _pin_non_employee(set_session: Callable[..., None]) -> None:
    set_session(is_employee=False, entity_id=37180)


@when("I open the queue app")
def _open_app(page: Page, queue_app_url: str) -> None:
    H.open_app(page, queue_app_url)


@when(parsers.parse('I set "{label}" to "{value}"'))
def _set_selector(page: Page, label: str, value: str) -> None:
    H.set_dropdown(page, label, value)


@then("the project-selection table is not visible")
def _project_table_absent(page: Page) -> None:
    expect(H.sidebar(page).get_by_label("Select orders (multi-select)")).to_have_count(0)


@then(parsers.parse('the "{label}" button is not visible'))
def _button_absent(page: Page, label: str) -> None:
    expect(page.get_by_role("button", name=label)).to_have_count(0)


@then(parsers.parse('the "{label}" selector is visible'))
def _selector_visible(page: Page, label: str) -> None:
    H.expect_dropdown_visible(page, label)
