"""GUI scenarios: a project container (no order items) keeps its plate placement.

A **project** container has no order items, so the app falls back to all of its
container samples. That fallback must still preserve plate placement: a plate-holding
project offers Plate rather than degrading to Vial (regression guard for the
order-item fallback flattening plate samples into vials).

Order containers (with billable order items) are covered in
``test_order_queue_type.py``.

Fixtures (see ``tests/gui/AGENTS.md`` for the defaults):

- 37210 — project: plate 50210 with 4 plate samples, no order items (scenario fixture).
- 37196 — project: 3 bare vials, no order items.
"""

from __future__ import annotations

from playwright.sync_api import Page
from pytest_bdd import given, parsers, scenarios, then, when

from tests.gui import _helpers as H

scenarios("features/project_queue_type.feature")


@given("the queue app is open as an employee")
def _open_app(page: Page, queue_app_url: str) -> None:
    H.open_app(page, queue_app_url)


@when(parsers.parse('I set "{label}" to "{value}"'))
def _set_selector(page: Page, label: str, value: str) -> None:
    H.set_dropdown(page, label, value)


@when(parsers.parse("I select order {container_id:d}"))
def _select_order(page: Page, container_id: int) -> None:
    H.select_order(page, container_id)


@then(parsers.parse('the "{label}" picker offers "{value}"'))
def _option_present(page: Page, label: str, value: str) -> None:
    H.expect_dropdown_options(page, label, value)


@then(parsers.parse('the "{label}" picker does not offer "{value}"'))
def _option_absent(page: Page, label: str, value: str) -> None:
    H.expect_dropdown_missing_option(page, label, value)
