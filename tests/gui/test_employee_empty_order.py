"""GUI scenario verifying the warning callout when an employee selects an order with no samples.

Work item #9: "Order-flow on an order with no samples should give a warning message".

The fix lives in ``queue_app.py``: a top-level cell that renders a ``kind="warn"``
callout when ``is_employee and selected_orders and full_samples_df.is_empty()``.
Unlike the non-employee path (which calls ``mo.stop()`` with a danger callout and
halts rendering), this path is non-halting — the employee can recover by picking a
different order.

Container 99999 ("Empty Container") is already in
``tests/gui/fixtures/projects.csv`` with Samples=0. No ``samples_99999.json``
fixture exists, so ``FakeBfabric._samples_for`` returns ``[]``, yielding an empty
``full_samples_df``.
"""

from __future__ import annotations

from playwright.sync_api import Page
from pytest_bdd import given, parsers, scenarios, then, when

from tests.gui import _helpers as H

scenarios("features/employee_empty_order.feature")


@given("the queue app is open as an employee")
def _open(page: Page, queue_app_url: str) -> None:
    H.open_app(page, queue_app_url)


@when(parsers.parse('I set "{label}" to "{value}"'))
def _set(page: Page, label: str, value: str) -> None:
    H.set_dropdown(page, label, value)


@when(parsers.parse("I select order {container_id:d}"))
def _select_order(page: Page, container_id: int) -> None:
    H.select_order(page, container_id)


@then(parsers.parse('a warning callout reads "{text}"'))
def _warning_callout(page: Page, text: str) -> None:
    H.assert_warn(page, text)
