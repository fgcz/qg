"""GUI scenario verifying the required-field validation callout on initial load.

The validation cell at ``queue_app.py:450-493`` collects validation errors and
renders them as a warn callout. When no order is selected (the default
employee-mode state) the first error is "Please select an order" (line 456).
"""

from __future__ import annotations

from playwright.sync_api import Page
from pytest_bdd import given, parsers, scenarios, then

from tests.gui import _helpers as H

scenarios("features/validation_required.feature")


@given("the queue app is open as an employee")
def _open_app(page: Page, queue_app_url: str) -> None:
    H.open_app(page, queue_app_url)


@then(parsers.parse('the validation callout warns "{text}"'))
def _validation_callout_warns(page: Page, text: str) -> None:
    H.assert_warn(page, text)
