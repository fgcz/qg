"""GUI scenario verifying the danger callout for a non-employee pinned to an empty container.

The conditional lives in ``queue_app.py`` around lines 931-938: when
``is_employee=False`` and ``full_samples_df.is_empty()``, the app calls
``mo.stop(True, mo.callout(mo.md("**No samples found in this container.**"), kind="danger"))``,
which halts rendering with a red danger callout. ``open_app()`` cannot be used because
its readiness probe waits for the Tech Area dropdown, which never renders past the stop.
"""

from __future__ import annotations

from collections.abc import Callable

from playwright.sync_api import Page
from pytest_bdd import given, parsers, scenarios, then, when

from tests.gui import _helpers as H

scenarios("features/non_employee_empty.feature")


@given(parsers.parse("a non-employee session pinned to container {container_id:d}"))
def _pin_non_employee(set_session: Callable[..., None], container_id: int) -> None:
    set_session(is_employee=False, entity_id=container_id)


@when("I open the queue app")
def _open_queue_app(page: Page, queue_app_url: str) -> None:
    page.goto(queue_app_url)


@then(parsers.parse('a danger callout reads "{text}"'))
def _danger_callout(page: Page, text: str) -> None:
    H.assert_danger(page, text)
