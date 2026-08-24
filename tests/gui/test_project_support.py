"""GUI scenarios: project and order containers enter the queue at their own entrypoint.

This is the home for the project-vs-order entrypoint distinction:

- A **project** container has no order items, so the app falls back to all of its
  container samples. That fallback must still preserve plate placement: a plate-holding
  project offers Plate rather than degrading to Vial (regression guard for the
  order-item fallback flattening plate samples into vials).
- An **order** container has order items, which drive its plate/vial placement.

Fixtures (see ``tests/gui/AGENTS.md`` for the defaults):

- 37210 — project: plate 50210 with 4 plate samples, no order items (scenario fixture).
- 37196 — project: 3 bare vials, no order items.
- 37180 — order: plate 50001 with 12 plate samples.
- 37182 — order: 6 bare vials.

Step implementations are defined here (pytest-bdd 8.x scopes step fixtures to the
module that calls ``scenarios``) and delegate to ``_helpers.py``.
"""

from __future__ import annotations

from playwright.sync_api import Page
from pytest_bdd import given, parsers, scenarios, then, when

from tests.gui import _helpers as H

scenarios("features/project_support.feature")


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
