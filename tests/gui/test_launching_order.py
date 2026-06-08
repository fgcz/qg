"""GUI scenarios for the launching-order pre-load.

When the app is opened *from* a B-Fabric order (``entity_id`` set and
``entity_class`` in {Container, Order, Project}), ``queue_app.py`` fetches that
order via ``ContainerCache.fetch_container_row`` and:

- defaults the **Tech Area** dropdown to the order's technology — mapping
  B-Fabric's ``Metabolomics/Biophysics`` to ``Metabolomics`` (queue_app.py:153);
- for employees, pre-selects the order's row in the project table via
  ``initial_selection`` (queue_app.py:866), so ``selected_orders`` is non-empty on
  first render and the samples auto-load — surfaced by the selected-orders banner.

Container 37190 (Area ``Metabolomics/Biophysics``) is served by the
``container_37190.json`` fixture; it complements the existing ``non_employee``
scenarios, which pin Proteomics container 37180 and never check the Tech-Area
default.
"""

from __future__ import annotations

from collections.abc import Callable

from playwright.sync_api import Page, expect
from pytest_bdd import given, parsers, scenarios, then, when

from tests.gui import _helpers as H

scenarios("features/launching_order.feature")


@given(parsers.parse("an employee session launched from order {container_id:d}"))
def _employee_from_order(set_session: Callable[..., None], container_id: int) -> None:
    set_session(is_employee=True, entity_id=container_id, entity_class="Order")


@given(parsers.parse("a non-employee session pinned to order {container_id:d}"))
def _non_employee_from_order(set_session: Callable[..., None], container_id: int) -> None:
    set_session(is_employee=False, entity_id=container_id)  # entity_class defaults to "Container"


@when("I open the queue app")
def _open_app(page: Page, queue_app_url: str) -> None:
    H.open_app(page, queue_app_url)


@then(parsers.parse('the "{label}" dropdown shows "{value}"'))
def _dropdown_shows(page: Page, label: str, value: str) -> None:
    H.expect_dropdown_value(page, label, value)


@then(parsers.parse("the selected-orders banner shows order {container_id:d}"))
def _banner_shows_order(page: Page, container_id: int) -> None:
    # Employee banner renders "**Selected:** N order(s): <ids> (<type>)" with raw ints
    # (no thousands separator); the order being pre-selected is what makes it non-empty.
    expect(page.get_by_text("Selected:", exact=False)).to_be_visible(timeout=10_000)
    expect(page.get_by_text(str(container_id), exact=False).first).to_be_visible(timeout=10_000)
