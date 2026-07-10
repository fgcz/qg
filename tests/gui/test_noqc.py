"""GUI scenarios for the `no_layout` (queue as-is) QC option.

`no_layout` is a synthetic, code-level QC-layout option (it is not backed by any
CSV row). It means "reserve no wells, inject no QC, use the samples exactly as
provided", so the Pattern picker is meaningless and hides — the same sidebar
mechanism the old `noqc` layout used. It is offered for both Plate and Vial
queues, but only for tech areas that opt in via
``tech_area_defaults.allow_no_layout`` — Proteomics opts out.

The employee path exercises a Metabolomics plate order (37195, Plate mode) and a
Metabolomics vial order (37196, Vial mode) — both offer `no_layout` — plus a
Proteomics plate order (37180) that does not.
"""

from __future__ import annotations

from playwright.sync_api import Page
from pytest_bdd import given, parsers, scenarios, then, when

from tests.gui import _helpers as H

scenarios("features/noqc.feature")


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


@then("the Pattern picker is hidden")
def _pattern_hidden(page: Page) -> None:
    H.expect_dropdown_hidden(page, "Pattern")
