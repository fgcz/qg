"""GUI scenario for the `no_layout` (plate as-is) QC option.

`no_layout` is a synthetic, code-level QC-layout option offered in Plate mode for
every tech area (it is not backed by any CSV row). It means "reserve no wells,
inject no QC, use the plate exactly as provided", so the Pattern picker is
meaningless and hides — the same sidebar mechanism the old `noqc` layout used.

The employee path selects a Metabolomics plate order (37195) and switches to Plate
mode so the QC Layout cascade offers `no_layout`.
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


@then("the Pattern picker is hidden")
def _pattern_hidden(page: Page) -> None:
    H.expect_dropdown_hidden(page, "Pattern")
