"""GUI scenarios for Queue-Type availability (plate / vial / mixed) and the
sampler-incompatibility warning.

Both behaviours come from ``make_queue_type_field``
(``queue_app_shared.py``): the Queue Type dropdown offers the intersection of the
sampler's supported queue types and the order's actual composition (Vial first),
and renders a warning callout when that intersection is empty. The composition is
classified per container by ``BfabricHelper.get_container_composition``.

Fixtures exercised (see ``tests/gui/AGENTS.md``):

- 37180 — plate-only (12 samples on plate 50001) → only Plate is offered.
- 37182 — vial-only (6 bare vials) → only Vial is offered.
- 37183 — mixed (4 samples on plate 50003 + 4 off-plate) → both offered, Vial default.
- 37180 + 37182 — selecting both OR-accumulates the flags → both offered.
- 37180 + Instrument ``LUMOS_2`` (auto-defaults the Vial-only ``MClass`` sampler)
  → empty intersection → warning; Vanquish on the same order → no warning.

This is the browser-side counterpart of the exhaustive unit test
``tests/test_queue_app_shared.py::TestMakeQueueTypeField``.
"""

from __future__ import annotations

from playwright.sync_api import Page, expect
from pytest_bdd import given, parsers, scenarios, then, when

from tests.gui import _helpers as H

scenarios("features/queue_type_availability.feature")


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


@then(parsers.parse('the "{label}" dropdown shows "{value}"'))
def _dropdown_shows(page: Page, label: str, value: str) -> None:
    H.expect_dropdown_value(page, label, value)


@then(parsers.parse('a warning reads "{text}"'))
def _warning_reads(page: Page, text: str) -> None:
    H.assert_warn(page, text)


@then("no sampler-incompatibility warning is shown")
def _no_incompat_warning(page: Page) -> None:
    expect(page.get_by_text("incompatible with this order's samples", exact=False)).to_have_count(0)
