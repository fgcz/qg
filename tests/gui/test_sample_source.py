"""GUI scenarios for switching between order items and container samples."""

from __future__ import annotations

import re

from playwright.sync_api import Page, expect
from pytest_bdd import given, parsers, scenarios, then, when

from tests.gui import _helpers as H

scenarios("features/sample_source.feature")


@given("the queue app is open as an employee")
def _open_app(page: Page, queue_app_url: str) -> None:
    H.open_app(page, queue_app_url)


@when(parsers.parse('I set "{label}" to "{value}"'))
def _set_selector(page: Page, label: str, value: str) -> None:
    H.set_dropdown(page, label, value)


@when(parsers.parse("I select order {container_id:d}"))
def _select_order(page: Page, container_id: int) -> None:
    H.select_order(page, container_id)


@when("I choose all container samples")
def _choose_all_container_samples(page: Page) -> None:
    field = page.get_by_label("All container samples")
    expect(field).to_be_visible(timeout=10_000)
    field.check()


@then(parsers.parse("the selection banner reports {n:d} samples"))
def _banner_reports_count(page: Page, n: int) -> None:
    expect(page.get_by_text(re.compile(rf"{n}\s+samples")).first).to_be_visible(timeout=15_000)


@then(parsers.parse('the sample type summary reads "{text}"'))
def _sample_type_summary(page: Page, text: str) -> None:
    expect(page.get_by_text(text, exact=False)).to_be_visible()


@then(parsers.parse('the "{label}" dropdown shows "{value}"'))
def _dropdown_shows(page: Page, label: str, value: str) -> None:
    H.expect_dropdown_value(page, label, value)


@then(parsers.parse("the no-order-items fallback is shown for container {container_id:d}"))
def _fallback_shown(page: Page, container_id: int) -> None:
    fallback = page.get_by_text(f"No order items for container(s) {container_id}", exact=False)
    expect(fallback).to_be_visible(timeout=15_000)


@then("no no-order-items fallback is shown")
def _fallback_hidden(page: Page) -> None:
    expect(page.get_by_text("No order items for container(s)", exact=False)).to_have_count(0)
