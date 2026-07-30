"""GUI acceptance scenarios for queue-name defaults and normalization."""

from __future__ import annotations

from typing import Any

from playwright.sync_api import Page
from pytest_bdd import given, parsers, scenarios, then, when

from tests.gui import _helpers as H

scenarios("features/queue_name.feature")


@given("the queue app is open as an employee", target_fixture="queue_name_state")
def _open_app(page: Page, queue_app_url: str) -> dict[str, Any]:
    H.open_app(page, queue_app_url)
    return {}


@when(parsers.parse('I set "{label}" to "{value}"'))
def _set_selector(page: Page, label: str, value: str) -> None:
    H.set_dropdown(page, label, value)


@when(parsers.parse("I select order {container_id:d}"))
def _select_order(page: Page, container_id: int) -> None:
    H.select_order(page, container_id)


@then("the queue name field shows the automatic seed placeholder")
def _expect_seed_placeholder(page: Page) -> None:
    H.expect_text_placeholder(page, "Queue name", "auto (seed)")


@when(parsers.parse('I set the queue name to "{queue_name}"'))
def _set_queue_name(page: Page, queue_name: str) -> None:
    H.set_text_and_blur(page, "Queue name", queue_name)


@then(parsers.parse('the sidebar shows the normalized queue name "{queue_name}"'))
def _expect_normalized_hint(page: Page, queue_name: str) -> None:
    H.expect_sidebar_text(page, f"Queue name: {queue_name}")


@when("I download the params JSON")
def _download_params(page: Page, queue_name_state: dict[str, Any]) -> None:
    page.locator('[role="radio"][value="Parameters"]').click()
    queue_name_state["params"] = H.download_params_json(page)


@then("the params queue name equals the hexadecimal seed")
def _expect_seed_name(queue_name_state: dict[str, Any]) -> None:
    parameters = queue_name_state["params"]["parameters"]
    assert parameters["queue_name"] == f"{parameters['seed']:08x}"


@then(parsers.parse('the params queue name is "{queue_name}"'))
def _expect_params_name(queue_name_state: dict[str, Any], queue_name: str) -> None:
    assert queue_name_state["params"]["parameters"]["queue_name"] == queue_name
