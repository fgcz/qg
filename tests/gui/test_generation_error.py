"""Scenario T20 — generation error surfaces a danger callout.

The Vanquish sampler has 4 trays (qg_configs/core/position/sampler.toml).
Container 37200 carries 5 plates; the generator raises ``ValueError("Not enough
trays …")`` from positionV2.py:51, caught in queue_app.py:1228-1240 and rendered
as ``mo.callout(..., kind="danger")`` at queue_app.py:1322.

This scenario is the only end-to-end proof that a generator-level failure
reaches the user instead of silently producing a broken queue.
"""

from __future__ import annotations

from playwright.sync_api import Page
from pytest_bdd import given, parsers, scenarios, then, when

from tests.gui._helpers import assert_danger, open_app, select_order, set_dropdown

scenarios("features/generation_error.feature")


@given("the queue app is open as an employee")
def _open(page: Page, queue_app_url: str) -> None:
    open_app(page, queue_app_url)


@when(parsers.parse('I set "{label}" to "{value}"'))
def _set(page: Page, label: str, value: str) -> None:
    set_dropdown(page, label, value)


@when(parsers.parse("I select order {container_id:d}"))
def _select_order(page: Page, container_id: int) -> None:
    select_order(page, container_id)


@then("a danger callout reports the generation error")
def _danger_callout(page: Page) -> None:
    # The generator raises "Not enough trays (4) for 5 plates"; the queue_app
    # wraps it as "Generation Error: …" in a kind="danger" callout.
    assert_danger(page, "Generation Error")
