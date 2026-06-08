"""Scenario T17 — Randomized order actually reorders samples.

Asserts that ``Randomization="random"`` reshuffles the user samples (every input
sample still present exactly once, but the sequence differs from the input
order). The 1/12! collision probability is negligible — under 3e-9.

The ``"blocked"`` option requires per-sample groupingvar metadata which the
hermetic fixtures don't carry today; that scenario is deferred until the fake
B-Fabric fixtures grow a groupingvar column.
"""

from __future__ import annotations

from pathlib import Path

import polars as pl
from playwright.sync_api import Page, expect
from pytest_bdd import given, parsers, scenarios, then, when

from tests.gui._helpers import open_app, select_order, set_dropdown

scenarios("features/randomization.feature")

_INPUT_ORDER = [f"Sample_{i:02d}" for i in range(1, 13)]


class _State:
    def __init__(self) -> None:
        self.downloaded_path: Path | None = None


@given("the queue app is open as an employee", target_fixture="state")
def _open(page: Page, queue_app_url: str) -> _State:
    open_app(page, queue_app_url)
    return _State()


@when(parsers.parse('I set "{label}" to "{value}"'))
def _set(page: Page, label: str, value: str) -> None:
    set_dropdown(page, label, value)


@when(parsers.parse("I select order {container_id:d}"))
def _select_order(page: Page, container_id: int) -> None:
    select_order(page, container_id)


@when("I upload to B-Fabric")
def _upload(page: Page) -> None:
    page.wait_for_load_state("networkidle", timeout=15_000)
    btn = page.get_by_role("button", name="Upload to B-Fabric")
    expect(btn).to_be_enabled(timeout=15_000)
    btn.click()


@then("I download the queue CSV")
def _download(page: Page, state: _State) -> None:
    page.wait_for_load_state("networkidle", timeout=15_000)
    link = page.get_by_role("link", name="Download Queue File")
    expect(link).to_be_visible(timeout=15_000)
    with page.expect_download(timeout=15_000) as info:
        link.click()
    path = info.value.path()
    assert path is not None, "Playwright did not materialize the download"
    state.downloaded_path = Path(path)


def _user_samples(state: _State) -> list[str]:
    """Return user-sample names from the downloaded CSV in queue order."""
    assert state.downloaded_path is not None, "Download step did not run"
    df = pl.read_csv(state.downloaded_path, skip_rows=1)
    return [name for name in df["Sample Name"].to_list() if name.startswith("Sample_")]


@then("every input sample appears exactly once")
def _every_sample_once(state: _State) -> None:
    actual = _user_samples(state)
    assert sorted(actual) == sorted(_INPUT_ORDER), (
        f"User samples in output ({sorted(actual)}) differ from input set ({sorted(_INPUT_ORDER)})"
    )


@then("the sample order differs from the input order")
def _order_differs(state: _State) -> None:
    actual = _user_samples(state)
    assert actual != _INPUT_ORDER, f"Randomization produced the input order; should be reshuffled. order: {actual}"
