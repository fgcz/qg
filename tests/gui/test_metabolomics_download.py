"""Scenario T16 — Metabolomics queue download verifies pos/neg polarity expansion.

Metabolomics splits each user sample into one positive- and one negative-polarity
injection. This is documented in CLAUDE.md and is the only sensible end-to-end
behaviour-level proof that the polarity-checkbox UI is wired through to the
generator.
"""

from __future__ import annotations

from pathlib import Path

import polars as pl
from playwright.sync_api import Page, expect
from pytest_bdd import given, parsers, scenarios, then, when

from tests.gui._helpers import open_app, select_order, set_dropdown

scenarios("features/metabolomics_download.feature")


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


@then("every Metabolomics sample appears in both polarities")
def _both_polarities(state: _State) -> None:
    assert state.downloaded_path is not None, "Download step did not run"
    df = pl.read_csv(state.downloaded_path, skip_rows=1)
    file_names = df["File Name"].to_list()
    for i in range(1, 7):
        sample = f"Met_{i:02d}"
        pos_rows = [fn for fn in file_names if sample in fn and "_pos" in fn]
        neg_rows = [fn for fn in file_names if sample in fn and "_neg" in fn]
        assert len(pos_rows) == 1, f"Expected 1 pos row for {sample}, got {len(pos_rows)}: {pos_rows}"
        assert len(neg_rows) == 1, f"Expected 1 neg row for {sample}, got {len(neg_rows)}: {neg_rows}"
