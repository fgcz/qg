"""Scenario T1 — Happy path: download a deterministic Proteomics queue.

Binds ``features/happy_path.feature`` to step implementations that delegate to
the shared helpers in ``_helpers.py``. The downloaded CSV is compared byte-for-
byte against the committed golden reference at
``fixtures/expected/proteomics_astral1_37180.csv``.

The golden CSV was generated on 2026-05-20 with the default app settings
(polarity=pos, user=analytic, inj_vol=1, plate_layout=Plate_96,
qc_layout=standard, queue_pattern=standard). The Gherkin freezes the date to
``2026-05-20`` so the byte comparison is unconditional — there is no fallback.
"""

from __future__ import annotations

from pathlib import Path

from playwright.sync_api import Page
from pytest_bdd import given, parsers, scenarios, then, when

from tests.gui._helpers import (
    open_app,
    select_order,
    set_dropdown,
    sidebar,
    upload_to_bfabric,
)

scenarios("features/happy_path.feature")

_GOLDEN_CSV = Path(__file__).resolve().parent / "fixtures" / "expected" / "proteomics_astral1_37180.csv"


# ----------------------------------------------------------------------------
# Step shared state
# ----------------------------------------------------------------------------


class _State:
    """Per-scenario mutable bag (pytest-bdd lacks a built-in scenario scope)."""

    def __init__(self) -> None:
        self.downloaded_path: Path | None = None


# ----------------------------------------------------------------------------
# Given / When / Then
# ----------------------------------------------------------------------------


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


@when(parsers.parse('the date is set to "{iso_date}"'))
def _set_date(page: Page, iso_date: str) -> None:
    # mo.ui.date renders as <input type="date">; fill expects YYYY-MM-DD.
    sidebar(page).locator('input[type="date"]').first.fill(iso_date)


@when("I upload to B-Fabric")
def _upload(page: Page) -> None:
    upload_to_bfabric(page)


@then("I can download the queue CSV")
def _download(page: Page, state: _State) -> None:
    # `mo.download(...)` renders as `<a href="" download="...">` rather than a
    # role=button element. The shared `download_queue` helper assumes the latter
    # (via `page.get_by_role("button", name="Download Queue File")`), so we drive
    # the anchor directly. Scoped by accessible text — no naked `get_by_label`.
    link = page.get_by_role("link", name="Download Queue File")
    with page.expect_download(timeout=15_000) as info:
        link.click()
    download = info.value
    path = download.path()
    assert path is not None, "Playwright did not materialize the download path"
    state.downloaded_path = Path(path)
    assert state.downloaded_path.exists(), "Playwright reported a download but the file is missing"
    assert state.downloaded_path.stat().st_size > 0, "Downloaded CSV is empty"


@then("the downloaded queue CSV matches the golden reference")
def _matches_golden(state: _State) -> None:
    assert state.downloaded_path is not None, "Download step did not run"
    actual = state.downloaded_path.read_bytes()
    expected = _GOLDEN_CSV.read_bytes()
    assert actual == expected, (
        "Downloaded CSV differs from golden reference.\n"
        f"  golden path: {_GOLDEN_CSV}\n"
        f"  actual path: {state.downloaded_path}\n"
    )
