"""Scenario: Local app happy path — upload CSV, generate, download.

Drives ``queue_app_local.py`` end-to-end through a Playwright browser:
upload a sample CSV → configure Proteomics/ASTRAL_1/Vial → verify the queue
preview renders → verify the Download Queue File link is active.

No B-Fabric dependency — the app and this test run on a core ``pip install qg``.
"""

from __future__ import annotations

from pathlib import Path

import pytest
from playwright.sync_api import Page, expect
from pytest_bdd import given, parsers, scenarios, then, when

# Import the app-agnostic helpers from the portal suite; they work unchanged
# because the local app uses the same sidebar/dropdown/download patterns.
from tests.gui._helpers import (
    set_dropdown,
    sidebar,
)

pytestmark = pytest.mark.local_gui

scenarios("features/local_happy_path.feature")

_FIXTURES_DIR = Path(__file__).resolve().parent / "fixtures"


# ---------------------------------------------------------------------------
# Given
# ---------------------------------------------------------------------------


@given("the local queue app is open")
def _open(page: Page, local_app_url: str) -> None:
    page.goto(local_app_url)
    # The app title is the first thing marimo renders; wait for it as the
    # readiness gate (equivalent to the portal waiting for "Tech Area").
    expect(page.get_by_text("Local Queue Generator", exact=False).first).to_be_visible(timeout=30_000)


# ---------------------------------------------------------------------------
# When
# ---------------------------------------------------------------------------


@when(parsers.parse('I upload the sample file "{filename}"'))
def _upload(page: Page, filename: str) -> None:
    path = _FIXTURES_DIR / filename
    assert path.exists(), f"Fixture missing: {path}"
    # marimo's mo.ui.file renders as a hidden <input type="file"> backed by a
    # button; set_input_files() drives it directly without opening the OS dialog.
    page.locator("input[type='file']").first.set_input_files(str(path))


@when(parsers.parse('I set "{label}" to "{value}"'))
def _set_dropdown(page: Page, label: str, value: str) -> None:
    set_dropdown(page, label, value)


@when(parsers.parse('I set the date to "{iso_date}"'))
def _set_date(page: Page, iso_date: str) -> None:
    sidebar(page).locator('input[type="date"]').first.fill(iso_date)


# ---------------------------------------------------------------------------
# Then
# ---------------------------------------------------------------------------


@then("the queue preview is visible")
def _queue_preview_visible(page: Page) -> None:
    # The queue preview tab heading is unique to a successfully generated queue.
    # Use a generous timeout to account for marimo's reactive settle after the
    # last parameter change.
    expect(page.get_by_text("Queue Preview", exact=False).first).to_be_visible(timeout=20_000)
    # Also assert no danger callout is showing (generation error would appear here).
    danger = page.locator("marimo-callout-output[data-kind*='danger']")
    expect(danger).to_have_count(0)


@then(parsers.parse('the "{label}" link is enabled'))
def _download_link_enabled(page: Page, label: str) -> None:
    # mo.download renders as <a download> — not a <button>. The link should be
    # present and not carry a disabled attribute.
    link = page.get_by_role("link", name=label).first
    expect(link).to_be_visible(timeout=15_000)
    expect(link).not_to_have_attribute("disabled", "")
