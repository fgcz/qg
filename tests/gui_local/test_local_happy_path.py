"""Scenario: Local app happy path — upload CSV, generate, download.

Drives ``queue_app_local.py`` end-to-end through a Playwright browser:
upload a sample CSV → configure Proteomics/ASTRAL_1/Vial → verify the queue
preview renders → verify the Download Queue File link is active.

No B-Fabric dependency — the app and this test run on a core ``pip install qg``.
"""

from __future__ import annotations

from importlib.resources import as_file, files

import pytest
from playwright.sync_api import Page, expect
from pytest_bdd import given, parsers, scenarios, then, when

from qg.apps.integrations.example_samples import list_example_sample_tables

# Import the app-agnostic helpers from the portal suite; they work unchanged
# because the local app uses the same sidebar/dropdown/download patterns.
from tests.gui._helpers import (
    set_dropdown,
    sidebar,
)

pytestmark = pytest.mark.local_gui

scenarios("features/local_happy_path.feature")


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
    # Source the upload fixture from the packaged example tables (single
    # authoritative copy) rather than a duplicated tests/ fixture.
    resource = files("qg.examples.sample_tables") / filename
    with as_file(resource) as path:
        assert path.exists(), f"Example asset missing: {path}"
        # marimo's mo.ui.file renders as a hidden <input type="file"> backed by a
        # button; set_input_files() drives it directly without opening the OS dialog.
        page.locator("input[type='file']").first.set_input_files(str(path))


@when("I load the bundled vial example")
def _load_vial_example(page: Page) -> None:
    # The example dropdown lives in the main content area (not the sidebar);
    # marimo renders it as a native <select> whose option values are positional
    # keys, so select by the visible label taken straight from the catalog.
    label = next(e.label for e in list_example_sample_tables() if e.mode == "vial")
    page.get_by_label("load a bundled example").select_option(label=label)


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
