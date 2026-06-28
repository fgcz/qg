"""Scenario: reproduce a saved run from a bundled self-contained params JSON.

Drives ``queue_app_local.py`` through Playwright: select a bundled params example,
then assert (1) the left config panel is replaced by the read-only "Reproducing a
saved run" summary (controls disabled), and (2) the Queue Preview renders the
regenerated queue with an enabled download and no generation error.

No B-Fabric dependency — runs on a core ``pip install qg``.
"""

from __future__ import annotations

import pytest
from playwright.sync_api import Page, expect

from qg.apps.integrations.example_params import list_example_params

pytestmark = pytest.mark.local_gui


def test_reproduce_from_bundled_params(page: Page, local_app_url: str) -> None:
    page.goto(local_app_url)
    expect(page.get_by_text("Local Queue Generator", exact=False).first).to_be_visible(timeout=30_000)

    # Load the bundled, self-contained proteomics run from its dropdown.
    label = next(e.label for e in list_example_params() if e.id == "repro_proteomics_12")
    page.get_by_label("bundled self-contained run").select_option(label=label)

    # The header acknowledges reproduce mode...
    expect(page.get_by_text("Reproducing", exact=False).first).to_be_visible(timeout=20_000)
    # ...and the left panel is replaced by the read-only summary (controls disabled).
    expect(page.get_by_text("Reproducing a saved run", exact=False).first).to_be_visible(timeout=20_000)

    # The queue regenerated from the embedded config — download enabled, no error.
    link = page.get_by_role("link", name="Download Queue File").first
    expect(link).to_be_visible(timeout=20_000)
    expect(link).not_to_have_attribute("disabled", "")
    expect(page.locator("marimo-callout-output[data-kind*='danger']")).to_have_count(0)
    # The preview reflects the LOADED run's randomization (blocked_uniform), proving
    # generation used the file's parameters rather than the sidebar defaults.
    expect(page.get_by_text("randomization: blocked_uniform", exact=False).first).to_be_visible(timeout=10_000)
