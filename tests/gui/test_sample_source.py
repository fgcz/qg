"""GUI coverage for switching between order items and container samples."""

import re

from playwright.sync_api import Page, expect

from tests.gui import _helpers as H


def test_sample_source_defaults_to_order_items_and_can_restore_container_samples(
    page: Page,
    queue_app_url: str,
) -> None:
    H.open_app(page, queue_app_url)
    H.set_dropdown(page, "Tech Area", "Proteomics")
    H.select_order(page, 37182)

    expect(page.get_by_text(re.compile(r"1\s+samples")).first).to_be_visible(timeout=15_000)
    expect(page.get_by_text("1 × Unspecified", exact=False)).to_be_visible()

    all_samples = page.get_by_label("All container samples")
    expect(all_samples).to_be_visible(timeout=10_000)
    all_samples.check()

    expect(page.get_by_text(re.compile(r"6\s+samples")).first).to_be_visible(timeout=15_000)
    expect(page.get_by_text("6 × Unspecified", exact=False)).to_be_visible()


def test_container_source_always_changes_a_plate_order_to_vial(
    page: Page,
    queue_app_url: str,
) -> None:
    H.open_app(page, queue_app_url)
    H.set_dropdown(page, "Tech Area", "Proteomics")
    H.select_order(page, 37180)

    H.expect_dropdown_value(page, "Queue Type", "Plate")

    page.get_by_label("All container samples").check()

    H.expect_dropdown_value(page, "Queue Type", "Vial")


def test_empty_order_items_disclose_fallback_only_for_order_item_source(
    page: Page,
    queue_app_url: str,
) -> None:
    H.open_app(page, queue_app_url)
    H.set_dropdown(page, "Tech Area", "Proteomics")
    H.set_dropdown(page, "Tech Area", "Metabolomics")
    H.select_order(page, 37196)

    fallback = page.get_by_text("No order items for container(s) 37196", exact=False)
    expect(fallback).to_be_visible(timeout=15_000)

    page.get_by_label("All container samples").check()
    expect(fallback).to_have_count(0)
