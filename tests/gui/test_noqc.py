"""GUI scenarios verifying the Pattern selector is hidden when QC Layout is ``noqc``.

The conditional lives in ``queue_app.py`` around lines 745-749: the Pattern
dropdown is only appended to the sidebar when ``qc_layout_field.value != "noqc"``.

The container is pinned via ``set_session(entity_id=37182)`` (vial-only) so the
``sample_type`` / ``plate_layout`` / ``qc_layout`` cascade has a non-empty state
when we switch Tech Area to Metabolomics — without a selected order, those
dropdowns collapse to empty and ``noqc`` never appears as an option. 37182 is
used over 37180 because its vial samples validate cleanly in Metabolomics mode
(plate-position samples like ``A1`` raise a ValidationError that hides the
sidebar entirely after Tech Area switches).
"""

from __future__ import annotations

from playwright.sync_api import Page
from pytest_bdd import given, parsers, scenarios, then, when

from tests.gui import _helpers as H

scenarios("features/noqc.feature")


@given("the queue app is open for a vial-only container")
def _open_app_with_container(page: Page, queue_app_url: str, set_session) -> None:
    # Container 37182 is registered with only vial samples (no plates fixture),
    # so the sample_type / plate_layout / qc_layout cascade has a non-empty
    # state when we switch Tech Area to Metabolomics.
    set_session(is_employee=False, entity_id=37182)
    H.open_app(page, queue_app_url)


@when(parsers.parse('I set "{label}" to "{value}"'))
def _set_selector(page: Page, label: str, value: str) -> None:
    H.set_dropdown(page, label, value)


@then("the Pattern picker is hidden")
def _pattern_hidden(page: Page) -> None:
    H.expect_dropdown_hidden(page, "Pattern")


@then("the Pattern picker is visible")
def _pattern_visible(page: Page) -> None:
    H.expect_dropdown_visible(page, "Pattern")
