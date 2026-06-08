"""GUI scenarios for the per-level concentration grid in ``queue_app.py``.

The grid is rendered at ``queue_app.py:346-422``: when the selected QC layout
contains ``standard``-type samples with ``levels`` (e.g. Metabolomics +
``cal_series``), a ``{level: {value, unit}}`` grid of widgets is shown. The
preset is 7 levels with values ``[100, 50, 25, 12, 6, 3, 1]`` in ``umol``.

This module asserts that the grid appears for ``cal_series`` and vanishes when
the user switches back to a non-cal layout (``standard``).

Note on session: the fixture container 37180 is registered as a Proteomics
project, so an employee-mode user cannot select it as a Metabolomics order
(the project table filters by Area). We therefore pin the container via a
non-employee session, which short-circuits the project table and populates
``selected_orders`` directly — letting the QC Layout dropdown's option list
populate from the chosen Tech Area independently of project metadata.
"""

from __future__ import annotations

from collections.abc import Callable

from playwright.sync_api import Page, expect
from pytest_bdd import given, parsers, scenarios, then, when

from tests.gui import _helpers as H

scenarios("features/concentrations.feature")


def _number_inputs(page: Page):
    """Sidebar concentration value inputs.

    Marimo wraps ``mo.ui.number`` in a ``<marimo-number>`` custom element whose
    underlying ``<input>`` is ``type='text'``; ``<marimo-number>`` is what we
    actually want to count (one per concentration row).
    """
    return H.sidebar(page).locator("marimo-number")


def _unit_dropdowns(page: Page):
    """Sidebar dropdowns whose option list includes ``umol`` — i.e. unit pickers."""
    return H.sidebar(page).locator("select").filter(has=page.locator("option", has_text="umol"))


# The cal_series QC layout has 7 calibration levels (samples.csv, cal1..cal7);
# this is a config-level fact and what "one row per calibration level" expands to.
_CAL_SERIES_LEVELS = 7


@given("a non-employee session pinned to a Metabolomics container")
def _pin_non_employee(set_session: Callable[..., None]) -> None:
    # Container 37180 is registered as Proteomics in projects.csv, but the
    # non-employee path bypasses the project-table area filter and lets the QC
    # Layout cascade populate from the chosen Tech Area directly — so we can
    # exercise Metabolomics layouts against this container without polluting
    # the project metadata.
    set_session(is_employee=False, entity_id=37180)


@when("I open the queue app")
def _open_app(page: Page, queue_app_url: str) -> None:
    H.open_app(page, queue_app_url)


@when(parsers.parse('I set "{label}" to "{value}"'))
def _set_dropdown(page: Page, label: str, value: str) -> None:
    # Switching an upstream dropdown (e.g. Tech Area) makes marimo re-render the
    # downstream dropdowns asynchronously, which creates two races:
    #   1. selecting before the option exists — wait for the option text first;
    #   2. a re-render landing just after `select_option`, rebuilding the widget
    #      at its default and silently dropping our choice. That left the
    #      cal_series concentration grid unrendered ~1 CI run in 4.
    # Defend against both: re-select until the chosen option actually reports as
    # selected. Matching `option:checked` by text is independent of how marimo
    # encodes the `<option value=...>`, and the locators re-resolve each loop so
    # a fresh post-re-render dropdown is targeted.
    dropdown = H.sidebar(page).get_by_label(label)
    expect(dropdown.locator("option", has_text=value)).to_have_count(1)
    selected = dropdown.locator("option:checked")
    for _ in range(15):
        dropdown.select_option(value)
        try:
            expect(selected).to_have_text(value, timeout=1_000)
            return
        except AssertionError:
            page.wait_for_timeout(200)
    expect(selected).to_have_text(value)  # retries exhausted — fail with context


@then("one row per calibration level is shown")
def _rows_visible(page: Page) -> None:
    # Once cal_series is the committed QC Layout (see the sticky select in
    # _set_dropdown), the grid renders reactively. The suite-wide 15s budget
    # covers the marimo round-trip.
    expect(_number_inputs(page)).to_have_count(_CAL_SERIES_LEVELS, timeout=15_000)


@then("each row offers a configurable concentration value and unit")
def _row_has_value_and_unit(page: Page) -> None:
    n_values = _number_inputs(page).count()
    expect(_unit_dropdowns(page)).to_have_count(n_values, timeout=15_000)


@then("the concentration grid is not visible")
def _grid_hidden(page: Page) -> None:
    expect(_number_inputs(page)).to_have_count(0)
    expect(_unit_dropdowns(page)).to_have_count(0)
