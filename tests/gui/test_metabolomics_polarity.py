"""GUI scenarios for the polarity-controlled method dropdowns in ``queue_app.py``.

The polarity widget is a ``mo.ui.batch`` of two checkboxes labelled ``pos`` and
``neg`` in the markdown template (queue_app.py:632-635). Both checkboxes are
always rendered; the tech-area-dependent ``_needs_both_polarities`` flag only
sets the **default** ``value`` of ``neg`` — ``True`` for Metabolomics/Lipidomics,
``False`` for Proteomics. Whether the corresponding method dropdown
(``Method Name (pos)`` / ``Method Name (neg)``) is rendered, however, follows
the live checkbox state (queue_app.py:596-625):

* ``pos`` checked + ``neg`` checked  -> both ``Method Name (pos)`` and
  ``Method Name (neg)`` dropdowns appear.
* ``pos`` checked + ``neg`` unchecked -> only the positive dropdown appears,
  and its label drops the suffix to plain ``Method Name``.

Locator note: the ``mo.ui.batch`` interpolates raw checkboxes into a markdown
string, so neither checkbox carries an accessible name. We address them
positionally via ``sidebar.get_by_role("checkbox").nth(0|1)`` — the order is
fixed by the dict passed to ``mo.ui.batch`` (``pos`` first, then ``neg``).
"""

from __future__ import annotations

from playwright.sync_api import Locator, Page, expect
from pytest_bdd import given, parsers, scenarios, then, when

from tests.gui._helpers import (
    expect_dropdown_hidden,
    expect_dropdown_visible,
    open_app,
    set_dropdown,
    sidebar,
)

scenarios("features/metabolomics_polarity.feature")

# Order matches the dict passed to mo.ui.batch at queue_app.py:634.
_POLARITY_INDEX = {"positive": 0, "negative": 1}
# Domain-name → widget-label mapping. The widget labels carry the parenthetical
# polarity suffix that distinguishes the two method pickers in the sidebar.
_METHOD_LABEL = {"positive": "Method Name (pos)", "negative": "Method Name (neg)"}


def _polarity_checkbox(page: Page, polarity: str) -> Locator:
    return sidebar(page).get_by_role("checkbox").nth(_POLARITY_INDEX[polarity])


@given("the queue app is open as an employee")
def _open_app(page: Page, queue_app_url: str) -> None:
    open_app(page, queue_app_url)


@when(parsers.parse('I set "{label}" to "{value}"'))
def _set_dropdown(page: Page, label: str, value: str) -> None:
    set_dropdown(page, label, value)


@when(parsers.parse("the user deselects the {polarity} polarity"))
def _deselect_polarity(page: Page, polarity: str) -> None:
    _polarity_checkbox(page, polarity).uncheck()


@then(parsers.parse("a method picker for the {polarity} polarity is visible"))
def _picker_visible(page: Page, polarity: str) -> None:
    expect_dropdown_visible(page, _METHOD_LABEL[polarity])


@then(parsers.parse("a method picker for the {polarity} polarity is not visible"))
def _picker_hidden(page: Page, polarity: str) -> None:
    expect_dropdown_hidden(page, _METHOD_LABEL[polarity])


@then(parsers.parse("the {polarity} polarity is selected"))
def _polarity_selected(page: Page, polarity: str) -> None:
    expect(_polarity_checkbox(page, polarity)).to_be_checked()


@then(parsers.parse("the {polarity} polarity is not selected"))
def _polarity_deselected(page: Page, polarity: str) -> None:
    expect(_polarity_checkbox(page, polarity)).not_to_be_checked()
