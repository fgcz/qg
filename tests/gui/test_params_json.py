"""GUI scenario for the Download Params JSON round-trip.

The "Download Params JSON" button lives at ``queue_app.py:1143`` and dumps the
resolved :class:`qg.params_models.QueueParameters` plus the :class:`QueueInput`
(``VialQueueInput | PlateQueueInput``). The JSON structure is::

    {
        "parameters": {"tech_area": ..., "instrument": ..., "sampler": ..., ...},
        "queue": {"batches": [...], "plates": [...], "cells": [...]},
    }

For a plate queue, each entry in ``queue.cells`` is a ``PlateCell`` whose
``sample.container_id`` (``params_models.py:39``) reflects the selected order.
"""

from __future__ import annotations

from typing import Any

from playwright.sync_api import Page
from pytest_bdd import given, parsers, scenarios, then, when

from tests.gui import _helpers as H

scenarios("features/params_json.feature")


# ----------------------------------------------------------------------------
# Shared fixture: the parsed params JSON is captured during the When step and
# read back in the Then steps. pytest-bdd shares state across steps via the
# test-function-scoped dict pattern below.
# ----------------------------------------------------------------------------


@given("the queue app is open as an employee", target_fixture="params_state")
def _open_app(page: Page, queue_app_url: str) -> dict[str, Any]:
    H.open_app(page, queue_app_url)
    return {}


@when(parsers.parse('I set "{label}" to "{value}"'))
def _set_selector(page: Page, label: str, value: str) -> None:
    H.set_dropdown(page, label, value)


@when(parsers.parse("I select order {container_id:d}"))
def _select_order(page: Page, container_id: int) -> None:
    H.select_order(page, container_id)


@when("I download the params JSON")
def _download_params(page: Page, params_state: dict[str, Any]) -> None:
    # The Download Params JSON link lives in the "Parameters" tab; inactive
    # tabs are hidden via CSS `display:none` (queue_app.py:1444) which makes the
    # link invisible to Playwright's default visibility filter. Switch tabs first.
    page.locator('[role="radio"][value="Parameters"]').click()
    params_state["json"] = H.download_params_json(page)


@then(
    parsers.parse('the params JSON has tech_area "{tech_area}" and instrument "{instrument}" and sampler "{sampler}"')
)
def _check_parameters(params_state: dict[str, Any], tech_area: str, instrument: str, sampler: str) -> None:
    data = params_state["json"]
    parameters = data["parameters"]
    assert parameters["tech_area"] == tech_area, parameters
    assert parameters["instrument"] == instrument, parameters
    assert parameters["sampler"] == sampler, parameters


@then(parsers.parse("the params JSON has at least one sample with container_id {container_id:d}"))
def _check_container_id(params_state: dict[str, Any], container_id: int) -> None:
    data = params_state["json"]
    queue = data["queue"]
    cells = queue.get("cells", [])
    matching = [c for c in cells if c.get("sample", {}).get("container_id") == container_id]
    assert matching, f"expected at least one cell with sample.container_id == {container_id}; got cells={cells!r}"


@then("the params JSON parameters include queue_type, queue_pattern, randomization, polarity, date, and user")
def _check_core_params(params_state: dict[str, Any]) -> None:
    parameters = params_state["json"]["parameters"]
    assert parameters["queue_type"] == "Plate", parameters
    assert parameters["queue_pattern"], "queue_pattern is empty"
    assert parameters["randomization"] in {"no", "random", "blocked"}, parameters["randomization"]
    # Proteomics uses single positive polarity (no expansion).
    assert parameters["polarity"] == ["pos"], parameters["polarity"]
    assert parameters["date"], "date is empty"
    assert "user" in parameters, parameters


@then("the params JSON parameters include method, inj_vol_override, and qc_frequency_override")
def _check_override_params(params_state: dict[str, Any]) -> None:
    parameters = params_state["json"]["parameters"]
    assert "method" in parameters, "method field missing"
    assert isinstance(parameters["method"], dict), parameters["method"]
    # ``inj_vol_field`` defaults to "1" — never empty — so the override is always
    # a float once parsed. ``qc_frequency_field`` defaults to empty placeholder,
    # parsed as ``None``. Both keys must exist on the JSON regardless.
    assert "inj_vol_override" in parameters, "inj_vol_override key missing"
    assert isinstance(parameters["inj_vol_override"], int | float), parameters["inj_vol_override"]
    assert "qc_frequency_override" in parameters, "qc_frequency_override key missing"
    assert parameters["qc_frequency_override"] is None or isinstance(parameters["qc_frequency_override"], int), (
        parameters["qc_frequency_override"]
    )
