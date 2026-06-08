"""GUI scenarios for the sampler-incompatibility warn callout.

The callout is rendered in ``queue_app.py:208-238``: when the intersection of
(sampler-supported queue types) and (order's actual sample composition) is
empty, the cell builds a ``mo.callout(..., kind="warn")`` whose markdown is
``"Sampler **<sampler>** is incompatible with this order's samples."``.

We use the non-employee path so the target container is auto-pinned via
``set_session(is_employee=False, entity_id=...)``; this avoids depending on
the employee project-table row-click flow.

Fixture combo for the positive scenario:

- Instrument ``LUMOS_2`` only registers the ``MClass`` sampler, which itself
  only supports Vial-mode queues (see ``qg_configs/core/position/sampler.toml``
  + ``sampler_plate_layouts.csv``; verified via
  ``QGConfiguration.to_overview_table()``). After setting Instrument the
  Sampler dropdown auto-defaults to MClass — no explicit Sampler step needed.
- Container ``37180`` is plate-only — all 12 samples are on plate ``50001``
  in ``tests/gui/fixtures/bfabric/plates_37180.json`` and none are bare vials,
  so ``BfabricHelper.get_container_composition`` returns
  ``has_plates=True, has_vials=False``.

Intersection of ``{Vial}`` (sampler) and ``{Plate}`` (order) is empty,
triggering the warning. The negative scenario uses Vanquish on the same
order; Vanquish supports both Vial and Plate, so the intersection is
``{Plate}`` and no warning renders.
"""

from __future__ import annotations

from collections.abc import Callable

from playwright.sync_api import Page, expect
from pytest_bdd import given, parsers, scenarios, then, when

from tests.gui import _helpers as H

scenarios("features/sampler_incompat.feature")


@given(parsers.parse("a non-employee session pinned to container {container_id:d}"))
def _pin_non_employee(set_session: Callable[..., None], container_id: int) -> None:
    set_session(is_employee=False, entity_id=container_id)


@when("I open the queue app")
def _open_app(page: Page, queue_app_url: str) -> None:
    H.open_app(page, queue_app_url)


@when(parsers.parse('I set "{label}" to "{value}"'))
def _set_selector(page: Page, label: str, value: str) -> None:
    H.set_dropdown(page, label, value)


@then(parsers.parse('a warning reads "{text}"'))
def _warning_reads(page: Page, text: str) -> None:
    H.assert_warn(page, text)


@then("no sampler-incompatibility warning is shown")
def _no_incompat_warning(page: Page) -> None:
    expect(page.get_by_text("incompatible with this order's samples", exact=False)).to_have_count(0)
