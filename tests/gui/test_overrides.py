"""Scenario T18 — inj_vol and qc_frequency overrides.

Two scenarios verify that the per-run override fields actually flow through to
the generated queue:

* ``inj_vol_field`` (queue_app.py:647) — a default of 1 µl; we set it to 5 and
  assert every user-sample row uses 5.0 in the Inj Vol column. (QC rows keep
  their template-defined volumes; the override only affects user samples.)
* ``qc_frequency_field`` (queue_app.py:546-550) — the standard Proteomics
  pattern injects QC every 8 samples; with 12 samples that yields one mid-run
  QC. With ``qc_frequency=4``, three QCs interleave the 12 samples.
"""

from __future__ import annotations

from pathlib import Path

import polars as pl
from playwright.sync_api import Page, expect
from pytest_bdd import given, parsers, scenarios, then, when

from tests.gui._helpers import open_app, select_order, set_dropdown, sidebar

scenarios("features/overrides.feature")


class _State:
    def __init__(self) -> None:
        self.downloaded_path: Path | None = None


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


def _fill_text_widget(page: Page, label_contains: str, value: str) -> None:
    """Fill the <input> inside a sidebar marimo-text widget identified by its label.

    ``mo.ui.text(label=...)`` renders as ``<marimo-text data-label='"..."'>`` (the
    label is a JSON-encoded markdown string in the attribute). ``get_by_label``
    sometimes resolves it via ARIA, but special characters in the label (here, µ)
    break the lookup — so we scope to the marimo-text component by its data-label
    substring and pick the inner input directly.
    """
    sidebar(page).locator(f'marimo-text[data-label*="{label_contains}"] input').first.fill(value)


@when(parsers.parse('I set the injection volume to "{value}"'))
def _set_inj_vol(page: Page, value: str) -> None:
    _fill_text_widget(page, "Inj Vol", value)


@when(parsers.parse('I set the QC frequency to "{value}"'))
def _set_qc_freq(page: Page, value: str) -> None:
    _fill_text_widget(page, "QC frequency", value)


@when("I upload to B-Fabric")
def _upload(page: Page) -> None:
    page.wait_for_load_state("networkidle", timeout=15_000)
    btn = page.get_by_role("button", name="Upload to B-Fabric")
    expect(btn).to_be_enabled(timeout=15_000)
    btn.click()


@then("I download the queue CSV")
def _download(page: Page, state: _State) -> None:
    page.wait_for_load_state("networkidle", timeout=15_000)
    link = page.get_by_role("link", name="Download Queue File")
    expect(link).to_be_visible(timeout=15_000)
    with page.expect_download(timeout=15_000) as info:
        link.click()
    path = info.value.path()
    assert path is not None, "Playwright did not materialize the download"
    state.downloaded_path = Path(path)


def _load_df(state: _State) -> pl.DataFrame:
    assert state.downloaded_path is not None, "Download step did not run"
    return pl.read_csv(state.downloaded_path, skip_rows=1)


@then(parsers.parse('every user-sample row uses injection volume "{vol}"'))
def _inj_vol_applied(state: _State, vol: str) -> None:
    df = _load_df(state)
    user_rows = df.filter(pl.col("Sample Name").str.starts_with("Sample_"))
    actual_vols = user_rows["Inj Vol"].cast(pl.Utf8).to_list()
    expected = float(vol)
    assert all(float(v) == expected for v in actual_vols), (
        f"Expected all user-sample Inj Vol to be {expected}; got {actual_vols}"
    )


@then(parsers.parse("the number of autoQC01 injections is at least {n:d}"))
def _autoqc01_count(state: _State, n: int) -> None:
    df = _load_df(state)
    qc_rows = df.filter(pl.col("Sample Name") == "autoQC01")
    assert qc_rows.height >= n, (
        f"Expected at least {n} autoQC01 injections; got {qc_rows.height}. "
        f"Sample Name column: {df['Sample Name'].to_list()}"
    )
