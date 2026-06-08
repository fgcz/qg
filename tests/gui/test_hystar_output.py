"""Scenario T12 — Hystar XML output for a TIMSTOF instrument.

TIMSTOF instruments (Proteomics: TIMSTOF_1, TIMSTOFFLEX_1) use the ``hystar``
output_format in ``qg_configs/core/formatting/instruments.csv``. The queue-app
``output_file_extension`` cell (``src/qg/apps/queue_app.py:1244``) maps that
output_format to a ``.xml`` extension; ``src/qg/hystar_xml_writer.py`` emits a
``<SampleTable>`` document with one ``<Sample>`` child per row.
"""

from __future__ import annotations

from pathlib import Path
from xml.etree import ElementTree as ET

from playwright.sync_api import Page
from pytest_bdd import given, parsers, scenarios, then, when

from tests.gui import _helpers as H

scenarios("features/hystar_output.feature")


class _State:
    """Per-scenario mutable bag for the downloaded file path."""

    def __init__(self) -> None:
        self.downloaded: Path | None = None
        self.suggested_filename: str | None = None


@given("the queue app is open as an employee", target_fixture="state")
def _open(page: Page, queue_app_url: str) -> _State:
    H.open_app(page, queue_app_url)
    return _State()


@when(parsers.parse('I set "{label}" to "{value}"'))
def _set(page: Page, label: str, value: str) -> None:
    H.set_dropdown(page, label, value)


@when(parsers.parse("I select order {container_id:d}"))
def _select_order(page: Page, container_id: int) -> None:
    H.select_order(page, container_id)


@when("I upload to B-Fabric")
def _upload(page: Page) -> None:
    H.upload_to_bfabric(page)


@then("I download the queue file")
def _download(page: Page, state: _State) -> None:
    # We need the suggested filename (to assert the extension), not just the
    # tmpfile path that Playwright materialises — capture both here. `mo.download`
    # renders as an `<a>` anchor (not a button), so we click via the link role.
    with page.expect_download(timeout=15_000) as info:
        page.get_by_role("link", name="Download Queue File").click()
    download = info.value
    path = download.path()
    assert path is not None, "Playwright did not materialize the download"
    state.downloaded = Path(path)
    state.suggested_filename = download.suggested_filename


@then(parsers.parse('the downloaded filename ends with "{suffix}"'))
def _filename_suffix(state: _State, suffix: str) -> None:
    assert state.suggested_filename is not None, "Download step did not run"
    assert state.suggested_filename.endswith(suffix), (
        f"Suggested filename {state.suggested_filename!r} does not end with {suffix!r}"
    )


@then("the downloaded file is a valid Hystar XML sequence")
def _is_valid_hystar(state: _State) -> None:
    assert state.downloaded is not None, "Download step did not run"
    root = ET.parse(state.downloaded).getroot()
    assert root.tag == "SampleTable", f"expected <SampleTable> root, got <{root.tag}>"
    samples = root.findall("Sample")
    assert samples, "Hystar XML contains no <Sample> elements"
    # Every <Sample> must carry the Hystar-required attributes — a partial writer
    # that emitted shells without DataPath/SuperMethod would silently break the
    # scheduler import.
    for sample in samples:
        for required in ("Position", "SampleID", "DataPath", "SuperMethod"):
            assert sample.get(required) is not None, (
                f"<Sample> missing required attribute {required!r}: {sample.attrib}"
            )
