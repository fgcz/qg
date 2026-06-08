"""GUI scenario for the Upload-to-B-Fabric mock workunit message.

The hermetic session uses ``MockFeederUploader`` (``src/qg/bfabric_utils.py:194-201``),
which returns a markdown string of the form ``"**[Mock]** Would create workunit
in container {container_id} with {n} resources."``. The queue app renders this
via ``mo.md(upload_result)`` at ``queue_app.py:1340`` after the user clicks the
``Upload to B-Fabric`` run button (``queue_app.py:1285``).
"""

from __future__ import annotations

from playwright.sync_api import Page
from pytest_bdd import given, parsers, scenarios, then, when

from tests.gui import _helpers as H

scenarios("features/upload_workunit.feature")


@given("the queue app is open as an employee")
def _open_app(page: Page, queue_app_url: str) -> None:
    H.open_app(page, queue_app_url)


@when(parsers.parse('I set "{label}" to "{value}"'))
def _set_selector(page: Page, label: str, value: str) -> None:
    H.set_dropdown(page, label, value)


@when(parsers.parse("I select order {container_id:d}"))
def _select_order(page: Page, container_id: int) -> None:
    H.select_order(page, container_id)


@when("I upload to B-Fabric")
def _upload(page: Page) -> None:
    H.upload_to_bfabric(page)


@then(parsers.parse("the upload-result panel mentions a mock workunit in container {container_id:d}"))
def _mock_workunit_message(page: Page, container_id: int) -> None:
    message = H.get_workunit_message(page)
    assert f"Would create workunit in container {container_id}" in message, (
        f"upload-result panel did not contain the mock workunit message; got: {message!r}"
    )


@then(parsers.parse("the upload bundles the queue file and parameters.json ({n:d} resources)"))
def _bundle_resource_count(page: Page, n: int) -> None:
    """The mock uploader's message includes ``with {len(params.resources)} resources``.

    queue_app.py:1186-1189 always bundles two resources — the queue file and a
    ``parameters.json``. Verifying the count proves the bundle dict was actually
    populated, not just that the upload happened.
    """
    message = H.get_workunit_message(page)
    assert f"with {n} resources" in message, f"upload-result panel did not report {n} resources; got: {message!r}"
