"""Pytest fixtures for hermetic GUI tests of `queue_app.py`.

The flow:

1. ``queue_app_url`` (session-scoped) spawns ``uvicorn tests.gui._test_app:app`` in a
   subprocess on a free port and waits for it to respond.
2. The subprocess reads ``QG_TEST_FIXTURES_DIR`` and installs ``_TEST_SESSION_FACTORY``
   so the marimo app's first cell gets a fake-backed ``AppSession``.
3. ``QG_CACHE_DIR`` is seeded with a per-instance projects CSV so the employee
   container-browser cell loads without contacting B-Fabric.
4. Playwright's ``page`` fixture (from ``pytest-playwright``) drives the rendered UI.
"""

from __future__ import annotations

import json
import os
import shutil
import socket
import subprocess
import sys
import time
from collections.abc import Callable
from pathlib import Path
from urllib.error import URLError
from urllib.request import Request, urlopen

import pytest

_REPO_ROOT = Path(__file__).resolve().parents[2]
_FIXTURES_DIR = Path(__file__).resolve().parent / "fixtures"
_BFABRIC_FIXTURES = _FIXTURES_DIR / "bfabric"
_PROJECTS_CSV = _FIXTURES_DIR / "projects.csv"
_FAKE_INSTANCE_SLUG = "fake.bfabric.test"


def pytest_addoption(parser: pytest.Parser) -> None:
    parser.addoption(
        "--gui-tier",
        action="store",
        default="hermetic",
        choices=("hermetic", "bfabric-test"),
        help=(
            "Choose the GUI test backend. 'hermetic' (default) uses a fake Bfabric "
            "client; 'bfabric-test' targets fgcz-bfabric-test.uzh.ch via the user's "
            "~/.bfabricpy.yml. Combine with '-m bfabric_test_instance' to run only "
            "the real-instance suite."
        ),
    )


def pytest_collection_modifyitems(config: pytest.Config, items: list[pytest.Item]) -> None:
    """Mark GUI items with `bfabric` (they run the portal app, which needs the
    qg[bfabric] extra) and, on Tier B, with `bfabric_test_instance`."""
    is_tier_b = config.getoption("--gui-tier") == "bfabric-test"
    gui_dir = Path(__file__).resolve().parent
    for item in items:
        if gui_dir in Path(item.fspath).resolve().parents:
            item.add_marker(pytest.mark.bfabric)
            if is_tier_b:
                item.add_marker(pytest.mark.bfabric_test_instance)


def _free_port() -> int:
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        s.bind(("127.0.0.1", 0))
        return s.getsockname()[1]


def _wait_for(url: str, *, timeout: float = 30.0) -> None:
    deadline = time.monotonic() + timeout
    last_err: Exception | None = None
    while time.monotonic() < deadline:
        try:
            with urlopen(url, timeout=1.0) as resp:  # noqa: S310 — localhost only
                if resp.status < 500:
                    return
        except (URLError, ConnectionError, TimeoutError) as exc:
            last_err = exc
            time.sleep(0.25)
    raise RuntimeError(f"queue app at {url} did not become ready in {timeout}s (last error: {last_err})")


def _tier_b_env() -> dict[str, str]:
    """Validate ~/.bfabricpy.yml is set up for the test instance and return env overrides."""
    bfabric_yml = Path.home() / ".bfabricpy.yml"
    if not bfabric_yml.exists():
        pytest.skip(f"Tier-B requires {bfabric_yml} pointing at fgcz-bfabric-test.uzh.ch", allow_module_level=False)
    return {"QG_ALLOW_UNAUTHENTICATED": "1"}


_QUEUE_APP_PATH = _REPO_ROOT / "src" / "qg" / "apps" / "queue_app.py"


def _sanity_check_app_notebook() -> None:
    """Fast static gate before the expensive GUI suite.

    A graph-level error in the marimo notebook (e.g. a variable defined in multiple
    cells) makes the app refuse to load, which fails *every* GUI test at startup
    after minutes of browser/uvicorn churn. ``marimo check`` catches that statically
    in <1s. Non-strict mode fails only on errors/critical, not formatting warnings.
    """
    result = subprocess.run(
        [sys.executable, "-m", "marimo", "check", str(_QUEUE_APP_PATH)],
        cwd=_REPO_ROOT,
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        pytest.fail(
            "marimo sanity check failed for queue_app.py — skipping GUI tests "
            "(the app would not load). Fix the notebook errors below:\n\n"
            f"{result.stdout}{result.stderr}",
            pytrace=False,
        )


@pytest.fixture(scope="session")
def queue_app_url(request: pytest.FixtureRequest, tmp_path_factory: pytest.TempPathFactory) -> str:
    """Spawn the queue app in a uvicorn subprocess and yield its base URL.

    Tier A (``--gui-tier=hermetic``, default): installs a fake-Bfabric session factory.
    Tier B (``--gui-tier=bfabric-test``): bypasses auth middleware but uses the real
    `Bfabric.connect()` against fgcz-bfabric-test.uzh.ch (creds in ~/.bfabricpy.yml).
    """
    _sanity_check_app_notebook()

    tier = request.config.getoption("--gui-tier")
    cache_root = tmp_path_factory.mktemp("qg_cache")

    extra_env: dict[str, str]
    if tier == "hermetic":
        instance_cache = cache_root / _FAKE_INSTANCE_SLUG
        instance_cache.mkdir(parents=True, exist_ok=True)
        shutil.copy(_PROJECTS_CSV, instance_cache / "bfabric_container.csv")
        extra_env = {"QG_TEST_FIXTURES_DIR": str(_BFABRIC_FIXTURES)}
    else:
        extra_env = _tier_b_env()

    port = _free_port()
    env = {
        **os.environ,
        **extra_env,
        "QG_CACHE_DIR": str(cache_root),
        "PYTHONPATH": str(_REPO_ROOT),
    }
    proc = subprocess.Popen(
        [
            sys.executable,
            "-m",
            "uvicorn",
            "tests.gui._test_app:app",
            "--host",
            "127.0.0.1",
            "--port",
            str(port),
            "--log-level",
            "warning",
        ],
        cwd=_REPO_ROOT,
        env=env,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
    )
    url = f"http://127.0.0.1:{port}"
    try:
        _wait_for(url, timeout=45.0)
    except Exception:
        proc.terminate()
        out = proc.stdout.read().decode(errors="replace") if proc.stdout else ""
        raise RuntimeError(f"uvicorn failed to start:\n{out}") from None

    try:
        yield url
    finally:
        proc.terminate()
        try:
            proc.wait(timeout=5)
        except subprocess.TimeoutExpired:
            proc.kill()


@pytest.fixture
def set_session(queue_app_url: str) -> Callable[..., None]:
    """Swap the active fake-Bfabric session for the next page load.

    Usage::

        def test_x(page, queue_app_url, set_session):
            set_session(is_employee=False, entity_id=37180)
            page.goto(queue_app_url)
            ...

    Pass ``entity_class="Order"`` to simulate an employee who opened the app from an
    order (the launching-order pre-load trigger); when omitted, the server derives it
    (``None`` for employees, ``"Container"`` for non-employees).

    Resets to the default (employee, no entity_id) after the test.
    """
    seen = False

    def _set(*, is_employee: bool = True, entity_id: int | None = None, entity_class: str | None = None) -> None:
        nonlocal seen
        seen = True
        payload: dict = {"is_employee": is_employee, "entity_id": entity_id}
        if entity_class is not None:  # omit → server derives None/"Container"
            payload["entity_class"] = entity_class
        body = json.dumps(payload).encode()
        req = Request(
            f"{queue_app_url}/_test/session",
            data=body,
            headers={"content-type": "application/json"},
            method="POST",
        )
        with urlopen(req, timeout=5.0) as resp:  # noqa: S310 — localhost only
            if resp.status != 200:
                raise RuntimeError(f"session swap failed: status={resp.status}")

    yield _set

    if seen:
        # Reset to default at teardown so subsequent tests start clean.
        _set(is_employee=True, entity_id=None)
