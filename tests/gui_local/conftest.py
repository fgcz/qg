"""Pytest fixtures for hermetic GUI tests of ``queue_app_local.py``.

The local app is entirely B-Fabric-free. It is spawned via ``marimo run``
(mirroring the production ``launcher_local.py`` path) rather than the portal's
uvicorn/FastAPI wrapper, so no ``qg[bfabric]`` extra is required. These tests
are NOT auto-marked ``bfabric`` — they run on a core install.

Flow:
1. ``local_app_url`` (session-scoped) spawns ``marimo run --headless --no-token``
   on a free port and polls until it responds.
2. Playwright's ``page`` fixture drives the rendered UI.
"""

from __future__ import annotations

import os
import socket
import subprocess
import sys
import time
from pathlib import Path
from urllib.error import URLError
from urllib.request import urlopen

import pytest

_REPO_ROOT = Path(__file__).resolve().parents[2]
_LOCAL_APP_PATH = _REPO_ROOT / "src" / "qg" / "apps" / "queue_app_local.py"


def _free_port() -> int:
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        s.bind(("127.0.0.1", 0))
        return s.getsockname()[1]


def _wait_for(url: str, *, timeout: float = 45.0) -> None:
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
    raise RuntimeError(f"local app at {url} did not become ready in {timeout}s (last error: {last_err})")


def _sanity_check_local_app() -> None:
    """Fast static gate: ``marimo check queue_app_local.py`` before the expensive suite.

    A graph-level error in the notebook (duplicate variable, etc.) makes the app
    refuse to load, failing every test at startup after a long wait. This catches
    it in <1s.
    """
    result = subprocess.run(
        [sys.executable, "-m", "marimo", "check", str(_LOCAL_APP_PATH)],
        cwd=_REPO_ROOT,
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        pytest.fail(
            "marimo sanity check failed for queue_app_local.py — skipping local GUI tests "
            "(the app would not load). Fix the notebook errors below:\n\n"
            f"{result.stdout}{result.stderr}",
            pytrace=False,
        )


@pytest.fixture(scope="session")
def local_app_url() -> str:
    """Spawn the local queue app via ``marimo run`` and yield its base URL.

    Uses ``--no-token`` so no authentication handshake is needed in tests.
    ``QG_CONFIG_DIR`` is left unset (defaults to the bundled ``qg_configs/``).
    """
    _sanity_check_local_app()

    port = _free_port()
    env = {
        **os.environ,
        "PYTHONPATH": str(_REPO_ROOT),
    }
    proc = subprocess.Popen(
        [
            sys.executable,
            "-m",
            "marimo",
            "run",
            "--headless",
            "--no-token",
            "--host",
            "127.0.0.1",
            "--port",
            str(port),
            str(_LOCAL_APP_PATH),
        ],
        cwd=_REPO_ROOT,
        env=env,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
    )
    url = f"http://127.0.0.1:{port}"
    try:
        _wait_for(url, timeout=60.0)
    except Exception:
        proc.terminate()
        out = proc.stdout.read().decode(errors="replace") if proc.stdout else ""
        raise RuntimeError(f"marimo run failed to start:\n{out}") from None

    try:
        yield url
    finally:
        proc.terminate()
        try:
            proc.wait(timeout=5)
        except subprocess.TimeoutExpired:
            proc.kill()
