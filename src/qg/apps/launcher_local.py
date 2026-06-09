"""Entry point for the standalone, B-Fabric-free local queue app.

``qg-app-local`` launches ``queue_app_local.py`` with ``marimo run``. Unlike the
portal launcher (``qg.gitlab.launcher``), this imports no B-Fabric/GitLab code and
works on a core ``pip install qg`` (no extra).
"""

from __future__ import annotations

import subprocess
import sys
from pathlib import Path


def launch_local_app() -> None:
    app_path = Path(__file__).with_name("queue_app_local.py")
    subprocess.run([sys.executable, "-m", "marimo", "run", str(app_path)], check=True)


if __name__ == "__main__":
    launch_local_app()
