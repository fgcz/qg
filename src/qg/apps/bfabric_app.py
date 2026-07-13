"""B-Fabric authenticated queue app (original entry point, no git pull)."""

import os

# Redirect marimo's XDG lookups to a writable location before importing it.
# The container runs as a non-root user with a read-only HOME, so marimo's
# default ~/.config and ~/.local/state probes raise "Permission denied" on
# every request. `marimo run` doesn't need persistent config or recents.
os.environ.setdefault("XDG_CONFIG_HOME", "/tmp/marimo-xdg-config")
os.environ.setdefault("XDG_STATE_HOME", "/tmp/marimo-xdg-state")

from pathlib import Path

from qg.apps._bfabric_auth import create_bfabric_fastapi_app
from qg.logging_setup import configure_logging

configure_logging()

app = create_bfabric_fastapi_app(
    Path(__file__).parent / "queue_app.py",
    app_name="queue-gen",
    mount_path="/queue-gen",
)

if __name__ == "__main__":
    import uvicorn

    uvicorn.run(app, host="localhost", port=8000)
