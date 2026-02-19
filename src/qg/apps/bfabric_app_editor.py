"""B-Fabric authenticated config editor (config_editor.py with git pull on startup)."""

from pathlib import Path

from loguru import logger

from qg.apps._bfabric_auth import create_bfabric_fastapi_app
from qg.gitlab._git import find_repo_root
from qg.gitlab.launcher import _git_pull

# Pull latest configs on startup
try:
    _git_pull(find_repo_root(Path(__file__).parent))
except Exception:
    logger.exception("Git pull failed on startup, continuing with current state")

app = create_bfabric_fastapi_app(Path(__file__).parent / "config_editor.py")
