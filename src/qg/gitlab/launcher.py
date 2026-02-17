"""Launcher: git pull + start queue app."""

import subprocess
import sys
from pathlib import Path

from loguru import logger


def _find_repo_root() -> Path:
    """Find the git repository root."""
    current = Path(__file__).resolve().parent
    for _ in range(10):
        if (current / ".git").exists():
            return current
        parent = current.parent
        if parent == current:
            break
        current = parent

    msg = "Cannot find git repository root"
    raise FileNotFoundError(msg)


def _git_pull(repo_root: Path) -> None:
    """Run git pull --ff-only, logging the result."""
    try:
        result = subprocess.run(
            ["git", "pull", "--ff-only"],
            cwd=repo_root,
            capture_output=True,
            text=True,
            timeout=30,
        )
        if result.returncode == 0:
            output = result.stdout.strip()
            if "Already up to date" in output:
                logger.info("Git: already up to date")
            else:
                logger.info("Git: updated\n{}", output)
        else:
            logger.warning("Git pull failed (continuing anyway): {}", result.stderr.strip())
    except subprocess.TimeoutExpired:
        logger.warning("Git pull timed out after 30s (continuing anyway)")
    except FileNotFoundError:
        logger.warning("Git not found on PATH (continuing anyway)")


def launch_queue_app() -> None:
    """Git pull + start queue app. CLI entry point."""
    repo_root = _find_repo_root()
    logger.info("Repository root: {}", repo_root)

    _git_pull(repo_root)

    app_path = repo_root / "src" / "qg" / "apps" / "queue_app.py"
    if not app_path.exists():
        logger.error("Queue app not found: {}", app_path)
        sys.exit(1)

    logger.info("Starting queue app: {}", app_path)
    subprocess.run(
        ["marimo", "run", str(app_path)],
        cwd=repo_root,
    )
