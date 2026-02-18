"""Launcher: git pull + start queue app."""

import subprocess
import sys
from pathlib import Path

from loguru import logger

from qg.gitlab._git import find_repo_root


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


def _launch_marimo_app(app_relative_path: str, *, review: bool = False) -> None:
    """Git pull + start a marimo app."""
    repo_root = find_repo_root(Path(__file__).parent)
    logger.info("Repository root: {}", repo_root)

    _git_pull(repo_root)

    app_path = repo_root / app_relative_path
    if not app_path.exists():
        logger.error("App not found: {}", app_path)
        sys.exit(1)

    cmd = ["marimo", "run", str(app_path)]
    if review:
        cmd += ["--", "--review"]

    logger.info("Starting: {}", app_path)
    subprocess.run(cmd, cwd=repo_root)


def launch_queue_app() -> None:
    """Git pull + start queue app. CLI entry point."""
    _launch_marimo_app("src/qg/apps/queue_app.py")


def launch_config_editor() -> None:
    """Git pull + start config editor in review mode. CLI entry point."""
    _launch_marimo_app("src/qg/apps/config_editor.py", review=True)
