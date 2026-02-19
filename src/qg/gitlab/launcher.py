"""Launcher: git pull + start queue/editor apps."""

from __future__ import annotations

import subprocess
import sys
from pathlib import Path
from typing import Annotated

import cyclopts
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


def _launch_marimo_app(
    app_relative_path: str,
    *,
    config_dir: str | None = None,
) -> None:
    """Git pull then start a marimo app.

    Args:
        app_relative_path: Path to the marimo app relative to the repo root.
        config_dir: If set, pass --config-dir to the app.
    """
    repo_root = find_repo_root(Path(__file__).parent)
    logger.info("Repository root: {}", repo_root)

    _git_pull(repo_root)

    app_path = repo_root / app_relative_path
    if not app_path.exists():
        logger.error("App not found: {}", app_path)
        sys.exit(1)

    cmd = ["marimo", "run", str(app_path)]

    # Collect extra args to pass after "--"
    extra_args: list[str] = []
    if config_dir:
        extra_args += ["--config-dir", config_dir]
    if extra_args:
        cmd += ["--"] + extra_args

    logger.info("Starting: {}", app_path)
    subprocess.run(cmd, cwd=repo_root)


def launch_queue_app() -> None:
    """Git pull + start queue app. CLI entry point."""
    app = cyclopts.App(help="Pull latest configs and start the queue generator app.")

    @app.default
    def main(
        config_dir: Annotated[
            str | None,
            cyclopts.Parameter(name=["--config-dir", "-c"], help="Config directory"),
        ] = None,
    ) -> None:
        """Pull latest configs and start the queue generator."""
        _launch_marimo_app("src/qg/apps/queue_app.py", config_dir=config_dir)

    app()


def launch_config_editor() -> None:
    """Git pull + start config editor in review mode. CLI entry point."""
    app = cyclopts.App(help="Pull latest configs and start the config editor (review mode).")

    @app.default
    def main(
        config_dir: Annotated[
            str | None,
            cyclopts.Parameter(name=["--config-dir", "-c"], help="Config directory"),
        ] = None,
    ) -> None:
        """Pull latest configs and start the config editor."""
        _launch_marimo_app("src/qg/apps/config_editor.py", config_dir=config_dir)

    app()
