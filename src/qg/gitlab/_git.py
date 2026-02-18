"""Shared git utilities for the gitlab package."""

from pathlib import Path


def find_repo_root(start: Path, max_depth: int = 10) -> Path:
    """Walk up from *start* to find the git repository root.

    Args:
        start: Directory to start searching from.
        max_depth: Maximum number of parent directories to check.

    Raises:
        FileNotFoundError: If no .git directory is found.
    """
    current = start.resolve()
    for _ in range(max_depth):
        if (current / ".git").exists():
            return current
        parent = current.parent
        if parent == current:
            break
        current = parent
    msg = "Cannot find git repository root"
    raise FileNotFoundError(msg)
