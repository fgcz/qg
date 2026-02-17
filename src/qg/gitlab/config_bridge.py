"""Bridge between local config files and GitLab merge requests."""

import subprocess
from pathlib import Path

from loguru import logger

from qg.gitlab.service import GitLabConfigService
from qg.gitlab.settings import load_gitlab_settings

# Repo-relative prefix for config files
_CONFIG_REPO_PREFIX = "qg_configs"


def _find_repo_root(config_dir: Path) -> Path:
    """Walk up from config_dir to find the git repo root."""
    current = config_dir.resolve()
    for _ in range(10):
        if (current / ".git").exists():
            return current
        parent = current.parent
        if parent == current:
            break
        current = parent
    msg = "Cannot find git repository root"
    raise FileNotFoundError(msg)


def _get_changed_config_files(repo_root: Path, config_dir: Path) -> list[Path]:
    """Use git to find config files that differ from HEAD."""
    config_rel = config_dir.relative_to(repo_root)
    result = subprocess.run(
        ["git", "diff", "--name-only", "HEAD", "--", str(config_rel)],
        cwd=repo_root,
        capture_output=True,
        text=True,
        timeout=10,
    )
    if result.returncode != 0:
        logger.warning("git diff failed: {}", result.stderr.strip())
        return []

    changed = []
    for line in result.stdout.strip().splitlines():
        path = repo_root / line
        if path.is_file():
            changed.append(path)
    return changed


def submit_config_dir(config_dir: Path, author: str, description: str) -> str:
    """Read changed config files from disk and submit as GitLab MR.

    This is the bridge between QGConfiguration.write_all() (which writes to disk)
    and GitLab (which needs file contents as strings).

    Only files that differ from the current git HEAD are included in the MR.

    Args:
        config_dir: Path to the local qg_configs directory.
        author: Author name for the merge request.
        description: Change description for the merge request.

    Returns:
        Merge request web URL.

    Raises:
        FileNotFoundError: If config_dir doesn't exist or settings file is missing.
        ValueError: If author or description is empty, or no files changed.
    """
    if not config_dir.is_dir():
        msg = f"Config directory not found: {config_dir}"
        raise FileNotFoundError(msg)

    if not author.strip():
        msg = "Author name is required"
        raise ValueError(msg)

    if not description.strip():
        msg = "Change description is required"
        raise ValueError(msg)

    # Only collect files that actually changed vs HEAD
    repo_root = _find_repo_root(config_dir)
    changed_paths = _get_changed_config_files(repo_root, config_dir)

    if not changed_paths:
        return ""

    file_contents: dict[str, str] = {}
    for file_path in sorted(changed_paths):
        repo_relative = f"{_CONFIG_REPO_PREFIX}/{file_path.relative_to(config_dir)}"
        file_contents[repo_relative] = file_path.read_text(encoding="utf-8")

    logger.info("Found {} changed config file(s): {}", len(file_contents), list(file_contents.keys()))

    # Load settings and submit
    settings = load_gitlab_settings()
    service = GitLabConfigService(
        gitlab_url=settings["url"],
        project=settings["project"],
        private_token=settings["private_token"],
    )

    return service.submit_config_update(
        file_contents=file_contents,
        author=author,
        description=description,
    )
