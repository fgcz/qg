"""Bridge between in-memory config edits and GitLab merge requests."""

from loguru import logger

from qg.gitlab.service import GitLabConfigService
from qg.gitlab.settings import load_gitlab_settings

# Repo-relative prefix for config files
_CONFIG_REPO_PREFIX = "qg_configs"


def submit_config_changes(
    original: dict[str, str],
    edited: dict[str, str],
    author: str,
    description: str,
) -> str:
    """Diff two in-memory config snapshots and submit changes as a GitLab MR.

    Args:
        original: Snapshot of file contents at startup (from serialize_all()).
        edited: Serialized editor contents (from serialize_all()).
        author: Author name for the merge request.
        description: Change description for the merge request.

    Returns:
        Merge request web URL, or empty string if nothing changed.

    Raises:
        FileNotFoundError: If settings file is missing.
        ValueError: If author or description is empty.
    """
    if not author.strip():
        msg = "Author name is required"
        raise ValueError(msg)

    if not description.strip():
        msg = "Change description is required"
        raise ValueError(msg)

    # Diff: find files that changed or were added
    changed = {k: v for k, v in edited.items() if v != original.get(k)}

    if not changed:
        return ""

    # Prepend repo-relative prefix to each key
    file_contents = {f"{_CONFIG_REPO_PREFIX}/{k}": v for k, v in changed.items()}

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
