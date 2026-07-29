"""GitLab API service for config review workflow."""

import re
from datetime import UTC, datetime

import gitlab
from gitlab import exceptions as gitlab_exceptions
from loguru import logger


class GitLabConfigService:
    """Create branches, atomic commits, and merge requests on GitLab."""

    def __init__(self, gitlab_url: str, project: str, private_token: str):
        """Connect to GitLab via python-gitlab.

        Args:
            gitlab_url: GitLab instance URL (e.g. "https://gitlab.example.com").
            project: Project path (e.g. "group/project").
            private_token: GitLab personal access token.
        """
        self._gl = gitlab.Gitlab(gitlab_url, private_token=private_token)
        self._project = self._gl.projects.get(project)

    def submit_config_update(
        self,
        file_contents: dict[str, str],
        author: str,
        description: str,
    ) -> str:
        """Create branch, atomic commit, and merge request.

        Args:
            file_contents: Mapping of repo-relative path → file content.
            author: Author name for branch naming and MR description.
            description: Change description for commit message and MR.

        Returns:
            Merge request web URL.

        Raises:
            gitlab.exceptions.GitlabError: On any GitLab API failure.
        """
        timestamp = datetime.now(UTC).strftime("%Y%m%d-%H%M%S")
        safe_author = re.sub(r"[^a-z0-9_-]", "", author.lower().replace(" ", "_"))
        branch_name = f"config-update/{timestamp}-{safe_author}"

        try:
            # Create branch from main
            self._project.branches.create({"branch": branch_name, "ref": "main"})
            logger.info("Created branch: {}", branch_name)

            # Build atomic commit with all file changes
            actions = [
                {
                    "action": "update",
                    "file_path": path,
                    "content": content,
                }
                for path, content in file_contents.items()
            ]

            commit_message = f"config: {description}\n\nAuthor: {author}"
            self._project.commits.create(
                {
                    "branch": branch_name,
                    "commit_message": commit_message,
                    "actions": actions,
                }
            )
            logger.info("Committed {} file(s) to {}", len(actions), branch_name)

            # Create merge request
            mr = self._project.mergerequests.create(
                {
                    "source_branch": branch_name,
                    "target_branch": "main",
                    "title": f"Config update: {description}",
                    "description": (
                        f"**Author:** {author}\n\n"
                        f"**Description:** {description}\n\n"
                        f"**Files changed:** {len(file_contents)}\n\n"
                        + "\n".join(f"- `{p}`" for p in sorted(file_contents))
                    ),
                    "remove_source_branch": True,
                }
            )
            logger.info("Created MR: {}", mr.web_url)
            return mr.web_url

        except gitlab_exceptions.GitlabError:
            logger.exception("GitLab API error during config submission")
            raise
