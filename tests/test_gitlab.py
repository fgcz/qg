"""Tests for qg.gitlab package."""

import textwrap
from unittest.mock import MagicMock, patch

import pytest

from qg.gitlab._git import find_repo_root
from qg.gitlab.config_bridge import submit_config_dir
from qg.gitlab.service import GitLabConfigService
from qg.gitlab.settings import load_gitlab_settings

# =============================================================================
# Git utilities tests
# =============================================================================


class TestFindRepoRoot:
    def test_finds_repo_root(self, tmp_path):
        (tmp_path / ".git").mkdir()
        sub = tmp_path / "a" / "b"
        sub.mkdir(parents=True)
        assert find_repo_root(sub) == tmp_path

    def test_raises_when_no_git_dir(self, tmp_path):
        with pytest.raises(FileNotFoundError, match="Cannot find git repository root"):
            find_repo_root(tmp_path, max_depth=3)

    def test_respects_max_depth(self, tmp_path):
        (tmp_path / ".git").mkdir()
        deep = tmp_path / "a" / "b" / "c" / "d"
        deep.mkdir(parents=True)
        # max_depth=2 won't reach .git from 4 levels deep
        with pytest.raises(FileNotFoundError):
            find_repo_root(deep, max_depth=2)


# =============================================================================
# Settings tests
# =============================================================================


class TestLoadGitlabSettings:
    def test_loads_valid_settings(self, tmp_path):
        settings_file = tmp_path / ".qg_settings.toml"
        settings_file.write_text(
            textwrap.dedent("""\
                [gitlab]
                url = "https://gitlab.example.com"
                project = "group/repo"
                private_token = "glpat-test"
            """)
        )
        result = load_gitlab_settings(settings_file)
        assert result["url"] == "https://gitlab.example.com"
        assert result["project"] == "group/repo"
        assert result["private_token"] == "glpat-test"

    def test_raises_on_missing_file(self, tmp_path):
        with pytest.raises(FileNotFoundError):
            load_gitlab_settings(tmp_path / "nonexistent.toml")

    def test_raises_on_missing_gitlab_section(self, tmp_path):
        settings_file = tmp_path / ".qg_settings.toml"
        settings_file.write_text("[other]\nkey = 'val'\n")
        with pytest.raises(ValueError, match="Missing \\[gitlab\\] section"):
            load_gitlab_settings(settings_file)

    def test_raises_on_missing_keys(self, tmp_path):
        settings_file = tmp_path / ".qg_settings.toml"
        settings_file.write_text('[gitlab]\nurl = "https://example.com"\n')
        with pytest.raises(ValueError, match="Missing keys"):
            load_gitlab_settings(settings_file)

    def test_raises_on_http_url(self, tmp_path):
        settings_file = tmp_path / ".qg_settings.toml"
        settings_file.write_text(
            textwrap.dedent("""\
                [gitlab]
                url = "http://gitlab.example.com"
                project = "group/repo"
                private_token = "glpat-test"
            """)
        )
        with pytest.raises(ValueError, match="GitLab URL must use HTTPS"):
            load_gitlab_settings(settings_file)

    def test_env_var_overrides_file_token(self, tmp_path, monkeypatch):
        settings_file = tmp_path / ".qg_settings.toml"
        settings_file.write_text(
            textwrap.dedent("""\
                [gitlab]
                url = "https://gitlab.example.com"
                project = "group/repo"
                private_token = "glpat-file-token"
            """)
        )
        monkeypatch.setenv("QG_GITLAB_TOKEN", "glpat-env-token")
        result = load_gitlab_settings(settings_file)
        assert result["private_token"] == "glpat-env-token"

    def test_file_token_used_when_no_env_var(self, tmp_path, monkeypatch):
        settings_file = tmp_path / ".qg_settings.toml"
        settings_file.write_text(
            textwrap.dedent("""\
                [gitlab]
                url = "https://gitlab.example.com"
                project = "group/repo"
                private_token = "glpat-file-token"
            """)
        )
        monkeypatch.delenv("QG_GITLAB_TOKEN", raising=False)
        result = load_gitlab_settings(settings_file)
        assert result["private_token"] == "glpat-file-token"


# =============================================================================
# Service tests
# =============================================================================


class TestGitLabConfigService:
    @patch("qg.gitlab.service.gitlab.Gitlab")
    def test_submit_config_update_creates_branch_commit_mr(self, mock_gitlab_cls):
        # Set up mock chain
        mock_gl = MagicMock()
        mock_gitlab_cls.return_value = mock_gl
        mock_project = MagicMock()
        mock_gl.projects.get.return_value = mock_project

        mock_mr = MagicMock()
        mock_mr.web_url = "https://gitlab.example.com/group/repo/-/merge_requests/1"
        mock_project.mergerequests.create.return_value = mock_mr

        service = GitLabConfigService(
            gitlab_url="https://gitlab.example.com",
            project="group/repo",
            private_token="glpat-test",
        )

        result = service.submit_config_update(
            file_contents={
                "qg_configs/core/formatting/instruments.csv": "col1,col2\na,b\n",
                "qg_configs/core/formatting/samples.csv": "col1,col2\nc,d\n",
            },
            author="Test User",
            description="Update instruments",
        )

        # Verify branch creation
        mock_project.branches.create.assert_called_once()
        branch_args = mock_project.branches.create.call_args[0][0]
        assert branch_args["ref"] == "main"
        assert "config-update/" in branch_args["branch"]
        assert "test_user" in branch_args["branch"]

        # Verify atomic commit
        mock_project.commits.create.assert_called_once()
        commit_args = mock_project.commits.create.call_args[0][0]
        assert len(commit_args["actions"]) == 2
        assert commit_args["actions"][0]["action"] == "update"

        # Verify MR creation
        mock_project.mergerequests.create.assert_called_once()
        mr_args = mock_project.mergerequests.create.call_args[0][0]
        assert mr_args["target_branch"] == "main"
        assert mr_args["remove_source_branch"] is True

        assert result == "https://gitlab.example.com/group/repo/-/merge_requests/1"

    @patch("qg.gitlab.service.gitlab.Gitlab")
    def test_branch_name_sanitizes_special_chars(self, mock_gitlab_cls):
        mock_gl = MagicMock()
        mock_gitlab_cls.return_value = mock_gl
        mock_project = MagicMock()
        mock_gl.projects.get.return_value = mock_project
        mock_mr = MagicMock()
        mock_mr.web_url = "https://example.com/mr/1"
        mock_project.mergerequests.create.return_value = mock_mr

        service = GitLabConfigService(
            gitlab_url="https://gitlab.example.com",
            project="group/repo",
            private_token="glpat-test",
        )
        service.submit_config_update(
            file_contents={"f.csv": "data"},
            author="user@domain/special~chars",
            description="test",
        )
        branch_args = mock_project.branches.create.call_args[0][0]
        branch_name = branch_args["branch"]
        # Branch name should only contain safe chars after the prefix
        author_part = branch_name.split("-", maxsplit=3)[-1]
        assert all(c.isalnum() or c in "_-" for c in author_part)


# =============================================================================
# Config bridge tests
# =============================================================================


class TestSubmitConfigDir:
    def test_submits_only_changed_files(self, tmp_path):
        # Create config directory with some files
        core_dir = tmp_path / "core" / "formatting"
        core_dir.mkdir(parents=True)
        (core_dir / "instruments.csv").write_text("col1,col2\na,b\n")
        (core_dir / "samples.csv").write_text("col1,col2\nc,d\n")

        # Mock git diff to return only instruments.csv as changed
        changed_paths = [core_dir / "instruments.csv"]

        with (
            patch("qg.gitlab.config_bridge.find_repo_root") as mock_repo_root,
            patch("qg.gitlab.config_bridge._get_changed_config_files") as mock_changed,
            patch("qg.gitlab.config_bridge.load_gitlab_settings") as mock_settings,
            patch("qg.gitlab.config_bridge.GitLabConfigService") as mock_service_cls,
        ):
            mock_repo_root.return_value = tmp_path
            mock_changed.return_value = changed_paths
            mock_settings.return_value = {
                "url": "https://gitlab.example.com",
                "project": "group/repo",
                "private_token": "glpat-test",
            }
            mock_service = MagicMock()
            mock_service.submit_config_update.return_value = "https://gitlab.example.com/mr/1"
            mock_service_cls.return_value = mock_service

            result = submit_config_dir(tmp_path, author="Test", description="Update")

            # Only the changed file should be submitted
            call_args = mock_service.submit_config_update.call_args
            file_contents = call_args.kwargs["file_contents"]
            assert "qg_configs/core/formatting/instruments.csv" in file_contents
            assert "qg_configs/core/formatting/samples.csv" not in file_contents
            assert result == "https://gitlab.example.com/mr/1"

    def test_returns_empty_string_on_no_changes(self, tmp_path):
        tmp_path.mkdir(exist_ok=True)
        with (
            patch("qg.gitlab.config_bridge.find_repo_root") as mock_repo_root,
            patch("qg.gitlab.config_bridge._get_changed_config_files") as mock_changed,
        ):
            mock_repo_root.return_value = tmp_path
            mock_changed.return_value = []
            result = submit_config_dir(tmp_path, author="Test", description="Update")
            assert result == ""

    def test_raises_on_missing_dir(self, tmp_path):
        with pytest.raises(FileNotFoundError, match="Config directory not found"):
            submit_config_dir(tmp_path / "nonexistent", author="Test", description="Update")

    def test_raises_on_empty_author(self, tmp_path):
        tmp_path.mkdir(exist_ok=True)
        with pytest.raises(ValueError, match="Author name is required"):
            submit_config_dir(tmp_path, author="", description="Update")

    def test_raises_on_empty_description(self, tmp_path):
        tmp_path.mkdir(exist_ok=True)
        with pytest.raises(ValueError, match="Change description is required"):
            submit_config_dir(tmp_path, author="Test", description="  ")
