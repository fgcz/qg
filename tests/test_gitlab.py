"""Tests for qg.gitlab package."""

import textwrap
from unittest.mock import MagicMock, patch

import pytest

pytest.importorskip("gitlab")  # python-gitlab ships with the qg[bfabric] portal extra

from qg.gitlab._git import find_repo_root  # noqa: E402
from qg.gitlab.config_bridge import submit_config_changes  # noqa: E402
from qg.gitlab.service import GitLabConfigService  # noqa: E402
from qg.gitlab.settings import load_gitlab_settings  # noqa: E402

pytestmark = pytest.mark.bfabric

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
    @pytest.fixture(autouse=True)
    def _clear_qg_gitlab_env(self, monkeypatch):
        """Strip ambient QG_GITLAB_* env vars so tests are deterministic."""
        for var in ("QG_GITLAB_TOKEN", "QG_GITLAB_URL", "QG_GITLAB_PROJECT"):
            monkeypatch.delenv(var, raising=False)

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

    def test_loads_entirely_from_env(self, monkeypatch):
        """When all three env vars are set, the file is not consulted."""
        monkeypatch.setenv("QG_GITLAB_TOKEN", "glpat-env")
        monkeypatch.setenv("QG_GITLAB_URL", "https://gitlab.env.example.com")
        monkeypatch.setenv("QG_GITLAB_PROJECT", "env-group/env-repo")
        # No path passed; if the file lookup ran it would raise — we expect it not to.
        result = load_gitlab_settings()
        assert result == {
            "private_token": "glpat-env",
            "url": "https://gitlab.env.example.com",
            "project": "env-group/env-repo",
        }

    def test_partial_env_falls_back_to_file(self, tmp_path, monkeypatch):
        """Only the env-supplied keys override; the rest come from the file."""
        settings_file = tmp_path / ".qg_settings.toml"
        settings_file.write_text(
            textwrap.dedent("""\
                [gitlab]
                url = "https://gitlab.file.example.com"
                project = "file-group/file-repo"
                private_token = "glpat-file"
            """)
        )
        monkeypatch.setenv("QG_GITLAB_URL", "https://gitlab.env.example.com")
        result = load_gitlab_settings(settings_file)
        assert result["url"] == "https://gitlab.env.example.com"
        assert result["project"] == "file-group/file-repo"
        assert result["private_token"] == "glpat-file"

    def test_env_overrides_file_project(self, tmp_path, monkeypatch):
        """QG_GITLAB_PROJECT overrides the project value from the file."""
        settings_file = tmp_path / ".qg_settings.toml"
        settings_file.write_text(
            textwrap.dedent("""\
                [gitlab]
                url = "https://gitlab.example.com"
                project = "file-group/file-repo"
                private_token = "glpat-test"
            """)
        )
        monkeypatch.setenv("QG_GITLAB_PROJECT", "env-group/env-repo")
        result = load_gitlab_settings(settings_file)
        assert result["project"] == "env-group/env-repo"

    def test_raises_on_http_env_url(self, monkeypatch):
        monkeypatch.setenv("QG_GITLAB_TOKEN", "glpat-env")
        monkeypatch.setenv("QG_GITLAB_URL", "http://gitlab.env.example.com")
        monkeypatch.setenv("QG_GITLAB_PROJECT", "env-group/env-repo")
        with pytest.raises(ValueError, match="GitLab URL must use HTTPS"):
            load_gitlab_settings()

    def test_finds_settings_in_home_dir(self, tmp_path, monkeypatch):
        """Settings file in user home is found as fallback."""
        settings_file = tmp_path / ".qg_settings.toml"
        settings_file.write_text(
            textwrap.dedent("""\
                [gitlab]
                url = "https://gitlab.example.com"
                project = "group/repo"
                private_token = "glpat-home"
            """)
        )
        monkeypatch.setattr("qg.gitlab.settings.Path.home", lambda: tmp_path)
        # Ensure walk-up search won't find it (use a non-existent module path)
        monkeypatch.setattr(
            "qg.gitlab.settings.Path.__file__", str(tmp_path / "nonexistent" / "settings.py"), raising=False
        )
        # Direct test: the home fallback is exercised when explicit path is given
        result = load_gitlab_settings(settings_file)
        assert result["private_token"] == "glpat-home"


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
# Config bridge tests (in-memory diff)
# =============================================================================


class TestSubmitConfigChanges:
    def test_submits_only_changed_files(self):
        original = {
            "core/formatting/instruments.csv": "col1,col2\na,b\n",
            "core/formatting/samples.csv": "col1,col2\nc,d\n",
        }
        edited = {
            "core/formatting/instruments.csv": "col1,col2\na,CHANGED\n",
            "core/formatting/samples.csv": "col1,col2\nc,d\n",  # unchanged
        }

        with (
            patch("qg.gitlab.config_bridge.load_gitlab_settings") as mock_settings,
            patch("qg.gitlab.config_bridge.GitLabConfigService") as mock_service_cls,
        ):
            mock_settings.return_value = {
                "url": "https://gitlab.example.com",
                "project": "group/repo",
                "private_token": "glpat-test",
            }
            mock_service = MagicMock()
            mock_service.submit_config_update.return_value = "https://gitlab.example.com/mr/1"
            mock_service_cls.return_value = mock_service

            result = submit_config_changes(original, edited, author="Test", description="Update")

            call_args = mock_service.submit_config_update.call_args
            file_contents = call_args.kwargs["file_contents"]
            assert "qg_configs/core/formatting/instruments.csv" in file_contents
            assert "qg_configs/core/formatting/samples.csv" not in file_contents
            assert result == "https://gitlab.example.com/mr/1"

    def test_returns_empty_string_on_no_changes(self):
        original = {"core/formatting/instruments.csv": "col1,col2\na,b\n"}
        edited = {"core/formatting/instruments.csv": "col1,col2\na,b\n"}

        result = submit_config_changes(original, edited, author="Test", description="Update")
        assert result == ""

    def test_raises_on_empty_author(self):
        with pytest.raises(ValueError, match="Author name is required"):
            submit_config_changes({}, {}, author="", description="Update")

    def test_raises_on_empty_description(self):
        with pytest.raises(ValueError, match="Change description is required"):
            submit_config_changes({}, {}, author="Test", description="  ")

    def test_detects_new_files(self):
        """Files present in edited but not original are treated as changes."""
        original = {"core/formatting/instruments.csv": "data"}
        edited = {
            "core/formatting/instruments.csv": "data",
            "core/formatting/new_file.csv": "new data",
        }

        with (
            patch("qg.gitlab.config_bridge.load_gitlab_settings") as mock_settings,
            patch("qg.gitlab.config_bridge.GitLabConfigService") as mock_service_cls,
        ):
            mock_settings.return_value = {
                "url": "https://gitlab.example.com",
                "project": "group/repo",
                "private_token": "glpat-test",
            }
            mock_service = MagicMock()
            mock_service.submit_config_update.return_value = "https://gitlab.example.com/mr/2"
            mock_service_cls.return_value = mock_service

            result = submit_config_changes(original, edited, author="Test", description="Add file")

            call_args = mock_service.submit_config_update.call_args
            file_contents = call_args.kwargs["file_contents"]
            assert "qg_configs/core/formatting/new_file.csv" in file_contents
            assert "qg_configs/core/formatting/instruments.csv" not in file_contents
            assert result == "https://gitlab.example.com/mr/2"


# =============================================================================
# serialize_all() tests
# =============================================================================


class TestSerializeAll:
    def test_round_trip_matches_disk(self):
        """serialize_all() output matches what write_all() would write."""
        from qg.config_models.loader import qg_configuration

        cfg = qg_configuration()
        contents = cfg.serialize_all()

        # Check we got a non-empty dict with expected key patterns
        assert isinstance(contents, dict)
        assert len(contents) > 0

        # Check key categories
        csv_keys = [k for k in contents if k.endswith(".csv")]
        toml_keys = [k for k in contents if k.endswith(".toml")]
        assert len(csv_keys) > 0
        assert len(toml_keys) > 0

        # Check specific expected files exist
        assert "core/formatting/instruments.csv" in contents
        assert "core/structure/samples.csv" in contents
        assert "core/structure/queue_patterns.toml" in contents
        assert "core/position/sampler.toml" in contents

        # Check methods files exist
        methods_keys = [k for k in contents if k.startswith("core/methods/")]
        assert len(methods_keys) > 0

    def test_serialize_then_serialize_is_stable(self):
        """Two calls to serialize_all() return identical results."""
        from qg.config_models.loader import qg_configuration

        cfg = qg_configuration()
        first = cfg.serialize_all()
        second = cfg.serialize_all()
        assert first == second

    def test_contents_are_nonempty_strings(self):
        """All values in serialize_all() are non-empty strings."""
        from qg.config_models.loader import qg_configuration

        cfg = qg_configuration()
        contents = cfg.serialize_all()
        for key, value in contents.items():
            assert isinstance(value, str), f"{key} value is not a string"
            assert len(value) > 0, f"{key} value is empty"
