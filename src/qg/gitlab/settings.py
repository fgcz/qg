"""Load GitLab settings from .qg_settings.toml."""

import os
import tomllib
from pathlib import Path

from loguru import logger

_SETTINGS_FILENAME = ".qg_settings.toml"


def _find_settings_file() -> Path:
    """Search for settings file by walking up from this file's directory, then user home."""
    # 1. Walk up from this file's directory (works for dev / editable installs)
    current = Path(__file__).resolve().parent
    for _ in range(10):
        candidate = current / _SETTINGS_FILENAME
        if candidate.exists():
            return candidate
        parent = current.parent
        if parent == current:
            break
        current = parent

    # 2. User home directory (works for uv-installed packages)
    home_candidate = Path.home() / _SETTINGS_FILENAME
    if home_candidate.exists():
        return home_candidate

    msg = (
        f"Cannot find {_SETTINGS_FILENAME}. "
        f"Place it in your project root or home directory (~/{_SETTINGS_FILENAME}). "
        f"Copy .qg_settings.toml.example and fill in your GitLab credentials."
    )
    raise FileNotFoundError(msg)


def load_gitlab_settings(path: Path | None = None) -> dict[str, str]:
    """Load GitLab settings from .qg_settings.toml.

    Searches: explicit path -> project root -> repo root.
    Token can be overridden via QG_GITLAB_TOKEN environment variable.

    Raises FileNotFoundError with clear message if missing.
    """
    if path is not None:
        if not path.exists():
            msg = f"Settings file not found: {path}"
            raise FileNotFoundError(msg)
        settings_path = path
    else:
        settings_path = _find_settings_file()

    logger.info("Loading GitLab settings from {}", settings_path)
    text = settings_path.read_text(encoding="utf-8")
    data = tomllib.loads(text)

    gitlab_section = data.get("gitlab")
    if not gitlab_section:
        msg = f"Missing [gitlab] section in {settings_path}"
        raise ValueError(msg)

    required_keys = ("url", "project", "private_token")
    missing = [k for k in required_keys if k not in gitlab_section]
    if missing:
        msg = f"Missing keys in [gitlab] section: {', '.join(missing)}"
        raise ValueError(msg)

    if not gitlab_section["url"].startswith("https://"):
        msg = f"GitLab URL must use HTTPS, got: {gitlab_section['url']}"
        raise ValueError(msg)

    # Environment variable overrides file token
    env_token = os.environ.get("QG_GITLAB_TOKEN")
    if env_token:
        logger.info("Using GitLab token from QG_GITLAB_TOKEN environment variable")
        gitlab_section["private_token"] = env_token

    return gitlab_section
