"""Load GitLab settings from .qg_settings.toml or environment variables."""

import os
import tomllib
from pathlib import Path

from loguru import logger

_SETTINGS_FILENAME = ".qg_settings.toml"
_REQUIRED_KEYS = ("url", "project", "private_token")
_ENV_VARS = {
    "private_token": "QG_GITLAB_TOKEN",
    "url": "QG_GITLAB_URL",
    "project": "QG_GITLAB_PROJECT",
}


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
        f"Place it in your project root or home directory (~/{_SETTINGS_FILENAME}), "
        f"or set {', '.join(_ENV_VARS.values())} in the environment. "
        f"Copy .qg_settings.toml.example and fill in your GitLab credentials."
    )
    raise FileNotFoundError(msg)


def _load_from_file(path: Path | None) -> dict[str, str]:
    """Read GitLab settings from a TOML file. Raises FileNotFoundError if missing."""
    if path is not None:
        if not path.exists():
            msg = f"Settings file not found: {path}"
            raise FileNotFoundError(msg)
        settings_path = path
    else:
        settings_path = _find_settings_file()

    logger.info("Loading GitLab settings from {}", settings_path)
    data = tomllib.loads(settings_path.read_text(encoding="utf-8"))
    section = data.get("gitlab")
    if not section:
        msg = f"Missing [gitlab] section in {settings_path}"
        raise ValueError(msg)
    return section


def load_gitlab_settings(path: Path | None = None) -> dict[str, str]:
    """Load GitLab settings from env vars and/or .qg_settings.toml.

    If all three of ``QG_GITLAB_TOKEN``, ``QG_GITLAB_URL``, ``QG_GITLAB_PROJECT``
    are set, the file is not consulted (production / portal path). Otherwise the
    file supplies the missing keys and any env var overrides its file value.

    Raises FileNotFoundError if the file is needed but missing, or ValueError if
    any required key is still missing or the resulting URL is not HTTPS.
    """
    env_values = {key: os.environ.get(var, "") for key, var in _ENV_VARS.items()}

    if all(env_values.values()):
        logger.info("Loading GitLab settings from environment")
        settings = dict(env_values)
    else:
        settings = _load_from_file(path)
        for key, value in env_values.items():
            if value:
                logger.info("Overriding GitLab {} from environment", key)
                settings[key] = value

    missing = [k for k in _REQUIRED_KEYS if not settings.get(k)]
    if missing:
        msg = f"Missing keys: {', '.join(missing)}"
        raise ValueError(msg)

    if not settings["url"].startswith("https://"):
        msg = f"GitLab URL must use HTTPS, got: {settings['url']}"
        raise ValueError(msg)

    return settings
