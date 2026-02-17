"""Load GitLab settings from .qg_settings.toml."""

import tomllib
from pathlib import Path

from loguru import logger

_SETTINGS_FILENAME = ".qg_settings.toml"


def _find_settings_file() -> Path:
    """Search for settings file: project root (next to pyproject.toml) → repo root."""
    # Walk up from this file to find the project root
    current = Path(__file__).resolve().parent
    for _ in range(10):
        candidate = current / _SETTINGS_FILENAME
        if candidate.exists():
            return candidate
        # Also check for pyproject.toml as project root marker
        if (current / "pyproject.toml").exists():
            if candidate.exists():
                return candidate
        parent = current.parent
        if parent == current:
            break
        current = parent

    msg = (
        f"Cannot find {_SETTINGS_FILENAME}. "
        f"Copy .qg_settings.toml.example to {_SETTINGS_FILENAME} and fill in your GitLab credentials."
    )
    raise FileNotFoundError(msg)


def load_gitlab_settings(path: Path | None = None) -> dict:
    """Load GitLab settings from .qg_settings.toml.

    Searches: explicit path → project root → repo root.
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

    return gitlab_section
