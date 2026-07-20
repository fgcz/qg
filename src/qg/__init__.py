"""Queue generation package for mass spectrometry instruments."""

from importlib.metadata import PackageNotFoundError, version


def _installed_version() -> str:
    """Resolve the installed package version used in queue provenance.

    Returns an empty string when package metadata is unavailable (e.g. an
    uninstalled source tree). Importing ``qg`` must never fail on this; the
    requirement is enforced at stamp time by ``current_qg_version()``.
    """
    try:
        return version("qg")
    except PackageNotFoundError:
        return ""


__version__ = _installed_version()
