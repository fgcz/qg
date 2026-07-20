"""Queue generation package for mass spectrometry instruments."""

from importlib.metadata import PackageNotFoundError, version


def _installed_version() -> str:
    """Resolve the installed package version used in queue provenance."""
    try:
        return version("qg")
    except PackageNotFoundError as exc:
        raise RuntimeError(
            "qg package metadata is unavailable; install the project before building queues"
        ) from exc


__version__ = _installed_version()
