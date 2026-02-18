"""Shared logging configuration for qg apps and CLI."""

from pathlib import Path

from loguru import logger


def configure_logging() -> None:
    """Configure loguru with file sink for GUI apps.

    Adds a daily-rotated log file at ~/.qg/logs/qg_YYYY-MM-DD.log.
    The default stderr sink is left unchanged.
    """
    log_dir = Path.home() / ".qg" / "logs"
    log_dir.mkdir(parents=True, exist_ok=True)
    logger.add(
        log_dir / "qg_{time:YYYY-MM-DD}.log",
        format="{time:YYYY-MM-DD HH:mm:ss} | {level:<8} | {message}",
        rotation="1 day",
        retention="30 days",
        level="INFO",
    )
