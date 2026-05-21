"""Shared logging configuration for qg apps and CLI."""

import sys
from pathlib import Path

from loguru import logger


def configure_logging() -> None:
    """Configure loguru sinks for GUI apps.

    - Daily-rotated file at ~/.qg/logs/qg_YYYY-MM-DD.log with full
      diagnostic info (variable dumps + extended traceback) for debugging.
    - Stderr stays quiet: INFO+ messages only, with terse tracebacks (no
      variable dumps) so the marimo CLI does not get flooded when a cell
      raises a user-recoverable error. The full detail is still in the log
      file.
    """
    log_dir = Path.home() / ".qg" / "logs"
    log_dir.mkdir(parents=True, exist_ok=True)

    logger.remove()  # drop loguru's default stderr handler (diagnose=True)
    logger.add(
        sys.stderr,
        level="INFO",
        backtrace=False,
        diagnose=False,
    )
    logger.add(
        log_dir / "qg_{time:YYYY-MM-DD}.log",
        format="{time:YYYY-MM-DD HH:mm:ss} | {level:<8} | {message}",
        rotation="1 day",
        retention="30 days",
        level="INFO",
        backtrace=True,
        diagnose=True,
    )
