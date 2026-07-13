"""Shared logging configuration for qg apps and CLI."""

import logging
import sys
from pathlib import Path

from loguru import logger

#: stdlib loggers uvicorn/marimo give their own handlers with propagate=False,
#: so a root-level intercept alone misses them. Access logs (uvicorn.access)
#: are deliberately NOT bridged — they are high-volume under marimo and stay on
#: stdout (docker logs), out of the 30-day file sink.
_BRIDGED_LOGGERS = ("uvicorn", "uvicorn.error", "uvicorn.asgi", "marimo")


class InterceptHandler(logging.Handler):
    """Forward stdlib ``logging`` records into loguru (standard recipe), so
    uvicorn / marimo / starlette / fastapi lines reach loguru's sinks —
    including the ~/.qg/logs file — instead of only stdout/stderr."""

    def emit(self, record: logging.LogRecord) -> None:
        try:
            level: str | int = logger.level(record.levelname).name
        except ValueError:
            level = record.levelno
        # Walk back to the frame that issued the log call so loguru reports the
        # real caller, not this handler.
        frame, depth = logging.currentframe(), 2
        while frame and frame.f_code.co_filename == logging.__file__:
            frame = frame.f_back
            depth += 1
        logger.opt(depth=depth, exception=record.exc_info).log(level, record.getMessage())


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

    # Route stdlib logging (uvicorn.error, marimo, starlette, fastapi) into
    # loguru so those records land in the file sink too, not just stderr.
    # uvicorn.access is intentionally excluded (see _BRIDGED_LOGGERS).
    intercept = InterceptHandler()
    logging.basicConfig(handlers=[intercept], level=logging.INFO, force=True)
    for name in _BRIDGED_LOGGERS:
        lg = logging.getLogger(name)
        lg.handlers = [intercept]
        lg.propagate = False
