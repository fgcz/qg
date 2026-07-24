"""Save queue generation artifacts for audit/debugging."""

from __future__ import annotations

import time
from datetime import UTC, datetime
from pathlib import Path
from typing import TYPE_CHECKING

import polars as pl
from loguru import logger

if TYPE_CHECKING:
    from qg.params_models import PositionedQueueInput, QueueInput

ARTIFACTS_DIR = Path.home() / ".qg" / "logs"
RETENTION_DAYS = 30

_ARTIFACT_SUFFIXES = frozenset({".json", ".csv"})


def build_timestamp() -> str:
    """Return a UTC timestamp string suitable for artifact filenames: YYYYMMDD_HHMMSS."""
    return datetime.now(tz=UTC).strftime("%Y%m%d_%H%M%S")


def build_artifact_stem(queue_input: QueueInput | PositionedQueueInput) -> str:
    """Build filename stem: {YYYYMMDD_HHMMSS}_{tech_area}_{instrument}_{sampler}."""
    params = queue_input.parameters
    ts = build_timestamp()
    sampler = params.sampler.replace(".", "_")
    return f"{ts}_{params.tech_area}_{params.instrument}_{sampler}"


def _cleanup_old_artifacts(directory: Path) -> None:
    """Remove old .json and .csv artifact files older than RETENTION_DAYS.

    Only touches artifact files (not loguru .log files which have their own retention).
    """
    cutoff = time.time() - RETENTION_DAYS * 86400
    for f in directory.iterdir():
        if f.is_file() and f.suffix in _ARTIFACT_SUFFIXES and f.stat().st_mtime < cutoff:
            f.unlink()


def save_positioning_artifacts(
    source_input: QueueInput,
    positioned_input: PositionedQueueInput,
    *,
    stem: str | None = None,
) -> str:
    """Save source and positioned queue JSON at an explicit commit point."""
    artifact_stem = stem or build_artifact_stem(source_input)
    try:
        ARTIFACTS_DIR.mkdir(parents=True, exist_ok=True)
        source_path = ARTIFACTS_DIR / f"{artifact_stem}_source_queue.json"
        source_path.write_text(source_input.model_dump_json(indent=2))
        positioned_path = ARTIFACTS_DIR / f"{artifact_stem}_positioned_queue.json"
        positioned_path.write_text(positioned_input.model_dump_json(indent=2))
        logger.info("Positioning artifacts saved | {}", artifact_stem)
        _cleanup_old_artifacts(ARTIFACTS_DIR)
    except Exception:
        logger.opt(exception=True).warning("Failed to save positioning artifacts")
    return artifact_stem


def save_generation_artifact(
    positioned_input: PositionedQueueInput,
    raw_queue: pl.DataFrame,
    *,
    stem: str | None = None,
) -> str:
    """Save raw generated queue rows at an explicit commit point."""
    artifact_stem = stem or build_artifact_stem(positioned_input)
    try:
        ARTIFACTS_DIR.mkdir(parents=True, exist_ok=True)
        csv_path = ARTIFACTS_DIR / f"{artifact_stem}_raw_queue.csv"
        raw_queue.write_csv(csv_path)
        logger.info("Generation artifact saved | {}", artifact_stem)
        _cleanup_old_artifacts(ARTIFACTS_DIR)
    except Exception:
        logger.opt(exception=True).warning("Failed to save generation artifact")
    return artifact_stem
