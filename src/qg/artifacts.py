"""Save queue generation artifacts for audit/debugging."""

from __future__ import annotations

import time
from datetime import UTC, datetime
from pathlib import Path
from typing import TYPE_CHECKING

from loguru import logger

if TYPE_CHECKING:
    from qg.generator import QueueRowTable
    from qg.params_models import QueueInput

ARTIFACTS_DIR = Path.home() / ".qg" / "logs"
RETENTION_DAYS = 30

_ARTIFACT_SUFFIXES = frozenset({".json", ".csv"})


def build_timestamp() -> str:
    """Return a UTC timestamp string suitable for artifact filenames: YYYYMMDD_HHMMSS."""
    return datetime.now(tz=UTC).strftime("%Y%m%d_%H%M%S")


def _build_stem(queue_input: QueueInput) -> str:
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


def save_artifacts(queue_input: QueueInput, queue_rows: QueueRowTable) -> None:
    """Save queue params JSON and raw queue CSV to ~/.qg/logs/.

    Never raises — logs errors internally so queue generation is not interrupted.
    """
    try:
        ARTIFACTS_DIR.mkdir(parents=True, exist_ok=True)
        stem = _build_stem(queue_input)

        # Queue parameters JSON
        params_path = ARTIFACTS_DIR / f"{stem}_params.json"
        params_path.write_text(queue_input.model_dump_json(indent=2))

        # Raw queue rows CSV (unformatted)
        csv_path = ARTIFACTS_DIR / f"{stem}_raw_queue.csv"
        queue_rows.to_table().write_csv(csv_path)

        logger.info("Artifacts saved | {}", stem)
        _cleanup_old_artifacts(ARTIFACTS_DIR)
    except Exception:
        logger.opt(exception=True).warning("Failed to save artifacts")
