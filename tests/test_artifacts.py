"""Tests for qg.artifacts module — build_timestamp and cleanup_old_artifacts."""

from __future__ import annotations

import re
import time
from pathlib import Path
from unittest.mock import patch

from qg.artifacts import _ARTIFACT_SUFFIXES, RETENTION_DAYS, _cleanup_old_artifacts, build_timestamp


class TestBuildTimestamp:
    def test_format_matches_expected(self):
        ts = build_timestamp()
        assert re.fullmatch(r"\d{8}_\d{6}", ts), f"Unexpected format: {ts}"

    def test_returns_utc(self):
        from datetime import UTC, datetime

        fake_utc = datetime(2026, 3, 15, 8, 30, 45, tzinfo=UTC)
        with patch("qg.artifacts.datetime") as mock_dt:
            mock_dt.now.return_value = fake_utc
            ts = build_timestamp()
        assert ts == "20260315_083045"
        mock_dt.now.assert_called_once_with(tz=UTC)


class TestCleanupOldArtifacts:
    def test_removes_old_json_and_csv(self, tmp_path: Path):
        old_time = time.time() - (RETENTION_DAYS + 1) * 86400
        for suffix in _ARTIFACT_SUFFIXES:
            f = tmp_path / f"old_artifact{suffix}"
            f.write_text("data")
            import os

            os.utime(f, (old_time, old_time))

        _cleanup_old_artifacts(tmp_path)

        remaining = list(tmp_path.iterdir())
        assert remaining == []

    def test_keeps_recent_files(self, tmp_path: Path):
        for suffix in _ARTIFACT_SUFFIXES:
            f = tmp_path / f"recent{suffix}"
            f.write_text("data")

        _cleanup_old_artifacts(tmp_path)

        remaining = list(tmp_path.iterdir())
        assert len(remaining) == len(_ARTIFACT_SUFFIXES)

    def test_ignores_non_artifact_suffixes(self, tmp_path: Path):
        old_time = time.time() - (RETENTION_DAYS + 1) * 86400
        log_file = tmp_path / "debug.log"
        log_file.write_text("log data")
        import os

        os.utime(log_file, (old_time, old_time))

        _cleanup_old_artifacts(tmp_path)

        assert log_file.exists()
