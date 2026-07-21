"""Tests for qg.artifacts module — build_timestamp and cleanup_old_artifacts."""

from __future__ import annotations

import json
import re
import time
from pathlib import Path
from unittest.mock import patch

import pytest

import qg.artifacts as artifacts
from qg.artifacts import (
    _ARTIFACT_SUFFIXES,
    RETENTION_DAYS,
    _cleanup_old_artifacts,
    build_timestamp,
    save_generation_artifact,
    save_positioning_artifacts,
)
from qg.config_models.loader import qg_configuration
from qg.generator import QueueGenerator

from .helpers import make_queue_input

CONFIG_DIR = Path(__file__).parent.parent / "qg_configs"


@pytest.fixture
def config():
    qg_configuration.cache_clear()
    return qg_configuration(CONFIG_DIR)


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


def test_explicit_commit_saves_source_positioned_and_raw(config, monkeypatch, tmp_path: Path):
    monkeypatch.setattr(artifacts, "ARTIFACTS_DIR", tmp_path)
    source = make_queue_input(config=config, num_samples=3)
    positioned = source.position_queue()
    raw_queue = QueueGenerator(config, positioned).build_rows().to_table()

    stem = save_positioning_artifacts(source, positioned, stem="run")
    save_generation_artifact(positioned, raw_queue, stem=stem)

    assert {path.name for path in tmp_path.iterdir()} == {
        "run_source_queue.json",
        "run_positioned_queue.json",
        "run_raw_queue.csv",
    }
    positioned_json = json.loads((tmp_path / "run_positioned_queue.json").read_text())
    assert len(positioned_json["queue"]["cells"]) == 3
    assert positioned_json["parameters"]["randomization"] == "no"


def test_pipeline_methods_do_not_write_artifacts(config, monkeypatch, tmp_path: Path):
    artifact_dir = tmp_path / "artifacts"
    monkeypatch.setattr(artifacts, "ARTIFACTS_DIR", artifact_dir)
    source = make_queue_input(config=config, num_samples=3)

    positioned = source.position_queue()
    QueueGenerator(config, positioned).write()

    assert not artifact_dir.exists()
