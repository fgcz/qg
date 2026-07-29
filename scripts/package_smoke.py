"""Build qg and verify the wheel's public packaging contract offline."""

from __future__ import annotations

import configparser
import subprocess
import tempfile
import zipfile
from pathlib import Path

_EXPECTED_ENTRY_POINTS = {
    "qg": "qg.cli.generate_queues:cli_main",
    "qg-assign-positions": "qg.cli.assign_positions:cli_main",
    "qg-validate": "qg.cli.validate_config:cli_main",
    "qg-app-local": "qg.apps.launcher_local:launch_local_app",
}


def _require_members(names: set[str], pattern: str, description: str) -> None:
    if not any(Path(name).match(pattern) for name in names):
        raise RuntimeError(f"Wheel is missing {description} ({pattern}).")


def _verify_wheel(wheel: Path) -> None:
    with zipfile.ZipFile(wheel) as archive:
        names = set(archive.namelist())
        package_members = {name for name in names if ".dist-info/" not in name}
        unexpected = sorted(name for name in package_members if not name.startswith("qg/"))
        if unexpected:
            raise RuntimeError(f"Wheel contains files outside qg/: {unexpected}")

        _require_members(names, "qg/examples/sample_tables/*.csv", "bundled sample tables")
        _require_members(names, "qg/examples/params/*.json", "bundled parameter examples")

        entry_point_files = [name for name in names if name.endswith(".dist-info/entry_points.txt")]
        if len(entry_point_files) != 1:
            raise RuntimeError(f"Expected one entry_points.txt, found {entry_point_files}")
        parser = configparser.ConfigParser()
        parser.read_string(archive.read(entry_point_files[0]).decode())
        actual = dict(parser["console_scripts"])
        missing = {name: target for name, target in _EXPECTED_ENTRY_POINTS.items() if actual.get(name) != target}
        if missing:
            raise RuntimeError(f"Wheel entry points are missing or incorrect: {missing}")


def main() -> None:
    """Build an sdist and wheel, then inspect the wheel without installing it."""
    with tempfile.TemporaryDirectory(prefix="qg-package-smoke-") as temp_dir:
        output_dir = Path(temp_dir)
        subprocess.run(["uv", "build", "--out-dir", str(output_dir)], check=True)
        wheels = list(output_dir.glob("qg-*.whl"))
        if len(wheels) != 1:
            raise RuntimeError(f"Expected one qg wheel, found {wheels}")
        _verify_wheel(wheels[0])
        print(f"Package smoke passed: {wheels[0].name}")


if __name__ == "__main__":
    main()
