"""Verify the B-Fabric-free installation profile and its test suite."""

from __future__ import annotations

import importlib
import importlib.util
import subprocess
import sys

_CORE_MODULES = (
    "qg",
    "qg.apps.queue_app_local",
    "qg.cli.generate_queues",
    "qg.cli.validate_config",
)


def main() -> int:
    """Check core imports without B-Fabric, then run the core test suite."""
    if importlib.util.find_spec("bfabric") is not None:
        raise RuntimeError("bfabric must be absent from the core installation profile.")

    for module_name in _CORE_MODULES:
        importlib.import_module(module_name)
    print("Core imports passed; bfabric is absent.")

    subprocess.run(
        [
            sys.executable,
            "-m",
            "pytest",
            "-m",
            "not bfabric",
            "--ignore=tests/gui",
            "--ignore=tests/gui_local",
        ],
        check=True,
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
