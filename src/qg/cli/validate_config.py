"""CLI entry point for config validation."""

import sys
from pathlib import Path

from qg.config import validate_all_configs


def cli_main() -> None:
    """CLI entry point for config validation.

    Usage:
        uv run qg-validate
    """
    config_dir = Path("qg_configs")
    success = validate_all_configs(config_dir)
    sys.exit(0 if success else 1)
