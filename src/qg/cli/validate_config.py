"""CLI entry point for config validation."""

import sys

from qg.config import ConfigValidationError, qg_config


def cli_main() -> None:
    """CLI entry point for config validation.

    Usage:
        uv run qg-validate
    """
    try:
        qg_config()  # Loads and validates, prints output, raises on failure
        print("\nValidation complete.")
        sys.exit(0)
    except (ValueError, ConfigValidationError) as e:
        print(f"\n{e}")
        sys.exit(1)
