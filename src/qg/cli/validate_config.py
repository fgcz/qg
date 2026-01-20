"""CLI entry point for config validation."""

import sys

from qg.config import qg_config


def cli_main() -> None:
    """CLI entry point for config validation.

    Usage:
        uv run qg-validate
    """
    try:
        qg_config()  # Loads and validates, prints output, raises on failure
        print("\nValidation complete.")
        sys.exit(0)
    except ValueError as e:
        print(f"\n{e}")
        sys.exit(1)
    except Exception as e:
        print(f"Failed to load configs: {e}")
        sys.exit(1)
