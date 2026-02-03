"""CLI entry point for config validation."""

import sys

import pydantic

from qg.config_models.loader import qg_configuration


def cli_main() -> None:
    """CLI entry point for config validation.

    Usage:
        uv run qg-validate
    """
    try:
        qg_configuration()  # Loads and validates, raises on failure
        print("\nValidation complete.")
        sys.exit(0)
    except (ValueError, pydantic.ValidationError) as e:
        print(f"\n{e}")
        sys.exit(1)
