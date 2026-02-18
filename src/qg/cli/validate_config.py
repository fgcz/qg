"""CLI entry point for config validation."""

import sys
from pathlib import Path
from typing import Annotated

import cyclopts
import pydantic

from qg.config_models.loader import qg_configuration


def cli_main() -> None:
    """CLI entry point for config validation."""
    app = cyclopts.App(help="Validate queue generation config files.")

    @app.default
    def main(
        config_dir: Annotated[
            Path,
            cyclopts.Parameter(name=["--config-dir", "-c"], help="Config directory"),
        ] = Path("qg_configs"),
    ) -> None:
        """Validate config files and cross-references."""
        try:
            qg_configuration(config_dir)
            print("\nValidation complete.")
            sys.exit(0)
        except (ValueError, pydantic.ValidationError) as e:
            print(f"\n{e}")
            sys.exit(1)

    app()
