"""CLI entry point for physical sample-position assignment."""

from __future__ import annotations

from pathlib import Path
from typing import Annotated

import cyclopts
from loguru import logger

from qg.params_models import read_queue_input, write_positioned_queue_input
from qg.positioning import position_queue


def cli_main() -> None:
    """Assign or validate physical positions and emit positioned queue JSON."""
    app = cyclopts.App(
        help="Assign vial samples to positions or validate an existing plate queue.",
    )

    @app.default
    def main(
        input_json: Annotated[
            Path,
            cyclopts.Parameter(help="Fully stamped vial or plate queue JSON"),
        ],
        *,
        output: Annotated[
            Path | None,
            cyclopts.Parameter(name=["--output", "-o"], help="Output JSON file (default: stdout)"),
        ] = None,
        verbose: Annotated[
            bool,
            cyclopts.Parameter(name=["--verbose", "-v"], help="Enable verbose logging"),
        ] = False,
    ) -> None:
        if not verbose:
            logger.disable("qg")

        queue_input = read_queue_input(input_json)
        positioned_input = position_queue(queue_input)
        if output is None:
            print(positioned_input.model_dump_json(indent=2))
        else:
            write_positioned_queue_input(positioned_input, output)

    app()


if __name__ == "__main__":
    cli_main()
