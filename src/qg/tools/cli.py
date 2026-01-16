"""CLI entry point for qg-tools with subcommands.

Usage:
    qg-tools sld-to-csv <sld_file> [--csv output.csv] [-v]
    qg-tools csv-to-params <input_path> [-o output_dir]
    qg-tools compare <generated> <original> <config> <output>
    qg-tools summarize <output> <input_files>...
    qg-tools merge [input_dir] [-o output.csv]
"""

from __future__ import annotations

import cyclopts

from qg.tools import compare, csv_to_params, merge, sld_to_csv, summarize

app = cyclopts.App(
    name="qg-tools",
    help="Queue generator tools for testing and validation.",
)

# Register subcommands
app.command(sld_to_csv.app)
app.command(csv_to_params.app)
app.command(compare.app)
app.command(summarize.app)
app.command(merge.app)


def main() -> None:
    """Main entry point for qg-tools CLI."""
    app()


if __name__ == "__main__":
    main()
