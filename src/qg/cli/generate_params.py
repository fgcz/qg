#!/usr/bin/env python
"""CLI for generating queue parameter JSON files from B-Fabric.

Iterates over orders and generates queue parameter JSON files for all valid
parameter combinations, fetching samples from B-Fabric.

Usage:
    uv run qg-params --user cpanse
    uv run qg-params --technology proteomics
    uv run qg-params --dry-run
    uv run qg-params --limit 5
    uv run qg-params --output-dir test_data/examples
"""

from __future__ import annotations

import json
from datetime import date
from pathlib import Path
from typing import Annotated

import cyclopts
from bfabric import Bfabric
from rich.console import Console

from qg.config import ConfigBundle, load_all_configs, load_all_configs
from qg.params_models import InputSample, QueueInput, QueueParameters
from qg.params_simulator import write_params

console = Console()


def load_orders(
    projects_file: Path,
    valid_technologies: set[str],
    technology_filter: str | None = None,
) -> list[dict]:
    """Load orders from projects JSON file.

    Args:
        projects_file: Path to projects JSON file.
        valid_technologies: Set of valid technology names from configs.
        technology_filter: Optional filter to only include this technology.
    """
    with open(projects_file) as f:
        projects_data = json.load(f)

    orders = []
    for project in projects_data:
        tech = project.get("technology", [""])[0] if project.get("technology") else ""

        if tech not in valid_technologies:
            continue

        if technology_filter and tech != technology_filter:
            continue

        for order in project.get("order", []):
            plate_count = order.get("plate_count", 0)
            sample_count = order.get("sample_count", 0)

            if sample_count == 0 and plate_count == 0:
                continue

            orders.append({
                "container_id": order["id"],
                "project_id": project["id"],
                "project_name": project.get("name", ""),
                "technology": tech,
                "sample_count": sample_count,
                "plate_count": plate_count,
                "container_type": "plates" if plate_count > 0 else "vials",
            })

    return orders


def fetch_samples(client: Bfabric, container_id: int) -> list[InputSample]:
    """Fetch samples from B-Fabric for a container."""
    result = client.read("sample", {"containerid": container_id}, max_results=None)
    samples_df = result.to_polars()

    if samples_df.is_empty():
        return []

    samples = []
    for row in samples_df.iter_rows(named=True):
        sample = InputSample(
            sample_name=row.get("name", ""),
            sample_id=row.get("id"),
            tube_id=row.get("tubeid"),
            position=row.get("_position"),
            grid_position=row.get("_gridposition"),
        )
        samples.append(sample)

    return sorted(samples, key=lambda x: x.sample_id)


def get_valid_combinations(
    technology: str,
    container_type: str,
    core_configs,
    ui_configs: ConfigBundle,
    sampler_filter: str | None = None,
) -> list[dict]:
    """Get all valid (instrument, sampler, software, pattern) combinations."""
    tech_instruments = core_configs.instruments.get_by_technology(technology)
    instrument_names = list({i.instrument for i in tech_instruments})

    container_suffix = ".vial" if container_type == "vials" else ".plate"

    combinations = []
    for instrument in instrument_names:
        valid_samplers = ui_configs.combinations.get_samplers_for_instrument(instrument)

        matching_samplers = [s for s in valid_samplers if s.endswith(container_suffix)]
        if not matching_samplers:
            matching_samplers = valid_samplers

        if sampler_filter:
            matching_samplers = [s for s in matching_samplers if s.startswith(sampler_filter)]

        for sampler in matching_samplers:
            combo = ui_configs.combinations.get_combination(instrument, sampler)
            if not combo:
                continue
            output_format = combo.output_format

            patterns = ui_configs.instrument_patterns.get_patterns_for_instrument(technology, instrument)

            for pattern in patterns:
                combinations.append({
                    "instrument": instrument,
                    "sampler": sampler,
                    "output_format": output_format,
                    "queue_pattern": pattern.queue_pattern,
                })

    return combinations


def generate_queue_params(
    order: dict,
    combination: dict,
    samples: list[InputSample],
    run_date: str,
    user: str = "",
) -> QueueInput:
    """Generate a QueueInput from order and combination data."""
    params = QueueParameters(
        container_id=order["container_id"],
        technology=order["technology"],
        instrument=combination["instrument"],
        sampler=combination["sampler"],
        output_format=combination["output_format"],
        queue_pattern=combination["queue_pattern"],
        polarity=[],  # Model validator sets default for metabolomics/lipidomics
        date=run_date,
        user=user,
        method="",
        randomization="no",
        inj_vol_override=None,
    )

    return QueueInput(parameters=params, samples=samples)


def params_filename(order: dict, combination: dict) -> str:
    """Generate a unique filename for the queue params."""
    tech = order["technology"]
    sampler = combination["sampler"].replace(".", "_")
    pattern = combination["queue_pattern"]
    container_id = order["container_id"]
    sample_count = order["sample_count"]
    instrument = combination["instrument"]
    return f"{tech}_{instrument}_{sampler}_{pattern}_c{container_id}_n{sample_count}.json"


def cli_main() -> None:
    """CLI entry point for queue params generation."""
    app = cyclopts.App(
        help="Generate queue parameter JSON files from B-Fabric orders.",
    )

    @app.default
    def main(
        *,
        projects_file: Annotated[
            Path,
            cyclopts.Parameter(
                name=["--projects", "-p"],
                help="Projects JSON file (default: proteomics_projects.json)",
            ),
        ] = Path("bfabric_cache/proteomics_projects.json"),
        output_dir: Annotated[
            Path,
            cyclopts.Parameter(
                name=["--output-dir", "-o"],
                help="Output directory for param JSONs",
            ),
        ] = Path("test_data/queue_params"),
        config_dir: Annotated[
            Path,
            cyclopts.Parameter(
                name=["--config-dir", "-c"],
                help="Config directory",
            ),
        ] = Path("qg_configs"),
        technology: Annotated[
            str | None,
            cyclopts.Parameter(help="Generate params only for this technology"),
        ] = None,
        sampler: Annotated[
            str | None,
            cyclopts.Parameter(help="Generate params only for this sampler (e.g., Vanquish, MClass48, Evosep)"),
        ] = None,
        user: Annotated[
            str,
            cyclopts.Parameter(help="Username for output path (e.g., cpanse)"),
        ] = "",
        limit: Annotated[
            int | None,
            cyclopts.Parameter(help="Limit to N orders (for testing)"),
        ] = None,
        dry_run: Annotated[
            bool,
            cyclopts.Parameter(help="Show what would be generated without creating files"),
        ] = False,
        bfabric_env: Annotated[
            str,
            cyclopts.Parameter(help="B-Fabric config environment (e.g., TEST, PRODUCTION)"),
        ] = "TEST",
    ) -> None:
        """Generate queue parameter JSON files for all valid parameter combinations."""
        console.print("Loading configuration files...")
        core_configs = load_all_configs(config_dir)
        ui_configs = load_all_configs(config_dir)

        # Validate sampler parameter against configs
        if sampler is not None:
            valid_samplers = core_configs.samplers.get_sampler_names()
            if sampler not in valid_samplers:
                console.print(f"[red]Invalid sampler: {sampler}. Valid: {valid_samplers}[/red]")
                return

        # Get valid technologies from configs
        valid_technologies = {i.technology for i in core_configs.instruments.instruments}

        console.print(f"Loading orders from {projects_file}...")
        orders = load_orders(projects_file, valid_technologies, technology_filter=technology)
        console.print(f"Found [cyan]{len(orders)}[/cyan] orders with samples")

        if limit:
            orders = orders[:limit]
            console.print(f"Limited to [cyan]{len(orders)}[/cyan] orders")

        if not dry_run:
            output_dir.mkdir(parents=True, exist_ok=True)

        client = None
        if not dry_run:
            console.print(f"Connecting to B-Fabric ({bfabric_env})...")
            client = Bfabric.connect(config_file_env=bfabric_env)

        run_date = date.today().strftime("%Y%m%d")
        total_params = 0
        total_errors = 0

        for i, order in enumerate(orders, 1):
            container_id = order["container_id"]
            order_technology = order["technology"]
            container_type = order["container_type"]

            console.print(
                f"\n[bold][{i}/{len(orders)}][/bold] Container {container_id} "
                f"([green]{order_technology}[/green], {container_type})"
            )

            combos = get_valid_combinations(
                order_technology,
                container_type,
                core_configs,
                ui_configs,
                sampler_filter=sampler,
            )

            if not combos:
                console.print(f"  [yellow]No valid combinations for {order_technology}/{container_type}[/yellow]")
                continue

            console.print(f"  [cyan]{len(combos)}[/cyan] valid combinations")

            if dry_run:
                for combo in combos:
                    filename = params_filename(order, combo)
                    console.print(f"    Would create: [dim]{filename}[/dim]")
                    total_params += 1
                continue

            samples = fetch_samples(client, container_id)
            if not samples:
                console.print("  [yellow]No samples found, skipping[/yellow]")
                total_errors += 1
                continue

            console.print(f"  Fetched [cyan]{len(samples)}[/cyan] samples")

            for combo in combos:
                queue_input = generate_queue_params(order, combo, samples, run_date, user)
                filename = params_filename(order, combo)
                filepath = output_dir / filename

                write_params(queue_input, filepath)
                console.print(f"    Created: [green]{filename}[/green]")
                total_params += 1

        console.print(f"\n{'=' * 60}")
        console.print("[bold]Summary:[/bold]")
        console.print(f"  Orders processed: [cyan]{len(orders)}[/cyan]")
        action = "would be generated" if dry_run else "generated"
        console.print(f"  Queue params {action}: [cyan]{total_params}[/cyan]")
        if total_errors:
            console.print(f"  Errors: [red]{total_errors}[/red]")
        if not dry_run:
            console.print(f"  Output directory: [dim]{output_dir}[/dim]")

    app()


if __name__ == "__main__":
    cli_main()
