#!/usr/bin/env python
"""Batch queue params generator for queue generation system.

Iterates over all orders in proteomics_projects.json and generates queue
parameter JSON files for all valid parameter combinations, fetching samples
from B-Fabric.

Usage (from project root):
    uv run python test_data/tools/generate_queue_params.py --user cpanse
    uv run python test_data/tools/generate_queue_params.py --technology proteomics
    uv run python test_data/tools/generate_queue_params.py --dry-run
    uv run python test_data/tools/generate_queue_params.py --limit 5
"""

from __future__ import annotations

import json
from datetime import date
from pathlib import Path
from typing import Annotated, Literal

import cyclopts
from bfabric import Bfabric
from rich.console import Console

from qg.config import load_all_configs, ConfigBundle

app = cyclopts.App(
    help="Generate queue parameter JSON files for all valid parameter combinations.",
)
console = Console()

# Paths relative to project root (script is run from there)
CONFIG_DIR = Path("qg_configs")
OUTPUT_DIR = Path("test_data/queue_params")

AREA_TO_TECH = {
    "Proteomics": "proteomics",
    "Metabolomics": "metabolomics",
    "Lipidomics": "lipidomics",
}

Technology = Literal["proteomics", "metabolomics", "lipidomics"]
Sampler = Literal["Vanquish", "MClass48", "Evosep"]


def load_orders(technology_filter: str | None = None) -> list[dict]:
    """Load orders from proteomics_projects.json."""
    with open("proteomics_projects.json") as f:
        projects_data = json.load(f)

    orders = []
    for project in projects_data:
        area = project.get("technology", [""])[0] if project.get("technology") else ""
        tech = AREA_TO_TECH.get(area)

        if technology_filter and tech != technology_filter:
            continue

        if tech is None:
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


def fetch_samples(client: Bfabric, container_id: int) -> list[dict]:
    """Fetch samples from B-Fabric for a container."""
    try:
        result = client.read("sample", {"containerid": container_id}, max_results=None)
        samples_df = result.to_polars()

        if samples_df.is_empty():
            return []

        samples = []
        for row in samples_df.iter_rows(named=True):
            sample = {
                "Sample Name": row.get("name", ""),
                "Sample ID": row.get("id"),
            }
            if "tubeid" in row and row["tubeid"]:
                sample["Tube ID"] = row["tubeid"]
            if "_position" in row and row["_position"]:
                sample["Position"] = row["_position"]
            if "_gridposition" in row and row["_gridposition"]:
                sample["GridPosition"] = row["_gridposition"]
            samples.append(sample)

        return sorted(samples, key=lambda x: x["Sample ID"])

    except Exception as e:
        console.print(f"  [red]Error fetching samples for container {container_id}: {e}[/red]")
        return []


def get_valid_combinations(
    technology: str,
    container_type: str,
    configs: ConfigBundle,
    sampler_filter: str | None = None,
) -> list[dict]:
    """Get all valid (instrument, sampler, software, pattern) combinations."""
    # Get instruments for this technology
    tech_instruments = configs.instruments.get_by_technology(technology)
    instrument_names = list({i.instrument for i in tech_instruments})

    container_suffix = ".vial" if container_type == "vials" else ".plate"

    combinations = []
    for instrument in instrument_names:
        # Get valid samplers for this instrument
        valid_samplers = configs.combinations.get_samplers_for_instrument(instrument)

        # Filter by container type
        matching_samplers = [s for s in valid_samplers if s.endswith(container_suffix)]
        if not matching_samplers:
            matching_samplers = valid_samplers

        # Filter by sampler if specified
        if sampler_filter:
            matching_samplers = [s for s in matching_samplers if s.startswith(sampler_filter)]

        for sampler in matching_samplers:
            combo = configs.combinations.get_combination(instrument, sampler)
            if not combo:
                continue
            software = combo.output_format

            # Get patterns for this instrument
            patterns = configs.instrument_patterns.get_patterns_for_instrument(technology, instrument)

            for pattern in patterns:
                combinations.append({
                    "instrument": instrument,
                    "sampler": sampler,
                    "software": software,
                    "pattern": pattern.pattern,
                })

    return combinations


def generate_queue_params(
    order: dict,
    combination: dict,
    samples: list[dict],
    run_date: str,
    user: str = "",
) -> dict:
    """Generate a queue parameters dictionary."""
    technology = order["technology"]
    polarity = ["pos", "neg"] if technology in ("metabolomics", "lipidomics") else []

    return {
        "parameters": {
            "container_id": order["container_id"],
            "technology": technology,
            "instrument": combination["instrument"],
            "sampler": combination["sampler"],
            "software": combination["software"],
            "pattern": combination["pattern"],
            "polarity": polarity,
            "date": run_date,
            "user": user,
            "method": "",
            "randomization": False,
            "inj_vol_override": None,
        },
        "samples": samples,
    }


def params_filename(order: dict, combination: dict) -> str:
    """Generate a unique filename for the queue params."""
    tech = order["technology"]
    sampler = combination["sampler"].replace(".", "_")
    pattern = combination["pattern"]
    container_id = order["container_id"]
    sample_count = order["sample_count"]
    instrument = combination["instrument"]
    return f"{tech}_{instrument}_{sampler}_{pattern}_c{container_id}_n{sample_count}.json"


@app.default
def main(
    *,
    technology: Annotated[
        Technology | None,
        cyclopts.Parameter(help="Generate params only for this technology"),
    ] = None,
    sampler: Annotated[
        Sampler | None,
        cyclopts.Parameter(help="Generate params only for this sampler"),
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
) -> None:
    """Generate queue parameter JSON files for all valid parameter combinations."""
    console.print("Loading configuration files...")
    configs = load_all_configs(CONFIG_DIR)

    console.print("Loading orders from proteomics_projects.json...")
    orders = load_orders(technology_filter=technology)
    console.print(f"Found [cyan]{len(orders)}[/cyan] orders with samples")

    if limit:
        orders = orders[:limit]
        console.print(f"Limited to [cyan]{len(orders)}[/cyan] orders")

    if not dry_run:
        OUTPUT_DIR.mkdir(parents=True, exist_ok=True)

    client = None
    if not dry_run:
        console.print("Connecting to B-Fabric...")
        client = Bfabric.connect(config_file_env="TEST")

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
            configs,
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
            params = generate_queue_params(order, combo, samples, run_date, user)
            filename = params_filename(order, combo)
            filepath = OUTPUT_DIR / filename

            filepath.write_text(json.dumps(params, indent=2))
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
        console.print(f"  Output directory: [dim]{OUTPUT_DIR}[/dim]")


if __name__ == "__main__":
    app()
