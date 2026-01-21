#!/usr/bin/env python
"""Find proteomics projects with samples in B-Fabric."""

import json
from pathlib import Path

import polars as pl
from bfabric import Bfabric
from loguru import logger

from qg.bfabric_utils import get_order_info, get_proteomics_projects_with_samples


def _get_cache_dir() -> Path:
    """Get the bfabric_cache directory path (project_root/bfabric_cache/)."""
    # Navigate from src/qg/cli/ up to project root
    return Path(__file__).parent.parent.parent.parent / "bfabric_cache"


def main():
    """Main entry point for finding proteomics projects."""
    # Connect to TEST environment
    client = Bfabric.connect(config_file_env="TEST")

    logger.info("Querying B-Fabric TEST for running proteomics projects with samples...")
    projects = get_proteomics_projects_with_samples(client, only_running=True)

    logger.info(f"Found {len(projects)} projects")
    for p in projects:
        logger.debug(f"  [{p['id']}] {p.get('name', 'N/A')} (samples: {p.get('countsamples', 0)})")

    # Get all order IDs
    all_order_ids = []
    for p in projects:
        for order in p.get("order", []):
            all_order_ids.append(order["id"])

    logger.info(f"Querying plate and sample info for {len(all_order_ids)} orders...")
    order_info = get_order_info(client, all_order_ids)

    # Add plate and sample counts to orders in projects
    for p in projects:
        for order in p.get("order", []):
            info = order_info.get(order["id"], {"plate_count": 0, "sample_count": 0})
            order["plate_count"] = info["plate_count"]
            order["sample_count"] = info["sample_count"]

    # Ensure data directory exists
    output_dir = _get_cache_dir()
    output_dir.mkdir(exist_ok=True)

    # CSV with key fields
    df = pl.DataFrame([
        {
            "id": p["id"],
            "name": p.get("name", ""),
            "status": p.get("status", ""),
            "countsamples": p.get("countsamples", 0),
            "countorders": p.get("countorders", 0),
            "tech_area": ", ".join(p.get("tech_area", [])),
            "created": p.get("created", ""),
        }
        for p in projects
    ])
    csv_path = output_dir / "proteomics_projects.csv"
    df.write_csv(csv_path)
    logger.info(f"Saved CSV to: {csv_path}")

    # Full JSON for all fields
    json_path = output_dir / "proteomics_projects.json"
    json_path.write_text(json.dumps(projects, indent=2, default=str))
    logger.info(f"Saved JSON to: {json_path}")

    return 0


if __name__ == "__main__":
    main()
