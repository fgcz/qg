"""Queue file generator for mass spectrometry instruments."""

from __future__ import annotations

import sys
import tomllib
from dataclasses import dataclass
from pathlib import Path
from typing import Literal

import polars as pl

from qg.config import (
    load_samples,
    load_samplers,
    load_queue_patterns,
    load_qc_layouts,
    load_output_formats,
    load_combinations,
)
from qg.config_models import QCPosition, requires_polarity
from qg.params_models import InputSample, QueueParameters, QueueInput
from qg.positions import get_sampler
from qg.queue_structure import build_queue_structure


# =============================================================================
# Internal Data Structures
# =============================================================================


@dataclass(slots=True)
class QueueRow:
    """Internal representation of a queue row."""

    run_number: int
    sample_type: Literal["user", "qc"]
    sample_id: str  # For user samples: B-Fabric sample_id; for QC: sample_id from config
    sample_name: str
    position: str
    tray: int | None = None  # Evosep only
    inj_vol: float = 0.0
    method: str = ""
    file_name: str = ""
    polarity: str | None = None  # For metabolomics/lipidomics
    data_path: str = ""
    container_id: int = 0


def assign_positions(
    structure: list[str],
    user_positions: list[str],
    qc_layout: dict[str, QCPosition],
) -> list[str]:
    """Assign positions to all slots in the queue structure.

    Pure function that maps the queue structure to physical positions.

    Args:
        structure: Queue structure from build_queue_structure()
        user_positions: Pre-generated positions for user samples
        qc_layout: QC sample_id -> position mapping from qc_layouts.toml

    Returns:
        List of positions parallel to structure
    """
    positions = []
    user_idx = 0

    for sample_id in structure:
        if sample_id == "default":
            positions.append(user_positions[user_idx])
            user_idx += 1
        else:
            pos = qc_layout.get(sample_id, "")
            # Handle Evosep dict positions
            if isinstance(pos, dict):
                positions.append(f"tray{pos.get('tray', 0)}:{pos.get('position_start', 0)}")
            else:
                positions.append(str(pos))

    return positions


@dataclass(slots=True)
class GenerationSummary:
    """Summary of queue generation."""

    input_file: str
    output_file: str | None
    technology: str
    instrument: str
    sampler: str
    queue_pattern: str
    container_id: int
    input_samples: int
    output_rows: int
    qc_rows: int
    sample_rows: int
    success: bool
    error: str | None = None

    def print_summary(self) -> None:
        """Print summary to stderr."""
        print("\n--- Generation Summary ---", file=sys.stderr)
        print(f"Input:       {self.input_file}", file=sys.stderr)
        print(f"Output:      {self.output_file or 'stdout'}", file=sys.stderr)
        print(f"Technology:  {self.technology}", file=sys.stderr)
        print(f"Instrument:  {self.instrument}", file=sys.stderr)
        print(f"Sampler:     {self.sampler}", file=sys.stderr)
        print(f"Pattern:     {self.queue_pattern}", file=sys.stderr)
        print(f"Container:   {self.container_id}", file=sys.stderr)
        print(f"Input samples: {self.input_samples}", file=sys.stderr)
        print(f"Output rows:   {self.output_rows} (QC: {self.qc_rows}, Samples: {self.sample_rows})", file=sys.stderr)
        if self.error:
            print(f"Error:       {self.error}", file=sys.stderr)
        print(f"Status:      {'SUCCESS' if self.success else 'FAILED'}", file=sys.stderr)
        print("-" * 28, file=sys.stderr)


# =============================================================================
# Queue Generator
# =============================================================================


class QueueGenerator:
    """Main queue generation logic."""

    def __init__(self, config_dir: Path):
        self.config_dir = config_dir
        self._load_configs()

    def _load_configs(self) -> None:
        """Load all configuration files."""
        self.samples_config = load_samples(self.config_dir / "samples.csv")
        self.samplers_config = load_samplers(self.config_dir / "sampler.toml")
        self.patterns_config = load_queue_patterns(self.config_dir / "queue_patterns.toml")
        self.qc_layouts_config = load_qc_layouts(self.config_dir / "qc_layouts.toml")
        self.output_formats_config = load_output_formats(self.config_dir / "output_formats.toml")
        self.combinations_config = load_combinations(self.config_dir / "combinations.csv")

        # Load instruments.csv for path templates and methods files
        self.instruments_df = pl.read_csv(self.config_dir / "instruments.csv")

        # Also load raw TOML for sampler details
        with open(self.config_dir / "sampler.toml", "rb") as f:
            self.samplers_raw = tomllib.load(f)

        # Cache for loaded methods DataFrames
        self._methods_cache: dict[str, pl.DataFrame] = {}

    def _load_methods(self, technology: str, instrument: str) -> pl.DataFrame | None:
        """Load methods CSV for a given technology/instrument."""
        cache_key = f"{technology}.{instrument}"
        if cache_key in self._methods_cache:
            return self._methods_cache[cache_key]

        row = self.instruments_df.filter(
            (pl.col("technology") == technology) &
            (pl.col("instrument") == instrument)
        )
        if row.is_empty():
            return None

        methods_file = row["methods_file"][0]
        methods_path = self.config_dir / methods_file
        if not methods_path.exists():
            return None

        methods_df = pl.read_csv(methods_path)
        self._methods_cache[cache_key] = methods_df
        return methods_df

    def _get_method_path(
        self,
        technology: str,
        instrument: str,
        sample_type: str,
        polarity: str | None = None,
        method_name: str = "",
    ) -> str:
        """Get method path for a sample.

        Args:
            technology: Technology (proteomics, metabolomics, lipidomics)
            instrument: Instrument name
            sample_type: Sample type (QC ID or "default" for user samples)
            polarity: Polarity for metabolomics/lipidomics ("pos" or "neg")
            method_name: Optional specific method name to use

        Returns:
            Method path or empty string if not found
        """
        methods_df = self._load_methods(technology, instrument)
        if methods_df is None:
            return ""

        # Filter by sample_type
        matches = methods_df.filter(pl.col("sample_type") == sample_type)

        # If no matches for specific sample_type, try "default"
        if matches.is_empty() and sample_type != "default":
            matches = methods_df.filter(pl.col("sample_type") == "default")

        if matches.is_empty():
            return ""

        # For technologies requiring polarity, filter by polarity in method_name
        if polarity and requires_polarity(technology):
            polarity_suffix = "_Pos" if polarity == "pos" else "_Neg"
            polarity_matches = matches.filter(
                pl.col("method_name").str.contains(polarity_suffix, literal=True)
            )
            if not polarity_matches.is_empty():
                matches = polarity_matches

        # If specific method_name requested, filter by it
        if method_name:
            name_matches = matches.filter(pl.col("method_name") == method_name)
            if not name_matches.is_empty():
                matches = name_matches

        # Return first match
        if matches.is_empty():
            return ""
        return matches["method_path"][0]

    def _get_path_template(self, technology: str, instrument: str) -> str:
        """Get path template for a given technology and instrument."""
        row = self.instruments_df.filter(
            (pl.col("technology") == technology) &
            (pl.col("instrument") == instrument)
        )
        if row.is_empty():
            return ""
        return row["path_template"][0]

    def _expand_path(self, template: str, container: int, user: str, date: str) -> str:
        """Expand path template with runtime values."""
        if not template:
            return ""
        return template.format(container=container, user=user, date=date)

    def generate(self, queue_input: QueueInput) -> list[QueueRow]:
        """Generate queue rows from input."""
        params = queue_input.parameters
        samples = queue_input.samples

        # Get pattern config
        pattern = self.patterns_config.get_pattern(params.technology, params.queue_pattern)
        if not pattern:
            raise ValueError(f"Pattern '{params.queue_pattern}' not found for {params.technology}")

        # Get sampler config
        sampler_parts = params.sampler.split(".")
        sampler_base = sampler_parts[0]
        container = sampler_parts[1] if len(sampler_parts) > 1 else "vial"

        sampler_raw = self.samplers_raw.get(sampler_base, {})
        container_config = sampler_raw.get(container, {})
        # Merge base + container config
        sampler_config = {k: v for k, v in sampler_raw.items() if not isinstance(v, dict)}
        sampler_config.update(container_config)

        # Get QC layout
        qc_layout = self.qc_layouts_config.get_layout(params.technology, params.sampler)
        if not qc_layout:
            raise ValueError(f"QC layout not found for {params.technology}.{params.sampler}")

        # Validate default sample definition exists
        default_sample = self.samples_config.get_sample(params.technology, "default")
        if not default_sample:
            raise ValueError(f"No 'default' sample definition for {params.technology}")

        # Get data path from path template
        path_template = self._get_path_template(params.technology, params.instrument)
        data_path = self._expand_path(path_template, params.container_id, params.user, params.date)

        # Step 1: Build queue structure
        structure = build_queue_structure(len(samples), pattern)

        # Step 2: Count user samples in structure
        num_user_samples = structure.count("default")

        # Step 3: Generate user positions (stateless)
        position_source = sampler_config.get("position_source", "generated")
        if position_source == "generated":
            sampler = get_sampler(params.sampler, sampler_config)
            user_positions = sampler.generate_positions(num_user_samples)
        else:
            # Positions come from input (plate mode)
            user_positions = [s.grid_position or "" for s in samples]

        # Step 4: Assign all positions (user + QC)
        positions = assign_positions(structure, user_positions, qc_layout)

        # Step 5: Populate rows (no position logic)
        rows = self._populate_queue(
            structure=structure,
            positions=positions,
            input_samples=samples,
            params=params,
            data_path=data_path,
        )

        return rows

    def _populate_queue(
        self,
        structure: list[str],
        positions: list[str],
        input_samples: list[InputSample],
        params: QueueParameters,
        data_path: str,
    ) -> list[QueueRow]:
        """Populate queue structure with sample data.

        Position assignment is already done - this only assembles rows.

        Args:
            structure: List of sample_ids from build_queue_structure()
            positions: Pre-computed positions parallel to structure
            input_samples: User samples from input
            params: Queue parameters
            data_path: Data path for output files

        Returns:
            List of populated QueueRow objects
        """
        rows: list[QueueRow] = []
        user_iter = iter(input_samples)
        run = 1
        polarities = params.polarity or [None]

        for idx, sample_id in enumerate(structure):
            sample_config = self.samples_config.get_sample(params.technology, sample_id)
            if not sample_config:
                continue

            # Get user sample if this is a user slot
            user: InputSample | None = None
            if sample_id == "default":
                user = next(user_iter, None)
                if not user:
                    continue

            # Position is pre-computed
            position = positions[idx]

            # Expand for each polarity
            for polarity in polarities:
                file_name = sample_config.file_name_template.format(
                    date=params.date,
                    run=f"{run:03d}",
                    container=params.container_id,
                    sample_id=str(user.sample_id) if user else "",
                    sample_name=user.sample_name if user else "",
                    polarity=polarity or "",
                )

                rows.append(QueueRow(
                    run_number=run,
                    sample_type="user" if sample_id == "default" else "qc",
                    sample_id=str(user.sample_id) if user else sample_id,
                    sample_name=user.sample_name if user else sample_config.sample_name,
                    position=position,
                    inj_vol=params.inj_vol_override or sample_config.inj_vol,
                    file_name=file_name,
                    polarity=polarity,
                    data_path=data_path,
                    method=self._get_method_path(
                        params.technology,
                        params.instrument,
                        sample_id,
                        polarity,
                        params.method if sample_id == "default" else "",
                    ),
                    container_id=params.container_id,
                ))
                run += 1

        return rows

    def to_csv(self, rows: list[QueueRow], output_format: str) -> str:
        """Format queue rows as CSV for the specified output format."""
        fmt = self.output_formats_config.get_format(output_format)
        if not fmt:
            raise ValueError(f"Unknown output format: {output_format}")

        columns = fmt.columns

        # Build CSV output
        output_lines = []

        # Header
        output_lines.append(",".join(columns.keys()))

        # Rows
        for row in rows:
            values = []
            for col_name, internal_field in columns.items():
                value = self._get_field_value(row, internal_field)
                # Quote values containing commas
                if "," in str(value):
                    value = f'"{value}"'
                values.append(str(value))
            output_lines.append(",".join(values))

        return "\n".join(output_lines)

    def _get_field_value(self, row: QueueRow, field_name: str) -> str:
        """Get field value from QueueRow by internal field name."""
        mapping = {
            "file_name": row.file_name,
            "data_path": row.data_path or "",
            "method": row.method or "",
            "position": row.position,
            "inj_vol": str(row.inj_vol),
            "sample_type": "Unknown" if row.sample_type == "user" else "QC",
            "sample_name": row.sample_name,
            "tray": str(row.tray) if row.tray else "",
            "xcalibur_method": row.method or "",
            "post_acquisition": "",
            "comment": "",
            "analysis_method": "",
        }
        return mapping.get(field_name, "")
