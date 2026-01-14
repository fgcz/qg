"""Queue file generator for mass spectrometry instruments."""

from __future__ import annotations

import sys
import tomllib
from dataclasses import dataclass
from pathlib import Path
from typing import Literal

import polars as pl
from pydantic import BaseModel, ConfigDict, Field, model_validator

from qg.config import (
    load_samples,
    load_samplers,
    load_queue_patterns,
    load_qc_layouts,
    load_output_formats,
    load_combinations,
)
from qg.positions import VanquishPositionGenerator


# =============================================================================
# Input Models (from JSON)
# =============================================================================


class InputSample(BaseModel):
    """A sample from B-Fabric input JSON."""

    model_config = ConfigDict(populate_by_name=True)

    sample_name: str = Field(..., alias="Sample Name")
    sample_id: int = Field(..., alias="Sample ID")
    tube_id: str | None = Field(default=None, alias="Tube ID")
    position: str | None = Field(default=None, alias="Position")
    grid_position: str | None = Field(default=None, alias="GridPosition")


class QueueParameters(BaseModel):
    """Queue generation parameters from input JSON."""

    container_id: int
    technology: Literal["proteomics", "metabolomics", "lipidomics"]
    instrument: str
    sampler: str  # e.g., "Vanquish.vial"
    software: Literal["xcalibur", "chronos", "hystar"]
    pattern: str  # e.g., "standard"
    polarity: list[Literal["pos", "neg"]] = Field(default_factory=list)
    date: str  # YYYYMMDD
    user: str = ""  # Username for output path (e.g., "cpanse")
    method: str = ""  # Method name for user samples (e.g., "DIA_60min")
    randomization: bool = False
    inj_vol_override: float | None = None

    @model_validator(mode="after")
    def set_default_polarity(self) -> "QueueParameters":
        """Set default polarity for metabolomics/lipidomics."""
        if not self.polarity and self.technology in ("metabolomics", "lipidomics"):
            self.polarity = ["pos", "neg"]
        return self


class QueueInput(BaseModel):
    """Complete input for queue generation."""

    parameters: QueueParameters
    samples: list[InputSample]


# =============================================================================
# Internal Data Structures
# =============================================================================


@dataclass
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


@dataclass
class GenerationSummary:
    """Summary of queue generation."""

    input_file: str
    output_file: str | None
    technology: str
    instrument: str
    sampler: str
    pattern: str
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
        print(f"Pattern:     {self.pattern}", file=sys.stderr)
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

        # For metabolomics/lipidomics, filter by polarity in method_name
        if polarity and technology in ("metabolomics", "lipidomics"):
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
        pattern = self.patterns_config.get_pattern(params.technology, params.pattern)
        if not pattern:
            raise ValueError(f"Pattern '{params.pattern}' not found for {params.technology}")

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

        # Get default sample definition for inj_vol and file_name_template
        default_sample = self.samples_config.get_sample(params.technology, "default")
        if not default_sample:
            raise ValueError(f"No 'default' sample definition for {params.technology}")

        # Get data path from path template
        path_template = self._get_path_template(params.technology, params.instrument)
        data_path = self._expand_path(path_template, params.container_id, params.user, params.date)

        # Initialize position generator
        position_source = sampler_config.get("position_source", "generated")
        if position_source == "generated" and sampler_base == "Vanquish":
            pos_gen = VanquishPositionGenerator(sampler_config)
        else:
            pos_gen = None

        # Build queue with QC interleaving
        rows = self._interleave_qc(
            params=params,
            samples=samples,
            pattern=pattern,
            qc_layout=qc_layout,
            default_sample=default_sample,
            position_generator=pos_gen,
            data_path=data_path,
        )

        return rows

    def _interleave_qc(
        self,
        params: QueueParameters,
        samples: list[InputSample],
        pattern,
        qc_layout: dict,
        default_sample,
        position_generator: VanquishPositionGenerator | None,
        data_path: str,
    ) -> list[QueueRow]:
        """Interleave user samples with QC injections."""
        rows: list[QueueRow] = []
        run_number = 1

        # Helper to add QC samples
        def add_qc_sequence(sequence: list[str], polarities: list[str]) -> None:
            nonlocal run_number
            for qc_id in sequence:
                qc_sample = self.samples_config.get_sample(params.technology, qc_id)
                if not qc_sample:
                    continue

                position = qc_layout.get(qc_id, "")
                if isinstance(position, dict):
                    # Evosep position - not supported yet
                    position = f"tray{position.get('tray', 0)}:{position.get('position_start', 0)}"

                # For metabolomics/lipidomics, expand by polarity
                for pol in (polarities if polarities else [None]):
                    file_name = self._expand_file_name(
                        template=qc_sample.file_name_template,
                        date=params.date,
                        run=run_number,
                        container=params.container_id,
                        sample_id=qc_id,
                        sample_name=qc_sample.sample_name,
                        polarity=pol,
                    )
                    # Get method path for this QC sample
                    method_path = self._get_method_path(
                        technology=params.technology,
                        instrument=params.instrument,
                        sample_type=qc_id,
                        polarity=pol,
                    )
                    rows.append(QueueRow(
                        run_number=run_number,
                        sample_type="qc",
                        sample_id=qc_id,
                        sample_name=qc_sample.sample_name,
                        position=str(position),
                        inj_vol=params.inj_vol_override or qc_sample.inj_vol,
                        file_name=file_name,
                        polarity=pol,
                        container_id=params.container_id,
                        data_path=data_path,
                        method=method_path,
                    ))
                    run_number += 1

        # Helper to add a user sample
        def add_user_sample(sample: InputSample, polarities: list[str]) -> None:
            nonlocal run_number

            # Get position
            if position_generator:
                position = position_generator.next_position()
            elif sample.grid_position:
                # Plate mode - use grid_position from input
                sampler_parts = params.sampler.split(".")
                sampler_raw = self.samplers_raw.get(sampler_parts[0], {})
                container_config = sampler_raw.get(sampler_parts[1] if len(sampler_parts) > 1 else "plate", {})
                pos_format = container_config.get("position_format", "{grid_position}")
                # Get first plate for user samples
                plates = sampler_raw.get("plates", ["Y"])
                qc_plate = sampler_raw.get("qc_plate", "B")
                user_plate = next((p for p in plates if p != qc_plate), plates[0])
                position = pos_format.format(plate=user_plate, grid_position=sample.grid_position)
            else:
                position = ""

            # For metabolomics/lipidomics, expand by polarity
            for pol in (polarities if polarities else [None]):
                file_name = self._expand_file_name(
                    template=default_sample.file_name_template,
                    date=params.date,
                    run=run_number,
                    container=params.container_id,
                    sample_id=str(sample.sample_id),
                    sample_name=sample.sample_name,
                    polarity=pol,
                )
                # Get method path for user sample
                method_path = self._get_method_path(
                    technology=params.technology,
                    instrument=params.instrument,
                    sample_type="default",
                    polarity=pol,
                    method_name=params.method,
                )
                rows.append(QueueRow(
                    run_number=run_number,
                    sample_type="user",
                    sample_id=str(sample.sample_id),
                    sample_name=sample.sample_name,
                    position=position,
                    inj_vol=params.inj_vol_override or default_sample.inj_vol,
                    file_name=file_name,
                    polarity=pol,
                    container_id=params.container_id,
                    data_path=data_path,
                    method=method_path,
                ))
                run_number += 1

        # Polarities to use
        polarities = params.polarity

        # Start sequence
        add_qc_sequence(pattern.start, polarities)

        # User samples with middle QC
        qc_frequency = pattern.run_QC_after_n_samples
        samples_since_qc = 0

        for i, sample in enumerate(samples):
            add_user_sample(sample, polarities)
            samples_since_qc += 1

            # Add middle QC after every qc_frequency samples
            if samples_since_qc >= qc_frequency and i < len(samples) - 1:
                add_qc_sequence(pattern.middle, polarities)
                samples_since_qc = 0

        # End sequence
        add_qc_sequence(pattern.end, polarities)

        return rows

    def _expand_file_name(
        self,
        template: str,
        date: str,
        run: int,
        container: int,
        sample_id: str,
        sample_name: str,
        polarity: str | None,
    ) -> str:
        """Expand file name template with values."""
        result = template.format(
            date=date,
            run=f"{run:03d}",
            container=container,
            sample_id=sample_id,
            sample_name=sample_name,
            polarity=polarity or "",
        )
        return result

    def to_csv(self, rows: list[QueueRow], software: str) -> str:
        """Format queue rows as CSV for the specified software."""
        output_format = self.output_formats_config.get_format(software)
        if not output_format:
            raise ValueError(f"Unknown output format: {software}")

        columns = output_format.columns

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
