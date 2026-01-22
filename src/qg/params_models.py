"""Pydantic models for queue parameters (runtime input)."""

from __future__ import annotations

import json
from pathlib import Path
from typing import TYPE_CHECKING, Literal, Self

from pydantic import BaseModel, ConfigDict, Field

if TYPE_CHECKING:
    import polars as pl

    from qg.config import ConfigBundle


class InputSample(BaseModel):
    """A sample from B-Fabric input JSON."""

    sample_name: str
    sample_id: int
    tube_id: str | None = None
    position: str | None = None
    grid_position: str | None = None
    grouping_var: str | None = None
    plate_id: int | None = None  # Plate ID for plate samples


class SampleGroup(BaseModel):
    """A group of samples from a single project/container."""

    model_config = ConfigDict(populate_by_name=True)

    container_id: int
    group_name: str | None = Field(default=None)
    samples: list[InputSample] = Field(default_factory=list)  # Can be empty for QC-only

    @property
    def effective_name(self) -> str:
        """Get display name (group_name or container_id as string)."""
        return self.group_name or str(self.container_id)


class QueueParameters(BaseModel):
    """Queue generation parameters from input JSON."""

    tech_area: str = Field(..., min_length=1, description="tech_area identifier")
    instrument: str
    sampler: str  # e.g., "Vanquish.vial"
    output_format: str = Field(..., min_length=1, description="Output format identifier")
    queue_pattern: str  # e.g., "standard"
    polarity: list[Literal["pos", "neg"]] = Field(default_factory=list)
    date: str  # YYYYMMDD
    user: str = ""  # Username for output path (e.g., "cpanse")
    # Method per polarity: {"pos": "DIA_60min", "neg": "DIA_60min"}
    # Keys are polarities ("pos", "neg"), values are method names (not paths)
    method: dict[str, str] = Field(default_factory=dict)
    randomization: Literal["no", "random", "blockrandom"] = "no"
    inj_vol_override: float | None = None
    qc_frequency_override: int | None = None  # Override run_QC_after_n_samples

    @classmethod
    def create(
        cls,
        configs: ConfigBundle,
        *,
        tech_area: str,
        instrument: str,
        sampler: str,
        output_format: str,
        queue_pattern: str,
        polarity: list[Literal["pos", "neg"]],
        date: str,
        user: str = "",
        method: dict[str, str] | None = None,
        randomization: Literal["no", "random", "blockrandom"] = "no",
        inj_vol_override: float | None = None,
        qc_frequency_override: int | None = None,
    ) -> Self:
        """Create validated QueueParameters.

        Validates that all referenced configs exist before construction.

        Args:
            configs: Configuration bundle for validation.
            tech_area: Technology area (Proteomics, Metabolomics, Lipidomics).
            instrument: Instrument name.
            sampler: Sampler name (e.g., "Vanquish.vial").
            output_format: Output format identifier.
            queue_pattern: Queue pattern name.
            polarity: List of polarities to run.
            date: Date string (YYYYMMDD).
            user: Username for output path.
            method: Method per polarity (e.g., {"pos": "DIA_60min"}).
            randomization: Randomization mode.
            inj_vol_override: Optional injection volume override.
            qc_frequency_override: Optional QC frequency override.

        Returns:
            Validated QueueParameters instance.

        Raises:
            ValueError: If any referenced config doesn't exist.
        """
        # Validate pattern exists
        if not configs.queue_patterns.get_pattern(tech_area, queue_pattern):
            raise ValueError(f"Pattern '{queue_pattern}' not found for {tech_area}")

        # Validate QC layout exists
        if not configs.qc_layouts.get_layout(tech_area, sampler):
            raise ValueError(f"QC layout not found for {tech_area}.{sampler}")

        # Validate output format exists
        if not configs.output_formats.get_format(output_format):
            raise ValueError(f"Output format '{output_format}' not found")

        # Validate instrument exists
        if not configs.instruments.get_instrument(tech_area, instrument):
            raise ValueError(f"Instrument '{instrument}' not found for {tech_area}")

        # Validate default sample exists
        if not configs.samples.get_sample(tech_area, "default"):
            raise ValueError(f"No 'default' sample definition for {tech_area}")

        return cls(
            tech_area=tech_area,
            instrument=instrument,
            sampler=sampler,
            output_format=output_format,
            queue_pattern=queue_pattern,
            polarity=polarity,
            date=date,
            user=user,
            method=method or {},
            randomization=randomization,
            inj_vol_override=inj_vol_override,
            qc_frequency_override=qc_frequency_override,
        )


class QueueInput(BaseModel):
    """Complete input for queue generation."""

    parameters: QueueParameters
    sample_groups: list[SampleGroup] = Field(..., min_length=1)

    def get_all_samples(self) -> list[InputSample]:
        """Get all samples flattened."""
        return [s for group in self.sample_groups for s in group.samples]

    def get_primary_container_id(self) -> int:
        """Get the primary container ID (first group's container_id)."""
        return self.sample_groups[0].container_id


def samples_from_dataframe(df: pl.DataFrame) -> list[InputSample]:
    """Convert a polars DataFrame to list of InputSample."""
    return [InputSample(**row) for row in df.to_dicts()]


def write_params(queue_input: QueueInput, output_path: str | Path) -> Path:
    """Write queue parameters to JSON file.

    Args:
        queue_input: The QueueInput object to serialize.
        output_path: Path to write the JSON file.

    Returns:
        Path to the written file.
    """
    output_path = Path(output_path)

    # Build parameters dict
    params = queue_input.parameters
    params_dict: dict = {
        "tech_area": params.tech_area,
        "instrument": params.instrument,
        "sampler": params.sampler,
        "output_format": params.output_format,
        "queue_pattern": params.queue_pattern,
        "polarity": params.polarity,
        "date": params.date,
        "user": params.user,
        "method": params.method,
        "randomization": params.randomization,
        "inj_vol_override": params.inj_vol_override,
    }

    output_dict: dict = {
        "parameters": params_dict,
        "sample_groups": [
            {
                "container_id": group.container_id,
                "group_name": group.group_name,
                "samples": [sample.model_dump(by_alias=True, exclude_none=True) for sample in group.samples],
            }
            for group in queue_input.sample_groups
        ],
    }

    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text(json.dumps(output_dict, indent=2))

    return output_path
