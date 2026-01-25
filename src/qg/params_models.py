"""Pydantic models for queue parameters (runtime input)."""

from __future__ import annotations

import json
from pathlib import Path
from typing import TYPE_CHECKING, Literal, Self

from pydantic import BaseModel, ConfigDict, Field

if TYPE_CHECKING:
    from qg.config import ConfigBundle


class InputSample(BaseModel):
    """A sample from B-Fabric input JSON."""

    sample_name: str
    sample_id: int
    tube_id: str | None = None
    position: str | None = None
    grid_position: str | int | None = None
    tray: str | int | None = None  # Plate letter (Y/R/B/G) or slot number
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
    sampler: str  # e.g., "Vanquish"
    layout_mode: Literal["plate", "vial"]
    output_format: str = Field(..., min_length=1, description="Output format identifier")
    queue_pattern: str  # e.g., "standard"
    polarity: list[Literal["pos", "neg"]] = Field(default_factory=list)
    date: str  # YYYYMMDD
    user: str = ""  # Username for output path (e.g., "cpanse")
    # Method per polarity: {"pos": "DIA_60min", "neg": "DIA_60min"}
    # Keys are polarities ("pos", "neg"), values are method names (not paths)
    method: dict[str, str] = Field(default_factory=dict)
    randomization: Literal["no", "random", "blocked"] = "no"
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
        layout_mode: Literal["plate", "vial"],
        output_format: str,
        queue_pattern: str,
        polarity: list[Literal["pos", "neg"]],
        date: str,
        user: str = "",
        method: dict[str, str] | None = None,
        randomization: Literal["no", "random", "blocked"] = "no",
        inj_vol_override: float | None = None,
        qc_frequency_override: int | None = None,
    ) -> Self:
        """Create validated QueueParameters.

        Validates that all referenced configs exist before construction.

        Args:
            configs: Configuration bundle for validation.
            tech_area: Technology area (Proteomics, Metabolomics, Lipidomics).
            instrument: Instrument name.
            sampler: Sampler name (e.g., "Vanquish").
            layout_mode: Layout mode ("plate" or "vial").
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

        # Validate QC layout exists (using full sampler key: sampler.layout_mode)
        sampler_key = f"{sampler}.{layout_mode}"
        if not configs.qc_layouts.get_layout(tech_area, sampler_key):
            raise ValueError(f"QC layout not found for {tech_area}.{sampler_key}")

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
            layout_mode=layout_mode,
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

    def get_sample_by_index(self, index: int) -> InputSample:
        """Get sample by flat index across all groups.

        Args:
            index: Flat index into get_all_samples() ordering.

        Returns:
            The InputSample at that index.

        Raises:
            IndexError: If index is out of range.
        """
        all_samples = self.get_all_samples()
        if index < 0 or index >= len(all_samples):
            raise IndexError(f"Sample index {index} out of range (0-{len(all_samples) - 1})")
        return all_samples[index]

    def update_sample_position(
        self,
        index: int,
        tray: str | int,
        grid_position: str | int,
    ) -> None:
        """Update position fields for sample at flat index.

        Mutates the sample in place.

        Args:
            index: Flat index into get_all_samples() ordering.
            tray: Plate letter (Y/R/B/G) or slot number.
            grid_position: Well position (A1/B2) or tip number (1-96).

        Raises:
            IndexError: If index is out of range.
        """
        # Find which group and local index
        current_idx = 0
        for group in self.sample_groups:
            group_size = len(group.samples)
            if current_idx + group_size > index:
                local_idx = index - current_idx
                sample = group.samples[local_idx]
                sample.tray = tray
                sample.grid_position = grid_position
                return
            current_idx += group_size
        raise IndexError(f"Sample index {index} out of range")

    def get_primary_container_id(self) -> int:
        """Get the primary container ID (first group's container_id)."""
        return self.sample_groups[0].container_id

    def write(self, output_path: str | Path) -> Path:
        """Write queue input to JSON file.

        Args:
            output_path: Path to write the JSON file.

        Returns:
            Path to the written file.
        """
        output_path = Path(output_path)

        # Build parameters dict
        params = self.parameters
        params_dict: dict = {
            "tech_area": params.tech_area,
            "instrument": params.instrument,
            "sampler": params.sampler,
            "layout_mode": params.layout_mode,
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
                for group in self.sample_groups
            ],
        }

        output_path.parent.mkdir(parents=True, exist_ok=True)
        output_path.write_text(json.dumps(output_dict, indent=2))

        return output_path
