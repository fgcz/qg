"""Queue file generator for mass spectrometry instruments.

Clean separation of concerns:
- QueueGeneratorBuilder: resolves configs, creates Generator
- QueueGenerator: executes pipeline, returns rows
- format_table: separate function for output formatting
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Any, Literal

import polars as pl
from pydantic import BaseModel

from qg.config_models import MethodsConfig, OutputFormat, QueuePattern, Sample, SamplesConfig
from qg.params_models import InputSample, QueueInput
from qg.positions import Sampler
from qg.queue_structure import _extract_groups, build_multi_container_queue_structure

# Type alias for position dict (used by SlotInfo/ExpandedSlot internal dataclasses)
PositionDict = dict[str, Any]

# =============================================================================
# Data Structures
# =============================================================================


class QueueRow(BaseModel):
    """A single row in the generated queue."""

    run_number: int
    sample_type: Literal["user", "qc"]
    sample_id: str
    sample_name: str
    tray: str | int  # First-class: plate letter or slot number
    grid_position: str | int  # First-class: "A1" or position number
    inj_vol: float = 0.0
    method: str = ""
    file_name: str = ""
    polarity: str = ""
    data_path: str = ""
    container_id: int = 0


class QueueRowTable(BaseModel):
    """Collection of queue rows (like CombinationsConfig pattern)."""

    rows: list[QueueRow]

    def to_table(self) -> pl.DataFrame:
        """Convert rows to polars DataFrame."""
        return pl.DataFrame([row.model_dump() for row in self.rows])


@dataclass(slots=True)
class SlotInfo:
    """A slot in the queue structure before polarity expansion."""

    idx: int
    sample_id: str  # "default" for user samples, qc_id for QC
    position: PositionDict  # Position dict
    sample_config: Sample
    user_sample: InputSample | None  # Only for "default" slots
    container_id: int  # Which container this slot belongs to


@dataclass(slots=True)
class ExpandedSlot:
    """A slot after polarity expansion. Fields added progressively."""

    slot: SlotInfo
    polarity: str
    run_number: int
    method: str = ""  # Added by resolve_methods()
    file_name: str = ""  # Added by format_file_names()


# =============================================================================
# Pipeline Functions
# =============================================================================


def build_slots(
    slot_entries: list,  # list[SlotEntry] from queue_structure
    positions: list[PositionDict],
    samples: list[InputSample],
    samples_config: SamplesConfig,
    tech_area: str,
) -> list[SlotInfo]:
    """Build slots from SlotEntry list. Uses lookup_sample_config."""
    slots: list[SlotInfo] = []
    user_iter = iter(samples)

    for idx, entry in enumerate(slot_entries):
        sample_cfg = samples_config.get_sample(tech_area, entry.sample_id)
        if not sample_cfg:
            continue

        user: InputSample | None = None
        if entry.sample_id == "default":
            user = next(user_iter, None)
            if not user:
                continue

        slots.append(
            SlotInfo(
                idx=idx,
                sample_id=entry.sample_id,
                position=positions[idx],
                sample_config=sample_cfg,
                user_sample=user,
                container_id=entry.container_id,
            )
        )

    return slots


def expand_polarities(
    slots: list[SlotInfo],
    polarities: list[str],
) -> list[ExpandedSlot]:
    """Loop over slots, duplicate for each polarity, assign run_number."""
    expanded: list[ExpandedSlot] = []
    run = 1

    for slot in slots:
        for polarity in polarities:
            expanded.append(
                ExpandedSlot(
                    slot=slot,
                    polarity=polarity,
                    run_number=run,
                )
            )
            run += 1

    return expanded


def resolve_methods(
    slots: list[ExpandedSlot],
    methods_config: MethodsConfig,
    tech_area: str,
    instrument: str,
    method: dict[str, str],
) -> list[ExpandedSlot]:
    """Loop over slots, resolve method for each.

    Args:
        slots: Expanded slots with polarity
        methods_config: Methods configuration
        tech_area: Technology area (proteomics, metabolomics, lipidomics)
        instrument: Instrument name
        method: Dict mapping polarity -> method_name (e.g., {"pos": "DIA_60min"})
    """
    for slot in slots:
        sample_id = slot.slot.sample_id
        polarity = slot.polarity

        # Get method name for user samples
        if sample_id == "default":
            method_name = method.get(polarity, "")
        else:
            # QC samples: method determined by sample_type, not user selection
            method_name = ""

        slot.method = methods_config.get_method_path(tech_area, instrument, sample_id, polarity, method_name)
    return slots


def format_file_names(
    slots: list[ExpandedSlot],
    date: str,
) -> list[ExpandedSlot]:
    """Loop over slots, format file_name for each (uses slot.container_id)."""
    for slot in slots:
        user = slot.slot.user_sample
        slot.file_name = slot.slot.sample_config.file_name_template.format(
            date=date,
            run=f"{slot.run_number:03d}",
            container=slot.slot.container_id,
            sample_id=str(user.sample_id) if user else "",
            sample_name=user.sample_name if user else "",
            polarity=slot.polarity,
        )
    return slots


def build_queue_rows(
    slots: list[ExpandedSlot],
    data_path: str,
    inj_vol_override: float | None,
) -> QueueRowTable:
    """Loop over slots, convert to QueueRow (uses slot.container_id)."""
    rows: list[QueueRow] = []

    for slot in slots:
        user = slot.slot.user_sample
        sample_cfg = slot.slot.sample_config
        pos = slot.slot.position

        rows.append(
            QueueRow(
                run_number=slot.run_number,
                sample_type="user" if slot.slot.sample_id == "default" else "qc",
                sample_id=str(user.sample_id) if user else slot.slot.sample_id,
                sample_name=user.sample_name if user else sample_cfg.sample_name,
                tray=pos["tray"],
                grid_position=pos["grid_position"],
                inj_vol=inj_vol_override or sample_cfg.inj_vol,
                file_name=slot.file_name,
                polarity=slot.polarity,
                data_path=data_path,
                method=slot.method,
                container_id=slot.slot.container_id,
            )
        )

    return QueueRowTable(rows=rows)


# =============================================================================
# Output Formatting (separate concern)
# =============================================================================


def format_table(
    queue_rows: QueueRowTable,
    output_format: OutputFormat,
) -> pl.DataFrame:
    """Format queue rows as DataFrame for the given output format.

    Args:
        queue_rows: QueueRowTable containing rows to format
        output_format: Output format specification with column mappings and position_format
    """
    # Convert to DataFrame using QueueRowTable.to_table()
    df = queue_rows.to_table()

    # Add formatted position column from tray and grid_position
    df = df.with_columns(
        pl.struct(["tray", "grid_position"])
        .map_elements(
            lambda s: output_format.position_format.format(tray=s["tray"], grid_position=s["grid_position"]),
            return_dtype=pl.Utf8,
        )
        .alias("position")
    )

    # Select and rename columns per output format
    # columns maps: {"Output Name": "internal_field", ...}
    df = df.select(
        [
            pl.col(internal).alias(output_name)
            for output_name, internal in output_format.columns.items()
            if internal in df.columns
        ]
    )

    return df


# =============================================================================
# Queue Generator
# =============================================================================


class QueueGenerator:
    """Generates queue CSV. Created by QueueGeneratorBuilder."""

    queue_input: QueueInput
    pattern: QueuePattern
    sampler: Sampler
    samples_config: SamplesConfig
    methods_config: MethodsConfig
    data_path: str
    output_format: OutputFormat

    def __init__(
        self,
        queue_input: QueueInput,
        pattern: QueuePattern,
        sampler: Sampler,
        samples_config: SamplesConfig,
        methods_config: MethodsConfig,
        data_path: str,
        output_format: OutputFormat,
    ) -> None:
        self.queue_input = queue_input
        self.pattern = pattern
        self.sampler = sampler
        self.samples_config = samples_config
        self.methods_config = methods_config
        self.data_path = data_path
        self.output_format = output_format

    def generate(self, samples: list[InputSample]) -> pl.DataFrame:
        """Execute the queue generation pipeline and return formatted DataFrame."""
        rows = self.build_rows(samples)
        return format_table(rows, self.output_format)

    def build_rows(self, samples: list[InputSample]) -> QueueRowTable:
        """Execute the queue generation pipeline to build rows."""
        params = self.queue_input.parameters
        groups = _extract_groups(self.queue_input)

        # Step 1: Build structure using groups
        slot_entries = build_multi_container_queue_structure(groups, self.pattern)
        structure = [s.sample_id for s in slot_entries]

        # Step 2: Assign all positions (user + QC) via sampler
        positions = self.sampler.assign_positions(structure, samples)

        # Step 3: Build slots (pass full slot_entries to preserve container_id)
        slots = build_slots(slot_entries, positions, samples, self.samples_config, params.tech_area)

        # Step 4: Expand polarities
        expanded = expand_polarities(slots, params.polarity)

        # Step 5: Resolve methods
        expanded = resolve_methods(expanded, self.methods_config, params.tech_area, params.instrument, params.method)

        # Step 6: Format file names (uses slot.container_id)
        expanded = format_file_names(expanded, params.date)

        # Step 7: Build queue rows (uses slot.container_id)
        return build_queue_rows(expanded, self.data_path, params.inj_vol_override)
