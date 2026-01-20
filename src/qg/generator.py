"""Queue file generator for mass spectrometry instruments.

Clean separation of concerns:
- QueueGeneratorBuilder: resolves configs, creates Generator
- QueueGenerator: executes pipeline, returns rows
- format_table: separate function for output formatting
"""

from __future__ import annotations

from collections.abc import Callable
from dataclasses import asdict, dataclass
from typing import Literal

import polars as pl

from qg.config_models import OutputFormat, QueuePattern, Sample
from qg.params_models import InputSample
from qg.positions import Sampler
from qg.queue_structure import build_multi_container_queue_structure

# =============================================================================
# Data Structures
# =============================================================================


@dataclass(slots=True)
class QueueRow:
    """A single row in the generated queue."""

    run_number: int
    sample_type: Literal["user", "qc"]
    sample_id: str
    sample_name: str
    position: str
    tray: int | None = None
    inj_vol: float = 0.0
    method: str = ""
    file_name: str = ""
    polarity: str | None = None
    data_path: str = ""
    container_id: int = 0


@dataclass(slots=True)
class SlotInfo:
    """A slot in the queue structure before polarity expansion."""

    idx: int
    sample_id: str  # "default" for user samples, qc_id for QC
    position: str
    sample_config: Sample
    user_sample: InputSample | None  # Only for "default" slots
    container_id: int  # Which container this slot belongs to


@dataclass(slots=True)
class ExpandedSlot:
    """A slot after polarity expansion. Fields added progressively."""

    slot: SlotInfo
    polarity: str | None
    run_number: int
    method: str = ""  # Added by resolve_methods()
    file_name: str = ""  # Added by format_file_names()


# Type alias for method resolver
MethodResolver = Callable[[str, str | None, str], str]


# =============================================================================
# Pipeline Functions
# =============================================================================


def lookup_sample_config(
    sample_id: str,
    samples_config: dict[str, Sample],
) -> Sample | None:
    """Get Sample config for a sample_id."""
    return samples_config.get(sample_id)


def build_slots(
    slot_entries: list,  # list[SlotEntry] from queue_structure
    positions: list[str],
    samples: list[InputSample],
    samples_config: dict[str, Sample],
) -> list[SlotInfo]:
    """Build slots from SlotEntry list. Uses lookup_sample_config."""
    slots: list[SlotInfo] = []
    user_iter = iter(samples)

    for idx, entry in enumerate(slot_entries):
        sample_cfg = lookup_sample_config(entry.sample_id, samples_config)
        if not sample_cfg:
            continue

        user: InputSample | None = None
        if entry.sample_id == "default":
            user = next(user_iter, None)
            if not user:
                continue

        slots.append(SlotInfo(
            idx=idx,
            sample_id=entry.sample_id,
            position=positions[idx],
            sample_config=sample_cfg,
            user_sample=user,
            container_id=entry.container_id,
        ))

    return slots


def expand_polarities(
    slots: list[SlotInfo],
    polarities: list[str | None],
) -> list[ExpandedSlot]:
    """Loop over slots, duplicate for each polarity, assign run_number."""
    expanded: list[ExpandedSlot] = []
    run = 1

    for slot in slots:
        for polarity in polarities:
            expanded.append(ExpandedSlot(
                slot=slot,
                polarity=polarity,
                run_number=run,
            ))
            run += 1

    return expanded


def resolve_methods(
    slots: list[ExpandedSlot],
    method_resolver: MethodResolver,
    method: dict[str, str],
) -> list[ExpandedSlot]:
    """Loop over slots, resolve method for each.

    Args:
        slots: Expanded slots with polarity
        method_resolver: Function to resolve method path from sample_type/polarity/method_name
        method: Dict mapping polarity -> method_name (e.g., {"pos": "DIA_60min"})
    """
    for slot in slots:
        sample_id = slot.slot.sample_id
        polarity = slot.polarity or "pos"  # Default to pos if no polarity

        # Get method name for user samples
        if sample_id == "default":
            method_name = method.get(polarity, "")
        else:
            # QC samples: method determined by sample_type, not user selection
            method_name = ""

        slot.method = method_resolver(sample_id, slot.polarity, method_name)
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
            polarity=slot.polarity or "",
        )
    return slots


def build_queue_rows(
    slots: list[ExpandedSlot],
    data_path: str,
    inj_vol_override: float | None,
) -> list[QueueRow]:
    """Loop over slots, convert to QueueRow (uses slot.container_id)."""
    rows: list[QueueRow] = []

    for slot in slots:
        user = slot.slot.user_sample
        sample_cfg = slot.slot.sample_config

        rows.append(QueueRow(
            run_number=slot.run_number,
            sample_type="user" if slot.slot.sample_id == "default" else "qc",
            sample_id=str(user.sample_id) if user else slot.slot.sample_id,
            sample_name=user.sample_name if user else sample_cfg.sample_name,
            position=slot.slot.position,
            inj_vol=inj_vol_override or sample_cfg.inj_vol,
            file_name=slot.file_name,
            polarity=slot.polarity,
            data_path=data_path,
            method=slot.method,
            container_id=slot.slot.container_id,
        ))

    return rows

# =============================================================================
# Output Formatting (separate concern)
# =============================================================================


def format_table(rows: list[QueueRow], output_format: OutputFormat) -> pl.DataFrame:
    """Format queue rows as DataFrame for the given output format."""
    df = pl.DataFrame([asdict(row) for row in rows])

    # Select and rename columns per output format
    # columns maps: {"Output Name": "internal_field", ...}
    df = df.select([
        pl.col(internal).alias(output_name)
        for output_name, internal in output_format.columns.items()
        if internal in df.columns
    ])

    return df

# =============================================================================
# Queue Generator
# =============================================================================


@dataclass(frozen=True)
class QueueGenerator:
    """Generates queue CSV. Created by QueueGeneratorBuilder."""

    pattern: QueuePattern
    sampler: Sampler
    samples_config: dict[str, Sample]
    method_resolver: MethodResolver
    polarities: list[str | None]
    date: str
    groups: list[tuple[int, int]]  # (container_id, num_samples) per group
    data_path: str
    method: dict[str, str]  # polarity -> method_name (e.g., {"pos": "DIA_60min"})
    inj_vol_override: float | None
    output_format: OutputFormat

    def generate(self, samples: list[InputSample]) -> pl.DataFrame:
        """Execute the queue generation pipeline and return formatted DataFrame."""
        rows = self.build_rows(samples)
        return format_table(rows, self.output_format)

    def build_rows(self, samples: list[InputSample]) -> list[QueueRow]:
        """Execute the queue generation pipeline to build rows."""
        # Step 1: Build structure using groups
        slot_entries = build_multi_container_queue_structure(self.groups, self.pattern)
        structure = [s.sample_id for s in slot_entries]

        # Step 2: Assign all positions (user + QC) via sampler
        positions = self.sampler.assign_positions(structure, samples)

        # Step 3: Build slots (pass full slot_entries to preserve container_id)
        slots = build_slots(slot_entries, positions, samples, self.samples_config)

        # Step 4: Expand polarities
        expanded = expand_polarities(slots, self.polarities)

        # Step 5: Resolve methods
        expanded = resolve_methods(expanded, self.method_resolver, self.method)

        # Step 6: Format file names (uses slot.container_id)
        expanded = format_file_names(expanded, self.date)

        # Step 7: Build queue rows (uses slot.container_id)
        return build_queue_rows(expanded, self.data_path, self.inj_vol_override)
