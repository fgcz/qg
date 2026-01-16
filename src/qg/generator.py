"""Queue file generator for mass spectrometry instruments.

Clean separation of concerns:
- QueueGeneratorBuilder: resolves configs, creates Generator
- QueueGenerator: executes pipeline, returns rows
- format_csv: separate function for output formatting
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Callable, Literal

from qg.config_models import Sample, QueuePattern, OutputFormat
from qg.params_models import InputSample
from qg.queue_structure import build_queue_structure
from qg.strategies import PositionAssigner


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
    structure: list[str],
    positions: list[str],
    samples: list[InputSample],
    samples_config: dict[str, Sample],
) -> list[SlotInfo]:
    """Build slots from structure. Uses lookup_sample_config."""
    slots: list[SlotInfo] = []
    user_iter = iter(samples)

    for idx, sample_id in enumerate(structure):
        sample_cfg = lookup_sample_config(sample_id, samples_config)
        if not sample_cfg:
            continue

        user: InputSample | None = None
        if sample_id == "default":
            user = next(user_iter, None)
            if not user:
                continue

        slots.append(SlotInfo(
            idx=idx,
            sample_id=sample_id,
            position=positions[idx],
            sample_config=sample_cfg,
            user_sample=user,
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
    method: str,
) -> list[ExpandedSlot]:
    """Loop over slots, resolve method for each."""
    for slot in slots:
        sample_id = slot.slot.sample_id
        slot.method = method_resolver(
            sample_id,
            slot.polarity,
            method if sample_id == "default" else "",
        )
    return slots


def format_file_names(
    slots: list[ExpandedSlot],
    date: str,
    container_id: int,
) -> list[ExpandedSlot]:
    """Loop over slots, format file_name for each."""
    for slot in slots:
        user = slot.slot.user_sample
        slot.file_name = slot.slot.sample_config.file_name_template.format(
            date=date,
            run=f"{slot.run_number:03d}",
            container=container_id,
            sample_id=str(user.sample_id) if user else "",
            sample_name=user.sample_name if user else "",
            polarity=slot.polarity or "",
        )
    return slots


def build_queue_rows(
    slots: list[ExpandedSlot],
    data_path: str,
    container_id: int,
    inj_vol_override: float | None,
) -> list[QueueRow]:
    """Loop over slots, convert to QueueRow."""
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
            container_id=container_id,
        ))

    return rows


# =============================================================================
# Queue Generator
# =============================================================================


@dataclass(frozen=True)
class QueueGenerator:
    """Generates queue rows. Created by QueueGeneratorBuilder."""

    pattern: QueuePattern
    position_assigner: PositionAssigner
    samples_config: dict[str, Sample]
    method_resolver: MethodResolver
    polarities: list[str | None]
    date: str
    container_id: int
    data_path: str
    method: str
    inj_vol_override: float | None

    def generate(self, samples: list[InputSample]) -> list[QueueRow]:
        """Execute the queue generation pipeline."""
        # Step 1: Build structure
        structure = build_queue_structure(len(samples), self.pattern)

        # Step 2: Assign all positions (user + QC)
        positions = self.position_assigner(structure, samples)

        # Step 3: Build slots
        slots = build_slots(structure, positions, samples, self.samples_config)

        # Step 4: Expand polarities
        expanded = expand_polarities(slots, self.polarities)

        # Step 5: Resolve methods
        expanded = resolve_methods(expanded, self.method_resolver, self.method)

        # Step 6: Format file names
        expanded = format_file_names(expanded, self.date, self.container_id)

        # Step 7: Build queue rows
        return build_queue_rows(
            expanded, self.data_path, self.container_id, self.inj_vol_override
        )


# =============================================================================
# Output Formatting (separate concern)
# =============================================================================


def format_csv(rows: list[QueueRow], output_format: OutputFormat) -> str:
    """Format queue rows as CSV for the given output format."""
    columns = output_format.columns
    lines = [",".join(columns.keys())]

    for row in rows:
        values = []
        for internal_field in columns.values():
            value = getattr(row, internal_field, "") or ""
            value = str(value)
            if "," in value:
                value = f'"{value}"'
            values.append(value)
        lines.append(",".join(values))

    return "\n".join(lines)
