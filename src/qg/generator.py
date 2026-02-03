"""Queue file generator for mass spectrometry instruments."""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING, Any, Literal

import polars as pl
from pydantic import BaseModel

from qg.config_models.formatting import Sample
from qg.config_models.loader import QGConfiguration
from qg.config_models.methods import MethodsConfig
from qg.config_models.structure import QueuePattern
from qg.params_models import PlateCell, PlateQueue, QueueInput, VialQueueInput
from qg.positions import SamplerStrategyV2
from qg.queue_structure import SlotEntry, build_multi_container_queue_structure
from qg.randomize import randomize_plate_queue

if TYPE_CHECKING:
    from qg.config_models.formatting import OutputFormat, SamplesConfig

PositionDict = dict[str, Any]


class QueueRow(BaseModel):
    """A single row in the generated queue."""

    run_number: int
    sample_type: Literal["user", "qc"]
    sample_id: str
    sample_name: str
    tray: str | int | None = None
    grid_position: str | int | None = None
    plate_id: int | None = None
    grouping_var: str | None = None
    inj_vol: float = 0.0
    method: str = ""
    file_name: str = ""
    polarity: str = ""
    data_path: str = ""
    container_id: int = 0


class QueueRowTable(BaseModel):
    """Collection of queue rows."""

    rows: list[QueueRow]

    def to_table(self) -> pl.DataFrame:
        return pl.DataFrame([row.model_dump() for row in self.rows])


@dataclass(slots=True)
class SlotInfo:
    """A slot in the queue structure before polarity expansion."""

    idx: int
    sample_id: str  # "default" for user samples, qc_id for QC
    position: PositionDict
    sample_config: Sample
    user_cell: PlateCell | None  # Only for "default" slots
    container_id: int


@dataclass(slots=True)
class ExpandedSlot:
    """A slot after polarity expansion."""

    slot: SlotInfo
    polarity: str
    run_number: int
    method: str = ""
    file_name: str = ""


def _build_slots(
    slot_entries: list[SlotEntry],
    plate_queue: PlateQueue,
    sampler: SamplerStrategyV2,
    samples_config: SamplesConfig,
    tech_area: str,
) -> list[SlotInfo]:
    """Build slots from SlotEntry list and PlateQueue."""
    slots: list[SlotInfo] = []
    cell_iter = iter(plate_queue.cells)

    for idx, entry in enumerate(slot_entries):
        sample_cfg = samples_config.get_sample(tech_area, entry.sample_id)
        if not sample_cfg:
            continue

        user_cell: PlateCell | None = None
        if entry.sample_id == "default":
            user_cell = next(cell_iter, None)
            if not user_cell:
                continue
            position = {
                "tray": plate_queue.plates[user_cell.plate_id].tray,
                "grid_position": user_cell.grid_position,
            }
        else:
            position = sampler.get_qc_position(entry.sample_id)

        slots.append(
            SlotInfo(
                idx=idx,
                sample_id=entry.sample_id,
                position=position,
                sample_config=sample_cfg,
                user_cell=user_cell,
                container_id=entry.container_id,
            )
        )

    return slots


def _expand_polarities(slots: list[SlotInfo], polarities: list[str]) -> list[ExpandedSlot]:
    """Duplicate slots for each polarity, assign run_number."""
    expanded: list[ExpandedSlot] = []
    run = 1
    for slot in slots:
        for polarity in polarities:
            expanded.append(ExpandedSlot(slot=slot, polarity=polarity, run_number=run))
            run += 1
    return expanded


def _resolve_methods(
    slots: list[ExpandedSlot],
    methods_config: MethodsConfig,
    tech_area: str,
    instrument: str,
    method: dict[str, str],
) -> list[ExpandedSlot]:
    """Resolve method path for each slot."""
    for slot in slots:
        sample_id = slot.slot.sample_id
        polarity = slot.polarity
        method_name = method.get(polarity, "") if sample_id == "default" else ""
        slot.method = methods_config.get_method_path(tech_area, instrument, sample_id, polarity, method_name)
    return slots


def _format_file_names(slots: list[ExpandedSlot], date: str) -> list[ExpandedSlot]:
    """Format file_name for each slot."""
    for slot in slots:
        cell = slot.slot.user_cell
        sample = cell.sample if cell else None
        slot.file_name = slot.slot.sample_config.file_name_template.format(
            date=date,
            run=f"{slot.run_number:03d}",
            container=slot.slot.container_id,
            sample_id=str(sample.sample_id) if sample else "",
            sample_name=sample.sample_name if sample else "",
            polarity=slot.polarity,
        )
    return slots


def _build_queue_rows(slots: list[ExpandedSlot], data_path: str, inj_vol_override: float | None) -> QueueRowTable:
    """Convert slots to QueueRows."""
    rows: list[QueueRow] = []
    for slot in slots:
        cell = slot.slot.user_cell
        sample = cell.sample if cell else None
        sample_cfg = slot.slot.sample_config
        pos = slot.slot.position

        rows.append(
            QueueRow(
                run_number=slot.run_number,
                sample_type="user" if slot.slot.sample_id == "default" else "qc",
                sample_id=str(sample.sample_id) if sample else slot.slot.sample_id,
                sample_name=sample.sample_name if sample else sample_cfg.sample_name,
                tray=pos["tray"],
                grid_position=pos["grid_position"],
                plate_id=cell.plate_id if cell else None,
                grouping_var=sample.grouping_var if sample else None,
                inj_vol=inj_vol_override or sample_cfg.inj_vol,
                file_name=slot.file_name,
                polarity=slot.polarity,
                data_path=data_path,
                method=slot.method,
                container_id=slot.slot.container_id,
            )
        )
    return QueueRowTable(rows=rows)


def format_table(queue_rows: QueueRowTable, output_format: OutputFormat) -> pl.DataFrame:
    """Format queue rows as DataFrame for the given output format."""
    df = queue_rows.to_table()
    df = df.with_columns(
        pl.struct(["tray", "grid_position"])
        .map_elements(
            lambda s: output_format.position_format.format(tray=s["tray"], grid_position=s["grid_position"]),
            return_dtype=pl.Utf8,
        )
        .alias("position")
    )
    df = df.select(
        [
            pl.col(internal).alias(output_name)
            for output_name, internal in output_format.columns.items()
            if internal in df.columns
        ]
    )
    return df


class QueueGenerator:
    """Generates queue CSV from configs and input parameters.

    Uses QGConfiguration (new config bundle) with NO dependencies on old config.py.
    """

    def __init__(self, config: QGConfiguration, queue_input: QueueInput, layout_mode: Literal["vial", "plate"]) -> None:
        self.queue_input = queue_input
        self._layout_mode = layout_mode
        params = queue_input.parameters

        # Resolve pattern
        pattern = config.queue_patterns.get_pattern(params.tech_area, params.queue_pattern)
        if pattern is None:
            raise ValueError(f"No pattern found for tech_area='{params.tech_area}', pattern='{params.queue_pattern}'")
        self.pattern: QueuePattern = pattern

        # Create sampler strategy using new SamplerStrategyV2
        # The strategy internally resolves sampler, plate_layout, and QC samples from config
        self.sampler = SamplerStrategyV2(
            sampler_name=params.sampler,
            layout_mode=layout_mode,
            config=config,
            tech_area=params.tech_area,
            qc_layout_name=self.pattern.qc_layout_name,
            plate_layout_name=params.plate_layout,
        )

        # Transform/validate queue to get PlateQueue
        if isinstance(queue_input, VialQueueInput):
            self.plate_queue: PlateQueue = self.sampler.assign_positions(
                queue_input.queue, one_container_per_tray=params.one_container_per_tray
            )
        else:
            self.plate_queue = self.sampler.assign_positions(queue_input.queue)

        # Store config references
        self.samples_config = config.samples
        self.methods_config = config.methods

        # Resolve data path
        instr = config.instruments.get_instrument(params.tech_area, params.instrument)
        self.data_path = ""
        if instr and instr.path_template:
            first_batch = next(iter(queue_input.queue.batches.values()), None)
            container_id = first_batch.container_id if first_batch else 0
            self.data_path = instr.path_template.format(
                container=container_id,
                user=params.user,
                date=params.date,
            )

        # Resolve output format
        output_format = config.output_formats.get_format(params.output_format)
        if output_format is None:
            raise ValueError(f"Unknown output format: {params.output_format}")
        self.output_format = output_format

    def generate(self) -> pl.DataFrame:
        """Execute pipeline and return formatted DataFrame."""
        rows = self.build_rows()
        return format_table(rows, self.output_format)

    def build_rows(self) -> QueueRowTable:
        """Execute the queue generation pipeline."""
        params = self.queue_input.parameters

        # Apply randomization (within plate/container boundaries)
        plate_queue = randomize_plate_queue(self.plate_queue, params.randomization)

        # Extract groups: (container_id, num_samples) for each container
        samples_per_container: dict[int, int] = {}
        for cell in plate_queue.cells:
            cid = cell.sample.container_id
            samples_per_container[cid] = samples_per_container.get(cid, 0) + 1
        groups = list(samples_per_container.items())

        # Build structure
        slot_entries = build_multi_container_queue_structure(groups, self.pattern, params.qc_frequency_override)

        # Build slots
        slots = _build_slots(slot_entries, plate_queue, self.sampler, self.samples_config, params.tech_area)

        # Expand polarities
        expanded = _expand_polarities(slots, params.polarity)

        # Resolve methods
        expanded = _resolve_methods(expanded, self.methods_config, params.tech_area, params.instrument, params.method)

        # Format file names
        expanded = _format_file_names(expanded, params.date)

        # Build queue rows
        return _build_queue_rows(expanded, self.data_path, params.inj_vol_override)
