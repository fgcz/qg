"""Queue file generator for mass spectrometry instruments."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Literal

import polars as pl
from pydantic import BaseModel

from qg.config_models.formatting import OutputFormat, Sample, SamplesConfig
from qg.config_models.loader import QGConfiguration
from qg.config_models.methods import MethodsConfig
from qg.config_models.positions import PlateLayout, get_grid_position_converter
from qg.params_models import PlateCell, PlateQueue, PlateQueueInput, QueueInput, VialQueueInput
from qg.positionV2 import create_assembled_sampler
from qg.qc_positions import Position, QCPositionProvider, create_qc_position_provider
from qg.queue_structure import SlotEntry, build_multi_container_queue_structure
from qg.randomize import randomize_plate_queue
from qg.utils import LayoutMode


class QueueRow(BaseModel):
    """A single row in the generated queue."""

    run_number: int
    sample_type: Literal["user", "qc"]
    sample_id: str
    sample_name: str
    tray: str | int
    grid_position: str
    row: str = ""
    col: int = 0
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
    sample_id: str  # DEFAULT_SAMPLE_ID for user samples, qc_id for QC
    position: Position
    sample_config: Sample
    user_cell: PlateCell | None  # Only for DEFAULT_SAMPLE_ID slots
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
    qc_provider: QCPositionProvider,
    samples_config: SamplesConfig,
    tech_area: str,
    default_sample_id: str,
) -> list[SlotInfo]:
    """Build slots from SlotEntry list and PlateQueue."""
    slots: list[SlotInfo] = []
    cell_iter = iter(plate_queue.cells)

    for idx, entry in enumerate(slot_entries):
        sample_cfg = samples_config.get_sample(tech_area, entry.sample_id)
        if not sample_cfg:
            continue

        user_cell: PlateCell | None = None
        if entry.sample_id == default_sample_id:
            user_cell = next(cell_iter, None)
            if not user_cell:
                continue
            position = Position(
                tray=plate_queue.plates[user_cell.plate_id].tray,
                grid_position=user_cell.grid_position,
                row=user_cell.row,
                col=user_cell.col,
            )
        else:
            position = qc_provider.get_position(entry.sample_id)

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
    default_sample_id: str,
) -> list[ExpandedSlot]:
    """Resolve method path for each slot."""
    for slot in slots:
        sample_id = slot.slot.sample_id
        polarity = slot.polarity
        method_name = method.get(polarity, "") if sample_id == default_sample_id else ""
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


def _build_queue_rows(
    slots: list[ExpandedSlot],
    data_path: str,
    inj_vol_override: float | None,
    default_sample_id: str,
) -> QueueRowTable:
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
                sample_type="user" if slot.slot.sample_id == default_sample_id else "qc",
                sample_id=str(sample.sample_id) if sample else slot.slot.sample_id,
                sample_name=sample.sample_name if sample else sample_cfg.sample_name,
                tray=pos.tray,
                grid_position=pos.grid_position,
                row=pos.row,
                col=pos.col,
                plate_id=cell.plate_id if cell else None,
                grouping_var=sample.grouping_var if sample else None,
                inj_vol=(inj_vol_override if slot.slot.sample_id == default_sample_id else None) or sample_cfg.inj_vol,
                file_name=slot.file_name,
                polarity=slot.polarity,
                data_path=data_path,
                method=slot.method,
                container_id=slot.slot.container_id,
            )
        )
    return QueueRowTable(rows=rows)


def write_queue(df: pl.DataFrame, output_format: OutputFormat) -> str:
    """Write queue DataFrame to string using the format's configured writer.

    Args:
        df: Formatted queue DataFrame
        output_format: OutputFormat with writer configuration

    Returns:
        String content (CSV or XML)
    """
    from qg.writers import get_writer

    writer = get_writer(output_format.writer)
    return writer(df)


def format_table(queue_rows: QueueRowTable, output_format: OutputFormat, plate_layout: PlateLayout) -> pl.DataFrame:
    """Format queue rows as DataFrame for the given output format."""
    df = queue_rows.to_table()

    # Apply grid_position conversion (e.g., alpha→flat for Chronos)
    converter = get_grid_position_converter(output_format.grid_position_conversion, plate_layout)
    df = df.with_columns(
        pl.col("grid_position")
        .map_elements(converter.convert_grid_position, return_dtype=pl.Utf8)
        .alias("grid_position")
    )

    gp_fmt = output_format.grid_position_format
    pos_fmt = output_format.position_format
    df = df.with_columns(
        pl.struct(["tray", "grid_position", "row", "col"])
        .map_elements(
            lambda s: pos_fmt.format(
                tray=s["tray"],
                grid_position=gp_fmt.format(row=s["row"], col=s["col"], grid_position=s["grid_position"]),
            ),
            return_dtype=pl.Utf8,
        )
        .alias("position")
    )
    LITERAL_PREFIX = "literal:"
    select_exprs = []
    for output_name, value in output_format.columns.items():
        if value.startswith(LITERAL_PREFIX):
            select_exprs.append(pl.lit(value[len(LITERAL_PREFIX) :]).alias(output_name))
        elif value in df.columns:
            select_exprs.append(pl.col(value).alias(output_name))
    df = df.select(select_exprs)
    return df


class QueueGenerator:
    """Generates queue CSV from configs and input parameters.

    Uses QGConfiguration (new config bundle) with NO dependencies on old config.py.
    """

    def __init__(self, config: QGConfiguration, queue_input: QueueInput) -> None:
        self.queue_input = queue_input
        self._config = config
        params = queue_input.parameters
        layout_mode = LayoutMode.PLATE if isinstance(queue_input, PlateQueueInput) else LayoutMode.VIAL

        # Pattern existence validated in QueueParameters.create()
        self.pattern = config.queue_patterns.get_pattern(params.tech_area, params.queue_pattern)

        # Create assembled sampler for position assignment
        assembled_sampler = create_assembled_sampler(
            sampler_name=params.sampler,
            layout_mode=layout_mode,
            config=config,
            tech_area=params.tech_area,
            pattern=self.pattern,
            plate_layout_name=params.plate_layout,
        )

        # Store QC layout and plate layout from assembled sampler (avoid recomputation)
        self._qc_layout = assembled_sampler.qc_layout
        self._plate_layout = config.plate_layouts.get_layout(params.plate_layout)

        # Transform/validate queue to get PlateQueue
        if isinstance(queue_input, VialQueueInput):
            self.plate_queue: PlateQueue = assembled_sampler.assign(
                queue_input.queue, one_container_per_tray=params.one_container_per_tray
            )
        else:
            self.plate_queue = assembled_sampler.assign(queue_input.queue)

        # Store config references
        self.samples_config = config.samples
        self.methods_config = config.methods

        # Resolve data path (instrument validated in QueueParameters.create())
        instr = config.instruments.get_instrument(params.tech_area, params.instrument)
        self.data_path = ""
        if instr.path_template:
            first_batch = next(iter(queue_input.queue.batches.values()), None)
            container_id = first_batch.container_id if first_batch else 0
            self.data_path = instr.path_template.format(
                container=container_id,
                user=params.user,
                date=params.date,
            )

        # Output format existence validated in QueueParameters.create()
        self.output_format = config.output_formats.get_format(params.output_format)

    def generate(self) -> pl.DataFrame:
        """Execute pipeline and return formatted DataFrame."""
        rows = self.build_rows()
        return format_table(rows, self.output_format, self._plate_layout)

    def write(self) -> str:
        """Generate queue and return as string in the appropriate format."""
        df = self.generate()
        return write_queue(df, self.output_format)

    @property
    def file_extension(self) -> str:
        """Return the appropriate file extension for the output format."""
        return self.output_format.file_extension

    def build_rows(self) -> QueueRowTable:
        """Execute the queue generation pipeline."""
        params = self.queue_input.parameters
        default_sample_id = self.samples_config.DEFAULT_SAMPLE_ID

        # Apply randomization (within plate/container boundaries)
        plate_queue = randomize_plate_queue(self.plate_queue, params.randomization)

        # Extract groups: (container_id, num_samples) for each container
        samples_per_container: dict[int, int] = {}
        for cell in plate_queue.cells:
            cid = cell.sample.container_id
            samples_per_container[cid] = samples_per_container.get(cid, 0) + 1
        groups = list(samples_per_container.items())

        # Build structure
        slot_entries = build_multi_container_queue_structure(
            groups, self.pattern, default_sample_id, params.qc_frequency_override
        )

        # Create QC position provider (reuses qc_layout from assembled sampler)
        qc_provider = create_qc_position_provider(
            qc_layout=self._qc_layout,
            slot_entries=slot_entries,
            default_sample_id=default_sample_id,
            plate_layout=self._plate_layout,
        )

        # Build slots
        slots = _build_slots(
            slot_entries, plate_queue, qc_provider, self.samples_config, params.tech_area, default_sample_id
        )

        # Expand polarities
        expanded = _expand_polarities(slots, params.polarity)

        # Resolve methods
        expanded = _resolve_methods(
            expanded, self.methods_config, params.tech_area, params.instrument, params.method, default_sample_id
        )

        # Format file names
        expanded = _format_file_names(expanded, params.date)

        # Build queue rows
        return _build_queue_rows(expanded, self.data_path, params.inj_vol_override, default_sample_id)
