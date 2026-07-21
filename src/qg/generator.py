"""Queue file generator for mass spectrometry instruments."""

from __future__ import annotations

import re
from dataclasses import dataclass
from typing import TYPE_CHECKING, Literal

import polars as pl
from loguru import logger
from pydantic import BaseModel

from qg.config_models.formatting import OutputFormat
from qg.config_models.methods import MethodsConfig
from qg.config_models.positions import PlateLayout, get_grid_position_converter
from qg.config_models.structure import Sample, SamplesConfig
from qg.params_models import PlateCell, PlateQueue, PositionedQueueInput
from qg.qc_layout import create_qc_layout
from qg.qc_positions import Position, QCPositionProvider, create_qc_position_provider
from qg.queue_structure import SlotEntry, build_multi_container_queue_structure
from qg.randomize import randomize_plate_queue
from qg.utils import get_position_function

if TYPE_CHECKING:
    from qg.config_models.loader import QGConfiguration

# Suffix appended to the basename of the last file of each container subqueue
# when mark_end_of_queue is enabled. The instrument turns "..._eoq" into
# "..._eoq.raw" / "..._eoq.d", giving downstream QC an end-of-queue signal.
EOQ_SUFFIX = "_eoq"


class QueueRow(BaseModel):
    """A single row in the generated queue.

    Two independent classification axes coexist:

    * ``slot_kind`` (``Literal["user", "qc"]``) — position-builder role used
      during queue construction (whether this slot holds a user sample or a
      QC sample).
    * ``sample_type`` (``Literal["Unknown", "Blank", "QC", "Std Bracket"]``) —
      sample category from ``samples.csv`` (the Xcalibur-accepted "Sample Type"
      values). User-sample slots are always ``"Unknown"``; QC slots inherit
      their template's category.
    * ``qc_class`` (``str | None``) — optional finer display category from
      ``samples.csv`` (e.g. ``"Pooled QC"``, ``"QC dilution series"``). Used only
      by visualizations to colour QC injections by type; ``None`` falls back to
      ``sample_type``. Never written to the instrument queue.
    """

    run_number: int
    slot_kind: Literal["user", "qc"]
    sample_id: str
    sample_name: str
    sample_type: Literal["Unknown", "Blank", "QC", "Std Bracket"] = "Unknown"
    qc_class: str | None = None
    level: int | None = None
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
        return pl.DataFrame([row.model_dump() for row in self.rows], infer_schema_length=None)


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
    method_name: str = ""
    file_name: str = ""


def _build_slots(
    slot_entries: list[SlotEntry],
    plate_queue: PlateQueue,
    qc_provider: QCPositionProvider,
    samples_config: SamplesConfig,
    tech_area: str,
    default_sample_id: str,
    plate_layout: PlateLayout,
) -> list[SlotInfo]:
    """Build slots from SlotEntry list and PlateQueue.

    User-cell row/column is derived from ``grid_position`` via ``plate_layout``.
    """
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
            row, col = plate_layout.split_alpha(user_cell.grid_position)
            position = Position(
                tray=plate_queue.plates[user_cell.plate_id].tray,
                grid_position=user_cell.grid_position,
                row=row,
                col=col,
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
        method_name = method.get(polarity, "")
        slot.method_name = method_name
        slot.method = methods_config.get_method_path(tech_area, instrument, sample_id, polarity, method_name)
    return slots


def _format_file_names(
    slots: list[ExpandedSlot],
    date: str,
    level_concentrations: dict[int, str] | None = None,
    mark_eoq: bool = True,
) -> list[ExpandedSlot]:
    """Format file_name for each slot.

    Runs of consecutive underscores in the rendered name are collapsed to a
    single underscore so that an empty placeholder (e.g. ``{concentration}``
    on a level that has no concentration assigned yet) does not surface as
    ``..__..`` in the filename.

    When ``mark_eoq`` is set, the last file of each container subqueue gets an
    ``EOQ_SUFFIX`` appended. The marker is keyed on ``SlotInfo.idx`` (not
    ``run_number``, which is assigned per polarity) so that both polarities of
    the final injection are marked.
    """
    level_concentrations = level_concentrations or {}
    for slot in slots:
        cell = slot.slot.user_cell
        sample = cell.sample if cell else None
        level = slot.slot.sample_config.level
        rendered = slot.slot.sample_config.file_name_template.format(
            date=date,
            run=f"{slot.run_number:03d}",
            container=slot.slot.container_id,
            sample_id=str(sample.sample_id) if sample else "",
            sample_name=sample.sample_name if sample else slot.slot.sample_config.sample_name,
            polarity=slot.polarity,
            method_name=slot.method_name.lower(),
            level=level if level is not None else "",
            concentration=level_concentrations.get(level, "") if level is not None else "",
        )
        slot.file_name = re.sub(r"_+", "_", rendered).strip("_")

    if mark_eoq:
        last_idx: dict[int, int] = {}
        for s in slots:
            cid = s.slot.container_id
            last_idx[cid] = max(last_idx.get(cid, -1), s.slot.idx)
        for s in slots:
            if s.slot.idx == last_idx[s.slot.container_id]:
                s.file_name += EOQ_SUFFIX
    return slots


def _build_queue_rows(
    slots: list[ExpandedSlot],
    path_template: str,
    user: str,
    date: str,
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

        data_path = (
            path_template.format(container=slot.slot.container_id, user=user, date=date) if path_template else ""
        )

        rows.append(
            QueueRow(
                run_number=slot.run_number,
                slot_kind="user" if slot.slot.sample_id == default_sample_id else "qc",
                sample_id=str(sample.sample_id) if sample else slot.slot.sample_id,
                sample_name=sample.sample_name if sample else sample_cfg.sample_name,
                sample_type=sample_cfg.sample_type,
                qc_class=sample_cfg.qc_class,
                level=sample_cfg.level,
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


def format_table(
    queue_rows: QueueRowTable,
    output_format: OutputFormat,
    plate_layout: PlateLayout,
    tech_area: str,
) -> pl.DataFrame:
    """Format queue rows as DataFrame for the given output format.

    `tech_area` selects the technology-specific column overlay (if any) from
    `output_format.columns_by_tech`; otherwise the base `output_format.columns`
    is used.
    """
    columns = output_format.columns_for(tech_area)
    df = queue_rows.to_table()

    if df.is_empty():
        return pl.DataFrame({col: pl.Series([], dtype=pl.Utf8) for col in columns})

    # Apply grid_position conversion (e.g., alpha→flat for Chronos)
    converter = get_grid_position_converter(output_format.grid_position_conversion, plate_layout)
    df = df.with_columns(
        pl.col("grid_position")
        .map_elements(converter.convert_grid_position, return_dtype=pl.Utf8)
        .alias("grid_position")
    )

    # Apply tray format (e.g., 1 → "EvoSlot 1" for Chronos)
    if output_format.tray_format:
        tray_fmt = output_format.tray_format
        df = df.with_columns(
            pl.col("tray").map_elements(lambda t: tray_fmt.format(tray=t), return_dtype=pl.Utf8).alias("tray")
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
    for output_name, value in columns.items():
        if value.startswith(LITERAL_PREFIX):
            select_exprs.append(pl.lit(value[len(LITERAL_PREFIX) :]).alias(output_name))
        elif value in df.columns:
            select_exprs.append(pl.col(value).alias(output_name))
    df = df.select(select_exprs)
    return df


class QueueGenerator:
    """Generate an instrument queue from injected config and positioned samples."""

    def __init__(self, config: QGConfiguration, queue_input: PositionedQueueInput) -> None:
        if not isinstance(queue_input, PositionedQueueInput):
            raise TypeError("QueueGenerator requires a PositionedQueueInput")

        self.queue_input = queue_input
        params = queue_input.parameters

        self.pattern = config.queue_patterns.get_pattern(params.tech_area, params.queue_pattern)
        self._plate_layout = config.plate_layouts.get_layout(params.plate_layout)
        sampler = config.samplers.get_sampler(params.sampler)
        self._qc_layout = create_qc_layout(
            config=config,
            tech_area=params.tech_area,
            pattern_sample_ids=self.pattern.get_all_sample_ids(),
            qc_layout_name=params.qc_layout_name,
            plate_layout_name=params.plate_layout,
            sampler_name=params.sampler,
            position_fun=get_position_function(sampler.position_fun),
            plate_layout=self._plate_layout,
        )
        self.plate_queue: PlateQueue = queue_input.queue

        # Store config references
        self.samples_config = config.samples
        self.methods_config = config.methods

        # Store path template for per-row formatting (container varies per slot)
        instr = config.instruments.get_instrument(params.tech_area, params.instrument)
        self._path_template = instr.path_template
        self._user = params.user
        self._date = params.date

        # Output format existence validated in QueueParameters.create()
        self.output_format = config.output_formats.get_format(params.output_format)
        self.tech_area = params.tech_area

    def generate(self) -> pl.DataFrame:
        """Execute pipeline and return formatted DataFrame."""
        rows = self.build_rows()
        return format_table(rows, self.output_format, self._plate_layout, self.tech_area)

    def write(self) -> str:
        """Generate queue and return as string in the appropriate format."""
        df = self.generate()
        return write_queue(df, self.output_format)

    @property
    def file_extension(self) -> str:
        """Return the appropriate file extension for the output format."""
        return self.output_format.file_extension

    @property
    def plate_layout(self) -> PlateLayout:
        """Return the resolved plate layout used for formatting the queue."""
        return self._plate_layout

    def build_rows(self) -> QueueRowTable:
        """Execute the queue generation pipeline."""
        params = self.queue_input.parameters
        default_sample_id = self.samples_config.DEFAULT_SAMPLE_ID
        logger.info(
            "Building queue | instrument={} | sampler={} | samples={}",
            params.instrument,
            params.sampler,
            len(self.plate_queue.cells),
        )

        # Apply randomization within plate/container boundaries. randomize_plate_queue
        # short-circuits "no" and owns RNG construction; params.seed is always concrete.
        plate_queue = randomize_plate_queue(self.plate_queue, params.randomization, params.seed)

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
            slot_entries,
            plate_queue,
            qc_provider,
            self.samples_config,
            params.tech_area,
            default_sample_id,
            self._plate_layout,
        )

        # Expand polarities
        expanded = _expand_polarities(slots, params.polarity)

        # Resolve methods
        expanded = _resolve_methods(
            expanded, self.methods_config, params.tech_area, params.instrument, params.method, default_sample_id
        )

        # Format file names
        expanded = _format_file_names(expanded, params.date, params.level_concentrations, params.mark_end_of_queue)

        # Build queue rows
        result = _build_queue_rows(
            expanded, self._path_template, self._user, self._date, params.inj_vol_override, default_sample_id
        )
        logger.info("Queue built | rows={} | format={}", len(result.rows), params.output_format)

        return result
