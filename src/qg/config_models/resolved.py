"""Self-contained configuration snapshot embedded in exported queue parameters.

A :class:`ResolvedConfig` captures the *minimal* configuration fragments a single
queue run resolved against — the one queue pattern, instrument, sampler, plate
layout, QC layout, and output format, plus the QC/standard sample definitions and
the instrument's methods. Inlining these values (rather than a checksum) keeps an
exported ``params.json`` human-readable and, crucially, lets the queue regenerate
byte-for-byte from the file alone — independent of the live ``qg_configs/`` tree
and immune to later edits of it.

The captured set is exactly the transitive closure that ``QueueGenerator`` reads
(see ``generator.py`` and ``positionV2.create_assembled_sampler`` /
``qc_layout.create_qc_layout``): samplers, plate layouts, QC layouts, samples,
methods, instruments, and output formats. The UI/validation-only configs
(``sampler_plate_layouts``, ``instrument_configs``, ``tech_area_defaults``) are
not read during generation and are reconstructed empty.
"""

from __future__ import annotations

from typing import TYPE_CHECKING

from pydantic import BaseModel, Field

from .formatting import Instrument, InstrumentsConfig, OutputFormat, OutputFormatsConfig
from .methods import Method, MethodsConfig, MethodsForInstrument
from .positions import (
    PlateLayout,
    PlateLayoutsConfig,
    QCLayoutsTipConfig,
    QCLayoutsWellConfig,
    QCSampleTip,
    QCSampleWell,
    Sampler,
    SamplerPlateLayoutsConfig,
    SamplersConfig,
)
from .structure import QueuePattern, QueuePatternsConfig, Sample, SamplesConfig
from .ui import InstrumentConfigsConfig, TechAreaDefaultsConfig

if TYPE_CHECKING:
    from qg.params_models import QueueParameters

    from .loader import QGConfiguration


class ResolvedConfig(BaseModel):
    """The minimal, inlined configuration a single run used.

    Enough to regenerate that exact queue without the external ``qg_configs/``.
    Built with :meth:`from_configuration` and re-expanded with
    :meth:`to_configuration`.
    """

    tech_area: str
    instrument: Instrument
    output_format_name: str
    output_format: OutputFormat
    plate_layout: PlateLayout
    sampler: Sampler
    queue_pattern_name: str
    queue_pattern: QueuePattern
    qc_layout_name: str
    # QC/standard sample definitions referenced by the pattern and QC layout, plus
    # the tech_area's mandatory ``default`` sample.
    samples: list[Sample]
    # Method rows for the (tech_area, instrument) pair used by this run.
    methods: list[Method]
    # Exactly one of these is populated, per the sampler type (well vs tip).
    qc_samples_well: list[QCSampleWell] = Field(default_factory=list)
    qc_samples_tip: list[QCSampleTip] = Field(default_factory=list)

    @classmethod
    def from_configuration(cls, config: QGConfiguration, params: QueueParameters) -> ResolvedConfig:
        """Extract the minimal config fragments ``params`` resolves against."""
        tech = params.tech_area
        instrument = config.instruments.get_instrument(tech, params.instrument)
        output_format = config.output_formats.get_format(params.output_format)
        plate_layout = config.plate_layouts.get_layout(params.plate_layout)
        sampler = config.samplers.get_sampler(params.sampler)
        pattern = config.queue_patterns.get_pattern(tech, params.queue_pattern)
        instr_methods = config.methods.get_methods(tech, params.instrument)

        if sampler.is_tip:
            qc_well: list[QCSampleWell] = []
            qc_tip = config.qc_layouts_tip.get_samples(tech, params.qc_layout_name, params.plate_layout)
            qc_ids = {s.sample_id for s in qc_tip}
        else:
            qc_tip = []
            qc_well = config.qc_layouts_well.get_samples(tech, params.qc_layout_name, params.plate_layout)
            qc_ids = {s.sample_id for s in qc_well if s.sample_id is not None}

        # build_rows resolves user samples through the tech's ``default`` sample and
        # each QC/standard injection by its sample_id; capture exactly those.
        needed_ids = {SamplesConfig.DEFAULT_SAMPLE_ID} | pattern.get_all_sample_ids() | qc_ids
        samples = [config.samples.get_sample(tech, sid) for sid in sorted(needed_ids)]

        return cls(
            tech_area=tech,
            instrument=instrument,
            output_format_name=params.output_format,
            output_format=output_format,
            plate_layout=plate_layout,
            sampler=sampler,
            queue_pattern_name=params.queue_pattern,
            queue_pattern=pattern,
            qc_layout_name=params.qc_layout_name,
            samples=samples,
            methods=list(instr_methods.methods),
            qc_samples_well=qc_well,
            qc_samples_tip=qc_tip,
        )

    def to_configuration(self) -> QGConfiguration:
        """Re-expand into a single-entry ``QGConfiguration`` usable by ``QueueGenerator``.

        Constructed directly (not via ``QGConfiguration.create``): the snapshot was
        already validated when it was extracted, so cross-validation is unnecessary
        and would needlessly require the UI-only configs.
        """
        from .loader import QGConfiguration

        methods = MethodsConfig()
        methods.add_instrument_methods(
            self.tech_area,
            self.instrument.instrument,
            MethodsForInstrument(
                config_path=MethodsConfig.config_folder / self.tech_area / f"{self.instrument.instrument}_methods.csv",
                methods=list(self.methods),
            ),
        )

        return QGConfiguration(
            instruments=InstrumentsConfig(instruments=[self.instrument]),
            output_formats=OutputFormatsConfig(formats={self.output_format_name: self.output_format}),
            plate_layouts=PlateLayoutsConfig(layouts={self.plate_layout.name: self.plate_layout}),
            samplers=SamplersConfig(samplers={self.sampler.name: self.sampler}),
            sampler_plate_layouts=SamplerPlateLayoutsConfig(mappings=[]),
            qc_layouts_well=QCLayoutsWellConfig(samples=list(self.qc_samples_well)),
            qc_layouts_tip=QCLayoutsTipConfig(samples=list(self.qc_samples_tip)),
            samples=SamplesConfig(samples=list(self.samples)),
            queue_patterns=QueuePatternsConfig(
                patterns={self.tech_area: {self.queue_pattern_name: self.queue_pattern}}
            ),
            methods=methods,
            instrument_configs=InstrumentConfigsConfig(configs=[]),
            tech_area_defaults=TechAreaDefaultsConfig(defaults=[]),
        )

    def differs_from(self, config: QGConfiguration, params: QueueParameters) -> bool:
        """True if extracting this run's config from ``config`` now would differ.

        Used to warn about configuration drift when both an embedded snapshot and a
        live ``qg_configs/`` are available.
        """
        try:
            live = ResolvedConfig.from_configuration(config, params)
        except KeyError:
            return True
        return live.model_dump() != self.model_dump()
