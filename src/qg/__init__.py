"""Queue generation package for mass spectrometry instruments.

This package provides tools for generating sample queues with QC injections
for XCalibur, Chronos, and Hystar software.
"""

__version__ = "0.1.0"

from qg.models import (
    Sample,
    SamplesConfig,
    Instrument,
    InstrumentsConfig,
    InstrumentPattern,
    InstrumentPatternsConfig,
    Combination,
    CombinationsConfig,
    GridSampler,
    EvosepSampler,
    SamplersConfig,
    QueuePattern,
    QueuePatternsConfig,
    QCLayoutsConfig,
    EvosepPosition,
    OutputFormat,
    OutputFormatsConfig,
)

from qg.config import (
    load_samples,
    load_instruments,
    load_instrument_patterns,
    load_combinations,
    load_samplers,
    load_queue_patterns,
    load_qc_layouts,
    load_output_formats,
    ConfigBundle,
    load_all_configs,
)

from qg.generator import (
    QueueInput,
    QueueParameters,
    InputSample,
    QueueRow,
    QueueGenerator,
    GenerationSummary,
    assign_positions,
)

from qg.structure import (
    build_queue_structure,
    compute_queue_counts,
    compute_middle_block_positions,
    compute_extended_positions,
)

from qg.positions import (
    SamplerPositionGenerator,
    VanquishSampler,
    MClass48Sampler,
    EvosepSampler as EvosepSamplerPosition,
    get_sampler,
)

from qg.generator import (
    build_queue_structure,
    assign_positions,
)

__all__ = [
    # Models
    "Sample",
    "SamplesConfig",
    "Instrument",
    "InstrumentsConfig",
    "InstrumentPattern",
    "InstrumentPatternsConfig",
    "Combination",
    "CombinationsConfig",
    "GridSampler",
    "EvosepSampler",
    "SamplersConfig",
    "QueuePattern",
    "QueuePatternsConfig",
    "QCLayoutsConfig",
    "EvosepPosition",
    "OutputFormat",
    "OutputFormatsConfig",
    # Config loading
    "load_samples",
    "load_instruments",
    "load_instrument_patterns",
    "load_combinations",
    "load_samplers",
    "load_queue_patterns",
    "load_qc_layouts",
    "load_output_formats",
    "ConfigBundle",
    "load_all_configs",
    # Generator
    "QueueInput",
    "QueueParameters",
    "InputSample",
    "QueueRow",
    "QueueGenerator",
    "GenerationSummary",
    "assign_positions",
    # Structure building
    "build_queue_structure",
    "compute_queue_counts",
    "compute_middle_block_positions",
    "compute_extended_positions",
    # Position generators
    "SamplerPositionGenerator",
    "VanquishSampler",
    "MClass48Sampler",
    "EvosepSamplerPosition",
    "get_sampler",
]
