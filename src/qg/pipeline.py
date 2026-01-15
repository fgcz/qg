"""Pipeline configuration for the queue generator.

The PipelineConfig holds strategy implementations for each pipeline step.
This allows the generator to be parameterized with different behaviors.
"""

from dataclasses import dataclass, field

from qg.strategies import PositionStrategy


@dataclass
class PipelineConfig:
    """Configuration for pipeline step implementations.

    Each field is an optional strategy. If None, the generator uses
    a default implementation based on the queue parameters and configs.

    Attributes:
        position_strategy: Strategy for generating/extracting user positions.
            If None, determined from sampler's position_source config.
    """

    position_strategy: PositionStrategy | None = None

    # Future strategy slots (add as needed):
    # build_structure_strategy: BuildStructureStrategy | None = None
    # assign_positions_strategy: AssignPositionsStrategy | None = None
    # populate_rows_strategy: PopulateRowsStrategy | None = None
    # format_output_strategy: FormatOutputStrategy | None = None
