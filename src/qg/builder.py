"""Builder for QueueGenerator.

The builder resolves all configurations and creates a fully-configured
QueueGenerator that simply executes the pipeline steps.
"""

from __future__ import annotations

import logging

from qg.config import ConfigBundle
from qg.config_models import QCLayoutPattern
from qg.generator import QueueGenerator
from qg.params_models import QueueInput
from qg.positions import create_sampler

logger = logging.getLogger(__name__)


class QueueGeneratorBuilder:
    """Builds a configured QueueGenerator from parameters.

    The builder resolves all configurations, validates them, and creates
    a QueueGenerator ready to execute the pipeline.
    """

    def __init__(self, configs: ConfigBundle):
        """Initialize with configuration bundle.

        Args:
            configs: ConfigBundle or path to config directory
        """
        self.configs = configs

    def build(self, queue_input: QueueInput) -> QueueGenerator:
        """Build a QueueGenerator for the given input.

        Args:
            queue_input: Complete queue input with parameters and samples/groups

        Returns:
            Configured QueueGenerator ready to generate queues

        Raises:
            ValueError: If configuration is invalid
        """

        params = queue_input.parameters

        # Resolve pattern (validated by QueueParameters.create())
        pattern = self.configs.queue_patterns.get_pattern(params.tech_area, params.queue_pattern)

        # Apply QC frequency override if specified
        if params.qc_frequency_override is not None:
            pattern = pattern.model_copy(update={"run_QC_after_n_samples": params.qc_frequency_override})

        # Resolve QC layout (validated by QueueParameters.create())
        qc_layout = self.configs.qc_layouts.get_layout(params.tech_area, params.sampler)

        # Create validated QC layout pattern (validates uniqueness)
        qc_layout_pattern = QCLayoutPattern.create(pattern, qc_layout)

        # Create sampler with validated layout
        sampler = create_sampler(params.sampler, self.configs.samplers, qc_layout_pattern)

        # Resolve data path from instrument config
        instr = self.configs.instruments.get_instrument(params.tech_area, params.instrument)
        data_path = ""
        if instr and instr.path_template:
            data_path = instr.path_template.format(
                container=queue_input.get_primary_container_id(),
                user=params.user,
                date=params.date,
            )

        # Resolve output format (validated by QueueParameters.create())
        output_format = self.configs.output_formats.get_format(params.output_format)

        return QueueGenerator(
            queue_input=queue_input,
            pattern=pattern,
            sampler=sampler,
            samples_config=self.configs.samples,
            methods_config=self.configs.methods,
            data_path=data_path,
            output_format=output_format,
        )
