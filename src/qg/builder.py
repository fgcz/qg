"""Builder for QueueGenerator.

The builder resolves all configurations and creates a fully-configured
QueueGenerator that simply executes the pipeline steps.
"""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING

from qg.config import ConfigBundle
from qg.config_models import QueuePattern, Sample
from qg.params_models import QueueInput
from qg.positions import QCLayoutPattern, create_sampler
from qg.queue_structure import _extract_groups

logger = logging.getLogger(__name__)

if TYPE_CHECKING:
    from qg.generator import QueueGenerator


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
        from qg.generator import QueueGenerator

        params = queue_input.parameters

        # Resolve pattern
        pattern = self.configs.queue_patterns.get_pattern(
            params.tech_area, params.queue_pattern
        )
        if not pattern:
            raise ValueError(
                f"Pattern '{params.queue_pattern}' not found for {params.tech_area}"
            )

        # Apply QC frequency override if specified
        if params.qc_frequency_override is not None:
            pattern = pattern.model_copy(
                update={"run_QC_after_n_samples": params.qc_frequency_override}
            )

        # Resolve QC layout
        qc_layout = self.configs.qc_layouts.get_layout(
            params.tech_area, params.sampler
        )
        if not qc_layout:
            raise ValueError(
                f"QC layout not found for {params.tech_area}.{params.sampler}"
            )

        # Create validated QC layout pattern (validates uniqueness)
        qc_layout_pattern = QCLayoutPattern.create(pattern, qc_layout)

        # Create sampler with validated layout
        sampler = create_sampler(
            params.sampler, self.configs.samplers, qc_layout_pattern
        )

        # Resolve samples config
        samples_config = self._resolve_samples_config(params.tech_area, pattern)

        # Extract groups from QueueInput
        groups = _extract_groups(queue_input)
        primary_container_id = queue_input.get_primary_container_id()

        # Resolve data path (uses primary container)
        data_path = self._resolve_data_path(params, primary_container_id)

        # Resolve polarities (default to empty string for proteomics)
        polarities: list[str] = list(params.polarity) if params.polarity else [""]

        # Resolve output format
        output_format = self.configs.output_formats.get_format(params.output_format)
        if not output_format:
            raise ValueError(f"Output format '{params.output_format}' not found")

        # Log resolved configuration
        logger.debug(
            "Building QueueGenerator:\n"
            "  tech_area=%s, instrument=%s, sampler=%s\n"
            "  pattern=%s (start=%s, middle=%s, end=%s)\n"
            "  sampler=%s\n"
            "  qc_positions=%s\n"
            "  samples_config=%s\n"
            "  polarities=%s\n"
            "  data_path=%s\n"
            "  date=%s, groups=%s",
            params.tech_area,
            params.instrument,
            params.sampler,
            params.queue_pattern,
            pattern.start,
            pattern.middle,
            pattern.end,
            type(sampler).__name__,
            list(qc_layout_pattern.positions.keys()),
            list(samples_config.keys()),
            polarities,
            data_path,
            params.date,
            groups,
        )

        return QueueGenerator(
            pattern=pattern,
            sampler=sampler,
            samples_config=samples_config,
            methods_config=self.configs.methods,
            tech_area=params.tech_area,
            instrument=params.instrument,
            polarities=polarities,
            date=params.date,
            groups=groups,
            data_path=data_path,
            method=params.method,
            inj_vol_override=params.inj_vol_override,
            output_format=output_format,
        )

    def _resolve_samples_config(
        self, tech_area: str, pattern: QueuePattern
    ) -> dict[str, Sample]:
        """Resolve all sample configs needed for the pattern."""
        samples: dict[str, Sample] = {}

        # Collect all sample_ids from pattern
        sample_ids = set()
        sample_ids.update(pattern.start)
        sample_ids.update(pattern.middle)
        sample_ids.update(pattern.end)
        sample_ids.add("default")  # Always need default

        for sample_id in sample_ids:
            config = self.configs.samples.get_sample(tech_area, sample_id)
            if config:
                samples[sample_id] = config

        if "default" not in samples:
            raise ValueError(f"No 'default' sample definition for {tech_area}")

        return samples

    def _resolve_data_path(self, params, container_id: int) -> str:
        """Resolve the data path from instrument config."""
        instr = self.configs.instruments.get_instrument(params.tech_area, params.instrument)
        if not instr or not instr.path_template:
            return ""

        return instr.path_template.format(
            container=container_id,
            user=params.user,
            date=params.date,
        )
