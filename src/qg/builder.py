"""Builder for QueueGenerator.

The builder resolves all configurations and creates a fully-configured
QueueGenerator that simply executes the pipeline steps.
"""

from __future__ import annotations

import logging
from pathlib import Path
from typing import TYPE_CHECKING

import polars as pl

from qg.config import ConfigBundle, load_all_configs
from qg.config_models import Sample, QueuePattern, requires_polarity
from qg.generator import MethodResolver
from qg.params_models import QueueInput
from qg.queue_structure import extract_groups
from qg.strategies import create_position_assigner

logger = logging.getLogger(__name__)

if TYPE_CHECKING:
    from qg.generator import QueueGenerator


class QueueGeneratorBuilder:
    """Builds a configured QueueGenerator from parameters.

    The builder resolves all configurations, validates them, and creates
    a QueueGenerator ready to execute the pipeline.
    """

    def __init__(self, configs: ConfigBundle | Path):
        """Initialize with configuration bundle.

        Args:
            configs: ConfigBundle or path to config directory
        """
        if isinstance(configs, Path):
            configs = load_all_configs(configs)
        self.configs = configs
        self._methods_cache: dict[str, pl.DataFrame] = {}

    def build(self, queue_input: QueueInput) -> "QueueGenerator":
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
            params.technology, params.queue_pattern
        )
        if not pattern:
            raise ValueError(
                f"Pattern '{params.queue_pattern}' not found for {params.technology}"
            )

        # Resolve QC layout
        qc_layout = self.configs.qc_layouts.get_layout(
            params.technology, params.sampler
        )
        if not qc_layout:
            raise ValueError(
                f"QC layout not found for {params.technology}.{params.sampler}"
            )

        # Resolve sampler config and create position assigner
        sampler_config = self.configs.samplers.get_sampler(params.sampler)
        position_assigner = create_position_assigner(
            params.sampler, sampler_config, qc_layout
        )

        # Resolve samples config
        samples_config = self._resolve_samples_config(params.technology, pattern)

        # Extract groups from QueueInput
        groups = extract_groups(queue_input)
        primary_container_id = queue_input.get_primary_container_id()

        # Extract group methods: container_id -> {polarity: method_name}
        group_methods = {
            g.container_id: g.method for g in queue_input.sample_groups if g.method
        }

        # Resolve data path (uses primary container)
        data_path = self._resolve_data_path(params, primary_container_id)

        # Create method resolver
        method_resolver = self._create_method_resolver(
            params.technology, params.instrument
        )

        # Resolve polarities
        polarities: list[str | None] = list(params.polarity) if params.polarity else [None]

        # Resolve output format
        output_format = self.configs.output_formats.get_format(params.output_format)
        if not output_format:
            raise ValueError(f"Output format '{params.output_format}' not found")

        # Log resolved configuration
        logger.debug(
            "Building QueueGenerator:\n"
            "  technology=%s, instrument=%s, sampler=%s\n"
            "  pattern=%s (start=%s, middle=%s, end=%s)\n"
            "  position_assigner=%s\n"
            "  qc_layout=%s\n"
            "  samples_config=%s\n"
            "  polarities=%s\n"
            "  data_path=%s\n"
            "  date=%s, groups=%s",
            params.technology,
            params.instrument,
            params.sampler,
            params.queue_pattern,
            pattern.start,
            pattern.middle,
            pattern.end,
            type(position_assigner).__name__,
            list(qc_layout.keys()),
            list(samples_config.keys()),
            polarities,
            data_path,
            params.date,
            groups,
        )

        return QueueGenerator(
            pattern=pattern,
            position_assigner=position_assigner,
            samples_config=samples_config,
            method_resolver=method_resolver,
            polarities=polarities,
            date=params.date,
            groups=groups,
            data_path=data_path,
            group_methods=group_methods,
            fallback_method=params.method,  # Backward compat: use global method as fallback
            inj_vol_override=params.inj_vol_override,
            output_format=output_format,
        )

    def _resolve_samples_config(
        self, technology: str, pattern: QueuePattern
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
            config = self.configs.samples.get_sample(technology, sample_id)
            if config:
                samples[sample_id] = config

        if "default" not in samples:
            raise ValueError(f"No 'default' sample definition for {technology}")

        return samples

    def _resolve_data_path(self, params, container_id: int) -> str:
        """Resolve the data path from instrument config."""
        row = self.configs.instruments_df.filter(
            (pl.col("technology") == params.technology)
            & (pl.col("instrument") == params.instrument)
        )
        if row.is_empty():
            return ""

        template = row["path_template"][0]
        if not template:
            return ""

        return template.format(
            container=container_id,
            user=params.user,
            date=params.date,
        )

    def _create_method_resolver(
        self, technology: str, instrument: str
    ) -> MethodResolver:
        """Create a method resolver function for the given technology/instrument."""
        methods_df = self._load_methods(technology, instrument)

        def resolve_method(
            sample_type: str,
            polarity: str | None,
            method_name: str,
        ) -> str:
            if methods_df is None:
                return ""

            # Filter by sample_type
            matches = methods_df.filter(pl.col("sample_type") == sample_type)

            # Fallback to "default" if no match
            if matches.is_empty() and sample_type != "default":
                matches = methods_df.filter(pl.col("sample_type") == "default")

            if matches.is_empty():
                return ""

            # Filter by polarity column (new approach)
            if polarity and "polarity" in methods_df.columns:
                polarity_matches = matches.filter(pl.col("polarity") == polarity)
                if not polarity_matches.is_empty():
                    matches = polarity_matches

            # Filter by specific method_name if provided
            if method_name:
                name_matches = matches.filter(pl.col("method_name") == method_name)
                if not name_matches.is_empty():
                    matches = name_matches

            if matches.is_empty():
                return ""
            return matches["method_path"][0]

        return resolve_method

    def _load_methods(self, technology: str, instrument: str) -> pl.DataFrame | None:
        """Load methods CSV for a given technology/instrument."""
        cache_key = f"{technology}.{instrument}"
        if cache_key in self._methods_cache:
            return self._methods_cache[cache_key]

        row = self.configs.instruments_df.filter(
            (pl.col("technology") == technology)
            & (pl.col("instrument") == instrument)
        )
        if row.is_empty():
            return None

        methods_file = row["methods_file"][0]
        methods_path = self.configs.config_dir / methods_file
        if not methods_path.exists():
            return None

        methods_df = pl.read_csv(methods_path)
        self._methods_cache[cache_key] = methods_df
        return methods_df
