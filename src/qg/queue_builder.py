"""Builder for QueueInput with validation."""

from __future__ import annotations

from typing import TYPE_CHECKING, Self

import polars as pl

from qg.params_models import InputSample, QueueInput, QueueParameters, SampleGroup

if TYPE_CHECKING:
    from qg.config import ConfigBundle


class QueueBuilder:
    """Fluent builder for QueueInput. Single-use: create new builder for each queue."""

    def __init__(self, configs: ConfigBundle) -> None:
        self.configs = configs
        self._parameters: QueueParameters | None = None
        self._sample_groups: list[SampleGroup] = []
        self._built = False

    def with_parameters(self, parameters: QueueParameters) -> Self:
        """Set queue parameters."""
        if self._built:
            raise RuntimeError("Builder already used. Create a new QueueBuilder.")
        self._parameters = parameters
        return self

    def add_samples_from_dataframe(self, df: pl.DataFrame, container_ids: list[int]) -> Self:
        """Add sample groups from DataFrame with container_id column.

        Args:
            df: DataFrame with samples. Must have a 'container_id' column.
            container_ids: List of container IDs to extract groups for.

        Returns:
            Self for method chaining.

        Raises:
            RuntimeError: If builder has already been used.
            ValueError: If DataFrame doesn't have 'container_id' column.
        """
        if self._built:
            raise RuntimeError("Builder already used. Create a new QueueBuilder.")
        if "container_id" not in df.columns:
            raise ValueError("DataFrame must have 'container_id' column")

        for container_id in container_ids:
            container_samples = df.filter(pl.col("container_id") == container_id).drop("container_id")
            if not container_samples.is_empty():
                samples = [InputSample(**row) for row in container_samples.to_dicts()]
                self._sample_groups.append(SampleGroup(container_id=container_id, samples=samples))
        return self

    def build(self) -> QueueInput:
        """Create QueueInput. Raises if incomplete or invalid.

        Returns:
            Validated QueueInput instance.

        Raises:
            RuntimeError: If builder has already been used.
            ValueError: If parameters not set, no samples added, or validation fails.
        """
        if self._built:
            raise RuntimeError("Builder already used. Create a new QueueBuilder.")
        if self._parameters is None:
            raise ValueError("Parameters not set. Call with_parameters() first.")
        if not self._sample_groups:
            raise ValueError("No samples added. Call add_samples_from_dataframe() first.")

        # Cross-validation: blocked randomization needs grouping_var
        if self._parameters.randomization == "blocked":
            all_samples = [s for g in self._sample_groups for s in g.samples]
            has_grouping = any(s.grouping_var is not None for s in all_samples)
            if not has_grouping:
                raise ValueError(
                    "randomization='blocked' requires samples with grouping_var set. Use 'random' for simple shuffle."
                )

        self._built = True
        return QueueInput(parameters=self._parameters, sample_groups=self._sample_groups)
