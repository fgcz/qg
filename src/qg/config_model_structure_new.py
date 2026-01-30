# =============================================================================
# Queue Pattern Models
# =============================================================================

from typing import Self

from pydantic import BaseModel, Field


class QueuePattern(BaseModel):
    """A queue pattern defining QC injection sequences."""

    description: str = Field(..., description="Human-readable description")
    qc_layout_name: str = Field(..., description="Reference to qc_layout in qc_layouts_*.csv")
    run_QC_after_n_samples: int = Field(..., gt=0, description="Insert QC block after this many samples")
    start: list[str] = Field(default_factory=list, description="QC samples at queue start")
    middle: list[str] = Field(default_factory=list, description="QC samples between sample batches")
    end: list[str] = Field(default_factory=list, description="QC samples at queue end")
    separation: list[str] | None = Field(default=None, description="QC samples between project groups")
    middle_extended: list[str] | None = Field(default=None, description="Extended middle pattern (dilution series)")
    middle_extended_frequency_multiplier: int | None = Field(
        default=None, description="Run middle_extended every N middle blocks"
    )

    @property
    def effective_separation(self) -> list[str]:
        """Get separation block, falling back to middle if not defined."""
        return self.separation if self.separation is not None else self.middle

    def get_all_sample_ids(self) -> set[str]:
        """Get all unique sample IDs referenced by this pattern."""
        sample_ids: set[str] = set()
        sample_ids.update(self.start)
        sample_ids.update(self.middle)
        sample_ids.update(self.end)
        if self.separation:
            sample_ids.update(self.separation)
        if self.middle_extended:
            sample_ids.update(self.middle_extended)
        return sample_ids


class QueuePatternsConfig(BaseModel):
    """All queue patterns by tech_area.

    Structure: tech_area -> pattern_name -> QueuePattern
    """

    patterns: dict[str, dict[str, QueuePattern]] = Field(default_factory=dict)

    def get_technologies(self) -> list[str]:
        """Get list of all technologies with patterns defined."""
        return list(self.patterns.keys())

    def get_pattern(self, tech_area: str, pattern_name: str) -> QueuePattern | None:
        """Get a specific pattern by tech_area and name."""
        tech_patterns = self.patterns.get(tech_area, {})
        return tech_patterns.get(pattern_name)

    def get_patterns_for_tech_area(self, tech_area: str) -> dict[str, QueuePattern]:
        """Get all patterns for a tech_area."""
        return self.patterns.get(tech_area, {})

    def get_pattern_names(self, tech_area: str) -> list[str]:
        """Get all pattern names for a tech_area."""
        return list(self.patterns.get(tech_area, {}).keys())

    def get_all_sample_ids(self, tech_area: str) -> set[str]:
        """Get all sample IDs referenced by patterns for a tech_area."""
        sample_ids: set[str] = set()
        for pattern in self.patterns.get(tech_area, {}).values():
            sample_ids.update(pattern.get_all_sample_ids())
        return sample_ids

    @classmethod
    def from_dict(cls, data: dict) -> Self:
        """Create QueuePatternsConfig from parsed TOML data.

        Args:
            data: Dict from tomllib.load() with pattern definitions.
                  Structure: tech_area -> pattern_name -> pattern_data

        Returns:
            QueuePatternsConfig with all patterns loaded
        """
        patterns: dict[str, dict[str, QueuePattern]] = {}

        for tech_area, tech_patterns in data.items():
            patterns[tech_area] = {}
            for pattern_name, pattern_data in tech_patterns.items():
                patterns[tech_area][pattern_name] = QueuePattern(**pattern_data)

        return cls(patterns=patterns)
