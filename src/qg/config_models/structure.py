# =============================================================================
# Queue Pattern Models
# =============================================================================

import tomllib
from pathlib import Path
from typing import ClassVar, Self

import polars as pl
from pydantic import BaseModel, Field, field_validator, model_validator


class QueuePattern(BaseModel):
    """A queue pattern defining QC injection sequences."""

    description: str = Field(..., description="Human-readable description")
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

    config_path: ClassVar[Path] = Path("core/structure/queue_patterns.toml")

    patterns: dict[str, dict[str, QueuePattern]] = Field(default_factory=dict)
    header_comments: str = ""

    def get_technologies(self) -> list[str]:
        """Get list of all technologies with patterns defined."""
        return list(self.patterns.keys())

    def get_pattern(self, tech_area: str, pattern_name: str) -> QueuePattern:
        """Get a specific pattern by tech_area and name."""
        if tech_area not in self.patterns:
            raise KeyError(f"No patterns found for tech_area '{tech_area}'")
        tech_patterns = self.patterns[tech_area]
        if pattern_name not in tech_patterns:
            available = list(tech_patterns.keys())
            raise KeyError(f"Pattern '{pattern_name}' not found for '{tech_area}'. Available: {available}")
        return tech_patterns[pattern_name]

    def get_patterns_for_tech_area(self, tech_area: str) -> dict[str, QueuePattern]:
        """Get all patterns for a tech_area."""
        return self.patterns.get(tech_area, {})

    def get_pattern_names(self, tech_area: str) -> list[str]:
        """Get all pattern names for a tech_area."""
        return list(self.patterns.get(tech_area, {}).keys())

    def get_compatible_patterns(self, tech_area: str, available_sample_ids: set[str]) -> dict[str, QueuePattern]:
        """Return patterns whose sample_ids are all available in the given set."""
        return {
            name: p
            for name, p in self.get_patterns_for_tech_area(tech_area).items()
            if p.get_all_sample_ids() <= available_sample_ids
        }

    def get_all_sample_ids(self, tech_area: str) -> set[str]:
        """Get all sample IDs referenced by patterns for a tech_area."""
        sample_ids: set[str] = set()
        for pattern in self.patterns.get(tech_area, {}).values():
            sample_ids.update(pattern.get_all_sample_ids())
        return sample_ids

    def to_dict(self) -> dict:
        """Convert to dict for TOML serialization."""
        result: dict = {}
        for tech_area, tech_patterns in self.patterns.items():
            result[tech_area] = {}
            for pattern_name, pattern in tech_patterns.items():
                result[tech_area][pattern_name] = pattern.model_dump(exclude_none=True)
        return result

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

    @classmethod
    def load(cls, config_dir: Path) -> Self:
        """Load queue patterns from config directory.

        Args:
            config_dir: Root config directory (e.g., qg_configs/)

        Returns:
            QueuePatternsConfig with all patterns loaded
        """
        from .loader import read_header_comments

        path = config_dir / cls.config_path
        raw_text = path.read_text()
        with open(path, "rb") as f:
            data = tomllib.load(f)
        result = cls.from_dict(data)
        result.header_comments = read_header_comments(raw_text)
        return result


# =============================================================================
# Sample Model
# =============================================================================

# Sample ID used for user samples in the queue structure.
# QC samples use specific identifiers (e.g., "QC01", "pooledQC") from config.
# This is defined at module level for use by Sample validator; SamplesConfig
# exposes it publicly as DEFAULT_SAMPLE_ID for external code.
_DEFAULT_SAMPLE_ID = "default"

# Valid placeholders in file_name_template
VALID_PLACEHOLDERS = {
    "date",
    "run",
    "container",
    "sample_id",
    "sample_name",
    "polarity",
}


class Sample(BaseModel):
    """A QC or default sample definition."""

    tech_area: str = Field(..., min_length=1, description="tech_area identifier")
    sample_id: str = Field(..., min_length=1, description="Unique sample ID within tech_area")
    sample_name: str = Field(default="", description="Display name (empty for 'default')")
    description: str = Field(default="", description="Human-readable description")
    inj_vol: float = Field(..., gt=0, description="Injection volume in uL")
    file_name_template: str = Field(..., min_length=1, description="Template for output file names")

    @field_validator("sample_name", "description", mode="before")
    @classmethod
    def empty_str_if_none(cls, v):
        """Convert None to empty string (CSV empty cells)."""
        return v if v is not None else ""

    @field_validator("file_name_template")
    @classmethod
    def validate_template_placeholders(cls, v: str) -> str:
        """Check that all placeholders in the template are valid."""
        import re

        placeholders = re.findall(r"\{(\w+)\}", v)
        invalid = set(placeholders) - VALID_PLACEHOLDERS
        if invalid:
            raise ValueError(f"Invalid placeholders: {invalid}. Valid: {VALID_PLACEHOLDERS}")
        return v

    @model_validator(mode="after")
    def validate_default_has_sample_id(self) -> Self:
        """Default samples should have {sample_id} in template."""
        if self.sample_id == _DEFAULT_SAMPLE_ID:
            if "{sample_id}" not in self.file_name_template:
                raise ValueError(f"'{_DEFAULT_SAMPLE_ID}' sample should have {{sample_id}} in file_name_template")
        return self


class SamplesConfig(BaseModel):
    """Collection of all samples."""

    config_path: ClassVar[Path] = Path("core/structure/samples.csv")
    DEFAULT_SAMPLE_ID: ClassVar[str] = _DEFAULT_SAMPLE_ID

    samples: list[Sample]

    @model_validator(mode="after")
    def validate_unique_keys(self) -> Self:
        """Check that (tech_area, sample_id) pairs are unique."""
        keys = [(s.tech_area, s.sample_id) for s in self.samples]
        if len(keys) != len(set(keys)):
            duplicates = [k for k in keys if keys.count(k) > 1]
            raise ValueError(f"Duplicate (tech_area, sample_id) pairs: {set(duplicates)}")
        return self

    @model_validator(mode="after")
    def validate_each_tech_has_default(self) -> Self:
        """Check that each tech_area has a 'default' sample."""
        techs_with_default = {s.tech_area for s in self.samples if s.sample_id == _DEFAULT_SAMPLE_ID}
        all_techs = {s.tech_area for s in self.samples}
        missing = all_techs - techs_with_default
        if missing:
            raise ValueError(f"Technologies missing '{_DEFAULT_SAMPLE_ID}' sample: {missing}")
        return self

    def get_by_tech_area(self, tech: str) -> list[Sample]:
        """Get all samples for a tech_area."""
        return [s for s in self.samples if s.tech_area == tech]

    def get_sample(self, tech: str, sample_id: str) -> Sample:
        """Get a specific sample by tech_area and sample_id."""
        for s in self.samples:
            if s.tech_area == tech and s.sample_id == sample_id:
                return s
        raise KeyError(f"Sample '{sample_id}' not found for tech_area '{tech}'")

    def to_table(self) -> pl.DataFrame:
        """Convert samples to a polars DataFrame."""
        return pl.DataFrame([s.model_dump() for s in self.samples])

    @classmethod
    def from_table(cls, df: pl.DataFrame) -> Self:
        """Create SamplesConfig from a DataFrame.

        Args:
            df: DataFrame with columns: tech_area, sample_id, sample_name, description, inj_vol, file_name_template

        Returns:
            SamplesConfig with all samples loaded
        """
        samples = [Sample(**row) for row in df.to_dicts()]
        return cls(samples=samples)

    @classmethod
    def load(cls, config_dir: Path) -> Self:
        """Load samples from config directory.

        Args:
            config_dir: Root config directory (e.g., qg_configs/)

        Returns:
            SamplesConfig with all samples loaded
        """
        path = config_dir / cls.config_path
        df = pl.read_csv(path)
        return cls.from_table(df)
