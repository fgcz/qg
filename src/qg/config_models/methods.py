# =============================================================================
# Method Models
# =============================================================================

from pathlib import Path
from typing import TYPE_CHECKING, ClassVar, Self

import polars as pl
from loguru import logger
from pydantic import BaseModel, Field, field_validator

if TYPE_CHECKING:
    from .formatting import InstrumentsConfig


class Method(BaseModel):
    """A method definition from methods CSV."""

    sample_type: str = Field(..., min_length=1, description="Sample type (default, QC02, QC01, clean, etc.)")
    polarity: str = Field(default="", description="Polarity (pos, neg, or empty for proteomics)")
    method_name: str = Field(..., min_length=1, description="Method name")
    method_path: str = Field(..., min_length=1, description="Full path to method file")

    @field_validator("polarity", mode="before")
    @classmethod
    def empty_str_if_none(cls, v):
        """Convert None to empty string (CSV empty cells)."""
        return v if v is not None else ""


class MethodsForInstrument(BaseModel):
    """Methods for a single instrument."""

    config_path: Path = Field(..., description="Repo-relative path this methods file was loaded from")
    methods: list[Method]

    def get_method_names(self, sample_type: str, polarity: str = "") -> set[str]:
        """Get available method names for a sample_type and polarity."""
        return {m.method_name for m in self.methods if m.sample_type == sample_type and m.polarity == polarity}

    def get_method(
        self,
        sample_type: str,
        polarity: str = "",
        method_name: str = "",
    ) -> Method | None:
        """Get a specific method.

        Args:
            sample_type: Sample type (default, QC02, QC01, clean, etc.)
            polarity: Polarity (pos, neg, or empty)
            method_name: Optional specific method name

        Returns:
            Method if found, None otherwise. Falls back to 'default' sample_type.
        """

        for m in self.methods:
            if m.sample_type == sample_type and m.polarity == polarity:
                if not method_name or m.method_name == method_name:
                    return m

        return None

    def get_method_path(
        self,
        sample_type: str,
        polarity: str = "",
        method_name: str = "",
    ) -> str:
        """Get method path for given parameters.

        Returns:
            Method path, or empty string if not found.
        """
        method = self.get_method(sample_type, polarity, method_name)
        return method.method_path if method else ""

    def to_table(self) -> pl.DataFrame:
        """Convert methods to a polars DataFrame."""
        if not self.methods:
            return pl.DataFrame()
        return pl.DataFrame([m.model_dump() for m in self.methods])

    @classmethod
    def from_table(cls, df: pl.DataFrame, config_path: Path) -> Self:
        """Create MethodsForInstrument from a DataFrame.

        Args:
            df: DataFrame with columns: sample_type, polarity, method_name, method_path
            config_path: Repo-relative path this methods file was loaded from

        Returns:
            MethodsForInstrument with all methods loaded
        """
        methods = [Method(**row) for row in df.to_dicts()]
        return cls(config_path=config_path, methods=methods)


class MethodsConfig(BaseModel):
    """All methods by tech_area and instrument.

    Structure: tech_area -> instrument -> MethodsForInstrument
    """

    config_folder: ClassVar[Path] = Path("core/methods")

    methods: dict[str, dict[str, MethodsForInstrument]] = Field(default_factory=dict)

    def get_methods(self, tech_area: str, instrument: str) -> MethodsForInstrument:
        """Get methods for a tech_area/instrument combination."""
        if tech_area not in self.methods:
            raise KeyError(f"No methods found for tech_area '{tech_area}'")
        tech_methods = self.methods[tech_area]
        if instrument not in tech_methods:
            raise KeyError(f"No methods found for instrument '{instrument}' in tech_area '{tech_area}'")
        return tech_methods[instrument]

    def get_method_path(
        self,
        tech_area: str,
        instrument: str,
        sample_type: str,
        polarity: str = "",
        method_name: str = "",
    ) -> str:
        """Get method path for given parameters."""
        methods = self.get_methods(tech_area, instrument)
        return methods.get_method_path(sample_type, polarity, method_name)

    def add_instrument_methods(
        self,
        tech_area: str,
        instrument: str,
        methods: MethodsForInstrument,
    ) -> None:
        """Add methods for an instrument."""
        if tech_area not in self.methods:
            self.methods[tech_area] = {}
        self.methods[tech_area][instrument] = methods

    def to_tables(self) -> dict[tuple[str, str], pl.DataFrame]:
        """Convert to dict of DataFrames for CSV serialization.

        Returns:
            Dict mapping (tech_area, instrument) -> DataFrame
        """
        tables: dict[tuple[str, str], pl.DataFrame] = {}
        for tech_area, tech_methods in self.methods.items():
            for instrument, instr_methods in tech_methods.items():
                tables[(tech_area, instrument)] = instr_methods.to_table()
        return tables

    def iter_methods(self):
        """Iterate over all (tech_area, instrument), MethodsForInstrument pairs."""
        for tech_area, tech_methods in self.methods.items():
            for instrument, instr_methods in tech_methods.items():
                yield (tech_area, instrument), instr_methods

    @classmethod
    def from_tables(cls, tables: dict[tuple[str, str], pl.DataFrame]) -> Self:
        """Create MethodsConfig from a dict of DataFrames.

        Args:
            tables: Dict mapping (tech_area, instrument) -> DataFrame.
                Each MethodsForInstrument gets a canonical config_path
                of ``core/methods/{tech_area}/{instrument}_methods.csv``.

        Returns:
            MethodsConfig with all methods loaded
        """
        config = cls()
        for (tech_area, instrument), df in tables.items():
            canonical = cls.config_folder / tech_area / f"{instrument}_methods.csv"
            methods = MethodsForInstrument.from_table(df, config_path=canonical)
            config.add_instrument_methods(tech_area, instrument, methods)
        return config

    @classmethod
    def load(cls, config_dir: Path, instruments: "InstrumentsConfig") -> Self:
        """Load all methods CSVs based on instruments config.

        Discovers methods files from instruments.methods_file paths and loads them
        into a MethodsConfig keyed by (tech_area, instrument).

        Args:
            config_dir: Root config directory (e.g., qg_configs/)
            instruments: InstrumentsConfig to determine which methods files to load

        Returns:
            MethodsConfig with all methods loaded
        """
        methods_dir = config_dir / cls.config_folder
        tables: dict[tuple[str, str], pl.DataFrame] = {}

        for instr in instruments.instruments:
            # methods_file is like "methods/Proteomics/ASTRAL_1_methods.csv"
            # Remove the "methods/" prefix to get relative path from methods_dir
            relative_path = instr.methods_file.removeprefix("methods/")
            methods_file = methods_dir / relative_path

            if not methods_file.exists():
                logger.warning(f"Methods file not found: {methods_file}")
                continue

            df = pl.read_csv(methods_file)
            # Store the actual file path (repo-relative) the data was loaded from
            config_path = cls.config_folder / relative_path
            tables[(instr.tech_area, instr.instrument)] = (df, config_path)

        config = cls()
        for (tech_area, instrument), (df, config_path) in tables.items():
            methods = MethodsForInstrument.from_table(df, config_path=config_path)
            config.add_instrument_methods(tech_area, instrument, methods)
        return config
