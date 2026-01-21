"""Read/write access to qg_configs/ directory for the config editor GUI.

This module provides the ConfigStore class that encapsulates all file I/O
for configuration files. The config_editor.py GUI uses this instead of
direct file reads to enable:
1. Centralized file access patterns
2. Write validation through ConfigBundle
3. Testability with mock data
"""

from pathlib import Path

import polars as pl

from qg.config import (
    ConfigBundle,
    ConfigValidationError,
    _combinations_from_df,
    _instrument_patterns_from_df,
    _instruments_from_df,
    _methods_from_dfs,
    _output_formats_from_toml_str,
    _qc_layouts_from_dfs,
    _queue_patterns_from_toml_str,
    _samples_from_df,
    _samplers_from_toml_str,
    _validate_all_configs,
)


class ConfigStore:
    """Read/write access to qg_configs/ directory for the config editor GUI.

    All methods return raw data (pl.DataFrame or str) without Pydantic validation.
    Validation happens through ConfigBundle before writing to disk.

    Getters return cached data if available, otherwise read from disk.
    Setters store data in instance variables without writing to disk.
    Call save() to persist changes after validation.
    """

    def __init__(self, config_dir: Path):
        """Initialize ConfigStore with the config directory path.

        Args:
            config_dir: Path to qg_configs/ directory containing core/ and ui/ subdirs
        """
        self._config_dir = config_dir
        self._core_dir = config_dir / "core"
        self._ui_dir = config_dir / "ui"

        # Cache for modified data (None = not modified, use disk)
        self._instruments: pl.DataFrame | None = None
        self._samples: pl.DataFrame | None = None
        self._qc_layouts_grid: pl.DataFrame | None = None
        self._qc_layouts_evosep: pl.DataFrame | None = None
        self._instrument_patterns: pl.DataFrame | None = None
        self._combinations: pl.DataFrame | None = None
        self._sampler_toml: str | None = None
        self._queue_patterns_toml: str | None = None
        self._output_formats_toml: str | None = None
        self._methods: dict[Path, pl.DataFrame] = {}  # path -> DataFrame

    # =========================================================================
    # CSV Getters - Core
    # =========================================================================

    def get_instruments(self) -> pl.DataFrame:
        """Load instruments.csv from core/ or return cached version.

        Returns:
            DataFrame with columns: tech_area, instrument, methods_file, path_template
        """
        if self._instruments is not None:
            return self._instruments
        return pl.read_csv(self._core_dir / "instruments.csv")

    def get_samples(self) -> pl.DataFrame:
        """Load samples.csv from core/ or return cached version.

        Returns:
            DataFrame with columns: tech_area, sample_id, sample_name, description,
                                   inj_vol, file_name_template
        """
        if self._samples is not None:
            return self._samples
        return pl.read_csv(self._core_dir / "samples.csv")

    def get_qc_layouts_grid(self) -> pl.DataFrame:
        """Load qc_layouts_grid.csv from core/ or return cached version.

        Returns:
            DataFrame with columns: tech_area, sampler, sample_id, plate, row, col
        """
        if self._qc_layouts_grid is not None:
            return self._qc_layouts_grid
        return pl.read_csv(self._core_dir / "qc_layouts_grid.csv", comment_prefix="#")

    def get_qc_layouts_evosep(self) -> pl.DataFrame:
        """Load qc_layouts_evosep.csv from core/ or return cached version.

        Returns:
            DataFrame with columns: tech_area, sampler, sample_id, tray,
                                   position_start, position_end
        """
        if self._qc_layouts_evosep is not None:
            return self._qc_layouts_evosep
        return pl.read_csv(self._core_dir / "qc_layouts_evosep.csv", comment_prefix="#")

    # =========================================================================
    # CSV Getters - UI
    # =========================================================================

    def get_instrument_patterns(self) -> pl.DataFrame:
        """Load instrument_patterns.csv from ui/ or return cached version.

        Returns:
            DataFrame with columns: tech_area, instrument, queue_pattern, is_default
        """
        if self._instrument_patterns is not None:
            return self._instrument_patterns
        return pl.read_csv(self._ui_dir / "instrument_patterns.csv")

    def get_combinations(self) -> pl.DataFrame:
        """Load combinations.csv from ui/ or return cached version.

        Returns:
            DataFrame with columns: instrument, sampler, output_format
        """
        if self._combinations is not None:
            return self._combinations
        return pl.read_csv(self._ui_dir / "combinations.csv")

    # =========================================================================
    # TOML Getters - Core
    # =========================================================================

    def get_sampler_toml(self) -> str:
        """Load sampler.toml content from core/ or return cached version.

        Returns:
            Raw TOML string content
        """
        if self._sampler_toml is not None:
            return self._sampler_toml
        return (self._core_dir / "sampler.toml").read_text()

    def get_queue_patterns_toml(self) -> str:
        """Load queue_patterns.toml content from core/ or return cached version.

        Returns:
            Raw TOML string content
        """
        if self._queue_patterns_toml is not None:
            return self._queue_patterns_toml
        return (self._core_dir / "queue_patterns.toml").read_text()

    def get_output_formats_toml(self) -> str:
        """Load output_formats.toml content from core/ or return cached version.

        Returns:
            Raw TOML string content
        """
        if self._output_formats_toml is not None:
            return self._output_formats_toml
        return (self._core_dir / "output_formats.toml").read_text()

    # =========================================================================
    # Methods Files
    # =========================================================================

    def list_methods_files(self) -> list[Path]:
        """List all methods CSV files in core/methods/.

        Returns:
            List of absolute paths to methods CSV files, sorted alphabetically
        """
        methods_dir = self._core_dir / "methods"
        if not methods_dir.exists():
            return []
        return sorted(methods_dir.glob("**/*.csv"))

    def get_methods(self, path: Path) -> pl.DataFrame:
        """Load a specific methods CSV file or return cached version.

        Args:
            path: Absolute path to the methods CSV file

        Returns:
            DataFrame with columns: method, polarity (and possibly others)

        Raises:
            FileNotFoundError: If the path doesn't exist and not in cache
        """
        if path in self._methods:
            return self._methods[path]
        return pl.read_csv(path)

    def get_methods_relative_path(self, path: Path) -> str:
        """Get the path relative to config_dir for display purposes.

        Args:
            path: Absolute path to a file within config_dir

        Returns:
            Relative path string (e.g., "core/methods/proteomics/ASTRAL_1_methods.csv")
        """
        return path.relative_to(self._config_dir).as_posix()

    # =========================================================================
    # CSV Setters - Core
    # =========================================================================

    def set_instruments(self, df: pl.DataFrame) -> None:
        """Store modified instruments data.

        Args:
            df: DataFrame with columns: tech_area, instrument, methods_file, path_template
        """
        self._instruments = df

    def set_samples(self, df: pl.DataFrame) -> None:
        """Store modified samples data.

        Args:
            df: DataFrame with columns: tech_area, sample_id, sample_name, description,
                                       inj_vol, file_name_template
        """
        self._samples = df

    def set_qc_layouts_grid(self, df: pl.DataFrame) -> None:
        """Store modified QC layouts grid data.

        Args:
            df: DataFrame with columns: tech_area, sampler, sample_id, plate, row, col
        """
        self._qc_layouts_grid = df

    def set_qc_layouts_evosep(self, df: pl.DataFrame) -> None:
        """Store modified QC layouts evosep data.

        Args:
            df: DataFrame with columns: tech_area, sampler, sample_id, tray,
                                       position_start, position_end
        """
        self._qc_layouts_evosep = df

    # =========================================================================
    # CSV Setters - UI
    # =========================================================================

    def set_instrument_patterns(self, df: pl.DataFrame) -> None:
        """Store modified instrument patterns data.

        Args:
            df: DataFrame with columns: tech_area, instrument, queue_pattern, is_default
        """
        self._instrument_patterns = df

    def set_combinations(self, df: pl.DataFrame) -> None:
        """Store modified combinations data.

        Args:
            df: DataFrame with columns: instrument, sampler, output_format
        """
        self._combinations = df

    # =========================================================================
    # TOML Setters - Core
    # =========================================================================

    def set_sampler_toml(self, content: str) -> None:
        """Store modified sampler TOML content.

        Args:
            content: Raw TOML string content
        """
        self._sampler_toml = content

    def set_queue_patterns_toml(self, content: str) -> None:
        """Store modified queue patterns TOML content.

        Args:
            content: Raw TOML string content
        """
        self._queue_patterns_toml = content

    def set_output_formats_toml(self, content: str) -> None:
        """Store modified output formats TOML content.

        Args:
            content: Raw TOML string content
        """
        self._output_formats_toml = content

    # =========================================================================
    # Methods Setter
    # =========================================================================

    def set_methods(self, path: Path, df: pl.DataFrame) -> None:
        """Store modified methods file data.

        Args:
            path: Absolute path to the methods CSV file
            df: DataFrame with columns: sample_type, polarity, method_name, method_path
        """
        self._methods[path] = df

    # =========================================================================
    # Change Tracking
    # =========================================================================

    def has_changes(self) -> bool:
        """Check if any config has been modified.

        Returns:
            True if any setter has been called, False otherwise
        """
        return (
            self._instruments is not None
            or self._samples is not None
            or self._qc_layouts_grid is not None
            or self._qc_layouts_evosep is not None
            or self._instrument_patterns is not None
            or self._combinations is not None
            or self._sampler_toml is not None
            or self._queue_patterns_toml is not None
            or self._output_formats_toml is not None
            or bool(self._methods)
        )

    def get_modified_files(self) -> list[str]:
        """Get list of files that have been modified.

        Returns:
            List of relative file paths that have pending changes
        """
        modified = []
        if self._instruments is not None:
            modified.append("core/instruments.csv")
        if self._samples is not None:
            modified.append("core/samples.csv")
        if self._qc_layouts_grid is not None:
            modified.append("core/qc_layouts_grid.csv")
        if self._qc_layouts_evosep is not None:
            modified.append("core/qc_layouts_evosep.csv")
        if self._instrument_patterns is not None:
            modified.append("ui/instrument_patterns.csv")
        if self._combinations is not None:
            modified.append("ui/combinations.csv")
        if self._sampler_toml is not None:
            modified.append("core/sampler.toml")
        if self._queue_patterns_toml is not None:
            modified.append("core/queue_patterns.toml")
        if self._output_formats_toml is not None:
            modified.append("core/output_formats.toml")
        for path in self._methods:
            modified.append(self.get_methods_relative_path(path))
        return modified

    def clear_changes(self) -> None:
        """Clear all cached changes, reverting to disk state."""
        self._instruments = None
        self._samples = None
        self._qc_layouts_grid = None
        self._qc_layouts_evosep = None
        self._instrument_patterns = None
        self._combinations = None
        self._sampler_toml = None
        self._queue_patterns_toml = None
        self._output_formats_toml = None
        self._methods.clear()

    # =========================================================================
    # Validation
    # =========================================================================

    def validate(self) -> ConfigBundle:
        """Build and validate a ConfigBundle from current state.

        Uses cached values if set, otherwise reads from disk.
        This validates all configs together using the same logic as qg_config().

        Returns:
            Validated ConfigBundle

        Raises:
            ConfigValidationError: If cross-reference validation fails
            pydantic.ValidationError: If Pydantic model validation fails
            tomllib.TOMLDecodeError: If TOML parsing fails
        """
        # Build config objects from current state (cached or disk)
        instruments = _instruments_from_df(self.get_instruments())
        samples = _samples_from_df(self.get_samples())
        instrument_patterns = _instrument_patterns_from_df(self.get_instrument_patterns())
        combinations = _combinations_from_df(self.get_combinations())
        samplers = _samplers_from_toml_str(self.get_sampler_toml())
        queue_patterns = _queue_patterns_from_toml_str(self.get_queue_patterns_toml())
        output_formats = _output_formats_from_toml_str(self.get_output_formats_toml())
        qc_layouts = _qc_layouts_from_dfs(
            self.get_qc_layouts_grid(), self.get_qc_layouts_evosep()
        )

        # Build methods dict from all methods files (cached or disk)
        methods_dfs: dict[Path, pl.DataFrame] = {}
        for path in self.list_methods_files():
            methods_dfs[path] = self.get_methods(path)
        methods = _methods_from_dfs(methods_dfs, instruments)

        # Create ConfigBundle
        bundle = ConfigBundle(
            samples=samples,
            instruments=instruments,
            instrument_patterns=instrument_patterns,
            combinations=combinations,
            samplers=samplers,
            queue_patterns=queue_patterns,
            qc_layouts=qc_layouts,
            output_formats=output_formats,
            methods=methods,
        )

        # Validate cross-references
        errors = _validate_all_configs(bundle)
        if errors:
            raise ConfigValidationError(errors)

        return bundle

    # =========================================================================
    # Persistence
    # =========================================================================

    def save(self, skip_validation: bool = False) -> list[str]:
        """Write all cached changes to disk after validation.

        Only writes files that have been modified via setters.
        After saving, the cache is cleared.

        Args:
            skip_validation: If True, skip ConfigBundle validation (not recommended)

        Returns:
            List of relative file paths that were written

        Raises:
            ValueError: If validation fails (unless skip_validation=True)
        """
        # Validate before writing (unless explicitly skipped)
        if not skip_validation:
            self.validate()

        saved = []

        # Core CSV files
        if self._instruments is not None:
            self._instruments.write_csv(self._core_dir / "instruments.csv")
            saved.append("core/instruments.csv")

        if self._samples is not None:
            self._samples.write_csv(self._core_dir / "samples.csv")
            saved.append("core/samples.csv")

        if self._qc_layouts_grid is not None:
            self._qc_layouts_grid.write_csv(self._core_dir / "qc_layouts_grid.csv")
            saved.append("core/qc_layouts_grid.csv")

        if self._qc_layouts_evosep is not None:
            self._qc_layouts_evosep.write_csv(self._core_dir / "qc_layouts_evosep.csv")
            saved.append("core/qc_layouts_evosep.csv")

        # UI CSV files
        if self._instrument_patterns is not None:
            self._instrument_patterns.write_csv(self._ui_dir / "instrument_patterns.csv")
            saved.append("ui/instrument_patterns.csv")

        if self._combinations is not None:
            self._combinations.write_csv(self._ui_dir / "combinations.csv")
            saved.append("ui/combinations.csv")

        # Core TOML files
        if self._sampler_toml is not None:
            (self._core_dir / "sampler.toml").write_text(self._sampler_toml)
            saved.append("core/sampler.toml")

        if self._queue_patterns_toml is not None:
            (self._core_dir / "queue_patterns.toml").write_text(self._queue_patterns_toml)
            saved.append("core/queue_patterns.toml")

        if self._output_formats_toml is not None:
            (self._core_dir / "output_formats.toml").write_text(self._output_formats_toml)
            saved.append("core/output_formats.toml")

        # Methods files
        for path, df in self._methods.items():
            df.write_csv(path)
            saved.append(self.get_methods_relative_path(path))

        # Clear cache after successful save
        self.clear_changes()

        return saved

    # =========================================================================
    # Properties
    # =========================================================================

    @property
    def config_dir(self) -> Path:
        """Return the base config directory path."""
        return self._config_dir

    @property
    def core_dir(self) -> Path:
        """Return the core config directory path."""
        return self._core_dir

    @property
    def ui_dir(self) -> Path:
        """Return the ui config directory path."""
        return self._ui_dir


def config_store(config_dir: Path | None = None) -> ConfigStore:
    """Factory function to create a ConfigStore instance.

    Args:
        config_dir: Path to qg_configs/ directory. If None, uses the default
                   location relative to this source file.

    Returns:
        ConfigStore instance for the specified directory
    """
    if config_dir is None:
        config_dir = Path(__file__).parent.parent.parent / "qg_configs"
    return ConfigStore(config_dir)
