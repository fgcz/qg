import csv
import os
from contextlib import contextmanager
from pathlib import Path

import numpy as np
import polars as pl
import rpy2.robjects
import rpy2.robjects.pandas2ri
from rpy2.rinterface_lib.embedded import RRuntimeError


def polars_to_r_dataframe(pl_df):
    """Convert Polars DataFrame to R DataFrame.

    Args:
        pl_df: Polars DataFrame

    Returns:
        R DataFrame object

    """
    with (
        rpy2.robjects.default_converter + rpy2.robjects.pandas2ri.converter
    ).context():
        r_df = rpy2.robjects.conversion.get_conversion().py2rpy(pl_df.to_pandas())

    return r_df


def r_dataframe_to_polars(r_df):
    """Convert R DataFrame to Polars with proper NA handling.

    Args:
        r_df: R DataFrame object from rpy2

    Returns:
        Polars DataFrame with proper null handling

    """
    data = {}
    for col_name in r_df.names:
        # Get the R column
        r_col = r_df.rx2(col_name)

        # Get NA mask from R
        na_mask = np.array(rpy2.robjects.r["is.na"](r_col), dtype=bool)

        # Convert to Python list
        values = list(r_col)

        # Replace NA positions with None
        values = [None if is_na else val for val, is_na in zip(values, na_mask)]

        data[col_name] = values

    return pl.DataFrame(data)


@contextmanager
def r_traceback_on_error():
    """Context manager that prints R traceback when an R error occurs.

    Usage:
        with r_traceback_on_error():
            # Call R code that might fail
            r_function()
    """
    try:
        yield
    except RRuntimeError as e:
        # Capture R traceback
        try:
            traceback_result = rpy2.robjects.r("traceback()")
            if traceback_result is not None and len(traceback_result) > 0:
                print("\n" + "=" * 70)
                print("R TRACEBACK:")
                print("=" * 70)
                for i, line in enumerate(traceback_result, 1):
                    print(f"{i}: {line}")
                print("=" * 70 + "\n")
        except Exception as tb_error:
            print(f"Failed to capture R traceback: {tb_error}")
        # Re-raise the original error
        raise


def load_test_manifest(manifest_path=None):
    """Load test manifest TSV file.

    Args:
        manifest_path: Path to manifest file. If None, uses default location.

    Returns:
        List of tuples (config_name, input_type, howOften) for pytest parametrization

    """
    if manifest_path is None:
        # Find the project root by looking for pyproject.toml
        current = Path(__file__).parent
        while current != current.parent:
            if (current / "pyproject.toml").exists():
                manifest_path = current / "data" / "test_manifest.tsv"
                break
            current = current.parent

        if manifest_path is None:
            raise FileNotFoundError(
                "Could not find project root with pyproject.toml. "
                "Please specify manifest_path explicitly."
            )

    test_cases = []
    with open(manifest_path, "r") as f:
        reader = csv.DictReader(f, delimiter="\t")
        for row in reader:
            test_cases.append(
                (
                    row["config_name"],
                    row["input_type"],
                    int(row["howOften"]),
                )
            )

    return test_cases


def _get_project_root():
    """Find project root by looking for pyproject.toml."""
    current = Path(__file__).parent
    while current != current.parent:
        if (current / "pyproject.toml").exists():
            return current
        current = current.parent
    raise FileNotFoundError("Could not find project root with pyproject.toml")


def _get_expected_output_path(config_name: str, input_type: str) -> Path:
    """Get path to expected output TSV file.

    Args:
        config_name: Name of the config function
        input_type: Type of input fixture

    Returns:
        Path to the expected output TSV file
    """
    project_root = _get_project_root()
    return project_root / "data" / "expected" / config_name / f"{input_type}.tsv"


def save_expected_output(df: pl.DataFrame, config_name: str, input_type: str):
    """Save DataFrame as expected output TSV.

    Args:
        df: DataFrame to save
        config_name: Name of the config function
        input_type: Type of input fixture
    """
    output_path = _get_expected_output_path(config_name, input_type)
    output_path.parent.mkdir(parents=True, exist_ok=True)

    # Save as TSV with proper formatting
    df.write_csv(output_path, separator="\t")
    print(f"Created expected output: {output_path}")


def load_expected_output(config_name: str, input_type: str) -> pl.DataFrame:
    """Load expected output TSV.

    Args:
        config_name: Name of the config function
        input_type: Type of input fixture

    Returns:
        Expected DataFrame

    Raises:
        FileNotFoundError: If expected output file doesn't exist
    """
    output_path = _get_expected_output_path(config_name, input_type)

    if not output_path.exists():
        raise FileNotFoundError(f"Expected output not found: {output_path}")

    return pl.read_csv(output_path, separator="\t")


def compare_with_expected(
    actual: pl.DataFrame, config_name: str, input_type: str, update: bool = None
) -> bool:
    """Compare actual output with expected output TSV.

    Args:
        actual: Actual output DataFrame
        config_name: Name of the config function
        input_type: Type of input fixture
        update: Whether to update expected output. If None, checks UPDATE_EXPECTED env var

    Returns:
        True if outputs match or were updated, False otherwise

    Raises:
        AssertionError: If outputs don't match and update=False
    """
    if update is None:
        update = os.environ.get("UPDATE_EXPECTED", "").lower() in ("1", "true", "yes")

    output_path = _get_expected_output_path(config_name, input_type)

    # If expected output doesn't exist, create it if update=True
    if not output_path.exists():
        if update:
            save_expected_output(actual, config_name, input_type)
            return True
        else:
            raise FileNotFoundError(
                f"Expected output not found: {output_path}\n"
                f"Run with UPDATE_EXPECTED=1 to create it."
            )

    # Load expected output
    expected = load_expected_output(config_name, input_type)

    # Reorder actual to match expected column order
    actual_reordered = actual.select(expected.columns)

    # Compare DataFrames using equals()
    try:
        if actual_reordered.equals(expected, null_equal=True):
            return True
    except Exception:
        pass  # Fall through to detailed comparison

    # If not equal, provide detailed diff
    if update:
        save_expected_output(actual, config_name, input_type)
        print(f"Updated expected output: {output_path}")
        return True

    # Show differences
    print("\n" + "=" * 70)
    print("MISMATCH: Actual output differs from expected")
    print("=" * 70)
    print(f"Expected: {output_path}")
    print(f"\nShape - Expected: {expected.shape}, Actual: {actual.shape}")

    # Check for column differences
    expected_cols = set(expected.columns)
    actual_cols = set(actual.columns)

    if expected_cols != actual_cols:
        print(f"\nColumn differences:")
        if expected_cols - actual_cols:
            print(f"  Missing columns: {expected_cols - actual_cols}")
        if actual_cols - expected_cols:
            print(f"  Extra columns: {actual_cols - expected_cols}")

    # Show schema differences
    print("\nColumn dtypes:")
    print("  Expected:", expected.schema)
    print("  Actual:  ", actual.schema)

    # Show first few rows of diff
    if expected.shape == actual.shape and expected_cols == actual_cols:
        print("\nFirst few differing rows:")
        diff_count = 0
        for i in range(min(10, expected.height)):
            exp_row = expected.row(i)
            act_row = actual_reordered.row(i)
            if exp_row != act_row:
                print(f"\n  Row {i}:")
                print(f"    Expected: {exp_row}")
                print(f"    Actual:   {act_row}")
                # Show column-by-column comparison
                for j, col in enumerate(expected.columns):
                    if exp_row[j] != act_row[j]:
                        print(f"      {col}: {exp_row[j]!r} != {act_row[j]!r}")
                diff_count += 1
                if diff_count >= 3:  # Limit to 3 rows
                    break

        if diff_count == 0:
            print("  (No row differences found - likely a dtype issue)")

    print("\n" + "=" * 70)
    print("To update expected output, run with UPDATE_EXPECTED=1")
    print("=" * 70 + "\n")

    raise AssertionError(
        f"Output mismatch for {config_name}/{input_type}. "
        f"See diff above or run with UPDATE_EXPECTED=1 to update."
    )
