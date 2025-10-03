import csv
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
