import numpy as np
import polars as pl
import pytest
import rpy2.robjects
import rpy2.robjects.pandas2ri
from contextlib import contextmanager
from inline_snapshot import snapshot
from rpy2.rinterface_lib.embedded import RRuntimeError
from rpy2.robjects.packages import importr


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


@pytest.hookimpl(hookwrapper=True)
def pytest_runtest_call(item):
    """Pytest hook to automatically show R tracebacks on test failures."""
    outcome = yield

    if outcome.excinfo is not None:
        exctype, value, tb = outcome.excinfo
        if isinstance(value, RRuntimeError):
            try:
                traceback_result = rpy2.robjects.r("traceback()")
                if traceback_result is not None and len(traceback_result) > 0:
                    print("\n" + "=" * 70)
                    print("R TRACEBACK (from pytest hook):")
                    print("=" * 70)
                    for i, line in enumerate(traceback_result, 1):
                        print(f"{i}: {line}")
                    print("=" * 70 + "\n")
            except Exception as tb_error:
                print(f"Failed to capture R traceback: {tb_error}")


@pytest.fixture(scope="module")
def qg_mod():
    """Load the qg package from development path."""
    # Load devtools if needed
    devtools = importr("devtools")

    # Load the development version from the specified path
    devtools.load_all("/Users/leo/code/qg")

    # Return the loaded module
    return importr("qg")
