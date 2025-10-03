import numpy as np
import polars as pl
import pytest
import rpy2.robjects
import rpy2.robjects.pandas2ri
from inline_snapshot import snapshot
from rpy2.robjects.packages import importr


@pytest.fixture(scope="module")
def qg_mod():
    """Load the qg package from development path."""
    # Load devtools if needed
    devtools = importr("devtools")

    # Load the development version from the specified path
    devtools.load_all("/Users/leo/code/qg")

    # Return the loaded module
    return importr("qg")