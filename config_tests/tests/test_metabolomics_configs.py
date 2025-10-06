"""Integration tests for metabolomics queue configurations."""

import polars as pl
import pytest

from config_tests import (
    compare_with_expected,
    load_test_manifest,
    polars_to_r_dataframe,
    r_dataframe_to_polars,
)
from config_tests.fixtures import get_input_fixture


# Load test manifest and create test parameters
test_cases = load_test_manifest()


@pytest.mark.parametrize("config_name,input_type,howOften", test_cases)
def test_metabolomics_config(qg_mod, config_name, input_type, howOften):
    """Test metabolomics config functions with various inputs.

    Args:
        qg_mod: R qg module fixture
        config_name: Name of the config function to test
        input_type: Type of input fixture to use
        howOften: QC insertion frequency parameter
    """
    # Get synthetic input data
    x = get_input_fixture(input_type)

    # Get the R function by name
    config_func = getattr(qg_mod, config_name)

    # Call the R config function
    r_df_out = config_func(x=polars_to_r_dataframe(x), howOften=howOften)

    # Convert result back to Polars
    df_out = r_dataframe_to_polars(r_df_out)

    # Basic structure validation
    assert df_out.shape[0] > 0, "Output should have at least one row"
    assert df_out.shape[1] > 0, "Output should have at least one column"

    # Check required columns exist
    required_columns = [
        "File Name",
        "Path",
        "Position",
        "Inj Vol",
        "L3 Laboratory",
        "Sample ID",
        "Sample Name",
        "Instrument Method",
    ]
    for col in required_columns:
        assert col in df_out.columns, f"Required column '{col}' missing from output"

    # Compare with expected output (TSV-based snapshot)
    compare_with_expected(df_out, config_name, input_type)


def test_config_function_exists(qg_mod):
    """Test that all config functions in manifest exist in the qg module."""
    test_cases = load_test_manifest()
    unique_configs = {case[0] for case in test_cases}

    for config_name in unique_configs:
        assert hasattr(
            qg_mod, config_name
        ), f"Config function '{config_name}' not found in qg module"


def test_output_row_count_increases_with_input(qg_mod):
    """Test that larger inputs produce more output rows (due to more samples)."""
    config_name = "qconfigMetabolomicsVanquishVialXCaliburSII_pos"
    howOften = 5

    # Get config function
    config_func = getattr(qg_mod, config_name)

    # Test with small input
    x_small = get_input_fixture("vial_small")
    r_out_small = config_func(x=polars_to_r_dataframe(x_small), howOften=howOften)
    df_out_small = r_dataframe_to_polars(r_out_small)

    # Test with large input
    x_large = get_input_fixture("vial_large")
    r_out_large = config_func(x=polars_to_r_dataframe(x_large), howOften=howOften)
    df_out_large = r_dataframe_to_polars(r_out_large)

    # Large input should produce more rows (since it has more samples)
    assert (
        df_out_large.shape[0] > df_out_small.shape[0]
    ), "Larger input should produce more output rows"


def test_polarity_suffix_in_filenames(qg_mod):
    """Test that positive polarity configs add '_pos' suffix to filenames."""
    config_name = "qconfigMetabolomicsVanquishVialXCaliburSII_pos"
    howOften = 5

    x = get_input_fixture("vial_small")
    config_func = getattr(qg_mod, config_name)
    r_out = config_func(x=polars_to_r_dataframe(x), howOften=howOften)
    df_out = r_dataframe_to_polars(r_out)

    # All filenames should end with _pos
    filenames = df_out["File Name"].to_list()
    for filename in filenames:
        assert filename.endswith(
            "_pos"
        ), f"Filename '{filename}' should end with '_pos'"
