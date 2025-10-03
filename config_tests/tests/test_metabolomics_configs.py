"""Integration tests for metabolomics queue configurations."""

import polars as pl
import pytest
from inline_snapshot import snapshot

from config_tests import (
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

    # Snapshot test for full output
    # This will capture the actual output on first run and compare on subsequent runs
    assert df_out.to_dict(as_series=False) == snapshot(
        {
            "File Name": [
                "{date}_{run}_C{container}_blank_pos",
                "{date}_{run}_C{container}_blank_pos",
                "{date}_{run}_C{container}_108mix_AA_pos",
                "{date}_{run}_C{container}_108mix_OAP_pos",
                "{date}_{run}_C{container}_pooledQC_pos",
                "{date}_{run}_C{container}_blank_pos",
                "{date}_{run}_C{container}_pooledQCDil1_pos",
                "{date}_{run}_C{container}_pooledQCDil2_pos",
                "{date}_{run}_C{container}_pooledQCDil3_pos",
                "{date}_{run}_C{container}_pooledQCDil4_pos",
                "{date}_{run}_C{container}_pooledQCDil5_pos",
                "{date}_{run}_C{container}_pooledQCDil6_pos",
                "{date}_{run}_C{container}_blank_pos",
                "{date}_{run}_C{container}_S990001_Sample_A_pos",
                "{date}_{run}_C{container}_S990002_Sample_B_pos",
                "{date}_{run}_C{container}_S990003_Sample_C_pos",
                "{date}_{run}_C{container}_blank_pos",
                "{date}_{run}_C{container}_108mix_AA_pos",
                "{date}_{run}_C{container}_108mix_OAP_pos",
                "{date}_{run}_C{container}_pooledQC_pos",
                "{date}_{run}_C{container}_blank_pos",
            ],
            "Path": [
                "D:\\Data2San\\p99000\\Metabolomics\\TEST\\user_20251003",
                "D:\\Data2San\\p99000\\Metabolomics\\TEST\\user_20251003",
                "D:\\Data2San\\p99000\\Metabolomics\\TEST\\user_20251003",
                "D:\\Data2San\\p99000\\Metabolomics\\TEST\\user_20251003",
                "D:\\Data2San\\p99000\\Metabolomics\\TEST\\user_20251003",
                "D:\\Data2San\\p99000\\Metabolomics\\TEST\\user_20251003",
                "D:\\Data2San\\p99000\\Metabolomics\\TEST\\user_20251003",
                "D:\\Data2San\\p99000\\Metabolomics\\TEST\\user_20251003",
                "D:\\Data2San\\p99000\\Metabolomics\\TEST\\user_20251003",
                "D:\\Data2San\\p99000\\Metabolomics\\TEST\\user_20251003",
                "D:\\Data2San\\p99000\\Metabolomics\\TEST\\user_20251003",
                "D:\\Data2San\\p99000\\Metabolomics\\TEST\\user_20251003",
                "D:\\Data2San\\p99000\\Metabolomics\\TEST\\user_20251003",
                "D:\\Data2San\\p99000\\Metabolomics\\TEST\\user_20251003",
                "D:\\Data2San\\p99000\\Metabolomics\\TEST\\user_20251003",
                "D:\\Data2San\\p99000\\Metabolomics\\TEST\\user_20251003",
                "D:\\Data2San\\p99000\\Metabolomics\\TEST\\user_20251003",
                "D:\\Data2San\\p99000\\Metabolomics\\TEST\\user_20251003",
                "D:\\Data2San\\p99000\\Metabolomics\\TEST\\user_20251003",
                "D:\\Data2San\\p99000\\Metabolomics\\TEST\\user_20251003",
                "D:\\Data2San\\p99000\\Metabolomics\\TEST\\user_20251003",
            ],
            "Position": [
                "Y:F1",
                "Y:F1",
                "Y:F9",
                "Y:E9",
                "Y:F8",
                "Y:F1",
                "Y:H2",
                "Y:H3",
                "Y:H4",
                "Y:H5",
                "Y:H6",
                "Y:H7",
                "Y:F1",
                "Y:A1",
                "Y:A2",
                "Y:A3",
                "Y:F1",
                "Y:F9",
                "Y:E9",
                "Y:F8",
                "Y:F1",
            ],
            "Inj Vol": [
                3.5,
                3.5,
                3.5,
                3.5,
                3.5,
                3.5,
                3.5,
                3.5,
                3.5,
                3.5,
                3.5,
                3.5,
                3.5,
                3.5,
                3.5,
                3.5,
                3.5,
                3.5,
                3.5,
                3.5,
                3.5,
            ],
            "L3 Laboratory": [
                "FGCZ",
                "FGCZ",
                "FGCZ",
                "FGCZ",
                "FGCZ",
                "FGCZ",
                "FGCZ",
                "FGCZ",
                "FGCZ",
                "FGCZ",
                "FGCZ",
                "FGCZ",
                "FGCZ",
                "FGCZ",
                "FGCZ",
                "FGCZ",
                "FGCZ",
                "FGCZ",
                "FGCZ",
                "FGCZ",
                "FGCZ",
            ],
            "Sample ID": [
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                None,
                990001,
                990002,
                990003,
                None,
                None,
                None,
                None,
                None,
            ],
            "Sample Name": [
                "blank",
                "blank",
                "108mix_AA",
                "108mix_OAP",
                "blank",
                "blank",
                "QC dil1",
                "QC dil2",
                "QC dil3",
                "QC dil4",
                "QC dil5",
                "QC dil6",
                "blank",
                "Sample_A",
                "Sample_B",
                "Sample_C",
                "blank",
                "108mix_AA",
                "108mix_OAP",
                "blank",
                "blank",
            ],
            "Instrument Method": [
                "D:\\Data2San\\p99000\\Metabolomics\\TEST\\user_20251003\\methods\\",
                "D:\\Data2San\\p99000\\Metabolomics\\TEST\\user_20251003\\methods\\",
                "D:\\Data2San\\p99000\\Metabolomics\\TEST\\user_20251003\\methods\\",
                "D:\\Data2San\\p99000\\Metabolomics\\TEST\\user_20251003\\methods\\",
                "D:\\Data2San\\p99000\\Metabolomics\\TEST\\user_20251003\\methods\\",
                "D:\\Data2San\\p99000\\Metabolomics\\TEST\\user_20251003\\methods\\",
                "D:\\Data2San\\p99000\\Metabolomics\\TEST\\user_20251003\\methods\\",
                "D:\\Data2San\\p99000\\Metabolomics\\TEST\\user_20251003\\methods\\",
                "D:\\Data2San\\p99000\\Metabolomics\\TEST\\user_20251003\\methods\\",
                "D:\\Data2San\\p99000\\Metabolomics\\TEST\\user_20251003\\methods\\",
                "D:\\Data2San\\p99000\\Metabolomics\\TEST\\user_20251003\\methods\\",
                "D:\\Data2San\\p99000\\Metabolomics\\TEST\\user_20251003\\methods\\",
                "D:\\Data2San\\p99000\\Metabolomics\\TEST\\user_20251003\\methods\\",
                "D:\\Data2San\\p99000\\Metabolomics\\TEST\\user_20251003\\methods\\",
                "D:\\Data2San\\p99000\\Metabolomics\\TEST\\user_20251003\\methods\\",
                "D:\\Data2San\\p99000\\Metabolomics\\TEST\\user_20251003\\methods\\",
                "D:\\Data2San\\p99000\\Metabolomics\\TEST\\user_20251003\\methods\\",
                "D:\\Data2San\\p99000\\Metabolomics\\TEST\\user_20251003\\methods\\",
                "D:\\Data2San\\p99000\\Metabolomics\\TEST\\user_20251003\\methods\\",
                "D:\\Data2San\\p99000\\Metabolomics\\TEST\\user_20251003\\methods\\",
                "D:\\Data2San\\p99000\\Metabolomics\\TEST\\user_20251003\\methods\\",
            ],
        }
    )


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
