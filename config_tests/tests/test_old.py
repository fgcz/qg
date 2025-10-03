import polars as pl
from inline_snapshot import snapshot
from config_tests import polars_to_r_dataframe, r_dataframe_to_polars


def test_initial(qg_mod):
    x = pl.DataFrame()
    r_df_out = qg_mod._pooledQCSplash(x=polars_to_r_dataframe(x))
    df_out = r_dataframe_to_polars(r_df_out)
    assert df_out.to_dict(as_series=False) == snapshot(
        {
            "File Name": [
                "{date}_{run}_C{container}_pooledQC",
                "{date}_{run}_C{container}_splash",
                "{date}_{run}_C{container}_blank",
            ],
            "Position": ["Y:H8", "Y:H9", "Y:H1"],
            "Sample Name": ["pooledQC", "splash", "blank"],
            "Inj Vol": [3.5, 3.5, 3.5],
        },
    )


def test_initial2(qg_mod):
    x = pl.DataFrame({"dummy": [2], "yz": ["A"]})
    r_df_out = qg_mod._pooledQCSplash(x=polars_to_r_dataframe(x))
    df_out = r_dataframe_to_polars(r_df_out)
    assert df_out.to_dict(as_series=False) == snapshot(
        {
            "dummy": [None, None, None],
            "yz": [None, None, None],
            "File Name": [
                "{date}_{run}_C{container}_pooledQC",
                "{date}_{run}_C{container}_splash",
                "{date}_{run}_C{container}_blank",
            ],
            "Position": ["Y:H8", "Y:H9", "Y:H1"],
            "Sample Name": ["pooledQC", "splash", "blank"],
            "Inj Vol": [3.5, 3.5, 3.5],
        },
    )


def test_interpolate_filenames(qg_mod):
    test_df = pl.DataFrame(
        {
            "File Name": [
                "{date}_{run}_C{container}",
                "{date}_{run}_xyz",
                "{date}_{run}_test_C{container}",
            ]
        }
    )
    result = qg_mod._interpolateFilenames(
        x=polars_to_r_dataframe(test_df), container=2000
    )
    df_out = r_dataframe_to_polars(result)
    assert df_out.to_dict(as_series=False) == snapshot(
        {
            "File Name": [
                "20251003_001_C2000",
                "20251003_002_xyz",
                "20251003_003_test_C2000",
            ]
        }
    )
