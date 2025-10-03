import numpy as np
import polars as pl
import rpy2.robjects
import rpy2.robjects.pandas2ri


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
