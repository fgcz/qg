"""Synthetic input data generators for testing queue configurations."""

import polars as pl


def create_vial_input_small() -> pl.DataFrame:
    """Create a small vial dataset with 3 sample rows.

    Returns:
        Polars DataFrame with required columns for vial-based configs
    """
    return pl.DataFrame(
        {
            "Sample ID": [990001, 990002, 990003],
            "Sample Name": ["Sample_A", "Sample_B", "Sample_C"],
            "File Name": [
                "{date}_{run}_C{container}_S990001_Sample_A",
                "{date}_{run}_C{container}_S990002_Sample_B",
                "{date}_{run}_C{container}_S990003_Sample_C",
            ],
            "Path": ["D:\\Data2San\\p99000\\Metabolomics\\TEST\\user_20251003"] * 3,
            "Position": ["Y:A1", "Y:A2", "Y:A3"],
            "Inj Vol": [3.5, 3.5, 3.5],
        }
    )


def create_vial_input_large() -> pl.DataFrame:
    """Create a large vial dataset with 24 sample rows.

    Returns:
        Polars DataFrame with required columns for vial-based configs
    """
    n_samples = 24
    sample_ids = [990000 + i for i in range(1, n_samples + 1)]

    return pl.DataFrame(
        {
            "Sample ID": sample_ids,
            "Sample Name": [
                f"Sample_{chr(65 + (i % 26))}{i // 26 + 1}" for i in range(n_samples)
            ],
            "File Name": [
                f"{{date}}_{{run}}_C{{container}}_S{sid}_Sample_{chr(65 + (i % 26))}{i // 26 + 1}"
                for i, sid in enumerate(sample_ids)
            ],
            "Path": ["D:\\Data2San\\p99000\\Metabolomics\\TEST\\user_20251003"]
            * n_samples,
            "Position": [
                f"Y:{chr(65 + (i // 9))}{(i % 9) + 1}" for i in range(n_samples)
            ],
            "Inj Vol": [3.5] * n_samples,
        }
    )


def get_input_fixture(input_type: str) -> pl.DataFrame:
    """Get input fixture by name.

    Args:
        input_type: Type of input fixture ('vial_small' or 'vial_large')

    Returns:
        Polars DataFrame with the requested fixture data

    Raises:
        ValueError: If input_type is not recognized
    """
    fixtures = {
        "vial_small": create_vial_input_small,
        "vial_large": create_vial_input_large,
    }

    if input_type not in fixtures:
        raise ValueError(
            f"Unknown input type: {input_type}. "
            f"Available types: {', '.join(fixtures.keys())}"
        )

    return fixtures[input_type]()
