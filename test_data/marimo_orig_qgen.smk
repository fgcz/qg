"""Queue generator validation workflow - starts from B-Fabric dump.

Pipeline:
1. Extract source CSVs from zip archive
2. Generate queue params JSON from CSV files
3. Run queue generator
4. Compare generated vs original
5. Launch marimo comparison app

Directory structure (all within qg_20260119_dump/):
  20260119_dump.zip    Source archive (~410 B-Fabric queue files)
  20260119_dump/       Extracted CSVs
  params_json/         Generated params
  qg_new/              Generated queues
  qg_logs/             Generation logs
  results/             Comparison results

Usage:
    snakemake -s marimo_orig_qgen.smk -j4 all
    snakemake -s marimo_orig_qgen.smk marimo_app
"""

import json
from pathlib import Path

# Directories - all within BASE_DIR for clean structure
BASE_DIR = Path("qg_20260119_dump")
SOURCE_ZIP = BASE_DIR / "20260119_dump.zip"
SOURCE_DIR = BASE_DIR / "20260119_dump"
PARAMS_DIR = BASE_DIR / "params_json"
GENERATED_DIR = BASE_DIR / "qg_new"
LOGS_DIR = BASE_DIR / "qg_logs"
RESULTS_DIR = BASE_DIR / "results"


# =============================================================================
# Helper Functions
# =============================================================================

def get_all_param_files(_wildcards):
    """Get all param JSON files after checkpoint completes."""
    checkpoints.params.get()
    params_dir = PARAMS_DIR
    if params_dir.exists():
        # Only return non-PARTIAL params (complete inference)
        return [str(f) for f in params_dir.glob("*_params.json")
                if "_PARTIAL_" not in f.name]
    return []


def get_comparison_results(_wildcards):
    """Get comparison result files after params are generated."""
    checkpoints.params.get()
    results = []
    if PARAMS_DIR.exists():
        for f in PARAMS_DIR.glob("*_params.json"):
            if "_PARTIAL_" not in f.name:
                results.append(str(RESULTS_DIR / f"{f.stem}.json"))
    return results


def get_original_csv(name):
    """Get path to original CSV from param name.

    Param file: resource_12345_params.json -> Original: resource_12345.csv
    """
    # Remove _params suffix to get original name
    orig_name = name.replace("_params", "")
    return str(SOURCE_DIR / f"{orig_name}.csv")


# =============================================================================
# Main Targets
# =============================================================================

rule all:
    """Run complete pipeline: params → generate → compare → summary."""
    input:
        RESULTS_DIR / "test_results.csv"


rule help:
    """Show available targets."""
    run:
        print("""
Queue Generator Validation Workflow

Source: qg_20260119_dump/20260119_dump.zip (409 B-Fabric queue files)

Directory structure (all within qg_20260119_dump/):
  20260119_dump.zip    Source archive
  20260119_dump/       Extracted CSVs
  params_json/         Generated params
  qg_new/              Generated queues
  qg_logs/             Generation logs
  results/             Comparison results

Pipeline:
  1. extract_zip  Extract source CSVs from zip
  2. params       Generate queue params JSON from CSVs
  3. generate     Run queue generator
  4. compare      Compare generated vs original
  5. summary      Create test_results.csv
  6. marimo_app   Launch comparison viewer

Targets:
  all             Run complete pipeline
  extract_zip     Extract source data only
  params          Generate params only
  marimo_app      Launch marimo comparison app
  clean           Remove qg_new/, results/, qg_logs/
  clean_all       Remove ALL intermediates (keeps zip)

Usage:
  snakemake -s marimo_orig_qgen.smk -j4 all
  snakemake -s marimo_orig_qgen.smk marimo_app
""")


# =============================================================================
# Phase 1: Extract Source Data and Generate Param JSONs
# =============================================================================

rule extract_zip:
    """Extract source CSV files from zip archive."""
    input:
        SOURCE_ZIP
    output:
        directory(SOURCE_DIR)
    shell:
        "unzip -q {input} -d {BASE_DIR}"


checkpoint params:
    """Generate param JSONs from source CSVs."""
    input:
        csv_dir=SOURCE_DIR
    output:
        directory(PARAMS_DIR)
    shell:
        """
        mkdir -p {output}
        uv run qg-tools csv-to-params {input.csv_dir}/ -o {output}/
        """


# =============================================================================
# Phase 2: Queue Generator Testing
# =============================================================================

rule generate_queue:
    """Run queue generator on a param file."""
    input:
        params=PARAMS_DIR / "{name}.json"
    output:
        csv=GENERATED_DIR / "{name}.csv"
    log:
        LOGS_DIR / "{name}.log"
    shell:
        """
        mkdir -p {GENERATED_DIR} {LOGS_DIR}
        cd .. && uv run qg test_data/{input.params} -o test_data/{output.csv} 2>test_data/{log} || touch test_data/{output.csv}
        """


rule compare:
    """Compare generated queue CSV with original reference CSV."""
    input:
        generated=GENERATED_DIR / "{name}.csv",
        config=PARAMS_DIR / "{name}.json"
    output:
        result=RESULTS_DIR / "{name}.json"
    params:
        original=lambda wildcards: get_original_csv(wildcards.name)
    shell:
        """
        mkdir -p {RESULTS_DIR}
        uv run qg-tools compare {input.generated} {params.original} {input.config} {output.result}
        """


rule summary:
    """Aggregate comparison results into test_results.csv database."""
    input:
        get_comparison_results
    output:
        RESULTS_DIR / "test_results.csv"
    shell:
        "uv run qg-tools summarize {output} {input}"


# =============================================================================
# Analysis App
# =============================================================================

rule marimo_app:
    """Launch test results marimo app (compare original vs generated)."""
    input:
        RESULTS_DIR / "test_results.csv"
    shell:
        "uv run marimo run ../src/qg/tools_apps/compare_existing_generated.py"


# =============================================================================
# Cleanup
# =============================================================================

rule clean:
    """Remove generated, results, and params (keeps source)."""
    shell:
        """
        rm -rf {GENERATED_DIR} {RESULTS_DIR} {LOGS_DIR} {PARAMS_DIR}
        echo "Cleaned {GENERATED_DIR}, {RESULTS_DIR}, {LOGS_DIR}, {PARAMS_DIR}"
        """


rule clean_all:
    """Remove ALL intermediates (keeps source zip)."""
    shell:
        """
        rm -rf {GENERATED_DIR} {RESULTS_DIR} {LOGS_DIR} {PARAMS_DIR} {SOURCE_DIR}
        echo "Cleaned all intermediates in {BASE_DIR}"
        """
