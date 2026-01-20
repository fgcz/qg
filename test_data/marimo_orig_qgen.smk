"""Queue generator validation workflow - starts from B-Fabric dump.

Pipeline:
1. Generate queue params JSON from CSV files (csv_to_paramsjson.py)
2. Run queue generator
3. Compare generated vs original
4. Launch marimo comparison app

Source: qg_20260119_dump/20260119_dump/*.csv (409 B-Fabric queue files)

Usage:
    snakemake -s marimo_orig_qgen.smk -j4 all
    snakemake -s marimo_orig_qgen.smk marimo_app
"""

import json
from pathlib import Path

# Directories
SOURCE_DIR = Path("qg_20260119_dump/20260119_dump")
PARAMS_DIR = Path("params_json")
GENERATED_DIR = Path("generated")
RESULTS_DIR = Path("results")


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
    params_dir = PARAMS_DIR
    if params_dir.exists():
        for f in params_dir.glob("*_params.json"):
            if "_PARTIAL_" not in f.name:
                results.append(f"results/{f.stem}.json")
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
        "results/test_results.csv"


rule help:
    """Show available targets."""
    run:
        print("""
Queue Generator Validation Workflow

Source: qg_20260119_dump/20260119_dump/*.csv (409 B-Fabric queue files)

Pipeline:
  1. params       Generate queue params JSON from CSVs
  2. generate     Run queue generator
  3. compare      Compare generated vs original
  4. summary      Create test_results.csv
  5. marimo_app   Launch comparison viewer

Targets:
  all             Run complete pipeline
  params_json     Generate params only
  marimo_app      Launch marimo comparison app
  clean           Remove generated/ and results/
  clean_all       Remove ALL intermediates

Usage:
  snakemake -s marimo_orig_qgen.smk -j4 all
  snakemake -s marimo_orig_qgen.smk marimo_app
""")


# =============================================================================
# Phase 1: CSV → Param JSON Generation
# =============================================================================

checkpoint params:
    """Generate param JSONs from source CSVs."""
    input:
        csv_dir=SOURCE_DIR
    output:
        directory("params_json")
    shell:
        """
        mkdir -p params_json
        qg-tools csv-to-params {input.csv_dir}/ -o params_json/
        """


# =============================================================================
# Phase 2: Queue Generator Testing
# =============================================================================

rule generate_queue:
    """Run queue generator on a param file."""
    input:
        params="params_json/{name}.json"
    output:
        csv="generated/{name}.csv"
    shell:
        """
        cd .. && qg test_data/{input.params} -o test_data/{output.csv} 2>/dev/null || touch test_data/{output.csv}
        """


rule compare:
    """Compare generated queue CSV with original reference CSV."""
    input:
        generated="generated/{name}.csv",
        config="params_json/{name}.json"
    output:
        result="results/{name}.json"
    params:
        original=lambda wildcards: get_original_csv(wildcards.name)
    shell:
        """
        qg-tools compare {input.generated} {params.original} {input.config} {output.result}
        """


rule summary:
    """Aggregate comparison results into test_results.csv database."""
    input:
        get_comparison_results
    output:
        "results/test_results.csv"
    shell:
        "qg-tools summarize {output} {input}"


# =============================================================================
# Analysis App
# =============================================================================

rule marimo_app:
    """Launch test results marimo app (compare original vs generated)."""
    input:
        "results/test_results.csv"
    shell:
        "marimo run ../src/qg/tools_apps/compare_existing_generated.py"


# =============================================================================
# Cleanup
# =============================================================================

rule clean:
    """Remove generated/ and results/ (keeps params)."""
    shell:
        """
        rm -rf generated/ results/
        echo "Cleaned generated/ and results/"
        """


rule clean_all:
    """Remove ALL intermediates."""
    shell:
        """
        rm -rf generated/ results/ params_json/
        echo "Cleaned all intermediates."
        """
