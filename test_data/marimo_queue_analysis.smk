"""Queue analysis workflow - starts from SLD files.

Pipeline:
1. Extract SLD files from zip
2. Convert SLD → CSV (sld_to_csv.py)
3. Merge CSVs for analysis
4. Launch marimo queue analysis app

Usage:
    snakemake -s marimo_queue_analysis.smk -j4 all
    snakemake -s marimo_queue_analysis.smk marimo_app
"""

from pathlib import Path

# Directories
REFERENCE_DIR = Path("reference")
INSTRUMENTS = ["astral", "ascend", "lumos2", "exploris2"]


# =============================================================================
# Main Targets
# =============================================================================

rule all:
    """Run complete pipeline: extract → convert → merge."""
    input:
        "reference/merged.csv"


rule help:
    """Show available targets."""
    run:
        print("""
Queue Analysis Workflow - SLD to CSV Pipeline

Source: reference/sld/ (SLD binary files from instruments)

Pipeline:
  1. extract      Extract SLD files from zip archives
  2. sld_to_csv   Convert SLD → CSV
  3. merge        Merge all CSVs into single file
  4. marimo_app   Launch queue analysis viewer

Targets:
  all             Run complete pipeline (up to merge)
  marimo_app      Launch marimo queue analysis app
  clean           Remove CSV files and merged.csv

Usage:
  snakemake -s marimo_queue_analysis.smk -j4 all
  snakemake -s marimo_queue_analysis.smk marimo_app
""")


# =============================================================================
# Phase 0: Extract SLD from Zip
# =============================================================================

rule extract:
    """Extract SLD files from zip archives in reference/zip/ to reference/sld/."""
    output:
        directory("reference/sld")
    shell:
        """
        mkdir -p reference/sld
        for zip in reference/zip/*.zip; do
            if [ -f "$zip" ]; then
                instrument=$(basename "$zip" .zip)
                mkdir -p "reference/sld/$instrument"
                unzip -o -j "$zip" -d "reference/sld/$instrument" '*.sld' 2>/dev/null || true
                echo "Extracted: $zip → reference/sld/$instrument/"
            fi
        done
        """


# =============================================================================
# Phase 1: SLD → CSV Conversion
# =============================================================================

rule sld_to_csv:
    """Convert all SLD files to CSV."""
    input:
        expand("reference/csv/{instrument}", instrument=INSTRUMENTS)


rule convert_sld:
    """Parse all SLD files in an instrument directory to CSV."""
    input:
        sld_dir="reference/sld/{instrument}"
    output:
        csv_dir=directory("reference/csv/{instrument}")
    log:
        "reference/csv/{instrument}.log"
    shell:
        "qg-tools sld-to-csv {input.sld_dir} --output-dir {output.csv_dir} --check --sanitize --log-file {log}"


# =============================================================================
# Phase 2: Merge CSVs
# =============================================================================

rule merge:
    """Merge all CSVs into single file for analysis."""
    input:
        csv_dirs=expand("reference/csv/{instrument}", instrument=INSTRUMENTS)
    output:
        "reference/merged.csv"
    shell:
        "qg-tools merge reference/csv --output {output}"


# =============================================================================
# Analysis App
# =============================================================================

rule marimo_app:
    """Launch queue analysis marimo app."""
    input:
        "reference/merged.csv"
    shell:
        "marimo run ../src/qg/tools_apps/queue_analysis_marimo.py"


# =============================================================================
# Cleanup
# =============================================================================

rule clean:
    """Remove CSV files and merged.csv (keeps SLD files)."""
    shell:
        """
        rm -rf reference/csv/ reference/merged.csv
        echo "Cleaned reference/csv/ and reference/merged.csv"
        """
