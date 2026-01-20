# Makefile for queue generator test workflows

SNAKEMAKE = snakemake
JOBS = 4

.PHONY: help sld csvqg sld-app csvqg-app clean clean-all

help:
	@echo "Queue Generator Test Workflows"
	@echo ""
	@echo "Targets:"
	@echo "  sld       Run SLD analysis pipeline (extract → convert → merge)"
	@echo "  sld-app   Launch marimo queue analysis app"
	@echo "  csvqg     Run CSV queue validation pipeline (params → generate → compare)"
	@echo "  csvqg-app Launch marimo comparison app"
	@echo "  clean     Clean generated outputs (keeps source data)"
	@echo "  clean-all Clean all intermediates (keeps zip archives)"
	@echo ""
	@echo "Usage:"
	@echo "  make sld       # Process SLD files from reference/sld/*.zip"
	@echo "  make csvqg     # Validate queue generator against B-Fabric dump"

# SLD Analysis (marimo_queue_analysis.smk)
sld:
	$(SNAKEMAKE) -s marimo_queue_analysis.smk -j$(JOBS) all

sld-app:
	$(SNAKEMAKE) -s marimo_queue_analysis.smk marimo_app

sld-clean:
	$(SNAKEMAKE) -s marimo_queue_analysis.smk clean

# CSV Queue Generator Validation (marimo_orig_qgen.smk)
csvqg:
	$(SNAKEMAKE) -s marimo_orig_qgen.smk -j$(JOBS) all

csvqg-app:
	$(SNAKEMAKE) -s marimo_orig_qgen.smk marimo_app

csvqg-clean:
	$(SNAKEMAKE) -s marimo_orig_qgen.smk clean

# Combined cleanup
clean: sld-clean csvqg-clean

clean-all:
	$(SNAKEMAKE) -s marimo_queue_analysis.smk clean
	$(SNAKEMAKE) -s marimo_orig_qgen.smk clean_all
