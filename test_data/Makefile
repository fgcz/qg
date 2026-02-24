# Makefile for queue generator test workflows

SNAKEMAKE = snakemake
JOBS = 4

.PHONY: help sld clean_sld

help:
	@echo "Queue Generator Test Workflows"
	@echo ""
	@echo "Targets:"
	@echo "  sld         Run SLD analysis pipeline and launch marimo app"
	@echo "  clean_sld   Clean SLD pipeline outputs"

# SLD Analysis (marimo_queue_analysis.smk)
sld:
	$(SNAKEMAKE) -s marimo_queue_analysis.smk -j$(JOBS) -q all all
	$(SNAKEMAKE) -s marimo_queue_analysis.smk -j1 marimo_app

# Cleanup
clean_sld:
	$(SNAKEMAKE) -s marimo_queue_analysis.smk -j1 clean
