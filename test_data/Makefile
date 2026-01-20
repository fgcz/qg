# Makefile for queue generator test workflows

SNAKEMAKE = snakemake
JOBS = 4

.PHONY: help sld csvqg clean_sld clean_csvqg

help:
	@echo "Queue Generator Test Workflows"
	@echo ""
	@echo "Targets:"
	@echo "  sld         Run SLD analysis pipeline and launch marimo app"
	@echo "  csvqg       Run CSV queue validation pipeline and launch marimo app"
	@echo "  clean_sld   Clean SLD pipeline outputs"
	@echo "  clean_csvqg Clean CSV queue validation outputs"

# SLD Analysis (marimo_queue_analysis.smk)
sld:
	$(SNAKEMAKE) -s marimo_queue_analysis.smk -j$(JOBS) -q all all
	$(SNAKEMAKE) -s marimo_queue_analysis.smk -j1 marimo_app

# CSV Queue Generator Validation (marimo_orig_qgen.smk)
csvqg:
	$(SNAKEMAKE) -s marimo_orig_qgen.smk -j$(JOBS) -q all all
	$(SNAKEMAKE) -s marimo_orig_qgen.smk -j1 marimo_app

# Cleanup
clean_sld:
	$(SNAKEMAKE) -s marimo_queue_analysis.smk -j1 clean

clean_csvqg:
	$(SNAKEMAKE) -s marimo_orig_qgen.smk -j1 clean
