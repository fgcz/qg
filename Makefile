.DEFAULT_GOAL := help

.PHONY: help projects configs queues queues-vanquish validate clean app editor parse-sld

PYTHON := uv run python
CONFIG_DIR := qg_configs
EXAMPLES_DIR := $(CONFIG_DIR)/examples

help:
	@echo "Queue Generation System"
	@echo ""
	@echo "Usage: make <target>"
	@echo ""
	@echo "Targets:"
	@echo "  projects         Fetch projects from B-Fabric (updates bfabric_cache/)"
	@echo "  configs          Generate all config JSONs from B-Fabric"
	@echo "  configs-vanquish Generate configs for Vanquish sampler only"
	@echo "  queues           Generate queue CSVs from all config JSONs"
	@echo "  queues-vanquish  Generate queue CSVs for Vanquish configs only"
	@echo "  validate         Validate all configuration files"
	@echo "  clean            Remove generated examples (JSON + CSV)"
	@echo "  parse-sld        Parse SLD files to CSVs (queue_files/)"
	@echo "  app              Run the marimo GUI app"
	@echo "  editor           Run the config editor app"

# Fetch projects from B-Fabric
projects:
	uv run qg-find-projects

# Generate all config JSONs from B-Fabric
configs:
	$(PYTHON) generate_all_configs.py --user $(USER)

# Generate configs for Vanquish sampler only
configs-vanquish:
	$(PYTHON) generate_all_configs.py --sampler Vanquish --user $(USER)

# Generate queue CSVs from all config JSONs
queues:
	@success=0; fail=0; \
	for json in $(EXAMPLES_DIR)/*.json; do \
		csv="$${json%.json}.csv"; \
		if uv run qg "$$json" -o "$$csv" -q 2>/dev/null; then \
			success=$$((success + 1)); \
		else \
			fail=$$((fail + 1)); \
		fi; \
	done; \
	echo "Queue generation complete: $$success succeeded, $$fail failed"

# Generate queue CSVs for Vanquish configs only
queues-vanquish:
	@success=0; fail=0; \
	for json in $(EXAMPLES_DIR)/*Vanquish*.json; do \
		csv="$${json%.json}.csv"; \
		if uv run qg "$$json" -o "$$csv" -q 2>/dev/null; then \
			success=$$((success + 1)); \
		else \
			fail=$$((fail + 1)); \
		fi; \
	done; \
	echo "Queue generation complete: $$success succeeded, $$fail failed"

# Validate all configuration files
validate:
	uv run qg-validate

# Remove generated examples
clean:
	rm -f $(EXAMPLES_DIR)/*.json $(EXAMPLES_DIR)/*.csv
	@echo "Cleaned $(EXAMPLES_DIR)"

# Run the marimo GUI app
app:
	uv run marimo run src/qg/apps/queue_app.py

# Run the config editor
editor:
	uv run marimo run src/qg/apps/config_editor.py

# Parse SLD files to CSVs (delegates to queue_files/Snakefile)
parse-sld:
	cd queue_files && snakemake -j4 all
