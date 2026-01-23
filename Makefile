.DEFAULT_GOAL := help

.PHONY: help app editor validate projects

help:
	@echo "Queue Generation System"
	@echo ""
	@echo "Usage: make <target>"
	@echo ""
	@echo "Targets:"
	@echo "  app        Run the marimo GUI app"
	@echo "  editor     Run the config editor app"
	@echo "  validate   Validate all configuration files"
	@echo "  projects   Fetch projects from B-Fabric"

# Run the marimo GUI app
app:
	uv run marimo run src/qg/apps/queue_app.py

# Run the config editor
editor:
	uv run marimo run src/qg/apps/config_editor.py

# Validate all configuration files
validate:
	uv run qg-validate

# Fetch projects from B-Fabric
projects:
	uv run qg-find-projects

deploy-test:
	docker compose -f docker-compose-test.yml build app
	docker compose -f docker-compose-test.yml up -d --force-recreate
