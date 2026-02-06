.DEFAULT_GOAL := help

.PHONY: help app app-all editor validate projects projects-all

help:
	@echo "Queue Generation System"
	@echo ""
	@echo "Usage: make <target>"
	@echo ""
	@echo "Targets:"
	@echo "  app            Run the marimo GUI app (active projects)"
	@echo "  app-all        Run the marimo GUI app (all projects)"
	@echo "  editor         Run the config editor app"
	@echo "  validate       Validate all configuration files"
	@echo "  projects       Fetch active projects from B-Fabric"
	@echo "  projects-all   Fetch all projects from B-Fabric (no status filter)"

# Run the marimo GUI app (active projects)
app:
	uv run marimo run src/qg/apps/queue_app.py

# Run the marimo GUI app (all projects)
app-all:
	uv run marimo run src/qg/apps/queue_app.py -- --all-projects

# Run the config editor
editor:
	uv run marimo run src/qg/apps/config_editor.py

# Validate all configuration files
validate:
	uv run qg-validate

# Fetch active projects from B-Fabric
projects:
	uv run qg-find-projects

# Fetch all projects from B-Fabric (no status filter)
projects-all:
	uv run qg-find-projects --all

deploy-test:
	docker compose -f docker-compose-test.yml build app
	docker compose -f docker-compose-test.yml up -d --force-recreate
