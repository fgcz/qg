.DEFAULT_GOAL := help

.PHONY: help app app-all app-type app-review editor editor-review validate projects projects-all projects-plates _check-not-fgcz

help:
	@echo "Queue Generation System"
	@echo ""
	@echo "Usage: make <target>"
	@echo ""
	@echo "Targets:"
	@echo "  app              Run the marimo GUI app (active projects)"
	@echo "  app-all          Run the marimo GUI app (all projects)"
	@echo "  app-type         Run the marimo GUI app (with Vial/Plate type column)"
	@echo "  app-review       Run the queue app with git pull (production)"
	@echo "  editor           Run the config editor app (no review workflow)"
	@echo "  editor-review    Run the config editor with git pull (review workflow)"
	@echo "  validate         Validate all configuration files"
	@echo "  projects         Fetch active projects from B-Fabric (fast)"
	@echo "  projects-all     Fetch all projects from B-Fabric (no status filter)"
	@echo "  projects-plates  Fetch active projects with plate detection (slow)"
	@echo ""
	@echo "Production deployments live under ../web-apps/portal/."

# Guard: dev targets below set QG_ALLOW_UNAUTHENTICATED=1 (disables auth). Refuse on fgcz* hosts.
_check-not-fgcz:
	@if hostname | grep -qi fgcz; then echo "Refusing dev-mode target on fgcz host."; exit 1; fi

# Run the marimo GUI app (active projects)
app: _check-not-fgcz
	QG_ALLOW_UNAUTHENTICATED=1 uv run marimo run src/qg/apps/queue_app.py

# Run the marimo GUI app (all projects)
app-all: _check-not-fgcz
	QG_ALLOW_UNAUTHENTICATED=1 uv run marimo run src/qg/apps/queue_app.py -- --all-projects

# Run the marimo GUI app (with Vial/Plate type column from cache)
app-type: _check-not-fgcz
	QG_ALLOW_UNAUTHENTICATED=1 uv run marimo run src/qg/apps/queue_app.py -- --container-type

# Run the config editor (no review workflow)
editor:
	uv run marimo run src/qg/apps/config_editor.py -- --no-review

# Run the queue app with git pull (production)
app-review:
	uv run qg-app

# Run the config editor with git pull (review workflow)
editor-review:
	uv run qg-editor

# Validate all configuration files
validate:
	uv run qg-validate

# Fetch active projects from B-Fabric
projects:
	uv run qg-find-projects

# Fetch all projects from B-Fabric (no status filter)
projects-all:
	uv run qg-find-projects --all

# Fetch active projects with plate detection (slow)
projects-plates:
	uv run qg-find-projects --check-plates
