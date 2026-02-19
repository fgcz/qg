.DEFAULT_GOAL := help

.PHONY: help app app-all app-review editor editor-review validate projects projects-all deploy-test deploy-queue deploy-editor

help:
	@echo "Queue Generation System"
	@echo ""
	@echo "Usage: make <target>"
	@echo ""
	@echo "Targets:"
	@echo "  app            Run the marimo GUI app (active projects)"
	@echo "  app-all        Run the marimo GUI app (all projects)"
	@echo "  app-review     Run the queue app with git pull (production)"
	@echo "  editor         Run the config editor app"
	@echo "  editor-review  Run the config editor with git pull (review workflow)"
	@echo "  validate       Validate all configuration files"
	@echo "  projects       Fetch active projects from B-Fabric"
	@echo "  projects-all   Fetch all projects from B-Fabric (no status filter)"
	@echo "  deploy-test    Build and start Docker test deployment (bfabric app, port 9505)"
	@echo "  deploy-queue   Build and start queue app (marimo, port 9506)"
	@echo "  deploy-editor  Build and start config editor (marimo, port 9507)"

# Run the marimo GUI app (active projects)
app:
	uv run marimo run src/qg/apps/queue_app.py

# Run the marimo GUI app (all projects)
app-all:
	uv run marimo run src/qg/apps/queue_app.py -- --all-projects

# Run the config editor
editor:
	uv run marimo run src/qg/apps/config_editor.py

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

deploy-test:
	docker compose -f docker-compose-test.yml build app
	docker compose -f docker-compose-test.yml up -d --force-recreate

deploy-queue:
	docker compose -f docker-compose-queue.yml build queue-app
	docker compose -f docker-compose-queue.yml up -d --force-recreate

deploy-editor:
	docker compose -f docker-compose-editor.yml build config-editor
	docker compose -f docker-compose-editor.yml up -d --force-recreate
