.DEFAULT_GOAL := help

.PHONY: help app app-local app-all app-type app-review editor editor-local editor-review editor-dash validate settings-init projects projects-all projects-plates _check-not-fgcz

help:
	@echo "Queue Generation System"
	@echo ""
	@echo "Usage: make <target>"
	@echo ""
	@echo "Targets:"
	@echo "  app              Run the marimo GUI app (active projects)"
	@echo "  app-local        Run the standalone CSV/XLSX upload app (no B-Fabric)"
	@echo "  app-all          Run the marimo GUI app (all projects)"
	@echo "  app-type         Run the marimo GUI app (with Vial/Plate type column)"
	@echo "  app-review       Run the queue app with git pull (production)"
	@echo "  editor           Run the config editor app (no review workflow)"
	@echo "  editor-local     Run the public Dash config viewer (validation only)"
	@echo "  editor-review    Run the config editor with git pull (review workflow)"
	@echo "  editor-dash      Run the full Dash config editor (validate + save + review)"
	@echo "  validate         Validate all configuration files"
	@echo "  settings-init    Create .qg_settings.toml from the example (GitLab review)"
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

# Run the standalone local app (CSV/XLSX upload, no B-Fabric, no auth bypass needed)
app-local:
	uv run marimo run src/qg/apps/queue_app_local.py

# Run the marimo GUI app (all projects)
app-all: _check-not-fgcz
	QG_ALLOW_UNAUTHENTICATED=1 uv run marimo run src/qg/apps/queue_app.py -- --all-projects

# Run the marimo GUI app (with Vial/Plate type column from cache)
app-type: _check-not-fgcz
	QG_ALLOW_UNAUTHENTICATED=1 uv run marimo run src/qg/apps/queue_app.py -- --container-type

# Run the config editor (no review workflow)
editor: _check-not-fgcz
	QG_ALLOW_UNAUTHENTICATED=1 uv run marimo run src/qg/apps/config_editor.py -- --no-review

# Run the public Dash config viewer (validation only, no save/review/auth)
editor-local:
	uv run qg-config-viewer

# Run the queue app with git pull (production)
app-review:
	uv run qg-app

# Run the config editor with git pull (review workflow)
editor-review:
	uv run qg-editor

# Run the full Dash config editor (validate + save + GitLab review; dev auth bypass)
editor-dash: _check-not-fgcz
	QG_ALLOW_UNAUTHENTICATED=1 uv run qg-editor-dash

# Validate all configuration files
validate:
	uv run qg-validate

# Create .qg_settings.toml (GitLab review workflow) from the example. Never overwrites.
settings-init:
	@if [ -f .qg_settings.toml ]; then \
		echo ".qg_settings.toml already exists -- not overwriting."; \
	else \
		cp .qg_settings.toml.example .qg_settings.toml && \
		echo "Created .qg_settings.toml (gitignored). Edit it: set [gitlab] url, project, private_token."; \
	fi

# Fetch active projects from B-Fabric
projects:
	uv run qg-find-projects

# Fetch all projects from B-Fabric (no status filter)
projects-all:
	uv run qg-find-projects --all

# Fetch active projects with plate detection (slow)
projects-plates:
	uv run qg-find-projects --check-plates
