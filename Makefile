.DEFAULT_GOAL := help

# Version component bumped by `make release` (patch, minor or major).
PART ?= patch

.PHONY: help app app-test app-local app-all app-type app-review editor editor-preview editor-review validate settings-init projects projects-all projects-plates sync sync-dry sync-tags release release-dry _check-not-fgcz

help:
	@echo "Queue Generation System"
	@echo ""
	@echo "Usage: make <target>"
	@echo ""
	@echo "Targets:"
	@echo "  app              Run the marimo GUI app against production (active projects)"
	@echo "  app-test         Run the marimo GUI app against the test B-Fabric instance"
	@echo "  app-local        Run the standalone CSV/XLSX upload app (no B-Fabric)"
	@echo "  app-all          Run the marimo GUI app (all projects)"
	@echo "  app-type         Run the marimo GUI app (with Vial/Plate type column)"
	@echo "  app-review       Run the queue app with git pull (production)"
	@echo "  editor           Run the config editor app (no review workflow)"
	@echo "  editor-preview   Run the marimo config editor in preview-only mode"
	@echo "  editor-review    Run the config editor with git pull (review workflow)"
	@echo "  validate         Validate all configuration files"
	@echo "  settings-init    Create .qg_settings.toml from the example (GitLab review)"
	@echo "  projects         Fetch active projects from B-Fabric (fast)"
	@echo "  projects-all     Fetch all projects from B-Fabric (no status filter)"
	@echo "  projects-plates  Fetch active projects with plate detection (slow)"
	@echo "  sync-dry         Preview the GitHub <-> GitLab main sync"
	@echo "  sync             Sync main GitHub -> GitLab, merge config MRs, GitLab -> GitHub"
	@echo "  sync-tags        Sync, and push release tags GitLab lacks (triggers image builds)"
	@echo "  release-dry      Preview the next release (version, changelog, commits)"
	@echo "  release          Bump version, tag and push; GitHub Actions publishes the image"
	@echo ""
	@echo "Production deployments live under ../web-apps/portal/."

# Guard: dev targets below set QG_ALLOW_UNAUTHENTICATED=1 (disables auth). Refuse on fgcz* hosts.
_check-not-fgcz:
	@if hostname | grep -qi fgcz; then echo "Refusing dev-mode target on fgcz host."; exit 1; fi

# Run the marimo GUI app against production (active projects). Keep the instance
# explicit: many developer configs intentionally default to TEST.
app: _check-not-fgcz
	BFABRICPY_CONFIG_ENV=PRODUCTION QG_ALLOW_UNAUTHENTICATED=1 uv run marimo run src/qg/apps/queue_app.py

# Run the marimo GUI app against the test B-Fabric instance.
app-test: _check-not-fgcz
	BFABRICPY_CONFIG_ENV=TEST QG_ALLOW_UNAUTHENTICATED=1 uv run marimo run src/qg/apps/queue_app.py

# Run the standalone local app (CSV/XLSX upload, no B-Fabric, no auth bypass needed)
app-local:
	uv run marimo run src/qg/apps/queue_app_local.py

# Run the marimo GUI app (all projects)
app-all: _check-not-fgcz
	BFABRICPY_CONFIG_ENV=PRODUCTION QG_ALLOW_UNAUTHENTICATED=1 uv run marimo run src/qg/apps/queue_app.py -- --all-projects

# Run the marimo GUI app (with Vial/Plate type column from cache)
app-type: _check-not-fgcz
	BFABRICPY_CONFIG_ENV=PRODUCTION QG_ALLOW_UNAUTHENTICATED=1 uv run marimo run src/qg/apps/queue_app.py -- --container-type

# Run the config editor (no review workflow)
editor: _check-not-fgcz
	QG_ALLOW_UNAUTHENTICATED=1 uv run marimo run src/qg/apps/config_editor.py -- --no-review

# Run the marimo config editor in preview-only mode (validate + reload, no save/review)
editor-preview: _check-not-fgcz
	QG_ALLOW_UNAUTHENTICATED=1 uv run marimo run src/qg/apps/config_editor.py -- --preview-only

# Run the queue app with git pull (production)
app-review:
	uv run qg-app

# Run the config editor with git pull (review workflow)
editor-review:
	uv run qg-editor

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

# Preview the GitHub <-> GitLab main sync without pushing or merging
sync-dry:
	uv run --no-project python scripts/sync_forges.py --dry-run

# Sync main across both forges and merge the config editor's merge requests
sync:
	uv run --no-project python scripts/sync_forges.py

# Same, plus push release tags GitLab is missing (each triggers a GitLab CI image build)
sync-tags:
	uv run --no-project python scripts/sync_forges.py --tags

# Preview the release: version bump, changelog entries, and commits since the last tag
release-dry:
	uv run --no-project python scripts/release.py --part $(PART) --dry-run

# Bump the version, date the changelog, commit, tag and push; the tag makes GitHub
# Actions build and publish ghcr.io/fgcz/qg. Override the bump with: make release PART=minor
release:
	uv run --no-project python scripts/release.py --part $(PART)
