# Documentation build targets
#
# The docs site is built with Zensical (config: ../mkdocs.yml). This
# Makefile provides thin wrappers around zensical.
#
# Requires: uv.

DOCS_PORT ?= 8109
SERVE_ON_PORT ?= $(wildcard $(HOME)/projects/bin/serve-on-port)

.PHONY: all serve build clean

all: build

# Live-preview the site (http://localhost:DOCS_PORT).
serve:
	cd .. && SERVE_PORT=$(DOCS_PORT) $(SERVE_ON_PORT) uv run --only-group docs zensical serve -a localhost:$(DOCS_PORT)

# Build the static site into ../public (same as the CI pages job).
build:
	cd .. && uv run --only-group docs zensical build --clean --strict

clean:
	rm -rf ../public
