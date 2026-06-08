# Documentation build targets
#
# The docs site is built with MkDocs Material (config: ../mkdocs.yml). This
# Makefile provides thin wrappers around mkdocs.
#
# Requires: uv.

.PHONY: all serve build clean

all: build

# Live-preview the site (http://127.0.0.1:8000).
serve:
	cd .. && uv run --only-group docs mkdocs serve

# Build the static site into ../public (same as the CI pages job).
build:
	cd .. && uv run --only-group docs mkdocs build

clean:
	rm -rf ../public
