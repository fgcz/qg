# Contributing

Thank you for improving qg. This repository uses `uv` for environment
management and MkDocs for documentation.

## Development Setup

```bash
uv sync
uv run qg-validate
uv run pytest
```

Run a single test with:

```bash
uv run pytest tests/test_file.py::test_name -v
```

## Documentation

Build or preview the documentation with:

```bash
cd docs
make build
make serve
```

Documentation source lives under `docs/`. Generated site output in `public/` is
not committed.

## Change Notes

Every bugfix or feature MR should add one terse bullet under `## [Unreleased]`
in `CHANGELOG.md`, in the appropriate Keep a Changelog subsection.

## Releases

Only create a release section when cutting a release. When bumping the project
version, run `uv lock` so `uv.lock` records the new version.
