# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

## [Unreleased]

### Fixed
- Lock the `User` field to the B-Fabric login for non-employees.
- Silence marimo "Permission denied" warnings on read-only HOME by redirecting `XDG_CONFIG_HOME` / `XDG_STATE_HOME` to `/tmp` before marimo imports.

## [0.4.0] - 2026-05-08

Multi-instance B-Fabric support: PROD and TEST no longer overwrite each other.

### Added
- `qg-refresh-cache` CLI: refreshes container caches per configured B-Fabric instance using `feeder_user_credentials`. Bare invocation lists instances; `--all` or explicit URLs runs them.
- `QueueParameters.bfabric_instance` stamped on saved `params.json` for provenance.

### Changed
- Container cache is per-instance: `<root>/<instance-host>/bfabric_container*.csv`. Root defaults to `<repo>/bfabric_cache`, override with `$QG_CACHE_DIR`. Re-run `qg-find-projects` once per instance.
- `qg-find-projects` uses bfabricpy's `@use_client` instead of hardcoded `PRODUCTION`.
- Authenticated banner shows the instance host alongside the login.

### Fixed
- Non-employee users can load plate-typed orders; shared-plate samples are filtered to the user's container.

## [0.3.2] - 2026-04-22

### Fixed
- Bumped `bfabric-asgi-auth` (and sibling `bfabric` / `bfabric-rest-proxy` packages from the same `bfabricPy` repo) to pick up the fix that respects `scope["root_path"]`, so the app works correctly when bound behind a reverse proxy at a non-root path.

## [0.3.1] - 2026-04-21

### Changed
- CI `build` job now cross-builds an `arm64` OCI image on the `amd64` GitLab runner via `buildah bud --platform linux/arm64`, matching the ARM64 deploy target. Requires `qemu-user-static` + registered `binfmt_misc` handlers on the runner host (one-time admin setup, kernel-global).
- `qg-find-projects` now includes orders in the `processed` state when listing active projects.

## [0.3.0] - 2026-04-20

### Changed
- Modernized B-Fabric authentication to use upstream `BfabricUser` from `bfabric-asgi-auth`.
- Bumped `bfabric` to 1.18.0 and refreshed related auth dependencies.

## [0.2.0]

Initial tracked release.
