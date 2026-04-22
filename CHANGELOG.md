# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/).

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
