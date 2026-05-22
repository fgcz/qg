# Deployment

Reference for the queue-gen deployments on `fgcz-r-039`. Both run as the `bfabric` user. The README has the short version; this file fills in the details.

> **Security:** never set `QG_ALLOW_UNAUTHENTICATED=1` on a deployment host (in `.env`, the environment, or anywhere else) — it disables authentication and runs every request as an employee.

## Architecture

Both apps are served by uvicorn (FastAPI + B-Fabric auth + marimo ASGI) and run continuously — when a user lands on the app from B-Fabric, they hit an already-running server. Code and configs (`qg_configs/`) are baked into the Docker image, so picking up config changes merged on GitLab requires a rebuild.

| Deployment | Source | Image build | Working directory |
|------------|--------|-------------|-------------------|
| Production | Tagged release on `main` | GitLab CI | `~/web-apps/portal/queue-gen` (managed by the [web-apps repo](https://gitlab.bfabric.org/proteomics/web-apps)) |

Both apps share a single image (the Dockerfile entrypoint is the queue app; the editor deployment overrides the entrypoint to `qg.apps.bfabric_app_editor:app`). Production deployment is managed via the `web-apps/portal/` repo for both.

## Production

Releases are two stages: tag → GitLab CI builds the image; then bump the pinned version in `web-apps` and redeploy.

### 1. Build the OCI image

Create a Git tag on `main` matching the version already bumped in `pyproject.toml`/`CHANGELOG.md` (see [Release Process](../CLAUDE.md#release-process)). The tag can be created locally (`git tag … && git push --tags`) or directly in the GitLab UI under *Repository → Tags → New tag*. **Do not force-push tags.**

GitLab CI cross-builds a `linux/arm64` OCI archive and writes it to:

```
/misc/container/gitlab/metabolomics/queue_gen/queue_gen-<tag>.oci.tar
```

### 2. Bump and deploy via web-apps

Update the pinned `IMAGE_TAG` in `portal/queue-gen/.env` in the [web-apps repo](https://gitlab.bfabric.org/proteomics/web-apps) (both `queue-gen` and `queue-gen-editor` services share that compose file) and commit so the deployed configuration stays recoverable. Then on `fgcz-r-039`:

```bash
ssh bfabric@localhost
cd ~/web-apps/portal/queue-gen && git pull && make deploy
```

### Config editor secrets

The editor needs a GitLab Project Access Token to open MRs. It is the `qg-config-bot` project access token on `gitlab.bfabric.org/metabolomics/queue-gen` (role: Developer, scope: `api`). MRs are opened by that bot user; the requesting employee's login is recorded in the commit message and MR description.

The token and the GitLab URL/project live in `portal/config/webapp.secrets.env` on the deploy host (shared with other portal apps, gitignored, `chmod 600`):

```
QG_GITLAB_TOKEN=glpat-...
QG_GITLAB_URL=https://gitlab.bfabric.org
QG_GITLAB_PROJECT=metabolomics/queue-gen
```

When all three are set, `qg.gitlab.settings.load_gitlab_settings()` skips the file lookup entirely. Token rotation is `chmod 600` edit + `make deploy` — no image rebuild needed.

## Cache refresh

`qg-refresh-cache` refreshes the per-instance container caches in one shot, using the deployed app's `feeder_user_credentials` (loaded from the same `.env` / environment as the running app). It replaces the previous workflow of swapping `~/.bfabricpy.yml` and re-running `qg-find-projects` per instance.

```bash
qg-refresh-cache                                    # lists available instances, exits non-zero
qg-refresh-cache --all                              # refresh every configured instance
qg-refresh-cache https://fgcz-bfabric.uzh.ch/bfabric  # explicit URL(s)
qg-refresh-cache --all --check-plates               # also probe plate endpoint (slow)
```

Per-instance failures are isolated (logged with traceback, other instances continue). The command exits non-zero if any instance failed. Outputs go to the same per-instance subdirectories as `qg-find-projects` (`<root>/<instance-host>/bfabric_container*.csv`), honoring `$QG_CACHE_DIR`.

`qg-find-projects` remains available for the developer workflow against a single instance via a local `bfabricpy.yml`.
