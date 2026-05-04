# Deployment

Reference for the queue-gen deployments on `fgcz-r-039`. Both run as the `bfabric` user. The README has the short version; this file fills in the details.

> **Security:** never set `QG_ALLOW_UNAUTHENTICATED=1` on a deployment host (in `.env`, the environment, or anywhere else) — it disables authentication and runs every request as an employee.

## Architecture

Both apps are served by uvicorn (FastAPI + B-Fabric auth + marimo ASGI) and run continuously — when a user lands on the app from B-Fabric, they hit an already-running server. Code and configs (`qg_configs/`) are baked into the Docker image, so picking up config changes merged on GitLab requires a rebuild.

| Deployment | Source | Image build | Working directory |
|------------|--------|-------------|-------------------|
| Production | Tagged release on `main` | GitLab CI | `~/web-apps/portal/queue-gen` (managed by the [web-apps repo](https://gitlab.bfabric.org/proteomics/web-apps)) |
| Test | `git pull` of this repo | Local `docker compose build` | `/scratch/A401_queue_gen` |

Currently only the queue app is part of the production deployment; the config editor runs from the test deployment only.

## Production

Releases are two stages: tag → GitLab CI builds the image; then bump the pinned version in `web-apps` and redeploy.

### 1. Build the OCI image

Create a Git tag on `main` matching the version already bumped in `pyproject.toml`/`CHANGELOG.md` (see [Release Process](../CLAUDE.md#release-process)). The tag can be created locally (`git tag … && git push --tags`) or directly in the GitLab UI under *Repository → Tags → New tag*. **Do not force-push tags.**

GitLab CI cross-builds a `linux/arm64` OCI archive and writes it to:

```
/misc/container/gitlab/metabolomics/queue_gen/queue_gen-<tag>.oci.tar
```

### 2. Bump and deploy via web-apps

Update the pinned image version in `portal/queue-gen/docker-compose.prod.yml` in the [web-apps repo](https://gitlab.bfabric.org/proteomics/web-apps) and commit so the deployed configuration stays recoverable. Then on `fgcz-r-039`:

```bash
ssh bfabric@localhost
cd ~/web-apps/portal/queue-gen
git pull
make deploy
```

## Test deployment

Built locally on the server from a checked-out branch. Used to verify changes end-to-end before tagging a production release.

| App | Description | Compose file | Port | Make target |
|-----|-------------|--------------|------|-------------|
| Queue app | B-Fabric authenticated queue generation GUI | `docker-compose-test.yml` | 9505 | `deploy-app` |
| Config editor | Edit config files with GitLab review/merge-request workflow | `docker-compose-editor.yml` | 9506 | `deploy-editor` |

### Mounts

- **Queue app:** `bfabric_cache/` is bind-mounted from the host, so `make projects` or pressing "Refresh Projects" in the GUI updates the project list without a rebuild.
- **Config editor:** the only external mount is `~/.qg_settings.toml` (GitLab API credentials for the review/MR workflow). The editor reads configs from the image, diffs changes in memory, and submits them to GitLab via the Commits API — it never writes to disk.

### First-time setup

```bash
ssh bfabric@localhost
mkdir -p /scratch/A401_queue_gen
cd /scratch/A401_queue_gen
git clone https://gitlab.bfabric.org/metabolomics/queue-gen.git .
```

For the config editor only, create `~/.qg_settings.toml` with a GitLab token (scope `api`, or `write_repository + read_api`):

```bash
cp .qg_settings.toml.example ~/.qg_settings.toml
# Edit ~/.qg_settings.toml and set private_token = "glpat-…"
```

The token may alternatively be supplied via the `QG_GITLAB_TOKEN` environment variable.

### Deploy / update

```bash
ssh bfabric@localhost
cd /scratch/A401_queue_gen
git pull
make deploy-app     # Queue app, port 9505
make deploy-editor  # Config editor, port 9506
```

### Verify

```bash
curl -k https://localhost:9505    # Queue app
curl -k https://localhost:9506    # Config editor
```
