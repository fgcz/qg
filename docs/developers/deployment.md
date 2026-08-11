# Deployment

Both queue-gen apps (queue app + config editor) run as the `bfabric` user on `fgcz-r-039`, served by uvicorn from a single Docker image. Deployment is managed via the [web-apps repo](https://gitlab.bfabric.org/proteomics/web-apps) checked out at `~/webapps` on the host.

> **Security:** never set `QG_ALLOW_UNAUTHENTICATED=1` on a deployment host — it disables auth and runs every request as an employee.

## Update to a new version (common case)

1. Cut the release in the qg repo with `make release` (see the Release Process in [`AGENTS.md`](https://github.com/fgcz/qg/blob/main/AGENTS.md#release-process)); it bumps the version, dates the changelog, and pushes the `vx.y.z` tag. The tag starts the GitHub Actions [`publish`](https://github.com/fgcz/qg/actions/workflows/publish.yml) workflow, which runs the CI suite and then pushes the multi-platform image to `ghcr.io/fgcz/qg:vx.y.z` (~10 min). Wait for it to go green — that image *is* the deployment artifact. **Do not force-push tags.**
2. In the [web-apps repo](https://gitlab.bfabric.org/proteomics/web-apps), bump `IMAGE_TAG` in `portal/queue-gen/.env` and commit. Both `queue-gen` and `queue-gen-editor` services share this file.
3. Deploy on the host:

   ```bash
   ssh -J fgcz-r-039 bfabric@localhost
   cd ~/webapps/portal/queue-gen && git pull && make deploy
   ```

That's it.

To roll back, set `IMAGE_TAG` back to the previous version in `.env` and run `make deploy` again.

## Reference

### Image build

The deployed image is `ghcr.io/fgcz/qg:<tag>`, built for `linux/amd64` + `linux/arm64` by the GitHub Actions `publish` workflow on every `v*` tag. The package is public, so the host pulls it without `docker login`. `portal/queue-gen/docker-compose.prod.yml` names it and `make deploy` pulls it.

Both apps share this image; the editor deployment overrides the entrypoint to `qg.apps.bfabric_app_editor:app`.

GitLab CI *also* cross-builds an arm64 OCI archive to NFS on a tag:

```
/misc/container/gitlab/metabolomics/queue_gen/queue_gen-<tag>.oci.tar
```

Nothing in the portal consumes that archive any more — it predates the ghcr registry. It is produced only if the tag is pushed to GitLab (`make sync-tags`), which is optional.

**Configs ship inside the image.** The Dockerfile copies the whole repo, `qg_configs/` included, and no compose service mounts a config volume. A merged config change therefore reaches production only through a new release — there is no config-only deploy. (`qg-app`/`qg-editor` do `git pull` at startup, but the portal entrypoint is uvicorn directly, so that path is not in play here.)

The image is built with `uv sync`, which installs the `qg[bfabric]` portal extra
via the `portal` dependency-group (`[tool.uv] default-groups`) — required for the
B-Fabric auth, LIMS loading, and workunit-upload features. The standalone local
app (`queue_app_local.py`) is not part of this deployment and needs no extra.

### Config editor secrets

The editor opens MRs as the project access token on [`gitlab.bfabric.org/bioinformatics/qg`](https://gitlab.bfabric.org/bioinformatics/qg) (role: Developer, scope: `api`). The requesting employee's login is recorded in the commit message and MR description. Those MRs are merged and carried back to GitHub by `make sync` — see [GitHub/GitLab sync](forge_sync.md).

Token + GitLab URL/project live in `~/webapps/portal/config/webapp.secrets.env` (shared with other portal apps, gitignored, `chmod 600`):

```
QG_GITLAB_TOKEN=glpat-...
QG_GITLAB_URL=https://gitlab.bfabric.org
QG_GITLAB_PROJECT=bioinformatics/qg
```

Token rotation: edit the file, `make deploy`. No image rebuild needed.

### Cache refresh

`qg-refresh-cache` refreshes per-instance container caches using the deployed app's `feeder_user_credentials` (from the same `.env` as the running app).

```bash
qg-refresh-cache                                       # list instances, exit non-zero
qg-refresh-cache --all                                 # refresh all configured instances
qg-refresh-cache https://fgcz-bfabric.uzh.ch/bfabric   # explicit URL(s)
qg-refresh-cache --all --check-plates                  # also probe plate endpoint (slow)
```

Per-instance failures are isolated (logged, other instances continue); exits non-zero if any failed. Outputs go to `<root>/<instance-host>/bfabric_container*.csv`, honoring `$QG_CACHE_DIR`.

`qg-find-projects` remains for the developer workflow against a single instance via a local `bfabricpy.yml`.
