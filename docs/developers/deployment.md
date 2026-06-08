# Deployment

Both queue-gen apps (queue app + config editor) run as the `bfabric` user on `fgcz-r-039`, served by uvicorn from a single Docker image. Deployment is managed via the [web-apps repo](https://gitlab.bfabric.org/proteomics/web-apps) checked out at `~/webapps` on the host.

> **Security:** never set `QG_ALLOW_UNAUTHENTICATED=1` on a deployment host — it disables auth and runs every request as an employee.

## Update to a new version (common case)

1. Tag the release on `main` via the [Tags page](https://gitlab.bfabric.org/metabolomics/queue-gen/-/tags) (or locally: `git tag … && git push --tags`). GitLab CI builds the OCI image automatically. **Do not force-push tags.**
2. In the [web-apps repo](https://gitlab.bfabric.org/proteomics/web-apps), bump `IMAGE_TAG` in `portal/queue-gen/.env` and commit. Both `queue-gen` and `queue-gen-editor` services share this file.
3. Deploy on the host:

   ```bash
   ssh -J fgcz-r-039 bfabric@localhost
   cd ~/webapps/portal/queue-gen && git pull && make deploy
   ```

That's it. Versions in `pyproject.toml`/`CHANGELOG.md` should already match the tag (see the Release Process section in [`AGENTS.md`](https://gitlab.bfabric.org/metabolomics/queue-gen/-/blob/main/AGENTS.md#release-process)).

To roll back, set `IMAGE_TAG` back to the previous version in `.env` and run `make deploy` again.

## Reference

### Image build

GitLab CI cross-builds a `linux/arm64` OCI archive on every tag and writes it to:

```
/misc/container/gitlab/metabolomics/queue_gen/queue_gen-<tag>.oci.tar
```

Both apps share this image; the editor deployment overrides the entrypoint to `qg.apps.bfabric_app_editor:app`.

### Config editor secrets

The editor opens MRs as the `qg-config-bot` project access token on `gitlab.bfabric.org/metabolomics/queue-gen` (role: Developer, scope: `api`). The requesting employee's login is recorded in the commit message and MR description.

Token + GitLab URL/project live in `~/webapps/portal/config/webapp.secrets.env` (shared with other portal apps, gitignored, `chmod 600`):

```
QG_GITLAB_TOKEN=glpat-...
QG_GITLAB_URL=https://gitlab.bfabric.org
QG_GITLAB_PROJECT=metabolomics/queue-gen
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
