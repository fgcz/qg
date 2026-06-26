# B-Fabric portal (FGCZ)

Most users never need this page. `qg` runs standalone — upload a sample table, get
a queue — with no FGCZ account (see the [local app](users/local_app.md)). This page
covers the **B-Fabric portal app**: the FGCZ deployment that browses LIMS orders,
loads samples directly, and uploads the generated queue back as a workunit. It
requires the `qg[bfabric]` extra and a B-Fabric-authenticated session.

## Install the portal extra

```bash
# Adds B-Fabric auth, LIMS sample loading, workunit upload, and the GitLab launcher
pip install 'qg[bfabric]'
uv sync                        # for development: installs the portal extra by default
```

The core install (`pip install qg`) has no `bfabric`, `fastapi`, `starlette`, or
`python-gitlab` dependency; the `qg[bfabric]` extra pulls them in.

## Run the portal app (dev)

```bash
QG_ALLOW_UNAUTHENTICATED=1 uv run marimo run src/qg/apps/queue_app.py
```

The portal app fails closed without a B-Fabric-authenticated request.
`QG_ALLOW_UNAUTHENTICATED=1` bypasses auth for local dev and runs as an employee —
**never set it in production.** The deployed entry point is
`uv run python src/qg/apps/bfabric_app.py` (needs `WebappIntegrationSettings`:
`VALIDATION_BFABRIC_INSTANCE`, `SUPPORTED_BFABRIC_INSTANCES`,
`FEEDER_USER_CREDENTIALS`). For the authentication and employee/non-employee access
model, see [user modes](developers/user_modes.md).

## Seed the B-Fabric project cache

- **Dev** (single instance, local `~/.bfabricpy.yml`): `uv run qg-find-projects`
- **Deployment** (all instances in `feeder_user_credentials`): `uv run qg-refresh-cache --all` — see [deployment](developers/deployment.md)

Both write `bfabric_cache/<instance>/bfabric_container.csv`, which the portal app
reads; its "Refresh Projects" button re-runs the dev-style write for the running
instance. These commands require the `qg[bfabric]` extra.

## Deployment

The queue app and config editor both run on `fgcz-r-039` (as the `bfabric` user)
from a single Docker image, deployed via the web-apps repo. The full procedure —
tag → CI image build, bumping `IMAGE_TAG`, redeploy, rollback, and secrets — lives in
**[deployment](developers/deployment.md)**.

> **Security:** never set `QG_ALLOW_UNAUTHENTICATED=1` on a deployment host — it
> disables auth and runs every request as an employee.

## See also

- [User modes (auth)](developers/user_modes.md) — authentication and the
  employee / non-employee access model.
- [Deployment](developers/deployment.md) — production deployment, cache refresh,
  and secrets.
