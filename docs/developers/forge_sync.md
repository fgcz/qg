# Syncing GitHub and GitLab

The repository lives on two forges with different roles:

| Forge | Project | Role |
|-------|---------|------|
| GitHub | [`fgcz/qg`](https://github.com/fgcz/qg) | development: branches, pull requests, CI merge gate, releases |
| GitLab | [`bioinformatics/qg`](https://gitlab.bfabric.org/bioinformatics/qg) | config review: the config editor opens `config-update/*` merge requests here; CI builds the deployment image on tags |

Both `main` branches must stay identical. Code enters on GitHub, config changes enter
on GitLab, so neither side can simply mirror the other.

## When to run it

There is no automation and no mirroring — the sync is a manual step, run from a
checkout by someone with Maintainer rights. Three occasions:

| Occasion | Command |
|----------|---------|
| The config editor opened a merge request you want reviewed and landed | `make sync` |
| Before cutting a release, so the release commit includes the merged config | `make sync` |
| You want the release tags to exist on GitLab too (optional) | `make sync-tags` |

`make sync-tags` is *not* part of deploying. The deployed image comes from
`ghcr.io/fgcz/qg`, built by GitHub Actions on a GitHub tag; the GitLab tag only
produces the legacy NFS OCI archive nothing consumes. See
[Deployment](deployment.md).

Running it more often than that is harmless: it is idempotent, so a run with
nothing to do prints the state and exits. Running it *less* often is what costs
you — config merge requests pile up on GitLab against an ever-older `main`, and
the chance that a config file was meanwhile edited on GitHub (a conflict you then
resolve by hand) grows with every skipped cycle.

Reviewing the config merge request is still a human step: the sync merges what is
open, it does not judge it. Review the diff in the GitLab UI first, or close the
merge request if it is wrong — a closed merge request is skipped.

## The sync cycle

```bash
make sync-dry   # report the planned actions
make sync       # apply them
```

`scripts/sync_forges.py` runs one cycle:

1. **GitHub → GitLab** — fast-forward GitLab `main` to GitHub `main`, so the config
   merge requests are re-based on current code.
2. **Merge config MRs** — merge every open merge request that targets `main` from a
   `config-update/*` branch, waiting for GitLab to recompute mergeability first. Merge
   requests from other branches are never touched: a hand-written GitLab MR stays a
   human decision.
3. **GitLab → GitHub** — fast-forward GitHub `main` to GitLab `main`, carrying the
   merged config commits back to the development repo.

`make sync-tags` runs the same cycle and additionally pushes GitHub release tags that
GitLab is missing. It is a separate target because **every pushed tag triggers a GitLab
CI build** that writes an OCI archive to NFS — an artifact the portal no longer uses, so
the cost buys only tag parity between the two forges.

> **One-time cleanup.** GitLab is currently missing several *old* tags
> (`v0.8.0`, `v0.8.1`, `v0.9.1`, `v0.9.3`) that predate the move to GitHub. Left alone
> they get pushed on the first `make sync-tags` and trigger a build each. To retire
> them without builds, push them once with the pipeline suppressed:
>
> ```bash
> git push -o ci.skip git@gitlab.bfabric.org:bioinformatics/qg.git v0.8.0 v0.8.1 v0.9.1 v0.9.3
> ```
>
> (Your checkout has only the GitHub `origin` remote, hence the explicit URL.)
>
> After that, `make sync-tags` only ever has the current release tag left to push.

## Guarantees

- **Fast-forward only.** No push uses `--force` and no history is rewritten. If the two
  `main` branches have diverged (each carrying commits the other lacks) the script
  aborts and asks for a manual merge rather than picking a winner.
- **Idempotent.** With both sides equal and no open config merge requests, a run does
  nothing and exits 0. A partial run (for example, a merge request that turned out to
  have conflicts) can be re-run after the cause is fixed.
- **No working-tree side effects.** The script operates on a bare clone under
  `~/.cache/qg-forge-sync.git`, created on first run. Your checkout is never touched.
  Deleting that directory only costs a re-clone.

The flip side of that last point: a sync advances both forges but leaves *your* `main`
behind, so **run `git pull` afterwards**. Otherwise the next commit you push is rejected
as non-fast-forward, and the "incoming" commits are merely the config change you just
merged yourself.

The exit code is 0 when both `main` branches end up at the same commit and every config
merge request was merged, and 1 when something was left for a human: an unmergeable
merge request (conflicts, blocked pipeline) or branches that still differ.

## Requirements

- `glab` authenticated against `gitlab.bfabric.org` (`glab auth status`) — used for all
  merge-request API calls, so the script itself holds no token.
- SSH access to GitLab with the **Maintainer** role on `bioinformatics/qg`: `main` only
  accepts pushes from Developers and above, and only Maintainers may merge.
- Git credentials for GitHub (`gh auth status`, or a credential helper).

## When a merge request will not merge

`make sync` reports the GitLab status verbatim (`broken_status` for conflicts,
`ci_still_running`, `not_approved`, …) and leaves the merge request open. Resolve it in
the GitLab UI, then re-run `make sync`. Conflicts are most likely when the config editor
and a GitHub commit changed the same file under `qg_configs/`.
