"""Sync ``main`` between the GitHub development repo and the GitLab config-review repo.

Development happens on GitHub (``fgcz/qg``); the config editor opens merge requests
against GitLab (``bioinformatics/qg``), so config changes enter there. One cycle:

1. fast-forward GitLab ``main`` onto GitHub ``main``;
2. merge every open ``config-update/*`` merge request into GitLab ``main``;
3. fast-forward GitHub ``main`` onto GitLab ``main``.

Every push is fast-forward only and no history is rewritten, so the cycle is safe to
repeat: with both sides equal and no open merge requests it does nothing. If the two
``main`` branches have diverged the run aborts and asks for a manual merge instead of
guessing.

Requires ``glab`` authenticated against gitlab.bfabric.org, SSH access to GitLab
(Maintainer role, to push ``main`` and merge), and git credentials for GitHub.
"""

from __future__ import annotations

import argparse
import json
import subprocess
import time
from pathlib import Path
from typing import Any
from urllib.parse import quote

GITHUB_URL = "https://github.com/fgcz/qg.git"
GITLAB_HOST = "gitlab.bfabric.org"
GITLAB_PROJECT = "bioinformatics/qg"
GITLAB_URL = f"git@{GITLAB_HOST}:{GITLAB_PROJECT}.git"
BRANCH = "main"
EDITOR_BRANCH_PREFIX = "config-update/"

CACHE_REPO = Path.home() / ".cache" / "qg-forge-sync.git"
PROJECT_API = f"projects/{quote(GITLAB_PROJECT, safe='')}"

_PENDING_MERGE_STATUS = frozenset({"checking", "unchecked", "preparing"})
_POLL_ATTEMPTS = 20
_POLL_SECONDS = 3


def _run(args: list[str]) -> str:
    """Run a command, returning stdout. Aborts with the captured stderr on failure."""
    result = subprocess.run(args, capture_output=True, text=True, check=False)
    if result.returncode != 0:
        msg = f"command failed ({result.returncode}): {' '.join(args)}\n{result.stderr.strip()}"
        raise SystemExit(msg)
    return result.stdout.strip()


def _git(*args: str) -> str:
    return _run(["git", "--git-dir", str(CACHE_REPO), *args])


def _glab(endpoint: str, method: str = "GET", fields: dict[str, str] | None = None) -> Any:
    args = ["glab", "api", "--hostname", GITLAB_HOST, "--method", method, endpoint]
    for key, value in (fields or {}).items():
        args += ["--field", f"{key}={value}"]
    return json.loads(_run(args))


def _ensure_cache_repo() -> None:
    """Create the bare working clone on first run and point both remotes at the forges."""
    if not (CACHE_REPO / "HEAD").is_file():
        CACHE_REPO.parent.mkdir(parents=True, exist_ok=True)
        _run(["git", "init", "--bare", "--quiet", str(CACHE_REPO)])
    existing = _git("remote").split()
    for name, url in (("github", GITHUB_URL), ("gitlab", GITLAB_URL)):
        _git("remote", "set-url" if name in existing else "add", name, url)


def _fetch(remote: str, *, with_tags: bool = False) -> str:
    """Fetch the sync branch from *remote* and return its commit."""
    refspecs = [f"+refs/heads/{BRANCH}:refs/remotes/{remote}/{BRANCH}"]
    if with_tags:
        refspecs.append("+refs/tags/*:refs/tags/*")
    _git("fetch", "--quiet", "--prune", remote, *refspecs)
    return _git("rev-parse", f"refs/remotes/{remote}/{BRANCH}")


def _is_ancestor(candidate: str, descendant: str) -> bool:
    result = subprocess.run(
        ["git", "--git-dir", str(CACHE_REPO), "merge-base", "--is-ancestor", candidate, descendant],
        capture_output=True,
        text=True,
        check=False,
    )
    return result.returncode == 0


def _relation(left: str, right: str) -> str:
    """Return how *left* relates to *right*: equal, behind, ahead or diverged."""
    if left == right:
        return "equal"
    if _is_ancestor(left, right):
        return "behind"
    if _is_ancestor(right, left):
        return "ahead"
    return "diverged"


def _diverged(target: str, source: str) -> SystemExit:
    return SystemExit(
        f"{target} {BRANCH} and {source} {BRANCH} have diverged; both carry commits the other lacks.\n"
        f"Resolve by hand (merge the two histories on GitHub, push, then re-run), because a\n"
        f"fast-forward would drop one side's work."
    )


def _fast_forward(remote: str, source_name: str, target_sha: str, source_sha: str, *, dry_run: bool) -> None:
    """Fast-forward *remote*'s branch to *source_sha*, or explain why nothing is pushed."""
    relation = _relation(target_sha, source_sha)
    if relation == "diverged":
        raise _diverged(remote, source_name)
    if relation == "equal":
        print(f"  {remote} {BRANCH} already matches {source_name} ({target_sha[:8]})")
        return
    if relation == "ahead":
        print(f"  {remote} {BRANCH} is ahead of {source_name}; nothing to push here")
        return
    print(f"  {remote} {BRANCH}: {target_sha[:8]} -> {source_sha[:8]} (fast-forward)")
    if not dry_run:
        _git("push", remote, f"{source_sha}:refs/heads/{BRANCH}")


def _open_editor_mrs() -> list[dict[str, Any]]:
    mrs = _glab(f"{PROJECT_API}/merge_requests?state=opened&target_branch={BRANCH}&per_page=100")
    return [mr for mr in mrs if mr["source_branch"].startswith(EDITOR_BRANCH_PREFIX)]


def _settled_mr(iid: int) -> dict[str, Any]:
    """Poll a merge request until GitLab has finished recomputing its mergeability."""
    mr = _glab(f"{PROJECT_API}/merge_requests/{iid}")
    for _ in range(_POLL_ATTEMPTS):
        if mr.get("detailed_merge_status") not in _PENDING_MERGE_STATUS:
            return mr
        time.sleep(_POLL_SECONDS)
        mr = _glab(f"{PROJECT_API}/merge_requests/{iid}")
    return mr


def _merge_editor_mrs(*, dry_run: bool) -> tuple[int, list[str]]:
    """Merge every open config-editor merge request.

    Returns the number merged and one report line per merge request left open.
    """
    mrs = _open_editor_mrs()
    if not mrs:
        print("  no open config-editor merge requests")
        return 0, []

    merged = 0
    unmerged: list[str] = []
    for listed in mrs:
        iid = listed["iid"]
        mr = _settled_mr(iid)
        status = mr.get("detailed_merge_status")
        if status != "mergeable":
            print(f"  !{iid} left open: {status}")
            unmerged.append(f"!{iid} {mr['source_branch']} ({mr['title']}): {status}")
            continue
        print(f"  merging !{iid} {mr['title']}")
        merged += 1
        if not dry_run:
            _glab(f"{PROJECT_API}/merge_requests/{iid}/merge", "PUT", {"sha": mr["sha"]})
    return merged, unmerged


def _remote_tags(url: str) -> set[str]:
    lines = _run(["git", "ls-remote", "--tags", url]).splitlines()
    return {line.split("refs/tags/", 1)[1] for line in lines if "refs/tags/" in line and not line.endswith("^{}")}


def _push_missing_tags(*, dry_run: bool) -> None:
    """Push release tags that exist on GitHub but not on GitLab."""
    missing = sorted(_remote_tags(GITHUB_URL) - _remote_tags(GITLAB_URL))
    if not missing:
        print("  GitLab has every GitHub tag")
        return
    print(f"  pushing {len(missing)} tag(s), each triggering a GitLab CI image build: {', '.join(missing)}")
    if not dry_run:
        _git("push", "gitlab", *[f"refs/tags/{tag}:refs/tags/{tag}" for tag in missing])


def main() -> int:
    """Run one GitHub -> GitLab -> merge -> GitHub sync cycle."""
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--dry-run", action="store_true", help="report the planned actions without pushing or merging")
    parser.add_argument("--tags", action="store_true", help="also push GitHub release tags missing on GitLab")
    args = parser.parse_args()

    if args.dry_run:
        print("DRY RUN — no pushes, no merges\n")

    _ensure_cache_repo()
    github_sha = _fetch("github", with_tags=args.tags)
    gitlab_sha = _fetch("gitlab")
    print(f"GitHub  {BRANCH}: {github_sha[:8]}")
    print(f"GitLab  {BRANCH}: {gitlab_sha[:8]}\n")

    print("1. GitHub -> GitLab")
    _fast_forward("gitlab", "GitHub", gitlab_sha, github_sha, dry_run=args.dry_run)

    print("2. config-editor merge requests")
    merged, unmerged = _merge_editor_mrs(dry_run=args.dry_run)

    print("3. GitLab -> GitHub")
    gitlab_sha = _fetch("gitlab")
    if args.dry_run and merged:
        print(f"  github {BRANCH}: {github_sha[:8]} -> GitLab {BRANCH} once the merge(s) above land (fast-forward)")
    else:
        _fast_forward("github", "GitLab", github_sha, gitlab_sha, dry_run=args.dry_run)

    if args.tags:
        print("4. release tags")
        _push_missing_tags(dry_run=args.dry_run)

    if args.dry_run:
        print("\nDry run complete; re-run without --dry-run to apply.")
        return 0

    github_sha = _fetch("github")
    gitlab_sha = _fetch("gitlab")
    print(f"\nGitHub {BRANCH}: {github_sha[:8]}\nGitLab {BRANCH}: {gitlab_sha[:8]}")
    if unmerged:
        print("\nMerge requests left open:")
        for line in unmerged:
            print(f"  {line}")
        return 1
    if github_sha != gitlab_sha:
        print("\nBranches still differ; re-run to finish the cycle.")
        return 1
    print("In sync.")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
