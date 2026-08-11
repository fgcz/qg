"""Cut a release: bump the version, date the changelog, commit, tag, and push.

Pushing the ``vx.y.z`` tag to GitHub is what triggers the ``publish`` workflow, which
runs the CI suite and pushes the deployment image to ``ghcr.io/fgcz/qg``. This script
does not build anything itself; it produces the tag that does.

It automates the mechanical half of the release process in ``AGENTS.md`` and refuses to
guess at the editorial half: consolidating and categorizing the ``## [Unreleased]``
entries is still done by hand before running this.
"""

from __future__ import annotations

import argparse
import json
import os
import re
import subprocess
import time
from datetime import UTC, datetime
from pathlib import Path

BRANCH = "main"
UNRELEASED = "## [Unreleased]"
PARTS = ("patch", "minor", "major")
_RUN_POLL_ATTEMPTS = 10
_RUN_POLL_SECONDS = 3


def _run(args: list[str]) -> str:
    """Run a command, returning stdout. Aborts with the captured stderr on failure."""
    result = subprocess.run(args, capture_output=True, text=True, check=False)
    if result.returncode != 0:
        msg = f"command failed ({result.returncode}): {' '.join(args)}\n{result.stderr.strip()}"
        raise SystemExit(msg)
    return result.stdout.strip()


def _repo_root() -> Path:
    return Path(_run(["git", "rev-parse", "--show-toplevel"]))


def _preflight() -> None:
    """Refuse to release from a dirty, detached, or out-of-date checkout."""
    if _run(["git", "status", "--porcelain"]):
        raise SystemExit("working tree is dirty; commit or stash first.")
    branch = _run(["git", "rev-parse", "--abbrev-ref", "HEAD"])
    if branch != BRANCH:
        raise SystemExit(f"on branch {branch}; releases are cut from {BRANCH}.")
    _run(["git", "fetch", "--quiet", "--tags", "origin"])
    if _run(["git", "rev-parse", "HEAD"]) != _run(["git", "rev-parse", f"origin/{BRANCH}"]):
        raise SystemExit(f"{BRANCH} differs from origin/{BRANCH}; pull or push first.")


def _current_version(pyproject: Path) -> str:
    match = re.search(r'^version = "([^"]+)"', pyproject.read_text(encoding="utf-8"), re.MULTILINE)
    if not match:
        raise SystemExit("cannot find the version in pyproject.toml.")
    return match.group(1)


def _bumped(version: str, part: str) -> str:
    major, minor, patch = (int(piece) for piece in version.split("."))
    if part == "major":
        return f"{major + 1}.0.0"
    if part == "minor":
        return f"{major}.{minor + 1}.0"
    return f"{major}.{minor}.{patch + 1}"


def _target_version(current: str, part: str) -> str:
    """Honor the bump-ahead rule: an unreleased version in pyproject.toml is the target."""
    if _run(["git", "tag", "--list", f"v{current}"]):
        return _bumped(current, part)
    print(f"  pyproject.toml already carries the unreleased {current}; releasing that")
    return current


def _unreleased_body(changelog: str) -> str:
    if UNRELEASED not in changelog:
        raise SystemExit(f"CHANGELOG.md has no {UNRELEASED} heading.")
    after = changelog.split(UNRELEASED, 1)[1]
    end = after.find("\n## ")
    return (after if end == -1 else after[:end]).strip()


def _write_version(pyproject: Path, current: str, target: str) -> None:
    text = pyproject.read_text(encoding="utf-8")
    pyproject.write_text(text.replace(f'version = "{current}"', f'version = "{target}"', 1), encoding="utf-8")
    _run(["uv", "lock"])


def _write_changelog(path: Path, target: str, date: str) -> None:
    text = path.read_text(encoding="utf-8")
    path.write_text(text.replace(UNRELEASED, f"{UNRELEASED}\n\n## [{target}] - {date}", 1), encoding="utf-8")


def _publish_run_url(tag: str) -> str | None:
    """Wait for the tag's publish run to register, so the caller can link to the build."""
    for _ in range(_RUN_POLL_ATTEMPTS):
        runs = json.loads(
            _run(["gh", "run", "list", "--workflow", "publish.yml", "--limit", "10", "--json", "headBranch,url"])
        )
        for run in runs:
            if run["headBranch"] == tag:
                return run["url"]
        time.sleep(_RUN_POLL_SECONDS)
    return None


def _release(target: str, current: str, root: Path, *, dry_run: bool) -> None:
    """Write the version and changelog, then commit, tag, and push both."""
    tag = f"v{target}"
    if dry_run:
        return

    changed = ["CHANGELOG.md"]
    if target != current:
        _write_version(root / "pyproject.toml", current, target)
        changed += ["pyproject.toml", "uv.lock"]
    _write_changelog(root / "CHANGELOG.md", target, datetime.now(UTC).strftime("%Y-%m-%d"))

    _run(["git", "add", *changed])
    _run(["git", "commit", "-m", f"chore: cut release {target}"])
    _run(["git", "push", "origin", BRANCH])
    _run(["git", "tag", "-a", tag, "-m", f"Release {target}"])
    _run(["git", "push", "origin", tag])
    print(f"  pushed {tag}")


def main() -> int:
    """Cut a release from the current main and hand the tag to GitHub Actions."""
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--part", choices=PARTS, default="patch", help="which version component to bump")
    parser.add_argument("--dry-run", action="store_true", help="report the planned release without writing or pushing")
    args = parser.parse_args()

    root = _repo_root()
    os.chdir(root)
    if args.dry_run:
        print("DRY RUN — nothing is written, committed, or pushed\n")

    _preflight()
    current = _current_version(root / "pyproject.toml")
    target = _target_version(current, args.part)
    tag = f"v{target}"
    if _run(["git", "tag", "--list", tag]):
        raise SystemExit(f"tag {tag} already exists.")

    body = _unreleased_body((root / "CHANGELOG.md").read_text(encoding="utf-8"))
    if not body:
        raise SystemExit(
            f"nothing under {UNRELEASED} in CHANGELOG.md.\n"
            f"Add a bullet for every change since the last release — config updates merged from the\n"
            f"editor's merge requests do not write one — then re-run."
        )

    last_tag = _run(["git", "describe", "--tags", "--abbrev=0"])
    print(f"\nReleasing {current} -> {target} as {tag}\n")
    print(f"Commits since {last_tag}:")
    print(_run(["git", "log", "--oneline", f"{last_tag}..HEAD"]))
    print(f"\n{UNRELEASED} entries:\n{body}\n")

    _release(target, current, root, dry_run=args.dry_run)

    if args.dry_run:
        print("Dry run complete; re-run without --dry-run to cut the release.")
        return 0

    url = _publish_run_url(tag)
    print(f"\nImage build: {url or 'not registered yet — check gh run list --workflow publish.yml'}")
    print(
        "\nNext, to deploy it (see docs/developers/deployment.md):\n"
        f"  1. wait for the publish run to finish, producing ghcr.io/fgcz/qg:{tag}\n"
        f"  2. set IMAGE_TAG={tag} in portal/queue-gen/.env in the web-apps repo and push\n"
        "  3. on the host: cd ~/webapps/portal/queue-gen && git pull && make deploy"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
