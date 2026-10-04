#!/usr/bin/env python3
"""Run the full local landing gate once, in the order the landing policy requires.

AGENTS.md, "Landing a branch": the full gate is everything CI runs plus the API audit
and the published-artifact consumer, and the audit is recorded first, because
DiagnosticCoverageJvmSuite reads the committed inventory; a testAll that runs before
the re-record passes against a stale one. The steps, in order:

  1. hygiene    sbt headerCheckAll scalafmtCheckAll scalafmtSbtCheck githubWorkflowCheck
  2. werror     GITHUB_ACTIONS=true sbt 'project rootJVM' Test/compile 'project rootJS' Test/compile
  3. audit      python3 tools/api-audit/run.py --record
  4. tests      sbt testAll checkBoundaries
  5. consumer   python3 tools/study-consumer/verify.py

The gate stops at the first failing step and names it. Each step's output goes to
target/landing-gate/<step>.log. At the end it prints the SHA and HEAD^{tree} the gate
covered, and whether the audit re-recorded tools/api-audit/inventory.json or
evidence.json; those must then be committed, and the gate covers the tree that
includes them.

Forked test JVMs prefer IPv6 for their handshake with sbt on local builds (build.sbt,
forkHandshakeOptions), so no JAVA_TOOL_OPTIONS workaround is needed.
"""
import argparse
import os
from pathlib import Path
import subprocess
import sys
import time

ROOT = Path(__file__).resolve().parents[2]
LOGS = ROOT / "target" / "landing-gate"
SBT = ["sbt", "-batch", "-J-Xmx6g"]

STEPS = [
    (
        "hygiene",
        SBT
        + [
            "headerCheckAll",
            "scalafmtCheckAll",
            "scalafmtSbtCheck",
            "githubWorkflowCheck",
        ],
        {},
    ),
    (
        "werror",
        SBT + ["project rootJVM", "Test/compile", "project rootJS", "Test/compile"],
        {"GITHUB_ACTIONS": "true"},
    ),
    ("audit", [sys.executable, "tools/api-audit/run.py", "--record"], {}),
    ("tests", SBT + ["testAll", "checkBoundaries"], {}),
    ("consumer", [sys.executable, "tools/study-consumer/verify.py"], {}),
]
NAMES = [name for name, _, _ in STEPS]
AUDIT_FILES = ["tools/api-audit/inventory.json", "tools/api-audit/evidence.json"]


def git(*args: str) -> str:
    return subprocess.run(
        ["git", *args], cwd=ROOT, check=True, capture_output=True, text=True
    ).stdout.strip()


def main() -> int:
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument(
        "--from",
        dest="start",
        choices=NAMES,
        default=NAMES[0],
        help="resume at this step after fixing a failure (earlier steps are not rerun)",
    )
    parser.add_argument(
        "--dry-run", action="store_true", help="print the steps without running them"
    )
    args = parser.parse_args()

    dirty = git("status", "--porcelain", "--untracked-files=no")
    if dirty and not args.dry_run:
        print("The worktree has uncommitted changes; the gate covers a committed tree:")
        print(dirty)
        return 2

    head, tree = git("rev-parse", "HEAD"), git("rev-parse", "HEAD^{tree}")
    for name, command, extra in STEPS[NAMES.index(args.start) :]:
        shown = "".join(f"{k}={v} " for k, v in extra.items())
        print(f"[{name}] {shown}{' '.join(command)}", flush=True)
        if args.dry_run:
            continue
        LOGS.mkdir(parents=True, exist_ok=True)
        log = LOGS / f"{name}.log"
        started = time.monotonic()
        with log.open("w") as out:
            result = subprocess.run(
                command,
                cwd=ROOT,
                env={**os.environ, **extra},
                stdout=out,
                stderr=subprocess.STDOUT,
            )
        minutes = (time.monotonic() - started) / 60
        if result.returncode != 0:
            print(
                f"[{name}] FAILED after {minutes:.1f} min (exit {result.returncode}); see {log}"
            )
            return 1
        print(f"[{name}] ok in {minutes:.1f} min", flush=True)

    if args.dry_run:
        return 0
    if git("rev-parse", "HEAD") != head:
        print("HEAD moved while the gate ran; rerun it for the new commit.")
        return 1
    print(f"Gate passed for {head} (tree {tree}).")
    changed = git("status", "--porcelain", "--", *AUDIT_FILES)
    if changed:
        print("The audit re-recorded these files; commit them before landing:")
        print(changed)
    return 0


if __name__ == "__main__":
    sys.exit(main())
