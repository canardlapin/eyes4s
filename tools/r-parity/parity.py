"""Shared harness for the eyesim parity generators.

Every generator that runs R does so through `r_session`:

    with parity.r_session("eyes4s-r-entropy-", eyesim_checkout) as r:
        r.rscript(HERE / "entropy.R", r.eyesim_library, INPUT, output)

The session reads the eyesim revision from baseline.json, installs exactly that Git object from
`git archive` into a temporary library, and runs each R script against the project-local package
library recorded in r-lock.json, inside the R session fixed by session.R. No generator names a
revision, so moving the pin is a change to baseline.json alone.

After every successful script the session checks what R actually loaded: each namespace must be
eyesim from the temporary library, a base package of the locked R version, or a locked package at
its locked version from the project library. See README.md.
"""
from __future__ import annotations

import contextlib
import itertools
import json
import os
from pathlib import Path
import re
import subprocess
import tarfile
import tempfile
from typing import Iterator

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[1]
MANIFEST = HERE / "baseline.json"
R_LOCK = HERE / "r-lock.json"
R_LIBRARY = HERE / "library"
R_SESSION = HERE / "session.R"
REVISION_PATTERN = re.compile(r"[0-9a-f]{40}")
# `--vanilla` without `--no-init-file`: R_PROFILE_USER names session.R instead of a user profile.
R_FLAGS = ["--no-save", "--no-restore", "--no-site-file", "--no-environ"]
# Inherited variables that would change the package library, profile or startup packages.
DROPPED = {
    "R_LIBS",
    "R_LIBS_USER",
    "R_LIBS_SITE",
    "R_PROFILE",
    "R_PROFILE_USER",
    "R_ENVIRON",
    "R_ENVIRON_USER",
    "R_DEFAULT_PACKAGES",
    "R_FUTURE_PLAN",
}
DEPENDENCY_FIELDS = ("Depends", "Imports", "LinkingTo")


def pinned_revision(manifest: Path = MANIFEST) -> str:
    """The eyesim revision pinned in baseline.json, the only place it is written by hand."""
    revision = json.loads(manifest.read_text())["eyesim"]["revision"]
    if not isinstance(revision, str) or REVISION_PATTERN.fullmatch(revision) is None:
        raise SystemExit(
            f"{manifest}: eyesim.revision must be a full lowercase Git SHA"
        )
    return revision


def load_lock() -> dict:
    if not R_LOCK.is_file():
        raise SystemExit(
            f"missing {R_LOCK.relative_to(ROOT)}; run python3 tools/r-parity/provision.py --update"
        )
    return json.loads(R_LOCK.read_text())


def same_version(a: str, b: str) -> bool:
    """R versions compare equal across the DESCRIPTION (1.3-65) and package_version (1.3.65) forms."""
    return a.replace("-", ".") == b.replace("-", ".")


def dcf_field(text: str, field: str) -> str | None:
    """One field of a DESCRIPTION file, with continuation lines joined."""
    match = re.search(
        rf"^{re.escape(field)}:[ \t]*(.*(?:\n[ \t]+.*)*)", text, re.MULTILINE
    )
    return None if match is None else " ".join(match.group(1).split())


def dependency_specs(text: str | None) -> list[tuple[str, str | None, str | None]]:
    """(name, operator, version) for each entry of a Depends/Imports/LinkingTo value, without R."""
    specs = []
    for item in (text or "").split(","):
        match = re.fullmatch(r"\s*([A-Za-z0-9.]+)\s*(?:\(\s*([<>=!]=?)\s*([^)\s]+)\s*\))?\s*", item)
        if match is not None and match.group(1) != "R":
            specs.append((match.group(1), match.group(2), match.group(3)))
    return specs


def dependency_names(text: str | None) -> list[str]:
    """Package names in a Depends/Imports/LinkingTo value, without version constraints or R."""
    return [name for name, _, _ in dependency_specs(text)]


def version_key(version: str) -> tuple[int, ...]:
    """R's package_version ordering: numeric components separated by dots or dashes."""
    return tuple(int(part) for part in re.split(r"[.-]", version) if part.isdigit())


def satisfies(version: str, operator: str | None, bound: str | None) -> bool:
    """Whether `version` meets one DESCRIPTION constraint such as (>= 0.4.11)."""
    if operator is None or bound is None:
        return True
    have, need = version_key(version), version_key(bound)
    return {
        ">=": have >= need,
        ">": have > need,
        "==": have == need,
        "<=": have <= need,
        "<": have < need,
        "!=": have != need,
    }.get(operator, False)


def eyesim_description(checkout: Path, revision: str | None = None) -> str:
    revision = revision or pinned_revision()
    require_revision(checkout, revision)
    return run_text(["git", "-C", str(checkout), "show", f"{revision}:DESCRIPTION"])


def eyesim_requirements(checkout: Path, revision: str | None = None) -> list[str]:
    """The pinned eyesim's Depends/Imports/LinkingTo entries, with any version constraints."""
    text = eyesim_description(checkout, revision)
    return sorted(
        {
            name if operator is None else f"{name} ({operator} {bound})"
            for field in DEPENDENCY_FIELDS
            for name, operator, bound in dependency_specs(dcf_field(text, field))
        }
    )


def require_revision(checkout: Path, revision: str) -> None:
    probe = subprocess.run(
        ["git", "-C", str(checkout), "cat-file", "-e", f"{revision}^{{commit}}"],
        capture_output=True,
        text=True,
    )
    if probe.returncode:
        raise SystemExit(
            f"eyesim checkout {checkout} does not contain the pinned revision {revision}"
        )


def run_text(args: list[str], env: dict[str, str] | None = None) -> str:
    return subprocess.run(
        args, check=True, text=True, capture_output=True, env=env
    ).stdout


def installed_version(library: Path, name: str) -> str | None:
    description = library / name / "DESCRIPTION"
    return (
        dcf_field(description.read_text(), "Version") if description.is_file() else None
    )


def r_environment(tmp: Path, profile: Path | None = R_SESSION) -> dict[str, str]:
    """The environment of every R process the harness starts.

    The library is the locked project library; the user and site libraries are named as paths
    that do not exist, which R drops. `profile` is session.R for scripts and a missing file for
    package installation, so no user profile is ever read.
    """
    env = {key: value for key, value in os.environ.items() if key not in DROPPED}
    absent = str(tmp / "absent")
    env.update(
        LC_ALL="C",
        LANG="C",
        LANGUAGE="en",
        TZ="UTC",
        OMP_NUM_THREADS="1",
        RGL_USE_NULL="TRUE",
        R_LIBS=str(R_LIBRARY),
        R_LIBS_USER=absent,
        R_LIBS_SITE=absent,
        R_PROFILE_USER=str(profile) if profile is not None else absent,
        R_PROFILE=absent,
        R_ENVIRON=absent,
        R_ENVIRON_USER=absent,
        EYES4S_R_LIBRARY=str(R_LIBRARY),
    )
    return env


class RSession:
    """A temporary directory, the locked R library and, once installed, the pinned eyesim."""

    def __init__(self, tmp: Path):
        self.tmp = tmp
        self.lock = load_lock()
        self.env = r_environment(tmp)
        self.eyesim_library: Path | None = None
        self._audits = itertools.count()
        self._check_r_version()
        self._check_library()

    def _check_r_version(self) -> None:
        version = run_text(
            ["Rscript", "--vanilla", "-e", "cat(as.character(getRversion()))"], self.env
        ).strip()
        if version != self.lock["R"]:
            raise SystemExit(
                f"r-lock.json records R {self.lock['R']}, but Rscript is R {version}"
            )

    def _check_library(self) -> None:
        problems = []
        for package in self.lock["packages"]:
            if package["source"] == "R":
                continue
            found = installed_version(R_LIBRARY, package["name"])
            if found is None or not same_version(found, package["version"]):
                problems.append(
                    f"{package['name']} {package['version']} (found {found or 'nothing'})"
                )
        if problems:
            raise SystemExit(
                f"{R_LIBRARY.relative_to(ROOT)} does not match r-lock.json; run "
                "python3 tools/r-parity/provision.py\n- " + "\n- ".join(problems)
            )

    def install_eyesim(self, checkout: Path) -> Path:
        """Install the pinned revision from `git archive`, never the checkout or an installed copy."""
        revision = pinned_revision()
        require_revision(checkout, revision)
        archive, source, library = (
            self.tmp / "eyesim.tar",
            self.tmp / "eyesim-source",
            self.tmp / "eyesim-library",
        )
        source.mkdir()
        library.mkdir()
        subprocess.run(
            [
                "git",
                "-C",
                str(checkout),
                "archive",
                "--format=tar",
                "--output",
                str(archive),
                revision,
            ],
            check=True,
        )
        with tarfile.open(archive) as tar:
            tar.extractall(source, filter="data")
        log = self.tmp / "eyesim-install.log"
        with log.open("w") as out:
            result = subprocess.run(
                ["R", "CMD", "INSTALL", f"--library={library}", str(source)],
                env=r_environment(self.tmp, profile=None),
                stdout=out,
                stderr=subprocess.STDOUT,
            )
        if result.returncode:
            raise RuntimeError(log.read_text())
        self.eyesim_library = library
        return library

    def rscript(
        self, script: Path, *args, check: bool = True, capture: bool = False
    ) -> subprocess.CompletedProcess:
        """Run one R script in the pinned session; audit what it loaded when it succeeds."""
        audit = self.tmp / f"audit-{next(self._audits)}.tsv"
        result = subprocess.run(
            ["Rscript", *R_FLAGS, str(script), *map(str, args)],
            env=self.env | {"EYES4S_R_AUDIT": str(audit)},
            text=True,
            capture_output=capture,
        )
        if check and result.returncode:
            raise subprocess.CalledProcessError(
                result.returncode, result.args, result.stdout, result.stderr
            )
        if result.returncode == 0:
            self._audit(script, audit)
        return result

    def _audit(self, script: Path, audit: Path) -> None:
        if not audit.is_file():
            raise RuntimeError(
                f"{script.name} finished without the session.R audit; was session.R bypassed?"
            )
        rows = [line.split("\t") for line in audit.read_text().splitlines()]
        (_, r_version), loaded = rows[0], rows[1:]
        locked = {package["name"]: package for package in self.lock["packages"]}
        library = R_LIBRARY.resolve()
        problems = []
        if r_version != self.lock["R"]:
            problems.append(f"R {r_version}, not the locked R {self.lock['R']}")
        for name, version, path in loaded:
            where = Path(path).resolve()
            package = locked.get(name)
            if name == "eyesim" and self.eyesim_library is not None:
                if where != self.eyesim_library.resolve():
                    problems.append(
                        f"eyesim from {path}, not the archive of the pinned revision"
                    )
            elif package is None:
                problems.append(f"{name} {version} from {path} is not in r-lock.json")
            elif not same_version(version, package["version"]):
                problems.append(f"{name} {version}, locked at {package['version']}")
            elif package["source"] != "R" and where != library:
                problems.append(
                    f"{name} from {path}, not {R_LIBRARY.relative_to(ROOT)}"
                )
        if problems:
            raise RuntimeError(
                f"{script.name} left the locked R environment:\n- "
                + "\n- ".join(problems)
            )


@contextlib.contextmanager
def r_session(prefix: str, eyesim: Path | None = None) -> Iterator[RSession]:
    """A pinned R session in a temporary directory, with the pinned eyesim when a checkout is given."""
    with tempfile.TemporaryDirectory(prefix=prefix) as name:
        session = RSession(Path(name))
        if eyesim is not None:
            session.install_eyesim(eyesim)
        yield session
