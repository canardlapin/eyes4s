#!/usr/bin/env python3
"""Provision the project-local R library that every parity generator runs against.

    python3 tools/r-parity/provision.py
        Make tools/r-parity/library hold exactly the packages and versions in r-lock.json.
    python3 tools/r-parity/provision.py --update --eyesim /path/to/eyesim
        Re-resolve r-lock.json: the Depends/Imports/LinkingTo closure of the pinned eyesim
        DESCRIPTION plus the harness and baseline roots named in the lock.

A locked package is taken, in order of preference, from an installed copy of exactly that
version in one of this machine's R libraries, from the current CRAN binary when its version
matches, or from the CRAN source archive of that version. `--update` keeps the versions already
in the project library, then prefers installed copies, then current CRAN releases. Base packages
come with R and are recorded, not installed. The library is git-ignored; r-lock.json is
committed and names every package a generator may load. R is needed only to regenerate
fixtures; no eyes4s build or test runs it.
"""
from __future__ import annotations

import argparse
import csv
import io
import json
import os
from pathlib import Path
import shutil
import subprocess
import sys

sys.dont_write_bytecode = True
import parity  # noqa: E402

CRAN = "https://cloud.r-project.org"
INVENTORY = r"""
args <- commandArgs(trailingOnly = TRUE)
project <- normalizePath(args[[1L]], mustWork = FALSE)
fields <- c("Package", "LibPath", "Version", "Priority", "Repository", "Depends", "Imports", "LinkingTo")
local <- setdiff(normalizePath(.libPaths()), project)
tables <- list(
  installed.packages(lib.loc = local, fields = "Repository"),
  if (dir.exists(project)) installed.packages(lib.loc = project, fields = "Repository")
)
if (identical(args[[2L]], "cran")) {
  cran <- available.packages(repos = args[[3L]], type = "source")
  cran <- cbind(cran, LibPath = "CRAN", Repository = "CRAN")
  tables <- c(tables, list(cran))
  binary <- if (.Platform$pkgType != "source") available.packages(repos = args[[3L]], type = .Platform$pkgType)
  if (!is.null(binary) && nrow(binary)) {
    tables <- c(tables, list(cbind(binary, LibPath = "CRAN-binary", Repository = "CRAN")))
  }
}
rows <- do.call(rbind, lapply(Filter(Negate(is.null), tables), function(t) t[, fields, drop = FALSE]))
rows[is.na(rows)] <- ""
rows <- gsub("[\t\n]+", " ", rows)
cat(as.character(getRversion()), "\n", sep = "")
write.table(rows, stdout(), sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE)
"""
INSTALL = r"""
args <- commandArgs(trailingOnly = TRUE)
lib <- args[[1L]]
.libPaths(lib)
spec <- read.delim(args[[2L]], colClasses = "character")
options(install.packages.compile.from.source = "never", repos = c(CRAN = args[[3L]]))
for (i in seq_len(nrow(spec))) {
  row <- spec[i, ]
  if (identical(row$how, "binary")) {
    install.packages(row$name, lib = lib, dependencies = FALSE, type = .Platform$pkgType)
  } else {
    install.packages(row$how, lib = lib, repos = NULL, type = "source")
  }
  found <- tryCatch(as.character(packageVersion(row$name, lib.loc = lib)), error = function(e) "")
  if (!identical(gsub("-", ".", found), gsub("-", ".", row$version))) {
    stop("installing ", row$name, " ", row$version, " left ", if (nzchar(found)) found else "nothing")
  }
}
"""


def inventory(cran: bool) -> tuple[str, list[dict[str, str]]]:
    """This machine's R version and every package it can see, in library-search order.

    Rows come from the machine's own libraries first, then the project library, then (with
    `cran`) the CRAN source and binary indexes. Only the ordinary R environment is used here,
    because copies are taken from the libraries the user already has.
    """
    env = {key: value for key, value in os.environ.items() if key not in parity.DROPPED}
    output = parity.run_text(
        [
            "Rscript",
            "--vanilla",
            "-e",
            INVENTORY,
            str(parity.R_LIBRARY),
            "cran" if cran else "local",
            CRAN,
        ],
        env,
    )
    version, table = output.split("\n", 1)
    return version.strip(), list(csv.DictReader(io.StringIO(table), delimiter="\t"))


def base_names(rows: list[dict[str, str]]) -> set[str]:
    return {row["Package"] for row in rows if row["Priority"] == "base"}


def project_rows(rows: list[dict[str, str]]) -> dict[str, dict[str, str]]:
    library = str(parity.R_LIBRARY.resolve())
    return {row["Package"]: row for row in rows if row["LibPath"] == library}


def resolve(
    roots: list[str],
    rows: list[dict[str, str]],
    requirements: list[tuple[str, str | None, str | None]] = (),
) -> dict[str, dict[str, str]]:
    """The dependency closure of `roots`, each package from the first row that offers a version
    meeting every constraint placed on it.

    Rows are preferred in this order: the project library, so that `--update` never moves an
    installed version that still qualifies; this machine's libraries in search order; CRAN.
    `requirements` are constraints known in advance (the pinned eyesim's); constraints found in
    chosen packages' own dependencies are added until the choice is stable.
    """
    library = str(parity.R_LIBRARY.resolve())
    ordered = sorted(rows, key=lambda row: row["LibPath"] != library)
    candidates: dict[str, list[dict[str, str]]] = {}
    for row in ordered:
        if row["LibPath"] != "CRAN-binary":
            candidates.setdefault(row["Package"], []).append(row)
    base = base_names(rows)
    constraints: dict[str, set[tuple[str, str]]] = {}
    for name, operator, bound in requirements:
        if operator is not None and bound is not None:
            constraints.setdefault(name, set()).add((operator, bound))
    while True:
        found = {name: set(bounds) for name, bounds in constraints.items()}
        chosen: dict[str, dict[str, str]] = {}
        todo = list(roots)
        while todo:
            name = todo.pop()
            if name in chosen:
                continue
            bounds = sorted(constraints.get(name, ()))
            options = [
                row
                for row in candidates.get(name, [])
                if all(parity.satisfies(row["Version"], op, bound) for op, bound in bounds)
            ]
            if not options:
                raise SystemExit(f"no installed copy or CRAN release of {name} meets {bounds}")
            chosen[name] = options[0]
            if name in base:
                continue
            for field in parity.DEPENDENCY_FIELDS:
                for dependency, operator, bound in parity.dependency_specs(options[0][field]):
                    todo.append(dependency)
                    if operator is not None and bound is not None:
                        found.setdefault(dependency, set()).add((operator, bound))
        if found == constraints:
            return chosen
        constraints = found


def topological(chosen: dict[str, dict[str, str]]) -> list[str]:
    """Package names with every dependency before its dependents, ties broken by name."""
    order: list[str] = []
    seen: set[str] = set()

    def visit(name: str) -> None:
        if name in seen:
            return
        seen.add(name)
        for field in parity.DEPENDENCY_FIELDS:
            for dependency in sorted(parity.dependency_names(chosen[name][field])):
                # Base packages come with R; their own dependencies are not resolved.
                if dependency in chosen:
                    visit(dependency)
        order.append(name)

    for name in sorted(chosen, key=str.lower):
        visit(name)
    return order


def materialize(wanted: list[tuple[str, str]], rows: list[dict[str, str]]) -> list[str]:
    """Make the project library hold exactly `wanted` (name, version) pairs; report each action."""
    parity.R_LIBRARY.mkdir(exist_ok=True)
    library = str(parity.R_LIBRARY.resolve())
    wanted_names = {name for name, _ in wanted}
    actions = []
    for entry in sorted(parity.R_LIBRARY.iterdir()):
        if entry.is_dir() and entry.name not in wanted_names:
            shutil.rmtree(entry)
            actions.append(f"removed {entry.name}")
    pending = []
    for name, version in wanted:
        found = parity.installed_version(parity.R_LIBRARY, name)
        if found is not None and parity.same_version(found, version):
            continue
        copy = next(
            (
                row
                for row in rows
                if row["Package"] == name
                and row["LibPath"] not in {library, "CRAN", "CRAN-binary"}
                and parity.same_version(row["Version"], version)
            ),
            None,
        )
        if copy is not None:
            shutil.rmtree(parity.R_LIBRARY / name, ignore_errors=True)
            shutil.copytree(
                Path(copy["LibPath"]) / name, parity.R_LIBRARY / name, symlinks=True
            )
            actions.append(f"copied {name} {version} from {copy['LibPath']}")
            continue
        binary = any(
            row["Package"] == name
            and row["LibPath"] == "CRAN-binary"
            and parity.same_version(row["Version"], version)
            for row in rows
        )
        current = any(
            row["Package"] == name
            and row["LibPath"] == "CRAN"
            and parity.same_version(row["Version"], version)
            for row in rows
        )
        if binary:
            how = "binary"
        elif current:
            how = f"{CRAN}/src/contrib/{name}_{version}.tar.gz"
        else:
            how = f"{CRAN}/src/contrib/Archive/{name}/{name}_{version}.tar.gz"
        pending.append((name, version, how))
        actions.append(
            f"installed {name} {version} from CRAN ({'binary' if binary else 'source'})"
        )
    if pending:
        spec = "name\tversion\thow\n" + "".join(
            f"{n}\t{v}\t{h}\n" for n, v, h in pending
        )
        spec_path = parity.R_LIBRARY / ".install-spec.tsv"
        spec_path.write_text(spec)
        try:
            env = {
                key: value
                for key, value in os.environ.items()
                if key not in parity.DROPPED
            }
            env |= {"R_LIBS": str(parity.R_LIBRARY), "LC_ALL": "C", "LANG": "C"}
            subprocess.run(
                [
                    "Rscript",
                    "--vanilla",
                    "-e",
                    INSTALL,
                    str(parity.R_LIBRARY),
                    str(spec_path),
                    CRAN,
                ],
                check=True,
                env=env,
            )
        finally:
            spec_path.unlink()
    return actions


def verify(lock: dict) -> list[str]:
    problems = []
    for package in lock["packages"]:
        if package["source"] == "R":
            continue
        found = parity.installed_version(parity.R_LIBRARY, package["name"])
        if found is None or not parity.same_version(found, package["version"]):
            problems.append(
                f"{package['name']} {package['version']} (found {found or 'nothing'})"
            )
    return problems


def render(lock: dict) -> str:
    """The lock as reviewable JSON: one line per field and one line per package."""
    head = [f"  {json.dumps(k)}: {json.dumps(v)}" for k, v in lock.items() if k != "packages"]
    body = ",\n".join(f"    {json.dumps(package)}" for package in lock["packages"])
    return "{\n" + ",\n".join(head) + ',\n  "packages": [\n' + body + "\n  ]\n}\n"


def update(eyesim: Path) -> None:
    lock = parity.load_lock()
    eyesim_roots = parity.eyesim_requirements(eyesim)
    requirements = parity.dependency_specs(", ".join(eyesim_roots))
    roots = sorted(
        {
            *(name for name, _, _ in requirements),
            *(name for group in lock["roots"].values() for name in group),
        }
    )
    r_version, rows = inventory(cran=True)
    while True:
        chosen = resolve(roots, rows, requirements)
        base = base_names(rows)
        order = [name for name in topological(chosen) if name not in base]
        actions = materialize([(name, chosen[name]["Version"]) for name in order], rows)
        for action in actions:
            print(action)
        if not actions:
            break
        r_version, rows = inventory(cran=True)
    installed = project_rows(rows)
    # Every base package is recorded: R loads them without any dependency naming them.
    packages = [
        {"name": name, "version": r_version, "source": "R"}
        for name in sorted(base, key=str.lower)
    ]
    for name in topological(chosen):
        if name not in base:
            row = installed[name]
            source = row["Repository"] or "unknown"
            packages.append({"name": name, "version": row["Version"], "source": source})
    unknown = [
        package["name"] for package in packages if package["source"] == "unknown"
    ]
    if unknown:
        raise SystemExit(f"packages without a recorded repository, lock unchanged: {unknown}")
    lock |= {"R": r_version, "eyesim_roots": eyesim_roots, "packages": packages}
    parity.R_LOCK.write_text(render(lock))
    print(
        f"Locked R {r_version} and {len(packages)} packages in {parity.R_LOCK.relative_to(parity.ROOT)}."
    )


def sync() -> None:
    lock = parity.load_lock()
    r_version, rows = inventory(cran=False)
    if r_version != lock["R"]:
        raise SystemExit(
            f"r-lock.json records R {lock['R']}, but Rscript is R {r_version}"
        )
    wanted = [(p["name"], p["version"]) for p in lock["packages"] if p["source"] != "R"]
    missing_locally = [
        (name, version)
        for name, version in wanted
        if not any(
            row["Package"] == name and parity.same_version(row["Version"], version)
            for row in rows
        )
    ]
    if missing_locally:
        r_version, rows = inventory(cran=True)
    for action in materialize(wanted, rows):
        print(action)
    problems = verify(lock)
    if problems:
        raise SystemExit(
            "the project library does not match r-lock.json:\n- "
            + "\n- ".join(problems)
        )
    print(
        f"{parity.R_LIBRARY.relative_to(parity.ROOT)} holds the {len(wanted)} locked packages for R {lock['R']}."
    )


def main() -> None:
    parser = argparse.ArgumentParser(
        description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter
    )
    parser.add_argument(
        "--update", action="store_true", help="re-resolve r-lock.json from its roots"
    )
    parser.add_argument(
        "--eyesim",
        type=Path,
        help="eyesim checkout containing the pinned revision (--update)",
    )
    args = parser.parse_args()
    if args.update:
        if args.eyesim is None:
            parser.error("--update needs --eyesim to read the pinned DESCRIPTION")
        update(args.eyesim)
    else:
        sync()


if __name__ == "__main__":
    main()
