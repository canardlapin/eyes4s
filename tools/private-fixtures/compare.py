#!/usr/bin/env python3
"""Compare eyes4s with a freshly installed eyesim revision on identical private density maps."""

import argparse
import json
import os
from pathlib import Path
import subprocess
import sys
import tarfile

from run import ROOT, build, digest, dump, implementation, outside_git


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--request", required=True, type=Path)
    parser.add_argument("--eyesim", required=True, type=Path, help="local reference Git checkout")
    parser.add_argument("--r-library", required=True, type=Path, help="installed reference dependencies")
    parser.add_argument("--output-dir", required=True, type=Path)
    args = parser.parse_args()
    os.umask(0o077)
    output = None
    try:
        if os.environ.get("CI") or os.environ.get("GITHUB_ACTIONS"):
            raise ValueError("Private comparisons are disabled in CI")
        request = outside_git(args.request)
        if not request.is_file() or request.stat().st_size == 0:
            raise ValueError("An external populated request is required")
        payload = request.read_bytes()
        json.loads(payload)
        checkout = args.eyesim.expanduser().resolve()
        revision = subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=checkout, text=True).strip()
        candidate = outside_git(args.output_dir)
        candidate.mkdir(mode=0o700, parents=True, exist_ok=False)
        output = candidate
        provenance = implementation()
        (output / "request.json").write_bytes(payload)
        library = output / "library"
        library.mkdir()
        # Install an exact Git object, even when the supplied reference working tree is dirty.
        archive = output / "eyesim.tar"
        with archive.open("xb") as stream:
            subprocess.run(["git", "archive", "--format=tar", revision], cwd=checkout,
                           stdout=stream, check=True)
        source = output / "eyesim-source"
        source.mkdir()
        with tarfile.open(archive) as stream:
            # Git source is untrusted: refuse links and paths escaping the extraction root.
            members = stream.getmembers()
            if any(not (source / m.name).resolve().is_relative_to(source)
                   or not (m.isfile() or m.isdir()) for m in members):
                raise ValueError("Reference archive contains an unsafe path or link")
            stream.extractall(source, members=members)
        env = dict(os.environ, R_LIBS=str(args.r_library.expanduser().resolve()))
        with (output / "reference.log").open("xb") as log:
            subprocess.run(["R", "CMD", "INSTALL", "--clean", f"--library={library}", str(source)],
                           env=env, cwd=output, stdout=log, stderr=subprocess.STDOUT, check=True)
            subprocess.run(["Rscript", "--vanilla", str(Path(__file__).with_name("shared_density.R")),
                            str(library), str(output / "request.json"), str(output)],
                           env=env, cwd=output, stdout=log, stderr=subprocess.STDOUT, check=True)
        classpath = build(output)
        with (output / "jvm.log").open("xb") as log:
            subprocess.run(["java", "-Xmx2g", "-cp", classpath,
                            "eyes4s.examples.SharedDensityParityCli"], cwd=output,
                           env=dict(os.environ, EYES4S_PRIVATE_RUN=str(output)),
                           stdout=log, stderr=subprocess.STDOUT, check=True)
        result = json.loads((output / "comparison.json").read_text())
        if implementation() != provenance:
            raise ValueError("Implementation changed during the run; rerun on fixed inputs")
        dump(output / "receipt.json", {"status": result["status"], "eyesim_revision": revision,
             "eyesim_archive_sha256": digest(archive), "request_sha256": digest(output / "request.json"),
             "implementation": provenance,
             "result": result})
        print("Shared-density eyesim comparison passed; private evidence is in the output directory.")
        return 0
    except (ValueError, OSError, KeyError, tarfile.TarError, subprocess.SubprocessError) as error:
        if output:
            dump(output / "failure.json", {"status": "failed", "error": str(error)})
            print("Comparison failed; inspect the external logs and failure report.", file=sys.stderr)
        else:
            print(f"Comparison preflight failed: {error}", file=sys.stderr)
        return 1


if __name__ == "__main__":
    sys.exit(main())
