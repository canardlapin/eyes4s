#!/usr/bin/env python3
"""Emit the compiled public workflows, independently read CSV/IPC, and freeze their receipt."""
import argparse, hashlib, json, os, subprocess, sys, tempfile
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
HERE = Path(__file__).resolve().parent
sys.dont_write_bytecode = True
sys.path.insert(0, str(ROOT / "tools/r-parity"))
import parity

p = argparse.ArgumentParser(description=__doc__)
p.add_argument("--check", action="store_true")
p.add_argument(
    "--bytes-only",
    action="store_true",
    help="compare the emitted artifacts with the receipt byte for byte, without the R and pyarrow readers",
)
p.add_argument(
    "--arrow-python", default=os.environ.get("EYES4S_ARROW_PYTHON", sys.executable)
)
a = p.parse_args()


def run(argv, **kw):
    return subprocess.run(argv, cwd=ROOT, check=True, **kw)


with tempfile.TemporaryDirectory(prefix="eyes4s-baseline-exports-") as temp:
    root = Path(temp)
    run(
        [
            "sbt",
            "-J-Xmx4g",
            f"ioJVM/Test/runMain eyes4s.examples.BaselineExportMain {root}",
        ]
    )
    if a.bytes_only:
        pinned = json.loads((HERE / "receipt-v1.json").read_text())["artifacts"]
        emitted = {
            f.name: dict(
                bytes=f.stat().st_size,
                sha256=hashlib.sha256(f.read_bytes()).hexdigest(),
            )
            for f in sorted(root.iterdir())
            if f.is_file()
        }
        assert (
            emitted == pinned
        ), f"Export bytes drift: {sorted(k for k in set(pinned)|set(emitted) if pinned.get(k)!=emitted.get(k))}"
        print(f"All {len(pinned)} pinned export artifacts are byte-identical.")
        sys.exit(0)
    with parity.r_session("eyes4s-export-reader-") as r:
        r.rscript(HERE / "check_exports.R", root)
    run([a.arrow_python, str(HERE / "check_arrow.py"), str(root)])
    version = run(
        [
            a.arrow_python,
            "-c",
            'import pyarrow,platform; print(f"pyarrow {pyarrow.__version__}; CPython {platform.python_version()}")',
        ],
        capture_output=True,
        text=True,
    ).stdout.strip()
    receipt = dict(
        schema=1,
        producer="ioJVM/Test/runMain eyes4s.examples.BaselineExportMain OUTPUT_DIRECTORY",
        reader_commands=[
            "LC_ALL=C Rscript tools/result-export/check_exports.R OUTPUT_DIRECTORY",
            "python tools/result-export/check_arrow.py OUTPUT_DIRECTORY",
        ],
        python_reader=version,
        arrow_writer="Apache Arrow Java 19.0.0",
        artifacts={
            f.name: dict(
                bytes=f.stat().st_size,
                sha256=hashlib.sha256(f.read_bytes()).hexdigest(),
            )
            for f in sorted(root.iterdir())
            if f.is_file()
        },
    )
    content = json.dumps(receipt, indent=2) + "\n"
    path = HERE / "receipt-v1.json"
    if a.check:
        # The reader's version names the environment that verified this run, not a property of the
        # artifacts: every other receipt field, including each artifact digest, must match exactly.
        pinned = json.loads(path.read_text())
        assert (
            {k: v for k, v in pinned.items() if k != "python_reader"}
            == {k: v for k, v in receipt.items() if k != "python_reader"}
        ), "Export receipt drift; inspect semantic changes before updating the pinned artifact receipt"
        if pinned["python_reader"] != version:
            print(
                f"Verified with {version}; the receipt was recorded with {pinned['python_reader']}."
            )
    else:
        path.write_text(content)
print(
    "Public export artifacts regenerated and independently verified; frozen receipt agrees."
)
