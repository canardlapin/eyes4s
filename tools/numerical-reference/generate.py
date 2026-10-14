#!/usr/bin/env python3
"""Pin an independent StrictMath oracle for portable smoother arithmetic."""
import argparse
import os
from pathlib import Path
import subprocess
import tempfile

HERE = Path(__file__).resolve().parent
ROOT = HERE.parent.parent
OUTPUT = ROOT / "surface/src/test/scala/eyes4s/surface/KernelMathReference.scala"


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--check", action="store_true")
    args = parser.parse_args()
    jdk = Path(os.environ["JAVA_HOME"]) / "bin" if os.environ.get("JAVA_HOME") else None
    with tempfile.TemporaryDirectory(prefix="eyes4s-kernel-math-oracle-") as temporary:
        directory = Path(temporary)
        candidate = directory / "KernelMathReference.scala"
        subprocess.run([str(jdk / "javac") if jdk else "javac", "-d", str(directory), str(HERE / "Generate.java")], cwd=directory, check=True)
        subprocess.run([str(jdk / "java") if jdk else "java", "-cp", str(directory), "Generate", str(candidate)], cwd=directory, check=True)
        generated = candidate.read_bytes()
    if args.check:
        if OUTPUT.read_bytes() != generated:
            raise SystemExit(f"Stale StrictMath fixture: regenerate {OUTPUT.relative_to(ROOT)}")
        print("StrictMath fixture is byte-identical")
    else:
        OUTPUT.write_bytes(generated)
        print(f"Wrote {OUTPUT.relative_to(ROOT)}")


if __name__ == "__main__":
    main()
