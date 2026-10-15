#!/usr/bin/env python3
"""Check native decomposition-family fixtures, or regenerate them with --write.

The JVM producer only prints documents. Files are written after its sbt process
exits, so compilation never races a changed test source. Scala.js independently
checks the same pinned documents in DecompositionCodecSuite.
"""
import argparse
import json
import os
from pathlib import Path
import subprocess

ROOT = Path(__file__).resolve().parents[2]
NAMES = {
    "decomposition-ols-plan-v1.json": "olsPlan",
    "decomposition-ols-result-v1.json": "olsResult",
    "decomposition-nnls-plan-v1.json": "nnlsPlan",
    "decomposition-nnls-result-v1.json": "nnlsResult",
    "decomposition-mixture-plan-v1.json": "mixturePlan",
    "decomposition-mixture-result-v1.json": "mixtureResult",
}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--write", action="store_true")
    args = parser.parse_args()
    result = subprocess.run(
        ["sbt", "-batch", "-J-Xmx4g", "codecJVM/Test/runMain eyes4s.codec.PrintDecompositionFixtures"],
        cwd=ROOT, env=os.environ, text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
    )
    log = ROOT / "target/decomposition-fixtures.log"
    log.parent.mkdir(parents=True, exist_ok=True)
    log.write_text(result.stdout)
    if result.returncode:
        raise SystemExit(f"Fixture producer failed; see {log}")
    documents = {}
    for line in result.stdout.splitlines():
        if line.startswith("DECOMPOSITION_FIXTURE "):
            _, name, text = line.split(" ", 2)
            if name in documents:
                raise SystemExit(f"Duplicate fixture {name}")
            json.loads(text)
            documents[name] = text
    if set(documents) != set(NAMES):
        raise SystemExit(f"Expected {set(NAMES)}, found {set(documents)}")
    source = ROOT / "codec/src/test/scala/eyes4s/codec/DecompositionCodecSuite.scala"
    header = source.read_text().split("package eyes4s.codec")[0]
    lines = [header + "package eyes4s.codec", "", "// format: off", "private[codec] object DecompositionFixtures:"]
    for name, field in NAMES.items():
        lines.append(f'  val {field}: String = """{documents[name]}"""')
    lines += ["// format: on", ""]
    outputs = {ROOT / "codec/src/test/resources/eyes4s" / name: text + "\n"
               for name, text in documents.items()}
    outputs[ROOT / "codec/src/test/scala/eyes4s/codec/DecompositionFixtures.scala"] = "\n".join(lines)
    for path, text in outputs.items():
        if args.write:
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(text)
        elif not path.exists() or path.read_text() != text:
            raise SystemExit(f"Fixture differs: {path}; use --write and review")
    print("Verified decomposition-family documents and their portable pinned source")


if __name__ == "__main__":
    main()
