#!/usr/bin/env python3
"""Check scanpath-study fixtures, or regenerate them with --write.

The JVM producer only prints documents. Files are written after its sbt process
exits, so compilation never races a changed test source. Scala.js independently
checks the same pinned documents in ScanpathStudyCodecSuite.
"""
import argparse
import json
import os
from pathlib import Path
import subprocess

ROOT = Path(__file__).resolve().parents[2]
NAMES = {
    "scanpath-study-multimatch-plan-v1.json": "multimatchPlan",
    "scanpath-study-multimatch-result-v1.json": "multimatchResult",
    "scanpath-study-overlap-plan-v1.json": "overlapPlan",
    "scanpath-study-overlap-result-v1.json": "overlapResult",
    "scanpath-study-transport-plan-v1.json": "transportPlan",
    "scanpath-study-transport-result-v1.json": "transportResult",
}

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--write", action="store_true")
    args = parser.parse_args()
    result = subprocess.run(
        ["sbt", "-batch", "-J-Xmx4g", "codecJVM/Test/runMain eyes4s.codec.PrintScanpathStudyFixtures"],
        cwd=ROOT, env=os.environ, text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
    )
    log = ROOT / "target/scanpath-study-fixtures.log"
    log.parent.mkdir(parents=True, exist_ok=True)
    log.write_text(result.stdout)
    if result.returncode:
        raise SystemExit(f"Fixture producer failed; see {log}")
    documents = {}
    for line in result.stdout.splitlines():
        if line.startswith("SCANPATH_STUDY_FIXTURE "):
            _, name, text = line.split(" ", 2)
            if name in documents:
                raise SystemExit(f"Duplicate fixture {name}")
            json.loads(text)
            documents[name] = text
    if set(documents) != set(NAMES):
        raise SystemExit(f"Expected {set(NAMES)}, found {set(documents)}")
    source = ROOT / "codec/src/test/scala/eyes4s/codec/ScanpathStudyCodecSuite.scala"
    header = source.read_text().split("package eyes4s.codec")[0]
    lines = [header + "package eyes4s.codec", "", "// format: off", "private[codec] object ScanpathStudyFixtures:"]
    for name, field in NAMES.items():
        lines.append(f'  val {field}: String = """{documents[name]}"""')
    lines += ["// format: on", ""]
    outputs = {ROOT / "codec/src/test/resources/eyes4s" / name: text + "\n"
               for name, text in documents.items()}
    outputs[ROOT / "codec/src/test/scala/eyes4s/codec/ScanpathStudyFixtures.scala"] = "\n".join(lines)
    for path, text in outputs.items():
        if args.write:
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_text(text)
        elif not path.exists() or path.read_text() != text:
            raise SystemExit(f"Fixture differs: {path}; use --write and review")
    print("Verified scanpath-study documents and their portable pinned source")


if __name__ == "__main__":
    main()
