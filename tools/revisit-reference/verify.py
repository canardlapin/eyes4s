#!/usr/bin/env python3
"""Replay the separately pinned new eyesim feature without changing the legacy baseline."""
import argparse
import hashlib
import json
import math
import os
from pathlib import Path
import subprocess
import tempfile

HERE = Path(__file__).resolve().parent
ROOT = HERE.parents[1]
INPUT = ROOT / 'tools/r-parity/fixtures/cases/fixation-revisits.json'
ORACLE = ROOT / 'tools/r-parity/fixtures/fixation-revisits.json'
OUTPUT = HERE / 'measured.json'


def compare(actual, expected, location='result'):
    if isinstance(expected, dict):
        assert actual.keys() == expected.keys(), location
        for key in expected:
            compare(actual[key], expected[key], f'{location}.{key}')
    elif isinstance(expected, list):
        assert len(actual) == len(expected), location
        for i, (a, b) in enumerate(zip(actual, expected, strict=True)):
            compare(a, b, f'{location}[{i}]')
    elif isinstance(expected, float):
        assert isinstance(actual, (int, float)) and math.isclose(actual, expected, rel_tol=1e-12, abs_tol=1e-12), (location, actual, expected)
    else:
        assert actual == expected, (location, actual, expected)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    source = parser.add_mutually_exclusive_group(required=True)
    source.add_argument('--eyesim', type=Path, help='Git checkout containing the supplemental pinned commit')
    source.add_argument('--source-dir', type=Path, help='Directory with the exact hash-checked R source files')
    parser.add_argument('--write', action='store_true', help='Record newly measured output after checking the independent oracle')
    args = parser.parse_args()
    pin = json.loads((HERE / 'source.json').read_text())
    with tempfile.TemporaryDirectory(prefix='eyes4s-revisit-reference-') as directory:
        scratch = Path(directory)
        for filename, digest in pin['files'].items():
            data = (args.source_dir / filename).read_bytes() if args.source_dir else subprocess.check_output(
                ['git','show',f"{pin['commit']}:R/{filename}"],cwd=args.eyesim)
            assert hashlib.sha256(data).hexdigest() == digest, f'Source hash mismatch: {filename}'
            (scratch / filename).write_bytes(data)
        environment = dict(os.environ, LC_ALL='C', LANG='C', LANGUAGE='en', TZ='UTC', OMP_NUM_THREADS='1')
        subprocess.run(['Rscript','--vanilla',str(HERE/'verify.R'),str(scratch),str(INPUT),str(scratch/'result.json')],env=environment,check=True)
        measured = json.loads((scratch/'result.json').read_text())
    expected = {r['case']:r for r in json.loads(ORACLE.read_text())['results']}
    for row in measured['results']:
        compare(row, expected[row['case']], row['case'])
    measured.update(source=pin,input_sha256=hashlib.sha256(INPUT.read_bytes()).hexdigest())
    text = json.dumps(measured,indent=2)+'\n'
    if args.write:
        OUTPUT.write_text(text)
    else:
        assert OUTPUT.read_text() == text, 'Measured output or runtime drift; inspect before recording a successor.'
    print(f"Supplemental eyesim replay: {len(measured['results'])} uniform-reference cases agree with the independent oracle.")


if __name__ == '__main__':
    main()
