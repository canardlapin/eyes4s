#!/usr/bin/env python3
"""Require every state, both scales, reviewed baseline hashes and executed tests."""
import importlib.util
import argparse
import hashlib
import json
import math
from pathlib import Path
import struct
import subprocess
import xml.etree.ElementTree as ET

ROOT = Path(__file__).resolve().parents[2]
HERE = Path(__file__).parent
_spec = importlib.util.spec_from_file_location('visual_candidate', HERE / 'candidate.py')
candidate = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(candidate)
SOURCE = 'studio/desktop/src/test/scala/eyes4s/studio/desktop/shell/VisualParitySuite.scala'
REPORT = 'studio/desktop/target/test-reports/TEST-eyes4s.studio.desktop.shell.VisualParitySuite.xml'
PIXEL_ACCOUNTING_ABS_TOLERANCE = 1e-15

def sha(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()

def png_size(path):
    data = path.read_bytes()
    if data[:8] != b'\x89PNG\r\n\x1a\n' or data[12:16] != b'IHDR':
        raise ValueError(f'Not a PNG: {path}')
    return struct.unpack('>II', data[16:24])

def resolve(root, relative):
    path = (root / relative).resolve()
    if not path.is_relative_to(root.resolve()):
        raise ValueError(f'Escaping receipt path: {relative}')
    return path

def verify(root, manifest, allow_functional=False):
    root = root.resolve()
    if manifest.get("pixel_policy") != {"maximum_channel_delta": 1, "maximum_changed_fraction": .001}:
        raise ValueError("Changed manifest pixel policy")
    expected = {f'{t}-{p}-{theme}' for t in ('t1', 't2', 't3')
                for p in ('Data', 'Explore', 'Analysis', 'Compare', 'Figures')
                for theme in ('light', 'dark')}
    states = manifest['states']
    if set(states) != expected or len(states) != 30:
        raise ValueError('Manifest must name exactly all30 matrix states')
    fingerprint = candidate.fingerprint(root)
    reports = ET.parse(root / REPORT).getroot()
    receipts = list((root / 'target/studio-snapshots/VisualParitySuite').glob('*/review.json'))
    if {p.parent.name for p in receipts} != expected or len(receipts) != 30:
        raise ValueError('Missing or duplicate matrix state receipts')
    result = []
    for state in states:
        cases = [c for c in reports.iter('testcase') if c.get('name') == state]
        if len(cases) != 1 or any(cases[0].find(t) is not None for t in ('failure', 'error', 'skipped')):
            raise ValueError(f'{state}: no unique executed passing XML case')
        path = root / 'target/studio-snapshots/VisualParitySuite' / state / 'review.json'
        value = json.loads(path.read_text())
        if value.get('schema') != 1 or (value.get('moment'), value.get('perspective'), value.get('theme')) != tuple(state.split('-')):
            raise ValueError(f'{state}: wrong state identity')
        if value.get('candidateFingerprint') != fingerprint:
            raise ValueError(f'{state}: changed source/resource/build candidate')
        if value['state'] != state or value['sourceSha256'] != sha(root / SOURCE) or value['problems']:
            raise ValueError(f'{state}: changed source or failed receipt')
        if (value['maximumChannelDelta'], value['maximumChangedFraction']) != (1, .001):
            raise ValueError(f'{state}: changed pixel policy')
        goldens = value['mode'] == 'linux-monocle-goldens'
        if not goldens and (not allow_functional or value['mode'] != 'functional-only'):
            raise ValueError(f'{state}: Linux golden comparisons required')
        if goldens and (not value.get('osName', '').lower().startswith('linux') or value.get('glassPlatform') != 'Monocle' or value.get('prismOrder') != 'sw'):
            raise ValueError(f'{state}: wrong golden rendering environment')
        if goldens and (value['nativeScaleX'], value['nativeScaleY']) != (1, 1):
            raise ValueError(f'{state}: golden environment must use native1x')
        logical = (value.get('logicalWidth'), value.get('logicalHeight'))
        if not all(isinstance(n, (int, float)) and math.isfinite(n) and n > 0 for n in logical):
            raise ValueError(f'{state}: invalid logical scene dimensions')
        if goldens and logical != (1440, 900):
            raise ValueError(f'{state}: golden scene dimensions must be1440x900')
        theme = state.rsplit('-', 1)[1]
        captures = value['captures']
        names = {Path(c['actual']).name for c in captures}
        if names != {f'{theme}-1x.png', f'{theme}-2x.png'} or len(captures) != 2:
            raise ValueError(f'{state}: both snapshot scales required')
        for capture in captures:
            actual = resolve(root, capture['actual'])
            if actual.parent != path.parent or sha(actual) != capture['actualSha256']:
                raise ValueError(f'{state}: changed actual capture')
            scale = int(actual.stem[-2])
            size = tuple(math.ceil(n * scale) for n in logical)
            if png_size(actual) != size or (capture['width'], capture['height']) != size:
                raise ValueError(f'{state}: wrong capture dimensions')
            for kind in ['repeat'] + (['comparison'] if goldens else []):
                diff = capture[kind]
                if not diff or not diff['accepted'] or (diff['width'], diff['height']) != size:
                    raise ValueError(f'{state}: failed {kind} comparison')
                if not 0 <= diff['changedPixels'] <= size[0] * size[1] or not 0 <= diff['maximumChannelDelta'] <= 1 or not 0 <= diff['changedFraction'] <= .001:
                    raise ValueError(f'{state}: {kind} exceeds pixel policy')
                if abs(diff['changedPixels'] / (size[0] * size[1]) - diff['changedFraction']) > PIXEL_ACCOUNTING_ABS_TOLERANCE:
                    raise ValueError(f'{state}: inconsistent {kind} pixel accounting')
            if goldens:
                baseline = resolve(root, capture['baseline'])
                wanted = root / 'studio/desktop/src/test/resources/eyes4s/studio/desktop/visual-parity' / state / actual.name
                if baseline != wanted or sha(baseline) != capture['baselineSha256'] or png_size(baseline) != size:
                    raise ValueError(f'{state}: changed or invalid reviewed baseline')
        result.append({'state': state, 'mode': value['mode'], 'reviewSha256': sha(path),
                       'captures': captures})
    return {'candidateFingerprint': fingerprint, 'states': result, 'xmlSha256': sha(root / REPORT), 'sourceSha256': sha(root / SOURCE)}

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--allow-functional-only', action='store_true',
                        help='Explicitly record functional/repeat checks without Linux pixel qualification')
    parser.add_argument('--output', type=Path, default=ROOT / 'target/studio-visual-parity/receipt.json')
    args = parser.parse_args()
    manifest = json.loads((HERE / 'manifest.json').read_text())
    try:
        result = verify(ROOT, manifest, args.allow_functional_only)
    except (OSError, ValueError, KeyError, ET.ParseError) as error:
        print(f'FAIL: {error}')
        return 1
    git = lambda arg: subprocess.check_output(['git', 'rev-parse', arg], cwd=ROOT, text=True).strip()
    result.update(schema=1, candidate=git('HEAD'), tree=git('HEAD^{tree}'),
                  manifestSha256=sha(HERE / 'manifest.json'),
                  qualification='functional/repeat only' if any(s['mode'] != 'linux-monocle-goldens' for s in result['states']) else 'Linux golden matrix passed')
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(result, indent=2) + '\n')
    print(f'PASS: 30 states /60 snapshots; {result["qualification"]}')
    return 0

if __name__ == '__main__':
    raise SystemExit(main())
