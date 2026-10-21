#!/usr/bin/env python3
"""Bind E2E02–25 to successful test cases from this checkout's Studio gate."""
import argparse
import hashlib
import json
from pathlib import Path
import subprocess
import xml.etree.ElementTree as ET

ROOT = Path(__file__).resolve().parents[2]
MANIFEST = Path(__file__).with_name('manifest.json')

def git(*args):
    return subprocess.check_output(['git', *args], cwd=ROOT, text=True).strip()

def passed_cases(path, contains):
    root = ET.parse(path).getroot()
    cases = [case for case in root.iter('testcase') if contains in case.get('name', '')]
    if not cases:
        raise ValueError(f'{path}: no case contains {contains!r}')
    bad = [case.get('name') for case in cases if any(case.find(tag) is not None for tag in ('failure', 'error', 'skipped'))]
    if bad:
        raise ValueError(f'{path}: failed, errored or skipped cases: {bad}')
    return [case.get('name') for case in cases]

def verify(check):
    module = check['module']
    axes = [''] if module == 'desktop' else ['.jvm', '.js']
    if check['scope'] == 'native backend JVM':
        axes = ['.jvm']
    source = ROOT / check['source']
    if not source.is_file():
        raise ValueError(f'Missing test source: {source}')
    results = []
    for axis in axes:
        report = ROOT / 'studio' / module / axis / 'target/test-reports' / ('TEST-' + check['suite'] + '.xml')
        names = passed_cases(report, check['case_contains'])
        results.append({'platform': axis or 'desktop', 'report': str(report.relative_to(ROOT)), 'report_sha256': hashlib.sha256(report.read_bytes()).hexdigest(), 'cases': names})
    return {**check, 'source_sha256': hashlib.sha256(source.read_bytes()).hexdigest(), 'executed': results}

def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output', type=Path, default=ROOT / 'target/studio-e2e/receipt.json')
    args = parser.parse_args()
    manifest = json.loads(MANIFEST.read_text())
    assert [s['id'] for s in manifest['scenarios']] == [f'E2E-{i:02}' for i in range(2, 26)]
    try:
        scenarios = [{**row, 'checks': [verify(c) for c in row['checks']]} for row in manifest['scenarios']]
        native = [verify(c) for c in manifest['native_regression']]
    except (OSError, ValueError, ET.ParseError) as error:
        print(f'FAIL: {error}')
        return 1
    receipt = {'schema': 1, 'candidate': git('rev-parse', 'HEAD'), 'tree': git('rev-parse', 'HEAD^{tree}'), 'manifest_sha256': hashlib.sha256(MANIFEST.read_bytes()).hexdigest(), 'scenarios': scenarios, 'native_regression': native, 'limits': manifest['limits']}
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(json.dumps(receipt, indent=2, ensure_ascii=False) + '\n')
    print(f'PASS: {len(scenarios)} scenarios E2E02–25; named cases passed on every required platform. Receipt: {args.output}')
    return 0

if __name__ == '__main__':
    raise SystemExit(main())
