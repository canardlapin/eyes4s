#!/usr/bin/env python3
"""Run the explicitly named-machine Studio interaction court without OS windows."""
import argparse
import datetime
import hashlib
import json
import os
from pathlib import Path
import platform
import subprocess
import sys

ROOT = Path(__file__).resolve().parents[2]


def output(command):
    return subprocess.check_output(command, cwd=ROOT, text=True).strip()


def digest(path):
    return hashlib.sha256(path.read_bytes()).hexdigest()


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--output', type=Path, default=ROOT / 'target/studio-performance')
    args = parser.parse_args()
    target = args.output.resolve()
    target.mkdir(parents=True, exist_ok=True)
    if any(target.iterdir()):
        parser.error('Use a new output directory; previous measurements are preserved.')
    if platform.system() != 'Darwin':
        parser.error('This frozen reference-machine court requires macOS.')
    env = os.environ.copy()
    for key in ('CI', 'JAVA_TOOL_OPTIONS', 'JDK_JAVA_OPTIONS', '_JAVA_OPTIONS', 'SBT_OPTS', 'JAVA_OPTS'):
        if env.get(key):
            parser.error(f'{key} is set; do not qualify an unrecorded override.')
    env['JAVA_HOME'] = output(['/usr/libexec/java_home', '-v', '25'])
    env['PATH'] = env['JAVA_HOME'] + '/bin:' + env['PATH']
    env['GITHUB_ACTIONS'] = 'true'
    env['EYES4S_STUDIO_PERFORMANCE'] = 'true'
    env['EYES4S_STUDIO_PERFORMANCE_OUTPUT'] = str(target)
    env.pop('EYES4S_STUDIO_FX_VISIBLE', None)
    fingerprint = output(['python3', 'tools/studio-visual-parity/candidate.py'])
    provenance = {
        'started': datetime.datetime.now(datetime.timezone.utc).isoformat(),
        'candidate': output(['git', 'rev-parse', 'HEAD']),
        'tree': output(['git', 'rev-parse', 'HEAD^{tree}']),
        'dirty': bool(output(['git', 'status', '--porcelain'])),
        'sourceFingerprint': fingerprint,
        'runnerSha256': digest(Path(__file__)),
        'protocolSha256': digest(ROOT / 'docs/studio/PERFORMANCE.md'),
        'machine': {
            'model': output(['/usr/sbin/sysctl', '-n', 'hw.model']),
            'cpu': output(['/usr/sbin/sysctl', '-n', 'machdep.cpu.brand_string']),
            'ramBytes': int(output(['/usr/sbin/sysctl', '-n', 'hw.memsize'])),
            'os': output(['/usr/bin/sw_vers', '-productVersion']),
            'osBuild': output(['/usr/bin/sw_vers', '-buildVersion']),
            'architecture': platform.machine(),
        },
        'environment': {'GITHUB_ACTIONS': 'true', 'JAVA_HOME': env['JAVA_HOME'],
                        'manualJvmOverrides': False, 'visibleFx': False},
        'command': ['sbt', 'studioDesktop/testOnly *InteractionPerformanceFxSuite'],
        'status': 'running',
    }
    record = target / 'run.json'
    record.write_text(json.dumps(provenance, indent=2) + '\n')
    with (target / 'run.log').open('w') as log:
        result = subprocess.run(provenance['command'], cwd=ROOT, env=env,
                                stdout=log, stderr=subprocess.STDOUT)
    provenance['exitCode'] = result.returncode
    unchanged = output(['python3', 'tools/studio-visual-parity/candidate.py']) == fingerprint
    provenance['inputsUnchanged'] = unchanged
    receipts = []
    for factor in (1, 10):
        file = target / f'fixture-{factor}x.json'
        if file.exists():
            receipts.append({'path': file.name, 'sha256': digest(file),
                             'status': json.loads(file.read_text())['status']})
    passed = result.returncode == 0 and unchanged and len(receipts) == 2 and all(
        r['status'] == 'passed' for r in receipts)
    provenance.update(status='passed' if passed else 'failed', receipts=receipts,
                      finished=datetime.datetime.now(datetime.timezone.utc).isoformat())
    record.write_text(json.dumps(provenance, indent=2) + '\n')
    print(json.dumps({'status': provenance['status'], 'record': str(record)}))
    return 0 if passed else 1


if __name__ == '__main__':
    sys.exit(main())
