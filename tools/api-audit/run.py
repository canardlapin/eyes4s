#!/usr/bin/env python3
"""Compile a candidate before tests; only a complete audit records qualification."""
import argparse
import json
import os
import shutil
import subprocess
import sys
import time
import uuid
from common import ROOT, OUT, MODULES, PROJECTS, fingerprint
from candidate import RUN_ENV, prepare_inventory, retire


def sbt(commands, cwd=ROOT, options=(), environment=None):
    subprocess.run(['sbt', '-J-Xmx4g', *options, *commands], cwd=cwd,
                   env=environment, check=True)


def main(argv=None):
    parser = argparse.ArgumentParser()
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument('--record', action='store_true',
                      help='record reviewed inventory/evidence after a full audit')
    mode.add_argument('--prepare', action='store_true',
                      help='compile fresh inventory for testAll; no execution evidence or qualification')
    args = parser.parse_args(argv)
    OUT.mkdir(parents=True, exist_ok=True)
    started = time.time()
    identity = fingerprint()
    environment = dict(os.environ)
    environment.pop(RUN_ENV, None)
    run_id = None if args.prepare else uuid.uuid4().hex
    try:
        if not args.prepare:
            # A new full generation cannot use stale invocation or test evidence.
            shutil.rmtree(OUT / 'execution', ignore_errors=True)
            for module in MODULES:
                for axis in ('jvm', 'js'):
                    shutil.rmtree(ROOT / module / f'.{axis}/target/test-reports', ignore_errors=True)
        sbt(['compileAll', 'apiAuditInputs'], environment=environment)
        sbt(['compile'] + ([] if args.prepare else ['auditAgent']), ROOT / 'tools/api-audit',
            environment=environment)
        sbt([f'runMain eyes4s.audittool.Inventory {ROOT} {axis} {OUT}/classpath-{axis}.txt {OUT}/inventory-{axis}.json'
             for axis in ('jvm', 'js')], ROOT / 'tools/api-audit', environment=environment)
        inventory = prepare_inventory(identity, run_id)
        if args.prepare:
            print(f'Prepared compiler inventory: {inventory}; no audit qualification recorded.')
            return
        environment[RUN_ENV] = run_id
        agent = (ROOT / 'tools/api-audit/target/jacoco-agent.txt').read_text().strip()
        # Bound fork-server resources and bind each test process to this generation.
        for module in MODULES:
            if fingerprint() != identity:
                raise ValueError('sources changed during execution; rerun against one candidate')
            sbt([PROJECTS[module] + 'JVM/test', PROJECTS[module] + 'JS/test'],
                options=('-Deyes4s.audit.agent=' + agent,), environment=environment)
        sbt([f'runMain eyes4s.audittool.Execution {ROOT} {OUT}/execution.json'],
            ROOT / 'tools/api-audit', environment=environment)
        subprocess.run([sys.executable, str(ROOT / 'tools/api-audit/report.py')],
                       env=environment, check=True)
        if fingerprint() != identity:
            raise ValueError('sources changed during execution; rerun against one candidate')
        receipt = {'fingerprint': identity, 'started': started, 'finished': time.time(),
                   'base_sha': subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=ROOT, text=True).strip()}
        (OUT / 'receipt.json').write_text(json.dumps(receipt, indent=2) + '\n')
        subprocess.run([sys.executable, str(ROOT / 'tools/api-audit/check.py')]
                       + (['--record'] if args.record else []), env=environment, check=True)
    finally:
        if run_id is not None:
            retire(run_id)


if __name__ == '__main__':
    main()
