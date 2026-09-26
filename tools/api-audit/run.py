#!/usr/bin/env python3
"""Produce fresh compiler and invocation evidence, then enforce the reviewed API contract."""
import argparse
import json
import shutil
import subprocess
import sys
import time
from common import ROOT, OUT, MODULES, PROJECTS, fingerprint

parser = argparse.ArgumentParser()
parser.add_argument('--record', action='store_true', help='write a candidate inventory/mapping for explicit review; never used in CI')
args = parser.parse_args()
OUT.mkdir(parents=True, exist_ok=True)
started = time.time()
identity = fingerprint()

def sbt(commands, cwd=ROOT, options=()):
    subprocess.run(['sbt','-J-Xmx4g',*options,*commands], cwd=cwd, check=True)

# A fresh generation cannot consume another candidate's stale .exec or XML files.
shutil.rmtree(OUT/'execution', ignore_errors=True)
for module in MODULES:
    for axis in ('jvm','js'):
        shutil.rmtree(ROOT/module/f'.{axis}/target/test-reports', ignore_errors=True)
sbt(['compileAll','apiAuditInputs'])
sbt(['compile','auditAgent'], ROOT/'tools/api-audit')
agent = (ROOT/'tools/api-audit/target/jacoco-agent.txt').read_text().strip()
# Keep sbt's fork-server resources bounded across hundreds of suite JVMs.
for module in MODULES:
    sbt([PROJECTS[module]+'JVM/test', PROJECTS[module]+'JS/test'], options=('-Deyes4s.audit.agent='+agent,))
sbt([f'runMain eyes4s.audittool.Inventory {ROOT} {axis} {OUT}/classpath-{axis}.txt {OUT}/inventory-{axis}.json' for axis in ('jvm','js')] + [f'runMain eyes4s.audittool.Execution {ROOT} {OUT}/execution.json'], ROOT/'tools/api-audit')
subprocess.run([sys.executable, str(ROOT/'tools/api-audit/report.py')], check=True)
if fingerprint() != identity:
    raise SystemExit('Sources changed during execution; rerun against one candidate')
receipt = {'fingerprint':identity,'started':started,'finished':time.time(),
           'base_sha':subprocess.check_output(['git','rev-parse','HEAD'],cwd=ROOT,text=True).strip()}
(OUT/'receipt.json').write_text(json.dumps(receipt,indent=2)+'\n')
subprocess.run([sys.executable,str(ROOT/'tools/api-audit/check.py')]+(['--record'] if args.record else []),check=True)
