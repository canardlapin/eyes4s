from pathlib import Path
import json,subprocess,sys
root=Path.cwd();sys.path.insert(0,str(root/'tools/api-audit'))
from common import OUT,fingerprint
mapping=root/'tools/api-audit/evidence.json'
original=mapping.read_bytes();results=[]
def reject(label, expected):
 r=subprocess.run([sys.executable,'tools/api-audit/check.py'],capture_output=True,text=True)
 output=r.stdout+r.stderr
 assert r.returncode!=0 and expected in output,(label,output)
 results.append({'mutation':label,'rejected':True,'diagnostic':output.strip()})
try:
 data=json.loads(original);entry=next(iter(data['entries']));data['entries'].pop(entry)
 mapping.write_text(json.dumps(data));reject('remove an entry/test mapping','missing or extra entry/test mappings')
 data=json.loads(original);suite=next(iter(data['suites']));data['suites'][suite][0]='this test never ran'
 mapping.write_text(json.dumps(data));reject('rename a mapped test to an arbitrary string','removed, misnamed, or changed test mapping')
finally:mapping.write_bytes(original)
probe=root/'kernel/src/main/scala/eyes4s/kernel/ApiAuditUnmappedProbe.scala'
assert not probe.exists()
receipt=OUT/'receipt.json';saved=receipt.read_bytes()
def sbt(commands,cwd=root):
 with open('/tmp/eyes4s-api-mutant-build.log','a') as output:
  subprocess.run(['sbt','-J-Xmx4g',*commands],cwd=cwd,stdout=output,stderr=subprocess.STDOUT,check=True)
def inventories():
 sbt([f'runMain eyes4s.audittool.Inventory {root} {axis} {OUT}/classpath-{axis}.txt {OUT}/inventory-{axis}.json' for axis in ('jvm','js')]+[f'runMain eyes4s.audittool.Execution {root} {OUT}/execution.json'],root/'tools/api-audit')
 subprocess.run([sys.executable,'tools/api-audit/report.py'],check=True)
try:
 probe.write_text('package eyes4s.kernel\nobject ApiAuditUnmappedProbe:\n  def value: Int = 17\n')
 sbt(['kernelJVM/compile','kernelJS/compile'])
 inventories()
 # This is an intentionally failing candidate, with real compiler output and no invented invocation.
 data=json.loads(saved);data['fingerprint']=fingerprint();receipt.write_text(json.dumps(data))
 reject('compile a new uninvoked public entry point','unexercised entries')
finally:
 probe.unlink(missing_ok=True)
 sbt(['kernelJVM/compile','kernelJS/compile'])
 inventories()
 receipt.write_bytes(saved)
subprocess.run([sys.executable,'tools/api-audit/check.py'],check=True)
(OUT/'mutations.json').write_text(json.dumps(results,indent=2)+'\n')
print('All three API drift mutants rejected; original sources, mappings and passing gate restored.')
