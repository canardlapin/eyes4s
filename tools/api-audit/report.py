#!/usr/bin/env python3
"""Join compiler declarations to exact JVM bytecode method probes; never infer invocation from names alone."""
import collections
import json
from pathlib import Path
ROOT = Path(__file__).resolve().parents[2]
OUT = ROOT / 'target/api-audit'
raw = json.loads((OUT / 'inventory-jvm.json').read_text())['entries']
execution = json.loads((OUT / 'execution.json').read_text())
classes = {c['name']:c for c in execution['classes']}
hits = {(x['owner'],x['name'],x['descriptor']):x['suites'] for x in execution['invocations']}
methods = {(c['name'],name):[m for m in c['methods'] if m['name']==name] for c in classes.values() for name in {m['name'] for m in c['methods']}}
# Explicit structural classifications retain their symbols in the reviewed inventory.
def category(x):
    kind=x['category']; flags=x['flags']; owner=x['ownerFlags']
    if kind!='runtime-entry':return kind
    if '$default$' in x['name']:return 'compiler-default-argument'
    if x['signature'].endswith(':scala.CanEqual'):return 'compile-time-equality-witness'
    if x['kind']=='value':
        if 'Flags.Module' in flags:return 'module-reference'
        if 'Flags.ParamAccessor' in flags:return 'structural-accessor'
        if 'Flags.Enum' in flags:return 'enum-case-value'
        if x['valueType'].startswith('scala.CanEqual['):return 'compile-time-equality-witness'
    if x['name']=='<init>':
        if 'Flags.Module' in owner:return 'module-initializer'
        if 'Flags.Case' in owner or 'Flags.Synthetic' in owner:return 'structural-constructor'
        if 'Flags.Abstract' in owner or 'Flags.Trait' in owner:return 'abstract-constructor'
    return kind

def encode(name):
    codes={'~':'$tilde','=':'$eq','<':'$less','>':'$greater','!':'$bang','#':'$hash','%':'$percent','^':'$up','&':'$amp','|':'$bar','*':'$times','/':'$div','+':'$plus','-':'$minus',':':'$colon','\\':'$bslash','?':'$qmark','@':'$at'}
    return ''.join(codes.get(c,c) for c in name) if name not in ('<init>','<clinit>') else name

def ancestors(name, seen=None):
    seen=set() if seen is None else seen
    if name in seen:return seen
    seen.add(name)
    if name in classes:
        for parent in [classes[name]['parent']]+classes[name]['interfaces']:ancestors(parent,seen)
    return seen
parents={name:ancestors(name) for name in classes}

rows=[]
for x in raw:
    row={**x,'classification':category(x)}
    if row['classification']=='runtime-entry':
        candidates=[m for m in methods.get((x['binaryOwner'],encode(x['name'])),[]) if not m['access'] & 0x40]
        if len(candidates)>1:
            positioned=[m for m in candidates if any(x['line']<=line<=x['endLine'] for line in m['lines'])]
            if len(positioned)==1:candidates=positioned
        row['bytecodeCandidates']=[m['descriptor'] for m in candidates]
        if len(candidates)==1:
            target=(x['binaryOwner'],encode(x['name']),candidates[0]['descriptor'])
            row['executedBy']=hits.get(target,[])
            row['status']='covered' if row['executedBy'] else 'uncovered'
        else:row['status']='unresolved-bytecode'
    if row['classification']=='abstract-member':
        declared=methods.get((x['binaryOwner'],encode(x['name'])),[])
        targets={(m['name'],m['descriptor']) for m in declared}
        implementations=[]
        for owner in classes:
            if owner != x['binaryOwner'] and x['binaryOwner'] in parents[owner]:
                for method in classes[owner]['methods']:
                    if (method['name'],method['descriptor']) in targets:
                        executed=hits.get((owner,method['name'],method['descriptor']),[])
                        if method['access'] & 0x40:
                            for call in method.get('calls',[]):
                                if call['owner']==owner:executed=sorted(set(executed)|set(hits.get((owner,call['name'],call['descriptor']),[])))
                        if executed:implementations.append({'owner':owner,'descriptor':method['descriptor'],'suites':executed})
        row['dispatchEvidence']=implementations
    rows.append(row)
(OUT/'report.json').write_text(json.dumps(rows,indent=2)+'\n')
gaps=[x for x in rows if x.get('status') in ('uncovered','unresolved-bytecode')]
(OUT/'uncovered.tsv').write_text('status\tmodule\tsymbol\tsource\tline\n'+''.join(f'{x["status"]}\t{x["source"].split("/")[0]}\t{x["id"]}\t{x["source"]}\t{x["line"]}\n' for x in gaps))
print('Declarations:',collections.Counter(x['classification'] for x in rows))
print('Runtime entries:',collections.Counter(x.get('status') for x in rows if 'status' in x))
print('Gaps by module:',collections.Counter(x['source'].split('/')[0] for x in gaps))
