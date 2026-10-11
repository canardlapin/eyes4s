#!/usr/bin/env python3
"""Run the Studio MultiMatch workspace against independent command-line pair scores."""
import argparse
from collections import defaultdict
import json
import math
import os
from pathlib import Path
import subprocess
from end_to_end import ui, validate
from run import build, digest, dump, implementation, outside_git


def schedule(p):
    pool=defaultdict(set)
    for s in p['sources']: pool[s['participant']].add(s['template'])
    return [dict(source=s['id'],reference=t,kind=kind) for s in p['sources']
        for kind,refs in [('matched',[s['template']]),('control',sorted(pool[s['participant']]-{s['template']}))] for t in refs]


def reductions(p, rows):
    grouped=defaultdict(list)
    for row in rows: grouped[row['source']].append(row)
    components=('shape','direction','length','position','duration')
    result=[]
    for source in p['sources']:
        own=grouped[source['id']]
        matches=[r for r in own if r['kind']=='matched']
        controls=[r for r in own if r['kind']=='control']
        if len(matches)!=1 or not controls:
            raise ValueError('A native query lacks its matched/control design')
        values=None
        if all('values' in r for r in own):
            values={}
            for c in components:
                observed=matches[0]['values'][c]
                baseline=math.fsum(r['values'][c] for r in controls)/len(controls)
                values[c]={'observed':observed,'baseline':baseline,'difference':observed-baseline}
        result.append({'id':source['id'],'values':values,'unavailable_pairs':sum('values' not in r for r in own)})
    return result


def main():
    ap=argparse.ArgumentParser(description=__doc__)
    for flag in ('request','output-dir'): ap.add_argument('--'+flag,type=Path,required=True)
    args=ap.parse_args(); os.umask(0o077)
    if os.environ.get('CI') or os.environ.get('GITHUB_ACTIONS'):
        raise ValueError('Private UI execution is disabled in CI')
    source=outside_git(args.request); p=json.loads(source.read_text()); validate(p)
    expected=schedule(p)
    if 'pairs' in p and p['pairs']!=expected:
        # Order is retained if all pair identities and multiplicities agree.
        key=lambda r:(r['source'],r['reference'],r['kind'])
        if sorted(map(key,p['pairs']))!=sorted(map(key,expected)):
            raise ValueError('Declared pair schedule differs')
    else: p['pairs']=expected
    out=outside_git(args.output_dir); out.mkdir(mode=0o700,parents=True,exist_ok=False)
    provenance=implementation(); source_hash=digest(source)
    dump(out/'request.json',p); dump(out/'multimatch-request.json',p)
    classpath=build(out)
    with (out/'native.log').open('x') as log:
        subprocess.run(['java','-Xmx3g','-XX:ActiveProcessorCount=4','-cp',classpath,'eyes4s.examples.PrivateMultiMatchCli'],cwd=out,env=dict(os.environ,EYES4S_PRIVATE_RUN=str(out)),stdout=log,stderr=subprocess.STDOUT,check=True)
    (out/'multimatch-eyes4s.jsonl').rename(out/'expected.jsonl')
    with (out/'expected.jsonl').open() as stream:
        dump(out/'expected-queries.json',reductions(p,[json.loads(line) for line in stream]))
    result=ui(out,'eyes4s.studio.desktop.journey.ScanpathWorkbenchFxSuite')
    if implementation()!=provenance or digest(source)!=source_hash:
        raise ValueError('Source or implementation changed during run')
    outputs={f.name:digest(f) for f in out.iterdir() if f.is_file() and f.suffix in ('.json','.jsonl','.png')}
    dump(out/'receipt.json',{'status':'passed','implementation':provenance,'input_sha256':source_hash,
        'scope':'Dedicated Studio scanpath workspace, not the density .eyes project runner',
        'ui':result,'artifacts_sha256':outputs})
    print('MultiMatch CLI/UI pair agreement and saved-workspace verification passed.')

if __name__=='__main__': main()
