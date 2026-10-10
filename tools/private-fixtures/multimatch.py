#!/usr/bin/env python3
"""Five-component MultiMatch on exactly the pairs of a verified private density run."""
import argparse
from collections import Counter, defaultdict
from concurrent.futures import ThreadPoolExecutor
import itertools
import json
import math
import os
from pathlib import Path
import shutil
import subprocess
import tarfile

from cohort import summarize
from run import ROOT, Tolerance, build, digest, dump, implementation, outside_git

COMPONENTS = ('shape', 'direction', 'length', 'position', 'duration')
TOLERANCE = Tolerance(absolute=1e-12, relative=1e-12)


def read(path):
    return json.loads(path.read_text())


def compare(request, folder):
    grouped = defaultdict(list)
    counts = Counter()
    maxima = dict.fromkeys(COMPONENTS, 0.0)
    discrepancies = []
    items = {v['id']: v for v in request['sources'] + request['templates']}
    with (folder/'multimatch-eyes4s.jsonl').open() as native, (folder/'multimatch-eyesim.jsonl').open() as reference:
        for expected, a, b in itertools.zip_longest(request['pairs'], native, reference):
            if expected is None or a is None or b is None:
                raise ValueError('MultiMatch pair cardinality differs')
            a, b = json.loads(a), json.loads(b)
            if any(v.get(k) != value for v in (a,b) for k,value in expected.items()):
                raise ValueError('MultiMatch exact pair identities differ')
            valid = {engine: 'values' in value and set(value['values']) == set(COMPONENTS)
                     and all(isinstance(x,(float,int)) and math.isfinite(x) for x in value['values'].values())
                     for engine,value in (('eyes4s',a),('eyesim',b))}
            short = any(len(items[expected[k]]['fixations']['x']) < 3 for k in ('source','reference'))
            if all(valid.values()):
                counts['comparable'] += 1
                delta = {k:a['values'][k]-b['values'][k] for k in COMPONENTS}
                for k in COMPONENTS:
                    maxima[k] = max(maxima[k],abs(delta[k]))
                if not all(TOLERANCE.matches(a['values'][k],b['values'][k]) for k in COMPONENTS):
                    discrepancies.append({**expected,'eyes4s':a['values'],'eyesim':b['values'],'delta':delta})
            elif short and not any(valid.values()):
                counts['short_in_both'] += 1
            else:
                counts['other_unavailable'] += 1
                discrepancies.append({**expected,'eyes4s':a,'eyesim':b})
            grouped[expected['source']].append((expected,a,b,valid))
    queries = []
    for source in request['sources']:
        pairs = grouped[source['id']]
        matched = [v for v in pairs if v[0]['kind']=='matched']
        controls = [v for v in pairs if v[0]['kind']=='control']
        if len(matched)!=1 or not controls:
            raise ValueError('A query lacks its declared match or controls')
        result = {'id':source['id'],'participant':source['participant'],'phase':source['phase'],
                  'template':source['template'],'controls':[v[0]['reference'] for v in controls],
                  'unavailable_pairs':[v[0] for v in pairs if not all(v[3].values())]}
        for engine,index in (('eyes4s',1),('eyesim',2)):
            if all(v[3][engine] for v in pairs):
                observed = matched[0][index]['values']
                baseline = {k:math.fsum(v[index]['values'][k] for v in controls)/len(controls) for k in COMPONENTS}
                result[engine] = {k:{'observed':observed[k],'baseline':baseline[k],
                                    'difference':observed[k]-baseline[k]} for k in COMPONENTS}
            else:
                result[engine] = None
        queries.append(result)
    dump(folder/'comparison.json', {'counts':dict(counts),'maximum_absolute_error':maxima,
        'disagreements':discrepancies,'queries':queries})
    return queries, counts, maxima, len(discrepancies)


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    for arg in ('request','density-run','r-library','output-dir'):
        parser.add_argument('--'+arg,required=True,type=Path)
    parser.add_argument("--workers",type=int,default=2,choices=range(1,5))
    args=parser.parse_args()
    os.umask(0o077)
    if os.environ.get('CI') or os.environ.get('GITHUB_ACTIONS'):
        raise ValueError('Private MultiMatch is disabled in CI')
    source=outside_git(args.request)
    request=read(source)
    source_hash=digest(source)
    density=outside_git(args.density_run)
    evidence=read(density/'receipt.json')
    if evidence['status']!='passed' or evidence['input_sha256']!=source_hash:
        raise ValueError('Density receipt does not qualify this request')
    out=outside_git(args.output_dir)
    out.mkdir(mode=0o700,parents=True,exist_ok=False)
    provenance=implementation()
    classpath=build(out)
    shards=evidence.get('participant_receipts',[{'receipt':str(density/'receipt.json'),'sha256':digest(density/'receipt.json')}])
    all_queries=[]; total=Counter(); maxima=dict.fromkeys(COMPONENTS,0.0); differences=0
    receipts=[]
    original_items={v['id']:v for v in request['sources']+request['templates']}
    def shard_run(index,entry):
        receipt_path=outside_git(Path(entry['receipt']))
        if digest(receipt_path)!=entry['sha256']:
            raise ValueError('Density shard receipt changed')
        parent=receipt_path.parent/'comparison'
        shard=read(parent/'request.json')
        pair_source=read(parent/'eyes4s.json')['sources']
        by_id={v['id']:v for v in shard['sources']}
        pairs=[]
        expected_pool=defaultdict(set)
        for v in shard['sources']:
            expected_pool[v['participant']].add(v['template'])
        if len(pair_source)!=len(by_id) or {v['id'] for v in pair_source}!=set(by_id):
            raise ValueError('Density pair source identities changed')
        for row in pair_source:
            owner=by_id[row['id']]['participant']
            controls=sorted(expected_pool[owner]-{row['template']})
            if sorted(row['controls'])!=controls:
                raise ValueError('Density control identities changed')
            if row['template'] != by_id[row['id']]['template']:
                raise ValueError('Density match changed')
            for kind,refs in (('matched',[row['template']]),('control',row['controls'])):
                pairs.extend({'source':row['id'],'reference':ref,'kind':kind} for ref in refs)
        for item in shard['sources']+shard['templates']:
            expected=original_items[item['id']]
            if item!=expected:
                raise ValueError('Density source coordinates or metadata changed')
        shard['pairs']=pairs
        folder=out/f'participant-{index:03d}'; folder.mkdir(mode=0o700)
        dump(folder/'multimatch-request.json',shard)
        reference_receipt=read(parent/'receipt.json')
        own_receipt=read(receipt_path)
        if digest(parent/'receipt.json')!=own_receipt['reference_receipt_sha256']:
            raise ValueError('Density reference receipt changed')
        # This library was freshly installed from the archived revision by compare.py.
        # Verify source archive identity, and retain that receipt in this run.
        if digest(parent/'eyesim.tar')!=reference_receipt['eyesim_archive_sha256']:
            raise ValueError('Reference archive changed')
        shutil.copyfile(parent/'receipt.json',folder/'density-reference-receipt.json')
        with (folder/'jvm.log').open('x') as log:
            subprocess.run(['java','-Xmx2g','-XX:ActiveProcessorCount=2','-cp',classpath,'eyes4s.examples.PrivateMultiMatchCli'],
                cwd=folder,env=dict(os.environ,EYES4S_PRIVATE_RUN=str(folder)),stdout=log,stderr=subprocess.STDOUT,check=True)
        library=folder/'library'; library.mkdir()
        ref_source=folder/'eyesim-source'; ref_source.mkdir()
        with tarfile.open(parent/'eyesim.tar') as archive:
            members=archive.getmembers()
            if any(not (ref_source/m.name).resolve().is_relative_to(ref_source)
                   or not (m.isfile() or m.isdir()) for m in members):
                raise ValueError('Unsafe reference archive')
            archive.extractall(ref_source,members=members)
        with (folder/'reference.log').open('x') as log:
            subprocess.run(['R','CMD','INSTALL','--clean',f'--library={library}',str(ref_source)],
                cwd=folder,env=dict(os.environ,R_LIBS=str(args.r_library)),stdout=log,stderr=subprocess.STDOUT,check=True)
            subprocess.run(['Rscript','--vanilla',str(Path(__file__).with_name('multimatch.R')),
                str(library),str(folder/'multimatch-request.json'),str(folder/'multimatch-eyesim.jsonl')],
                cwd=folder,env=dict(os.environ,R_LIBS=str(args.r_library)),stdout=log,stderr=subprocess.STDOUT,check=True)
        queries,counts,errors,disagreements=compare(shard,folder)
        receipt={'path':str(folder/'comparison.json'),'sha256':digest(folder/'comparison.json')}
        return queries,counts,errors,disagreements,receipt
    with ThreadPoolExecutor(max_workers=args.workers) as executor:
        for index,result in enumerate(executor.map(lambda pair: shard_run(*pair),enumerate(shards))):
            queries,counts,errors,disagreements,receipt=result
            all_queries.extend(queries); total.update(counts); differences+=disagreements
            maxima={k:max(maxima[k],errors[k]) for k in COMPONENTS}
            receipts.append(receipt)
            print(f'MultiMatch participant {index+1}/{len(shards)} accounted for',flush=True)
    summaries={}
    included=[q for q in all_queries if q['eyes4s'] is not None and q['eyesim'] is not None]
    ids={q['id'] for q in included}
    selected={**request,'sources':[s for s in request['sources'] if s['id'] in ids]}
    for component in COMPONENTS:
        summaries[component]=summarize(selected,[{**q,**{e:q[e][component] for e in ('eyes4s','eyesim')}} for q in included])
    if digest(source)!=source_hash or implementation()!=provenance:
        raise ValueError('Source or implementation changed during MultiMatch run')
    dump(out/'summary.json',{'components':summaries,
        'unavailable_queries':[q for q in all_queries if q['id'] not in ids],
        'policy':'Require all exact density pairs; a short/failed control makes the query unavailable, without resampling',
        'normalization':'Physical screen diagonal, original screen-pixel coordinates; same cropped fixation sequences as density',
        'grouping':False,'omitted_reference_component':'mm_position_emd is not a MultiMatch dimension'})
    dump(out/'receipt.json',{'status':'passed' if differences==0 else 'review-required',
        'implementation':provenance,'input_sha256':source_hash,'density_receipt_sha256':digest(density/'receipt.json'),
        'queries':len(all_queries),'fully_comparable_queries':len(included),'pairs':dict(total),
        'disagreements':differences,'maximum_absolute_error':maxima,'tolerance':vars(TOLERANCE),
        'participant_receipts':receipts,'summary_sha256':digest(out/'summary.json')})


if __name__=='__main__':
    main()
