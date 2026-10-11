#!/usr/bin/env python3
"""Frozen sampled-control analysis on retained native estimator arrays.

Ranks are cached per map. Every rank vector is exported for an independent R
check; a deterministic direct-call sample checks eyes4s and eyesim themselves.
"""
import argparse
from collections import defaultdict
import csv
import hashlib
import json
import math
import os
from pathlib import Path
import numpy as np
from cohort import summarize
from end_to_end import validate
from run import digest, dump, outside_git


def ranks(values):
    x = np.asarray(values, dtype=float)
    if not np.isfinite(x).all() or len(x) < 2 or np.ptp(x) == 0:
        raise ValueError('Spearman requires finite, nonconstant map values')
    order = np.argsort(x, kind='stable')
    sorted_x = x[order]
    starts = np.r_[0, np.flatnonzero(sorted_x[1:] != sorted_x[:-1]) + 1]
    ends = np.r_[starts[1:], len(x)]
    out = np.empty(len(x))
    out[order] = np.repeat((starts + ends + 1) / 2, ends - starts)
    return out


def controls(source, pool, seed, count):
    # Hash priorities give a stable draw independent of language RNG, source row
    # order, threads and resume boundaries. The exact identities are persisted.
    eligible = sorted(set(pool) - {source['template']})
    def priority(ref):
        return hashlib.sha256(json.dumps([seed,source['id'],ref],separators=(',',':')).encode()).digest()
    return sorted(eligible, key=lambda ref:(priority(ref),ref))[:count]


def score(maps, source, chosen):
    def unit(value):
        r = ranks(value); r -= r.mean()
        return r / np.linalg.norm(r)
    unit_maps = {key:unit(v) for key,v in maps.items()}
    result = []
    for s in source:
        u = unit_maps[s['id']]
        observed = float(np.dot(u, unit_maps[s['template']]))
        baseline = math.fsum(float(np.dot(u,unit_maps[t])) for t in chosen[s['id']]) / len(chosen[s['id']])
        result.append({'id':s['id'],'participant':s['participant'],'phase':s['phase'],
            'observed':observed,'baseline':baseline,'difference':observed-baseline})
    return result


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    for key in ('request','density-run','protocol','output-dir'):
        ap.add_argument('--'+key,type=Path,required=True)
    args = ap.parse_args()
    os.umask(0o077)
    if os.environ.get("CI") or os.environ.get("GITHUB_ACTIONS"):
        raise ValueError("Private planned analysis is disabled in CI")
    source = outside_git(args.request); root = outside_git(args.density_run)
    protocol = json.loads(outside_git(args.protocol).read_text())
    if (protocol['method'] != 'spearman' or type(protocol['controls']) is not int
            or protocol['controls'] < 1 or type(protocol['seed']) is not int):
        raise ValueError('Unsupported protocol')
    p = json.loads(source.read_text()); validate(p)
    receipt = json.loads((root/'receipt.json').read_text())
    if receipt['status'] != 'passed' or receipt['input_sha256'] != digest(source):
        raise ValueError('Density evidence does not qualify the input')
    out = outside_git(args.output_dir); out.mkdir(parents=True,exist_ok=False,mode=0o700)
    pool = defaultdict(set)
    for s in p['sources']: pool[s['participant']].add(s['template'])
    schedule = {s['id']:controls(s,pool[s['participant']],protocol['seed'],protocol['controls']) for s in p['sources']}
    dump(out/'schedule.json',schedule)
    all_scores=[]; inputs=[]
    items={i['id']:i for i in p['sources']+p['templates']}
    for index, entry in enumerate(receipt['participant_receipts']):
        # Locate preserved bundles by basename, but verify the exact receipt hash.
        old = Path(entry['receipt']); folder = root/old.parent.name
        if digest(folder/'receipt.json') != entry['sha256']:
            raise ValueError('Participant receipt changed')
        folder=folder/'comparison'
        q=json.loads((folder/'request.json').read_text())
        if any(items.get(i['id'])!=i for i in q['sources']+q['templates']):
            raise ValueError('Stored map operands differ from source request')
        native=json.loads((folder/'eyes4s.json').read_text())
        reference=json.loads((folder/'eyesim-maps.json').read_text())
        maps={'eyes4s':{m['id']:m['values'] for m in native['maps']},
              'eyesim':{m['id']:m['values'] for m in reference['maps']}}
        part=out/f'participant-{index:03d}'; part.mkdir()
        chosen={s['id']:schedule[s['id']] for s in q['sources']}
        scores={engine:score(values,q['sources'],chosen) for engine,values in maps.items()}
        # All pairs share exact identities; no engine is allowed to resample.
        rows=[{'id':s['id'],'participant':s['participant'],'phase':s['phase'],
            'controls':chosen[s['id']],**{e:scores[e][i] for e in scores}}
            for i,s in enumerate(q['sources'])]
        all_scores.extend(rows)
        verification={'nx':p['nx'],'ny':p['ny'],'maps':maps,
            'ranks':{e:{k:ranks(v).tolist() for k,v in ms.items()} for e,ms in maps.items()},
            'scores':rows,'sources':q['sources']}
        dump(part/'verification.json',verification)
        inputs.append({'participant':entry['participant'],'receipt_sha256':entry['sha256'],
            'native_maps_sha256':digest(folder/'eyes4s.json'),
            'reference_maps_sha256':digest(folder/'eyesim-maps.json'),
            'verification_sha256':digest(part/'verification.json')})
        print(f'Prepared sampled scores {index+1}/{len(receipt["participant_receipts"])}',flush=True)
    dump(out/'scores.json',all_scores)
    dump(out/'summary.json',summarize(p,all_scores))
    dump(out/'receipt.json',{'status':'awaiting-independent-checks','input_sha256':digest(source),
        'density_receipt_sha256':digest(root/'receipt.json'),'protocol_sha256':digest(args.protocol),
        'schedule_sha256':digest(out/'schedule.json'),'scores_sha256':digest(out/'scores.json'),
        'inputs':inputs,'script_sha256':digest(Path(__file__))})

if __name__=='__main__': main()
