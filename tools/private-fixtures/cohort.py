#!/usr/bin/env python3
"""Run every declared query, partitioned by participant without changing its control pool."""
import argparse
from collections import defaultdict
from itertools import combinations
import json
import math
import os
from pathlib import Path
import subprocess
import sys

from end_to_end import validate
from run import dump, digest, implementation, outside_git


def mean(values):
    return math.fsum(values) / len(values)


def summarize(request, scores):
    metadata = {s['id']: s for s in request['sources']}
    if len(scores) != len(metadata) or {s['id'] for s in scores} != set(metadata):
        raise ValueError('Summary must account for exactly every query')
    groups = defaultdict(list)
    for row in scores:
        source = metadata[row['id']]
        if row['participant'] != source['participant'] or row['phase'] != source['phase']:
            raise ValueError('Summary query metadata differs')
        key = (row['participant'], row['phase'], source.get('object_type', 'unspecified'),
               source.get('probe', 'unspecified'))
        groups[key].append(row)
    cells = []
    for key, rows in sorted(groups.items()):
        cell = dict(zip(('participant', 'phase', 'cue', 'probe'), key))
        cell.update(queries=len(rows), **{engine: mean([r[engine]['difference'] for r in rows])
                    for engine in ('eyes4s', 'eyesim')})
        cell['delta'] = cell['eyes4s'] - cell['eyesim']
        cells.append(cell)
    conditions = defaultdict(list)
    for cell in cells:
        conditions[(cell['phase'], cell['cue'], cell['probe'])].append(cell)
    population = []
    for key, rows in sorted(conditions.items()):
        population.append(dict(zip(('phase', 'cue', 'probe'), key)) | {
            'participants': len(rows), 'queries': sum(r['queries'] for r in rows),
            **{engine: mean([r[engine] for r in rows]) for engine in ('eyes4s', 'eyesim', 'delta')}})
    contrasts = []
    # Conditional contrasts respect absent design cells: pair participants only where
    # both observed cells exist, without filling in structural or data exclusions.
    for factor, fixed in (('cue', 'probe'), ('probe', 'cue')):
        strata = defaultdict(dict)
        for cell in cells:
            strata[(cell['phase'], cell[fixed])].setdefault(cell[factor], {})[cell['participant']] = cell
        for (phase, fixed_value), levels in sorted(strata.items()):
            for a, b in combinations(sorted(levels), 2):
                participants = sorted(set(levels[a]) & set(levels[b]))
                if not participants:
                    continue
                differences = [{
                    'participant': p,
                    **{e: levels[a][p][e] - levels[b][p][e] for e in ('eyes4s', 'eyesim')}
                } for p in participants]
                contrasts.append({'phase': phase, 'factor': factor, fixed: fixed_value,
                    'minuend': a, 'subtrahend': b, 'participants': len(participants),
                    'unpaired_participants': sorted(set(levels[a]) ^ set(levels[b])),
                    **{e: mean([r[e] for r in differences]) for e in ('eyes4s', 'eyesim')},
                    'participant_differences': differences})
    return {'participant_cells': cells, 'condition_means': population, 'contrasts': contrasts,
            'weighting': 'Mean of trials within participant cell; equal participant means; paired participants for each contrast',
            'scope': 'Descriptive estimator comparison, not manuscript sampled-control inference or a fitted statistical model'}


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    for flag in ('request', 'eyesim', 'r-library', 'output-dir'):
        parser.add_argument('--' + flag, required=True, type=Path)
    parser.add_argument('--ui', action='store_true')
    parser.add_argument('--resume', action='store_true', help='Validate and reuse completed participant receipts')
    args = parser.parse_args()
    os.umask(0o077)
    if os.environ.get('CI') or os.environ.get('GITHUB_ACTIONS'):
        raise ValueError('Private cohort execution is disabled in CI')
    source = outside_git(args.request)
    request = json.loads(source.read_text())
    validate(request)
    original_hash, provenance = digest(source), implementation()
    out = outside_git(args.output_dir)
    out.mkdir(mode=0o700, parents=True, exist_ok=args.resume)
    manifest = {'input_sha256': original_hash, 'implementation': provenance,
                'ui': args.ui, 'reference_revision': subprocess.check_output(
                    ['git', '-C', str(args.eyesim), 'rev-parse', 'HEAD'], text=True).strip()}
    if args.resume:
        if json.loads((out / 'manifest.json').read_text()) != manifest:
            raise ValueError('Resume requires the same implementation, source, reference, and UI setting')
    else:
        dump(out / 'manifest.json', manifest)
    participants = sorted({s['participant'] for s in request['sources']})
    scores, receipts = [], []
    for index, participant in enumerate(participants):
        shard = {**request, 'sources': [s for s in request['sources'] if s['participant'] == participant],
                 'templates': [s for s in request['templates'] if s['participant'] == participant]}
        validate(shard)
        shard_path = out / f'participant-{index:03d}.json'
        result = out / f'participant-{index:03d}'
        if not shard_path.exists():
            dump(shard_path, shard)
        elif json.loads(shard_path.read_text()) != shard:
            raise ValueError('Participant request changed')
        completed = sorted(out.glob(f'participant-{index:03d}*/receipt.json'))
        if args.resume and completed:
            result = completed[-1].parent
        receipt_path = result / 'receipt.json'
        if not (args.resume and receipt_path.exists()):
            # A failed output remains evidence; a resume uses a fresh attempt directory.
            attempt = 0
            while result.exists():
                attempt += 1
                result = out / f'participant-{index:03d}-attempt-{attempt}'
            command = [sys.executable, str(Path(__file__).with_name('end_to_end.py')),
                       '--request', str(shard_path), '--eyesim', str(args.eyesim),
                       '--r-library', str(args.r_library), '--output-dir', str(result)]
            if args.ui:
                command.append('--ui')
            subprocess.run(command, check=True)
            receipt_path = result / 'receipt.json'
        receipt = json.loads(receipt_path.read_text())
        if (receipt['status'] != 'passed' or receipt['implementation'] != provenance
                or receipt['input_sha256'] != digest(shard_path)):
            raise ValueError('Participant receipt does not qualify this request')
        reference = json.loads((result / 'comparison/receipt.json').read_text())
        if reference['eyesim_revision'] != manifest['reference_revision']:
            raise ValueError('Reference revision changed')
        scores.extend(json.loads((result / 'comparison/end-to-end.json').read_text())['scores'])
        receipts.append({'participant': participant, 'receipt': str(receipt_path),
                         'sha256': digest(receipt_path)})
        print(f'Participant {index + 1}/{len(participants)} verified', flush=True)
    if digest(source) != original_hash or implementation() != provenance:
        raise ValueError('Source or implementation changed during cohort run')
    dump(out / 'summary.json', summarize(request, scores))
    dump(out / 'receipt.json', {'status': 'passed', **manifest, 'participants': len(participants),
         'queries': len(scores), 'participant_receipts': receipts,
         'summary_sha256': digest(out / 'summary.json')})


if __name__ == '__main__':
    main()
