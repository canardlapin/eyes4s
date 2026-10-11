#!/usr/bin/env python3
"""Freeze a common query/reference population before comparing density and scanpaths."""
import argparse
from collections import Counter, defaultdict
import json
import os
from pathlib import Path
from end_to_end import validate
from run import digest, dump, outside_git


def select(request, minimum=3):
    validate(request)
    if type(minimum) is not int or minimum < 3:
        raise ValueError('Common scanpath eligibility requires an integer minimum >= 3')
    templates = {t['id']: t for t in request['templates']}
    decisions = []
    candidates = []
    for s in request['sources']:
        reasons = []
        if len(s['fixations']['x']) < minimum:
            reasons.append('query_below_minimum')
        if len(templates[s['template']]['fixations']['x']) < minimum:
            reasons.append('matched_template_below_minimum')
        decisions.append({'id': s['id'], 'participant': s['participant'], 'phase': s['phase'],
                          'cue': s.get('object_type'), 'probe': s.get('probe'), 'reasons': reasons})
        if not reasons:
            candidates.append(s)
    # The original estimand defines the pool by templates represented by queries,
    # shared across all phases. Reapply that definition after eligibility, once.
    pools = defaultdict(set)
    for s in candidates:
        pools[s['participant']].add(s['template'])
    retained = {s['id'] for s in candidates if len(pools[s['participant']]) >= 2}
    for d in decisions:
        if not d['reasons'] and d['id'] not in retained:
            d['reasons'].append('no_distinct_eligible_control')
    sources = [s for s in request['sources'] if s['id'] in retained]
    used = {s['template'] for s in sources}
    template_decisions = [{'id': t['id'], 'participant': t['participant'],
        'reason': ('retained' if t['id'] in used else 'template_below_minimum'
                   if len(t['fixations']['x']) < minimum else 'no_retained_query_represents_template')}
        for t in request['templates']]
    result = {**request, 'sources': sources,
              'templates': [t for t in request['templates'] if t['id'] in used],
              'min_fixations': minimum,
              'common_eligibility': {'minimum': minimum,
                  'rule': 'Apply minimum to queries and matched templates before freezing the represented reference pool across phases; require a distinct control',
                  'changed_estimand': True, 'query_decisions': decisions,
                  'template_decisions': template_decisions}}
    if not sources:
        raise ValueError('No queries have common eligibility and a distinct control')
    validate(result)
    strata = defaultdict(Counter)
    for d in decisions:
        key = (d['phase'], d['cue'], d['probe'])
        strata[key]['original'] += 1
        strata[key]['retained' if not d['reasons'] else 'excluded'] += 1
    accounting = {'original_queries': len(decisions), 'retained_queries': len(sources),
        'original_templates': len(templates), 'retained_templates': len(used),
        'original_participants': len({s['participant'] for s in request['sources']}),
        'retained_participants': len({s['participant'] for s in sources}),
        'excluded_participants': sorted({s['participant'] for s in request['sources']} - {s['participant'] for s in sources}),
        'query_reason_counts': dict(Counter(r for d in decisions for r in d['reasons'])),
        'template_reason_counts': dict(Counter(d['reason'] for d in template_decisions)),
        'strata': [dict(zip(('phase','cue','probe'), k), **v) for k,v in sorted(strata.items())]}
    return result, accounting


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument('--request', type=Path, required=True)
    ap.add_argument('--output-dir', type=Path, required=True)
    args = ap.parse_args()
    os.umask(0o077)
    if os.environ.get("CI") or os.environ.get("GITHUB_ACTIONS"):
        raise ValueError("Private eligibility execution is disabled in CI")
    source, out = outside_git(args.request), outside_git(args.output_dir)
    request, accounting = select(json.loads(source.read_text()))
    out.mkdir(parents=True, mode=0o700, exist_ok=False)
    dump(out/'request.json', request)
    dump(out/'eligibility.json', accounting)
    dump(out/'receipt.json', {'status':'prepared', 'source_sha256':digest(source),
        'request_sha256':digest(out/'request.json'), 'accounting_sha256':digest(out/'eligibility.json'),
        'selection_script_sha256':digest(Path(__file__)),
        'interpretation':'Separate sensitivity analysis with a changed eligible population and control pool; original results retained'})
    print(json.dumps(accounting | {'strata':'see eligibility.json'}))

if __name__ == '__main__':
    main()
