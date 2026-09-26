#!/usr/bin/env python3
"""Fail closed on public API drift, absent invocation probes, or stale/failed named suites."""
import argparse
import collections
import hashlib
import json
import xml.etree.ElementTree as ET
from common import ROOT, OUT, MODULES, PROJECTS, fingerprint

BASE = ROOT / 'tools/api-audit'
parser = argparse.ArgumentParser()
parser.add_argument('--record', action='store_true')
args = parser.parse_args()
def fail(message): raise SystemExit('API audit: '+message)
def read(path): return json.loads(path.read_text())
def key(row): return row['kind']+' '+row['id']
def write(path, value): path.write_text(json.dumps(value,indent=2,sort_keys=True)+'\n')
receipt = read(OUT/'receipt.json')
if receipt['fingerprint'] != fingerprint(): fail('source candidate changed; run tools/api-audit/run.py')
rows = read(OUT/'report.json')
execution = read(OUT/'execution.json')
gaps = [x for x in rows if x.get('status') not in (None,'covered')]
missing_dispatch = [x for x in rows if x['classification']=='abstract-member' and not x['dispatchEvidence']]
if missing_dispatch: fail('uninvoked abstract members '+repr([x['id'] for x in missing_dispatch]))
if gaps: fail(f'{len(gaps)} unexercised entries; inspect target/api-audit/uncovered.tsv')

# Every suite in the generation must have successful XML; exact named test cases are retained.
suites = {}
for module in MODULES:
    for axis in ('jvm','js'):
        files = list((ROOT/module/f'.{axis}/target/test-reports').glob('TEST-*.xml'))
        if not files: fail(f'missing {module}/{axis} test reports')
        for file in files:
            if file.stat().st_mtime < receipt['started']: fail('stale test report '+str(file))
            tree = ET.parse(file).getroot()
            if any(int(tree.get(n,'0')) for n in ('failures','errors','skipped')) or tree.findall('.//failure') or tree.findall('.//error') or tree.findall('.//skipped'):
                fail('unsuccessful suite '+str(file))
            tests = sorted(t.get('name') for t in tree.findall('.//testcase'))
            if not tests or any(not n for n in tests): fail('suite lacks named execution '+str(file))
            suites[PROJECTS[module]+axis.upper()+'/'+tree.get('name')] = tests
for hit in execution['invocations']:
    for suite in hit['suites']:
        if suite not in suites: fail('invocation from an absent or renamed suite '+suite)

# Preserve all declarations in generated reports, and freeze intentional public declarations.
# Compiler scaffolding is mechanically classified in report.py, never hand-excluded by name.
public = [x for x in rows if x['classification'] not in ('internal','compiler-generated')]
js = {key(x): x for x in read(OUT/'inventory-js.json')['entries'] if x['category'] not in ('internal','compiler-generated')}
jvm = {key(x): x for x in public}
if set(js)-set(jvm): fail('unexpected JS-only declarations '+repr(sorted(set(js)-set(jvm))[:8]))
for k in set(jvm)&set(js):
    if jvm[k]['sourceType'] != js[k]['sourceType']: fail('unexplained platform type difference '+k)
only_jvm = set(jvm)-set(js)
for k in only_jvm:
    if not jvm[k]['source'].startswith('io/.jvm/src/main/'): fail('unexplained JVM-only declaration '+k)
inventory = {}
for x in public:
    inventory[key(x)] = [x['classification'],x['source'],'jvm' if key(x) in only_jvm else 'jvm+js',x['deprecated'],
        sorted(i for i in x['inherited'] if i.startswith('eyes4s.')), x['sourceType']]
if args.record:
    # One symbol per line keeps the large, complete inventory diffable.
    (BASE/'inventory.json').write_text('{\n'+',\n'.join('  '+json.dumps(k)+': '+json.dumps(v) for k,v in sorted(inventory.items()))+'\n}\n')
elif inventory != read(BASE/'inventory.json'):
    previous = read(BASE/'inventory.json')
    drift = [k for k in set(previous)|set(inventory) if previous.get(k)!=inventory.get(k)]
    fail(f'unmapped or changed public inventory ({len(drift)}): '+repr(sorted(drift)[:8]))

classes = {c['name']: c for c in execution['classes']}
class_suites = collections.defaultdict(set)
for hit in execution['invocations']: class_suites[hit['owner']].update(hit['suites'])
def ancestors(name, visited=None):
    visited = set() if visited is None else visited
    if name in visited: return visited
    visited.add(name)
    if name in classes:
        for parent in [classes[name]['parent']]+classes[name]['interfaces']: ancestors(parent,visited)
    return visited
parents = {name:ancestors(name) for name in classes}
abstracts = [x for x in public if x['kind']=='class' and ('Flags.Trait' in x['flags'] or 'Flags.Abstract' in x['flags'])]
phantoms = {'eyes4s.kernel.Unit2D','eyes4s.kernel.Unit2D$.Px','eyes4s.kernel.Unit2D$.Deg','eyes4s.kernel.Unit2D$.Mm','eyes4s.kernel.Unit2D$.Norm'}

def choose(candidates, module):
    # Prefer public consumer tests, then direct module conformance, then published laws.
    return min(candidates,key=lambda s:('consumer.' not in s, not s.startswith(PROJECTS[module]+'JVM/'), '/eyes4s.laws.' not in s, len(suites[s]),s))

if args.record:
    mapping = {key(x):choose(x['executedBy'],x['source'].split('/')[0]) for x in rows if x.get('status')=='covered'}
    abstraction_map = {}
    for x in abstracts:
        if x['id'] in phantoms:
            abstraction_map[key(x)] = {'classification':'phantom-unit','reason':'A type index, deliberately no runtime values; GeometrySuite typeCheckErrors proves unit separation.'}
            continue
        candidates = [n for n in classes if n!=x['binaryName'] and x['binaryName'] in parents[n] and not classes[n]['access']&0x600 and class_suites[n]]
        if not candidates: fail('no executed concrete inhabitant '+key(x))
        available = set().union(*(class_suites[n] for n in candidates))
        preferred = {
            "eyes4s.laws.AoiLaws":"lawsJVM/eyes4s.laws.AoiLawsSuite",
            "eyes4s.laws.CodecLaws":"lawsJVM/eyes4s.laws.CodecLawsSuite",
            "eyes4s.laws.ContrastLaws":"lawsJVM/eyes4s.laws.ContrastSuite",
            "eyes4s.laws.DetectorMetamorphicLaws":"lawsJVM/eyes4s.laws.DetectorMetamorphicLawsSuite",
            "eyes4s.laws.MachineLaws":"lawsJVM/eyes4s.laws.KernelLawsSuite",
            "eyes4s.laws.ManifestLaws":"lawsJVM/eyes4s.laws.ManifestLawSuite",
            "eyes4s.laws.MeasureLaws":"lawsJVM/eyes4s.laws.KernelLawsSuite",
            "eyes4s.laws.PayloadLaws":"lawsJVM/eyes4s.laws.ArtifactCodecLawSuite",
            "eyes4s.laws.RegionLaws":"lawsJVM/eyes4s.laws.KernelLawsSuite",
            "eyes4s.laws.SampleSequenceLaws":"lawsJVM/eyes4s.laws.DetectLawsSuite",
            "eyes4s.laws.ScanpathLaws":"lawsJVM/eyes4s.laws.ScanpathLawsSuite",
            "eyes4s.laws.SurfaceLaws":"lawsJVM/eyes4s.laws.KernelLawsSuite",
            "eyes4s.laws.TemporalSupportLaws":"lawsJVM/eyes4s.laws.TemporalSupportLawsSuite",
            "eyes4s.laws.Timestamped":"lawsJVM/eyes4s.laws.DetectLawsSuite",
            "eyes4s.laws.WarpLaws":"lawsJVM/eyes4s.laws.KernelLawsSuite",
            "eyes4s.compare.Kernel":"lawsJVM/consumer.KernelConformanceSuite",
            "eyes4s.compare.Compare":"lawsJVM/eyes4s.laws.KernelLawsSuite",
            "eyes4s.compare.SymmetricCompare":"lawsJVM/eyes4s.laws.KernelLawsSuite",
            "eyes4s.compare.Metric":"lawsJVM/eyes4s.laws.KernelLawsSuite",
            "eyes4s.compare.Semimetric":"lawsJVM/eyes4s.laws.KernelLawsSuite",
            "eyes4s.compare.Divergence":"lawsJVM/eyes4s.laws.KernelLawsSuite",
            "eyes4s.compare.BoundedCompare":"compareJVM/eyes4s.compare.ComparisonWorkSuite",
            "eyes4s.compare.ComparisonCursor":"compareJVM/eyes4s.compare.ComparisonWorkSuite",
            "eyes4s.compare.Alignment":"compareJVM/eyes4s.compare.AlignmentSuite",
            "eyes4s.kernel.Warp":"lawsJVM/eyes4s.laws.KernelLawsSuite",
            "eyes4s.kernel.Region":"lawsJVM/eyes4s.laws.KernelLawsSuite",
            "eyes4s.kernel.Surface":"lawsJVM/eyes4s.laws.KernelLawsSuite",
            "eyes4s.kernel.Module":"lawsJVM/eyes4s.laws.KernelLawsSuite",
            "eyes4s.kernel.Detector":"lawsJVM/eyes4s.laws.DetectLawsSuite",
            "eyes4s.kernel.Machine":"lawsJVM/eyes4s.laws.KernelLawsSuite",
            "eyes4s.plan.Stepwise":"lawsJVM/eyes4s.laws.ExecutionLawsSuite",
            "eyes4s.surface.Smoother":"surfaceJVM/eyes4s.surface.SmootherSuite",
            "eyes4s.design.Contrastable":"lawsJVM/eyes4s.laws.ContrastSuite",
            "eyes4s.design.ScoreMean":"lawsJVM/eyes4s.laws.ContrastSuite",
            "eyes4s.design.KeyDigest":"lawsJVM/eyes4s.laws.ContrastSuite",
            "eyes4s.plan.Diagnose":"planJVM/eyes4s.plan.DiagnosticCatalogSuite",
            "eyes4s.codec.ArtifactDecoders":"lawsJVM/eyes4s.laws.ManifestLawSuite",
            "eyes4s.codec.ByteSource":"lawsJVM/eyes4s.laws.ManifestLawSuite"
        }.get(x['id'])
        if preferred and preferred not in available: fail('preferred conformance did not instantiate '+x['id']+': '+preferred)
        suite = preferred or choose(available,x['source'].split('/')[0])
        instance = min(n for n in candidates if suite in class_suites[n])
        abstraction_map[key(x)] = {'classification':'data-sum' if 'Flags.Enum' in x['flags'] else 'behavior-or-sealed-data', 'instance':instance,'suite':suite}
    chosen = set(mapping.values()) | {x['suite'] for x in abstraction_map.values() if 'suite' in x}
    write(BASE/'evidence.json',{'entries':mapping,'abstractions':abstraction_map,
        'suites':{s:suites[s] for s in sorted(suites) if 'JVM/' in s},
        'jsSuites':{s:suites[s] for s in sorted(suites) if 'JS/' in s},
        'abstractDispatch':{key(x):x['dispatchEvidence'] for x in rows if x['classification']=='abstract-member'}})

reviewed = read(BASE/'evidence.json')
abstract_members = {key(x):x for x in rows if x['classification']=='abstract-member'}
if set(reviewed['abstractDispatch']) != set(abstract_members): fail('abstract member mapping drift')
for k, selected in reviewed['abstractDispatch'].items():
    if not selected: fail('abstract member lacks a reviewed implementation '+k)
    current = abstract_members[k]['dispatchEvidence']
    for witness in selected:
        if not any(w['owner']==witness['owner'] and w['descriptor']==witness['descriptor'] and set(witness['suites']) <= set(w['suites']) for w in current):
            fail('abstract dispatch witness disappeared '+k)
runtime = {key(x):x for x in rows if 'status' in x}
if set(reviewed['entries']) != set(runtime): fail('missing or extra entry/test mappings')
for k,suite in reviewed['entries'].items():
    if suite not in runtime[k]['executedBy']: fail('mapped suite did not invoke '+k+': '+suite)
    if suite not in reviewed['suites']: fail('mapping lacks named tests '+suite)
for suite, names in (reviewed['suites']|reviewed['jsSuites']).items():
    if not names or names != suites.get(suite): fail('removed, misnamed, or changed test mapping '+suite)
if set(reviewed['abstractions']) != {key(x) for x in abstracts}: fail('abstraction inventory drift')
for x in abstracts:
    spec = reviewed['abstractions'][key(x)]
    if spec['classification']=='phantom-unit':
        if x['id'] not in phantoms: fail('unreviewed phantom exemption '+key(x))
    elif spec['instance'] not in classes or x['binaryName'] not in parents[spec['instance']] or spec['suite'] not in class_suites[spec['instance']]:
        fail('uninhabited or unexecuted abstraction '+key(x))
    elif spec['suite'] not in reviewed['suites']: fail('abstraction lacks successful named conformance '+key(x))
summary = {'publicDeclarations':len(public),'runtimeEntries':len(runtime),'uncovered':0,'abstractions':len(abstracts),
           'phantomUnits':len(phantoms),'jvmOnlyDeclarations':len(only_jvm),
           'executedSuites':len(suites),'mappedSuites':len(reviewed['suites']),'mappedJsSuites':len(reviewed['jsSuites']),
           'classificationCounts':dict(collections.Counter(x['classification'] for x in rows)),
           'candidate':receipt}
write(OUT/'summary.json',summary)
print(f'API audit passed: {len(runtime)} runtime entries, {len(abstracts)} abstractions, {len(only_jvm)} explicit JVM-only declarations; zero uncovered entries.')
