#!/usr/bin/env python3
"""Verify a pinned human-annotation corpus and report coverage and annotator agreement.

Raw upstream data stays in a separate cache; no detector is evaluated by this gate.
"""
import argparse
import hashlib
import json
from pathlib import Path
import urllib.request
import numpy as np
import scipy
from scipy.io import loadmat

HERE = Path(__file__).resolve().parent


def admitted_bytes(entry, cache):
    target = cache / entry['sha256']
    if not target.exists():
        data = urllib.request.urlopen(entry['url'], timeout=60).read()
        if len(data) != entry['bytes'] or hashlib.sha256(data).hexdigest() != entry['sha256']:
            raise ValueError(f"Downloaded corpus bytes differ: {entry['path']}")
        target.write_bytes(data)
    data = target.read_bytes()
    if len(data) != entry['bytes'] or hashlib.sha256(data).hexdigest() != entry['sha256']:
        raise ValueError(f"Cached corpus bytes differ: {entry['path']}")
    return target


def inspect(manifest, cache):
    annotations, sources, groups = [], {}, {}
    for entry in manifest['files']:
        path = admitted_bytes(entry, cache)
        if not entry['path'].endswith('.mat'):
            continue
        raw = loadmat(path, simplify_cells=True)
        data = raw['ETdata']
        pos = data['pos']
        if pos.ndim != 2 or pos.shape[1] != 6 or pos.shape[0] < 2:
            raise ValueError(f"Unexpected source shape: {entry['path']} {pos.shape}")
        times, x, y, labels = (pos[:,i] for i in (0,3,4,5))
        observed_times = np.isfinite(times).all() and (np.diff(times)>0).all() and (times==np.floor(times)).all()
        if not np.isfinite(labels).all() or not (labels==np.floor(labels)).all():
            raise ValueError(f"Noninteger annotation: {entry['path']}")
        if not set(map(int,labels)) <= set(map(int,manifest['labels'])):
            raise ValueError(f"Unknown labels: {entry['path']}")
        label_counts={manifest['labels'][str(k)]:int((labels==k).sum()) for k in map(int,manifest['labels'])}
        name, annotator = entry['recording'], entry['annotator']
        if (name,annotator) in sources:
            raise ValueError(f"Duplicate annotation: {name}/{annotator}")
        sources[name,annotator] = (pos[:,:5], labels)
        groups.setdefault(name,[]).append(annotator)
        annotations.append(dict(recording=name,annotator=annotator,stimulus_class=entry['stimulus_class'],
            samples=len(pos),timestamp_span_us=int(times[-1]-times[0]) if observed_times else None,
            timestamp_evidence='observed-strict-integer-microseconds' if observed_times else 'missing-or-invalid-requires-explicit-adapter-policy',
            missing_timestamp_samples=int((~np.isfinite(times)).sum()),
            sample_frequency_hz=float(data['sampFreq']),screen_metres=np.asarray(data['screenDim']).tolist(),
            screen_pixels=np.asarray(data['screenRes']).tolist(),view_distance_metres=float(data['viewDist']),
            nonfinite_coordinate_samples=int((~np.isfinite(x)|~np.isfinite(y)).sum()),
            zero_zero_coordinate_samples=int(((x==0)&(y==0)).sum()),
            label_counts=label_counts,sha256=entry['sha256']))
    agreement=[]
    for name,annotators in sorted(groups.items()):
        if sorted(annotators)!=['MN','RA']:
            agreement.append(dict(recording=name,status='missing-second-annotator',annotators=sorted(annotators)))
            continue
        (data_a,a),(data_b,b)=(sources[name,k] for k in ('MN','RA'))
        if data_a.shape != data_b.shape or not np.array_equal(data_a,data_b,equal_nan=True):
            agreement.append(dict(recording=name,status='source-samples-differ',annotators=['MN','RA']))
            continue
        confusion=[[int(((a==i)&(b==j)).sum()) for j in range(7)] for i in range(7)]
        agreement.append(dict(recording=name,status='aligned',samples=len(a),
            exact_label_agreements=int((a==b).sum()),confusion_rows_MN_columns_RA=confusion))
    coverage={kind:sorted({a['recording'] for a in annotations if a['stimulus_class']==kind}) for kind in ('image','dots','video')}
    return dict(schema=1,stage='corpus-admission-only',dataset=manifest['dataset'],revision=manifest['revision'],
        corpus_manifest_sha256=hashlib.sha256((HERE/'corpus.json').read_bytes()).hexdigest(),
        decoder=dict(numpy=np.__version__,scipy=scipy.__version__),annotations=annotations,
        stimulus_coverage=coverage,annotator_agreement=agreement,
        unavailable=['reading stimulus corpus','detector performance results','robustness curves',
          'independently annotated fixation centres','independently annotated peak velocities'],
        limitations=['No detector pass or accuracy claim is made by corpus admission.',
          'Source annotation glissade is retained; it is not silently merged into saccade.',
          'Undefined/unlabelled samples are retained in the agreement denominator and confusion matrix.',
          'Coordinate invalidity and blink/loss mapping require an explicit adapter policy before detection.',
          'Timestamp span excludes the final sample duration; no nominal-duration inference is reported.'])


def main():
    parser=argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--cache',type=Path,required=True)
    parser.add_argument('--check',action='store_true')
    args=parser.parse_args()
    args.cache.mkdir(parents=True,exist_ok=True)
    manifest=json.loads((HERE/'corpus.json').read_text())
    report=inspect(manifest,args.cache)
    encoded=json.dumps(report,indent=2)+'\n'
    output=HERE/'admission.json'
    if args.check:
        if output.read_text()!=encoded: raise ValueError('Corpus admission report drift')
    else: output.write_text(encoded)
    print(f"Verified {len(report['annotations'])} annotations; {len(report['annotator_agreement'])} unique recordings; detector qualification remains pending.")
if __name__=='__main__': main()
