# Human-annotation corpus admission

This is the first gate of `bd-01M02N4CCS9425KPD4S0A0S9HX`, not a detector
validation report. `corpus.json` pins Lund2013 source revision, individual download
URLs, byte lengths and SHA-256 digests. Raw data is fetched to a separate cache;
none is bundled into the Scala artifacts or copied into this repository.

The upstream [EyeMovementDetectorEvaluation repository](https://github.com/richardandersson/EyeMovementDetectorEvaluation)
contains the two-annotator dataset used by Andersson et al. (2017),
DOI [10.3758/s13428-016-0738-9](https://doi.org/10.3758/s13428-016-0738-9).
It carries a GPL-3.0 license, whose exact bytes are pinned in the manifest.
We select the entire `data used in the article` subset and apply the single
replacement explicitly recommended in upstream `fix_by_Zemblys2018/readme.md`.
The replacement source remains separately named and hashed.

Reproduce with Python 3 and the recorded decoder versions:

```sh
uv run --with numpy==2.4.3 --with scipy==1.17.1 \
  tools/detector-benchmark/admit.py --cache /tmp/eyes4s-detector-corpus --check
```

Omit `--check` only to deliberately regenerate `admission.json`. Every cached or
downloaded file is verified before decoding. A changed file fails, without silently
refetching over a corrupt cache. No R, browser, credential or detector training is used.

The report contains 68 annotation files for 34 unique recordings: 14 image,
11 moving-dot and 9 video recordings. Labels preserve the upstream vocabulary:
fixation, saccade, glissade, pursuit, blink and undefined (with zero reserved as
unlabelled). The ontology comes from the pinned annotation tool, not a guessed
mapping from a secondary dataset. Thirty annotation files lack valid observed
timestamps. Five paired annotations have different source samples; those pairs are
reported rather than forced into a misleading sample agreement calculation.
Aligned pairs retain the full MN-by-RA confusion matrix, including undefined labels.

Remaining gates are deliberately visible:

- Define the adapter's nominal-timing policy for missing clocks and its treatment
  of invalid coordinates, blink/loss and glissade before running a detector.
- Freeze event matching and tolerance, tuning/evaluation partition and aggregation.
- Execute the actual eyes4s detector configurations and produce per-class/stimulus
  event scores plus controlled noise/dropout/rate-degradation curves.
- Admit a human-annotated reading corpus. Reading is absent here.
- Treat centre and peak-velocity reference errors as unavailable unless independently
  annotated reference values are supplied; event labels alone do not supply them.

Admission proves file identity, parsability and measured annotation coverage. It is
not a passing detector score, a consensus ground truth, or a release qualification.
