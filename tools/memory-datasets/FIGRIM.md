# FIGRIM qualification contract

This is the admission and spatial-calculation slice of Mote `figrim-qualification`.
It does not qualify participant-level inference, chronological scanpath analysis,
paper replication, performance or the complete public researcher workflow.

## Regenerate the evidence

Use the manifest downloader from the repository root. NumPy/SciPy are optional
dependencies in `requirements.txt`; `audit` additionally needs GNU Octave.

```sh
python3 tools/memory-datasets/datasets.py fetch figrim --file allImages_release.mat
python3 tools/memory-datasets/datasets.py verify figrim
python3 tools/memory-datasets/datasets.py audit figrim
python3 tools/memory-datasets/datasets.py check figrim
python3 -m unittest discover -s tools/memory-datasets -p 'test_*.py' -v
```

`audit` and `check` use unique directories under `.datasets/memory/figrim`, or
the external cache selected with `--cache`/`EYES4S_DATASETS_DIR`. Each top-level
receipt records the source receipt, manifest digest, Git revision/tree/dirty
flag and hashes of generated artifacts. Retain both receipts: the Octave audit
does not run Scala, and the Scala check alone shares its source loader with its
NumPy numerical reference. Neither command downloads research data.

Commit only code, the manifest and synthetic tests. The downloaded MAT, source
identifiers, decoded rows, figures, logs and generated receipts stay in the cache.
No source initials or recording labels are embedded in the tests.

## Source contract and boundaries

The [pinned release README](https://github.com/cvzoya/figrim/blob/725dc8b77dd780850ca1197b6db12c8fc7c5f024/targetData/README.md)
defines `userdata(j)` consistently across images and identifies `enc`, `rec`,
`rec2` with the three presentations. Empty userdata fields mean that the image
was not presented to that slot. SDT codes are 1 hit, 2 false alarm, 3 miss,
4 correct rejection. The additional value -1 is preserved and flagged as a
missing sentinel; the reason for that value is not established by the README.
Empty response arrays and unexpected codes remain separate states.

The actual release contains 67 active slots/recording labels and 42 distinct
initials labels. These are different identifiers, not verified counts of unique
people. [Kragel and Voss's reanalysis](https://doi.org/10.1037/xge0000827)
reports 40 participants in its FIGRIM study; no authoritative public mapping from
release slots to those participants or exclusions was located. Never merge slots
using initials or parse recording-label fragments into invented person IDs.

The source also contains 758 populated image-slot records with `NaN` trial
numbers. Those values remain NaN, distinct from empty trial metadata. No lag,
presentation time or trial order is reconstructed from them.

The [published map code](https://github.com/cvzoya/figrim/blob/725dc8b77dd780850ca1197b6db12c8fc7c5f024/compMapsLocsForImage.m)
rounds coordinates, filters to positive pixels within the image dimensions and
indexes the matrix as `[row=y, column=x]`. The qualified raster conversion is
MATLAB rounding (ties away from zero), retain rounded coordinates in 1..1000,
then subtract one for eyes4s's zero-origin right/down frame. Raw doubles remain
available separately. `raw_zero_x/y` are preserved coordinates, not a second
qualified estimator or an implicit change to the raster calculation.

The MAT arrays have no timestamps or durations. Source row ordinals are retained,
including gaps from rejected coordinates, but chronology is unresolved. The map
code is order-insensitive; it cannot prove that row order is fixation order. The
reanalysis paper describes serialized sequences without a public preprocessing
implementation establishing their relationship to these MAT rows. No artificial
times or durations are created to construct timed Scanpath values.

Six documented duplicate-image pairs are kept as original filenames and assigned
canonical families. The current spatial capability check uses one lexical-first
same-slot/same-category different-family encoding control. This is a reproducible
software workload, not the balanced control estimator required for inference.

## Accounting and independent checks

| Artifact | Contract |
| --- | --- |
| `phase-ledger.tsv` | Every image/slot/phase, including empty slots, missing fields, empty gaze and all-invalid gaze. Source identifiers, SDT and admission counts remain explicit. |
| `location-ledger.tsv` | Every original coordinate, original row ordinal, rounded coordinate, admission flag/reason and transformed coordinates. Invalid values are retained. |
| `bins.tsv` | Sparse integer counts with explicit x/y cell identities. |
| `admission.json` | Counts, missingness, identity-label inventory, geometry and qualification limits. |
| `trials.csv`, `locations.tsv`, `pairs.tsv` | The actual inputs used by the Scala spatial check. |
| `semantic-pairs.tsv` | Audit-only translation of the actual execution pair IDs through trial identities; never a substitute made from descriptive pair metadata. |
| `scala-bins.tsv`, `scores.tsv` | Actual eyes4s results, compared against cell counts and cosine references. |

Octave reads the original MAT with `load`; SciPy reads it with `loadmat`. The
implementations independently apply admission, histogramming and baseline pair
selection. The audit compares all metadata and coordinate values exactly after
numeric text parsing (NaN equals NaN for this comparison), with no numeric
tolerance. It rejects missing or duplicate keys. Unsupported geometry, grid,
weighting or duplicate-family declarations cannot receive a PASS for an unchanged
computation. The witness deliberately supports the shipped duplicate contract;
custom manifests that change it are blocked.

On the pinned source, the expected full accounting is:

| Quantity | Count |
| --- | ---: |
| Images | 630 |
| Image-slot-phase records | 126,630 |
| Original coordinate rows | 182,535 |
| Admitted / rejected coordinate rows | 182,532 / 3 |
| Nonempty admitted phase sets | 30,012 |
| Occupied grid cells across phase sets | 142,010 |
| Eligible baseline retrieval comparisons | 19,718 |
| Retrieval sets without an eligible baseline comparison | 208 |

The eyes4s runner builds `PointMeasure`, `Grid` and normalized `Surface` values,
verifies every occupied bin, and applies `Distribution.cosine`. The existing
cosine absolute tolerance remains 1e-12; it is not widened to obtain agreement.
Actual cell checks are necessary because an axis swap shared by every map can
leave all cosine scores unchanged.

Synthetic tests cover nonsquare asymmetric geometry, positive/negative half-pixel
ties, boundaries, invalid coordinates, empty/missing phases, integer MATLAB
arrays, source NaN metadata, SDT shape/codes and duplicate families. Deliberate
identity, phase, response, ordinal, coordinate, cell and executed-pair mutations
must fail. The Octave test is skipped when Octave is absent; the explicit `audit`
command then reports BLOCKED, never a successful shared-reader fallback.

## Remaining qualification work

Participant linkage and chronological row meaning require additional authoritative
source information. Those block independent-person controls, participant-level
uncertainty and real-data order analysis, respectively. Session/slot-scoped spatial
work can continue without resolving them, provided it is labeled accordingly.

Real-image overlay validation is also outstanding. The pinned README advertises
`http://figrim.mit.edu/Targets.zip` (45,109,886 bytes). During this assessment the
HTTPS hostname certificate did not validate, and two attempts using the published
HTTP URL reset mid-download. No archive was admitted and no real overlay is
claimed. Asymmetric synthetic geometry and exact raw/cell comparison passed.

Later slices must replace the lexical control with explicit balanced spatial
contrasts; calibrate any inference on suitable synthetic data; demonstrate public
save/reopen/export workflows; and measure the frozen workload's resources. A
null empirical memory effect is compatible with correct software. The full
repository landing gate, merge and publication remain separate from this slice.
