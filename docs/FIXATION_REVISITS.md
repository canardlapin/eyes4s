# Fixation revisits and conditional spatial expectations

`FixationRevisits.analyze` in `eyes4s-compare` scores an ordered `Scanpath[U]`
using a checked `RevisitConfig[U]` and an optional `PointMeasure[U]` reference.
Parse the radius with `Distance.px` or `Distance.deg`, then pass it to
`RevisitConfig.of`; both constructors return `Either`. The configuration refuses
zero radius, invalid integer lag bounds and non-positive work budgets.

This is a direct analysis API. Subject/item reference pooling, saved study recipes,
result-table exports and Studio controls are subsequent integration work.

## What is counted

For fixation index `i`, eligible history contains every `j` with
`minimumLag <= i - j <= maximumLag`. The maximum defaults to `None` (unbounded),
and the minimum defaults to two. Indices are zero-based positions in the supplied
scanpath; importing or selecting a path must preserve any original-row mapping
through the existing admission evidence.

A match has distance **strictly less than** `openRadius`. `IncomingStepAtLeastRadius`
additionally requires the distance from the preceding fixation to be at least the
radius. `Unrestricted` disables this arrival condition. The norm is explicitly
`FixationGroundDistance.Euclidean` or `Manhattan`, including the existing numerical
convention. Euclidean distance uses the shared deterministic, scaled fdlibm hypot.
An unrepresentable distance returns an error naming both indices.

The arrival condition is not evidence that gaze left and re-entered the matched
location's region. With radius ten, `0 -> -9 -> 9` meets the arrival condition
while all three positions lie inside the original radius. No region segmentation,
missing-gap interpolation, duration weighting of observed fixations or timestamp
sorting occurs in this analysis. `Scanpath` has already checked event ordering,
finite centres and positive event durations. Empty episodes remain an admission
outcome; a singleton path returns one ineligible row.

Each input fixation has one `RevisitRow`. Its eligibility is `InsufficientHistory`,
`IncomingStepTooShort` or `Eligible`. Eligible rows carry the nearest eligible
distance, including on a miss. A return carries a `RevisitMatch` for the most recent
qualifying fixation, its lag, and its own distance. This distance need not be the
nearest distance. Non-opportunities have neither distance nor match.

Observed `rate` is revisits divided by opportunities, with `None` at zero
opportunities. It is not the fraction of all fixations. Results retain the original
path, configuration and optional reference, including its frame and provenance.

## Reference expectations

For each opportunity, condition the supplied reference measure on the same arrival
rule. The numerator is the reference mass within the union of all eligible
historical neighbourhoods and satisfying arrival; the denominator is all reference
mass satisfying arrival. Overlapping historical neighbourhoods never multiply a
reference point's mass. The result retains both masses and the number of reference
positions satisfying arrival (including zero-weight positions).

Uniform weights reproduce a fixation-weighted spatial pool. Duration weights or
weights giving each episode equal total mass define different baselines. Reference
frames must agree through `Agreement`; points must be finite. Empty and zero-mass
references remain explicit unavailable expectations. Reference coordinates have no
time axis, so they need no shared clock with the observed path.

`RevisitExpectation` distinguishes `NotRequested`, `NotAnOpportunity`,
`Available`, and `Unavailable` with `EmptyReference`, `ZeroReferenceMass` or
`NoDepartureSupport`. An available expectation carries a probability and its
denominator. Summary expectations are `NotRequested`, `NoOpportunities`,
`Incomplete` with the unavailable fixation indices, or `Available` with expected
counts/rate and observed-minus-expected counts/rate. A single missing opportunity
prevents a complete expected total; the observed count and denominator remain intact.

These probabilities condition on observed history. Excess revisits are descriptive
residuals, not a significance test or evidence that a complete simulated scanpath
would have the reported expected count. Subject/item pooling must declare episode
exclusions, repeated-viewing exclusions, geometry and weights in the study design.

## Cost and verification

For `n` fixations, `m` reference positions and maximum eligible-history size `L`,
the scorer takes `O(nL + nm)` time and `O(m)` working storage plus `O(n)` returned
rows. It allocates no distance matrix. Integer reference coverage counts update as
history enters and expires. Conditional masses are summed directly in reference
order with Neumaier compensation, avoiding cancellation from subtracting a large
excluded mass. The exact nearest-distance diagnostic still scans eligible history.

`maximumWork` defaults to 50 million distance evaluations/reference point visits.
A conservative bound is computed with `BigInt` before working-array allocation.
It includes reference validation, historical comparisons, coverage changes and
conditional reference scans. It can refuse a workload whose realized cost would
be smaller after non-departures; it never silently truncates history or samples
reference points.

`FixationRevisitLaws.analysis` publishes independent exhaustive laws. The portable
unit suite covers exact boundaries, most-recent versus nearest matching, expiry,
weighted and incomplete references, frames, numerical overflow, work refusal and
compile-time unit/lag rejection. Independent rational-predicate fixtures regenerate
with `python3 tools/r-parity/generate_revisits.py --check`.

The required historical eyesim baseline predates this feature and remains unchanged.
The supplemental [source pin](../tools/revisit-reference/source.json) and
[measured replay](../tools/revisit-reference/measured.json) cover the newer R method.
Run `python3 tools/revisit-reference/verify.py --eyesim /path/to/eyesim` with a
checkout containing that commit. The tool reads only pinned Git objects, verifies
their source hashes, compares the R outputs to the independent oracle, and checks
the recorded R/package versions. `--source-dir` accepts the exact hash-checked
source files instead; `--write` explicitly records a successor after review.
This is a source-method replay, not a full R package qualification. Weighted
reference cases have independent native oracles because the R method is unweighted.

The new R method's clean-input definition is the compatibility target. Its silent
row dropping, fractional-lag truncation, incomplete provenance and numerical
overflow are intentional native divergences. The measure is not equivalent to
[Voss et al. (2011)](https://pmc.ncbi.nlm.nih.gov/articles/PMC3150890/)'s backtracking
sequence definition, which excluded `A-B-C-A`;
[Kragel et al. (2021)](https://pmc.ncbi.nlm.nih.gov/articles/PMC8213228/)'s spatial-return
definition is the closer precedent.
