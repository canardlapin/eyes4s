# Deterministic fixation trajectories and finite controls

`FixationTrajectory.fromScanpath` reuses a checked, strictly ordered Scanpath.
`FixationTrajectory.empty(frame, clock)` represents empty support. `sample` takes
a query clock, exact microsecond `Instant`s and a `TrajectoryEndpoint`:

- `OnsetRange` selects the latest onset and is missing before the first or after
  the last onset. The last onset itself is included, also for a singleton path.
- `HoldLastOnset` uses the same step rule but holds the final fixation indefinitely.

Neither policy consults fixation offsets: a gap keeps the preceding position.
Queries can be repeated or out of order. Every time is retained with its source
fixation index and point, or a named missing reason. Clock compatibility goes
through `Agreement`. This is deterministic evaluation, not random sampling.

The pinned `sample_fixations` fast and slow calls return the same object: both
hold the final fixation after its onset and take the later row on duplicated
onsets, and they match `HoldLastOnset` on ordered and singleton paths.
`OnsetRange` is the native alternative that is missing after the last onset; it
agrees with the reference up to that onset. Native Scanpath rejects duplicated
onsets. R's empty fixation-group constructor fails; native empty trajectory
returns one missing row per query. The reference errors and warnings remain in
the fixture.

`DurationReplication` takes a parsed positive `ReplicationPeriod` and a required
maximum row count. Counts are `max(1, durationMicros / periodMicros)` using exact
integer division. Negative durations and limits are typed errors. The total is
checked with arbitrary-precision arithmetic before any replica allocation, so
neither Int nor Long overflow changes cardinality. Replicas retain source indices
and fixation objects; they do not pretend to form a Scanpath with duplicate onsets.

At duration 290000 microseconds and period 10000 microseconds native replication
produces 29 rows, as the pinned `rep_fixations` now does: it counts
`floor(duration * resolution)` with a floating-point tolerance, where earlier
revisions truncated `0.29 / (1/100)` to 28. Zero and sub-period durations produce one row under
the standalone count contract; actual fixation construction still requires
positive duration.

## Finite matched controls

Use typed `Pairing` relations to restrict strata and require different match
identities before `bottomK`. Occurrence keys distinguish repeated candidate rows;
a distinct occurrence of a true match is still excluded by the match relation.
All eligible nonmatching occurrences remain candidates. The selected count is
`min(cap, eligible)` and is retained separately from eligible count. A positive
cap is required; zero is a typed configuration error. R instead interprets zero
as no control analysis and omits its control columns.

`FiniteControlReferenceSuite` checks candidate multiplicity, strata, both counts,
seed effects, row-order invariance and duplicate occurrence-key ambiguity. The
existing sampler mutation suite additionally rejects ten faulty selection rules.
There is no promise of R PRNG identity. R seed and RNGkind are recorded in
`tools/r-parity/fixtures/sampling.json` with both optimized density-cosine and
generic fixation-overlap facade calls.

The pinned R algorithm removes every true-match copy, keeps each distinct
template once, and only then draws up to the cap without replacement. With two
copies of match A and one B, the exhaustive control for an A row is B alone, with
similarity 0 on orthogonal maps, and the control for B is A once. Its realized
count is `min(cap, distinct eligible templates)`, so a cap of one always yields a
control when one is eligible. Native relations reach the same population and
denominators when the right-hand table holds one row per template; pairing
against source occurrences instead keeps their multiplicity by construction.
`FiniteControlReferenceSuite` checks the template-table pairing against every
exhaustive reference row. The draws themselves are not claimed equal. Missing and
duplicate reference handling is separately pinned by the matched-control and
template-CV fixtures.

The shared trajectory is consumed by `PreparedDensityLookup.along`; the
[density fixture](DENSITY_SAMPLING.md) checks both `sample_density(times)` and
`template_sample` using this same primitive. Overlap and temporal bins reuse it;
neither needs a second time interpolator.

Both JVM and Scala.js run the conformance tests without R. The exact-onset source
mutant failed three tests, and the original source was restored. Offline regeneration:

```sh
python3 tools/r-parity/generate_sampling.py --eyesim /path/to/eyesim --check
```
