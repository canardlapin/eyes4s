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

The pinned `sample_fixations` fast and slow calls match these policies on ordered
and singleton paths. On duplicated onsets R's fast path averages coordinates and
its slow path takes the later row; native Scanpath rejects duplicates. R's empty
fixation-group constructor fails; native empty trajectory returns one missing row
per query. The reference errors and warnings remain in the fixture.

`DurationReplication` takes a parsed positive `ReplicationPeriod` and a required
maximum row count. Counts are `max(1, durationMicros / periodMicros)` using exact
integer division. Negative durations and limits are typed errors. The total is
checked with arbitrary-precision arithmetic before any replica allocation, so
neither Int nor Long overflow changes cardinality. Replicas retain source indices
and fixation objects; they do not pretend to form a Scanpath with duplicate onsets.

At duration 290000 microseconds and period 10000 microseconds native replication
produces 29 rows. Pinned R truncates `0.29 / (1/100)` to 28 because of binary
floating-point rounding. This is an intentional precision divergence, not a reason
to round native counts down. Zero and sub-period durations produce one row under
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

The pinned R algorithm samples the candidate multiset first, then removes only
the first true-match occurrence. Other true-match copies can survive. With two
copies of match A and one B, its exhaustive A control mean includes A and B;
with orthogonal maps it is 0.5 over two controls. Native eligibility removes both
A copies first, yielding the single B control with similarity 0. Cap-one R draws
can leave no control after exclusion; native positive-cap selection has a control
whenever its eligible pool is nonempty. These denominator differences are retained,
not presented as sampling parity. Missing and duplicate reference handling is
separately pinned by the matched-control and template-CV fixtures.

The shared trajectory is consumed by `PreparedDensityLookup.along`; the
[density fixture](DENSITY_SAMPLING.md) checks both `sample_density(times)` and
`template_sample` using this same primitive. Overlap and temporal bins reuse it;
neither needs a second time interpolator.

Both JVM and Scala.js run the conformance tests without R. The exact-onset source
mutant failed three tests, and the original source was restored. Offline regeneration:

```sh
python3 tools/r-parity/generate_sampling.py --eyesim /path/to/eyesim --check
```
