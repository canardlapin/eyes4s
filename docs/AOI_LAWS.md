# AOI accounting laws

`eyes4s-laws` publishes `AoiLaws.accounting(assignments, tolerance)`, a Discipline
rule set over a `Gen[AoiAssignment[U]]`. Use it with assignments from your own
validated recordings and AOI geometry:

The complete [AoiLawsSuite](../laws/src/test/scala/eyes4s/laws/AoiLawsSuite.scala) supplies checked assignments and a named tolerance, then executes the published laws on both platforms.

The suite checks the following contracts of the shipped static sample-time API:

- One membership per input sample, preserving blink, signal loss and off-surface
  reasons; membership agrees with source geometry and the chosen spatial policy.
- Visible union, background, exclusions and policy-censored time partition
  represented temporal support.
- Each area's dwell equals its independently selected sample support.
- First entry is the first assigned sample's onset relative to the recording start;
  a zero-support sample can still record an entry.
- Runs count maximal consecutive membership, with background, blink and signal
  loss separating runs.
- Summed dwell equals visible union plus duplicated time. A sample in three areas
  contributes one duration to union and two extra durations to duplicated time.
- Dwell proportions divide by visible union plus background. They are absent
  when that denominator is zero, including all-excluded and zero-support inputs.

Durations are exact integer microseconds. Expected totals use `BigInt` to avoid
hiding overflow in the oracle. Only proportions use the supplied named
`Tolerance`; the repository suite uses absolute `1e-15`, relative `1e-14`.
The oracle reclassifies source samples through their regions and policy, rather
than trusting `accountingHolds` or deriving every expectation from the reported
memberships. Temporal support itself is supplied by the separately tested core
ledger. Smallest-containing tests use the declared raster resolution.

Generators should cover all membership policies, two- and three-way overlap,
half-open region edges, background, each exclusion reason, and censored support.
For `RejectOverlap`, supply successful non-overlapping assignments to this suite;
`AoiSuite` separately checks rejected overlaps and operand-bearing diagnostics.
`AoiLawsSuite` also pins an analytic eight-sample example and checks zero
denominators. It runs on JVM and Scala.js.

## Mutation evidence

The suite rejects each mutant below. Each was applied to the actual `Aoi.scala`
implementation, tested with `sbt 'lawsJVM/testOnly eyes4s.laws.AoiLawsSuite'`, and
reverted before the next one; they are source mutations, not edited expected values.

| Mutant | Production expression changed | Observed rejection |
|---|---|---|
| Duplicate dwell | Add twice each sample's support in `AoiAssignment.measure` | Per-area dwell and proportion laws; analytic example |
| Drop background | Return zero `backgroundTime` from `AoiAssignment.report` | Conservation, independent totals and proportion laws; analytic example |
| Collapse higher overlap | Add only one extra duration whenever membership exceeds one, instead of `duration * (ids.length - 1)` | Independent overlap totals and dwell partition laws; analytic three-way example |

The recovered entry and run laws were also tested by replacing production first-entry
latencies with zero and run counts with zero. Both mutants failed the published laws
on JVM and Scala.js; the production source was restored byte for byte. Analytic cases
pin a zero-support entry and a recording with a nonzero start.

## Scope

The laws cover the static sample-time API. They do not cover the deferred entity-trace
API, generic entity keys, or the public pairwise-overlap and multiplicity tables that the
broader relational-attention design requires (PRD RA-1/RA-6).
