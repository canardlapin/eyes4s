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
- `sampleRunCount` counts maximal consecutive sample membership, with background, blink and signal
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

## Fixation accounting and visits

`AoiSet.assign(scanpath, policy)` returns a `FixationAoiAssignment` with one membership
per input fixation. `FixationAoiLaws.accounting(assignments, tolerance)` publishes the
corresponding Discipline rules. The sample API remains available with the explicit
`AoiMetric.sampleRunCount` and CSV estimand `sample_run_count`.

A fixation-level visit is a maximal run of contained fixations in fixation order.
It survives intervening saccades, blinks, signal loss and other gaps. A background or
off-surface fixation ends the visit. Every visit starts with an entry, including a
contained first fixation, so `entryCount == visitCount`; `fixationCount` counts all
contained fixations. First-entry latency is relative to the first fixation onset,
not an inferred recording onset. These conventions implement the decision recorded
in bead `bd-01M2R18YTC8BF4T1890QX991RW`.

Dwell sums only fixation durations. Its proportion uses on-surface fixation time,
including background, and is absent when that denominator is zero. The report keeps
union, background, off-surface and inter-fixation time separately. Those times partition
the scanpath extent; duplicated AOI time accounts for overlaps without inflating
the denominator. A scanpath does not identify the causes of its gaps, so the report
does not label those gaps as measured blinks or saccades.

The laws independently reclassify centres against geometry under every membership
policy. They check exact accounting, dwell, fixation counts, first entries, visits,
ordered transitions and proportions. An all-contained scanpath has exactly one visit;
visit count never exceeds contained fixation count. The generator explicitly includes
all-contained paths and positive gaps. `FixationAoiLawsSuite` and `FixationAoiSuite`
also pin analytic gap, interruption, overlap and zero-denominator examples on both
platforms. Frame checks use `Agreement`, and overlap refusal names the fixation index,
centre, frame and competing AOIs.

The published fixation laws reject production mutations that split visits at positive
gaps, add a microsecond to every contained fixation's dwell, or divide by the elapsed
scanpath extent. All three mutants failed on JVM and Scala.js; the implementation was
restored byte for byte before ordinary verification.

`FixationAoiCodecs.assignment(schema)` persists the assignment under a caller-supplied
versioned schema. It reuses the established scanpath and AOI geometry wire forms,
retains source-supported fixation evidence, and replays assignment on decode. Stored
memberships, time accounting, measures and transitions must agree with that replay.

`FixationAoiTables.tables(assignment, key)` projects metrics and transitions into the
shared `eyes4s-results` table layer. CSV and Arrow use their existing writers. Times
and counts are Int64; missing proportions and first entries retain validity and
absence reasons. Each table names the fixation-time denominator and visit convention,
and binds exact geometry, timing, policy and source evidence in its input digest.
The sample export estimand is now `sample_run_count`; previous `run_count` columns
represented the same sample-level quantity and must not be read as fixation visits.

## Scope

The laws cover the static sample-time and fixation-time APIs. They do not cover the deferred entity-trace
API, generic entity keys, or the public pairwise-overlap and multiplicity tables that the
broader relational-attention design requires (PRD RA-1/RA-6).
