# Serializable epoch plans

`eyes4s.plan.EpochPlan` resolves a `MarkSelector[K]` against an `ObservedTimeline[K]`.
Keys use `cats.Eq[K]`; selection is data, with no captured predicate. `First` and `Last`
use the timeline's chronological order, with stable input order at equal timestamps.
`Nth(NonNegativeLong)` is zero-based. `RequireUnique` accepts exactly one match.
Missing, ambiguous and out-of-range selection errors carry the trial, selector and count.

Construct a plan with `EpochPlan.of(selector, window, binWidth, finalBin)`. The window
uses relative signed microseconds and the width is a checked `PositiveSpan`. Resolve
with `plan.resolve(trialKey, observedTimeline, expectedClock, maximumBins)`, where the
explicit `NonNegativeLong` budget bounds allocation. A successful result exposes the
anchor, absolute half-open interval, ordered bins and optional excluded tail.

- `RequireExactDivision` rejects a nonzero remainder.
- `IncludeShortFinal` includes the final shorter interval.
- `ExcludeAndReport` returns the remainder as `excludedTail`.

For anchor 100 microseconds, window [-5,6), width 4: included bins are [95,99),
[99,103), [103,106). Excluding the final bin returns [103,106) as the tail. Empty
windows produce no bins or tail. A sub-bin window is either rejected, one short bin,
or entirely the returned tail. Anchor arithmetic and bin counts use exact integers;
overflow (including total duration exceeding Span) and budget excess fail before allocating bins. Clock checks use `Agreement`.

`EpochCodecs.plan(recipeSchema, keyCodec)` persists the recipe under caller-supplied
versioned identities, like the conditional domain codecs. Every time and Nth index is
a decimal string, preserving signed 64-bit times beyond JavaScript's exact-number range.
Unknown policies/versions, negative indices, nonpositive widths and reversed windows
are rejected. The key codec retains its own nested schema envelope.

`EpochPlanSuite` checks the partition against independent microsecond enumeration,
all selection/tail policies, large timestamps and named failures. `EpochCodecSuite`
pins a v1 JSON recipe and its executable meaning on JVM and Scala.js. This adds the
selector/binning boundary; it does not assert eyesim point-density sampling parity or
silently add epoch resolution to existing saved temporal recipes.
