# Test repetition without hiding the controls

Within-participant reinstatement compares the same stimulus across occasions, against different
stimuli across those occasions within that participant. The participant restriction is part of
the question, not a default inferred from row order.

```scala mdoc
import eyes4s.design.*
final case class ViewingKey(participant: String, stimulus: String, occasion: Int)
val participant = Projection.named[ViewingKey, String]("participant")(_.participant)
val stimulus = Projection.named[ViewingKey, String]("stimulus")(_.stimulus)
val occasion = Projection.named[ViewingKey, Int]("occasion")(_.occasion)
val matched = Pairing.within[ViewingKey]
  .sameOn(participant).sameOn(stimulus).differentOn(occasion)
  .excludingSelf.canonicalUndirected
matched.render
```

Canonical-undirected pairs store each edge once and require a symmetric comparison. Directed
pairs retain orientation. `meanByEndpoint` explicitly mirrors an undirected edge to its two
endpoints; it is not the same denominator as `meanEdges`.

`RepetitionDesign` constructs the matched/control designs over these operations. Its `bottomK`
controls use keyed priorities: increasing the cap retains the previous sample; permuting rows
does not change the selected keys. Report the realized counts, seed and sample identifier.

The saved two-phase workflow is the ordinary `StudyPlan` in [getting started](getting-started.md).
`TemporalStudyPlan` adds named windows and observed coverage, retaining clipped fixation duration,
missing support and empty windows; it does not sample a static template along a trajectory.
Arbitrary all-occasion sampled-design persistence and the full eyesim method matrix remain gaps.

See [migration](migration.md) before comparing to eyesim's phase-only `repetitive_similarity`:
that function does not encode this within-participant reinstatement question.
