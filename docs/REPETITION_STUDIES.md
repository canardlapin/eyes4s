# Repetition studies: make the comparison set explicit

Use this workflow when the question is: **does a participant reinstate the same stimulus more
than other stimuli on another occasion?** The result is a per-focal-trial matched mean, control
mean and signed difference, with selected, successful, failed and contributing counts.

This is not the same estimand as grouping every trial by phase across participants. Both can
be expressed with ordinary relations; neither should be selected by an undocumented default.

## Start with fixation summaries

The runnable [RepetitionGuide.scala](../io/src/test/scala/eyes4s/examples/RepetitionGuide.scala)
uses only public APIs. It is compiled and exercised on JVM and Scala.js by
[RepetitionGuideSuite](../io/src/test/scala/eyes4s/io/RepetitionGuideSuite.scala).
The [synthetic CSV](../tools/r-parity/fixtures/matched-control.csv) has twelve trials and 48
fixations: two participants, three stimuli and two phases. It supplies onset and duration in
microseconds and pixel coordinates on an explicitly declared 2-by-2 frame. This tiny geometry
is an analytic fixture, not a recommended experimental resolution.

Call `RepetitionGuide.admit(csv, frame)` with your declared pixel frame. Inspect `sourceRows`,
`accepted` and `rejected` before calling `run`. The key is participant/image/phase; do not use
that schema to collapse several repetitions of the same phase. Use a custom key and an occasion
projection containing the repetition identifier for those experiments. A malformed fixation
rejects its whole trial with row-level evidence; `requireComplete` refuses incomplete admission.
No raw samples or recording identity are invented from fixation summaries.

## Choose the route

### One directed phase comparison, saved and exported

`RepetitionGuide.run(imported, grid, "recall", "encode")` constructs a duration-weighted,
binned cosine `StudyPlan`, saves it as versioned JSON, parses it back, executes it and builds
a tidy CSV document. Handle the returned `Either`; `RepetitionGuide.message` renders its
typed failures. The output exposes the input, saved description, typed result and export.

The matched reference is the same participant and stimulus in the reference phase. Controls
are the same participant and different stimuli in that phase. For this fixture there is one
matched reference and two controls per recall trial. The study retains estimation failures,
per-pair results, reduction counts and signed contrasts. See [saved studies](SAVED_STUDIES.md)
for plan inspection and [fixation studies](FIXATION_STUDIES.md) for admission and export schemas.
The saved JSON is a description referencing an input artifact, not an archive of the input CSV.

### Every distinct occasion, with optional bounded controls

`RepetitionDesign.withinParticipant(participant, stimulus, occasion, controlSelection)` builds
two inspectable `PairDesign.WithinDirected` values from typed key projections:

- `matched`: same participant, same stimulus, different occasion, excluding the full-key self edge;
- `controls`: same participant, different stimulus, different occasion, excluding self.

The default is exhaustive. Supply an ordinary keyed `Selection` for finite-cap controls;
matched references remain exhaustive. `evaluate(trials, inputHash, comparison, specification)`
returns both directed edge analyses, including pairing diagnostics and provenance. Call
`contrast(FailurePolicy.RequireAll)` for per-left-endpoint means and differences, or inspect
the edge rows first. The comparison's score must support `ScoreMean` and `Contrastable`;
the facade does not presume every score can be averaged or subtracted.

Both orientations are retained when both trials are focal candidates. Duplicate full keys are
excluded and reported by pairing; unmatched keys remain in the reduction coverage. With
`SuccessfulOnly`, failures remain visible and the contributing denominator is the number of
successful comparisons, not the number selected. Cap membership is stable under input reordering
because it uses the existing keyed sampler. Sampling is not an uncertainty estimate or a claim
of equivalence to exhaustive controls.

The exact direct composition, finite-cap example and low-level equivalence assertions are in
[RepetitionSuite](../design/src/test/scala/eyes4s/design/RepetitionSuite.scala). This facade does
not introduce a second sampler or reduction implementation. **The saved two-phase route above
does not yet persist arbitrary all-occasion projections or finite-cap repetition designs.**

## What the reference actually measures

At pinned eyesim revision `ecb9c496257bce51acd5330af6a5e7a8d5b84e05`,
`repetitive_similarity(condition_var = "phase", method = "cosine")` compares each row to all
other rows in the same phase and to all rows in other phases. On this input its denominators
are five and six, across participants and stimuli. The explicit reinstatement design above
has denominators one and two. These are different estimands, not interchangeable API spellings.

[repetition.json](../tools/r-parity/fixtures/repetition.json) records public R pairwise/reduced
outputs, duplicated-row behavior, singleton missing values and empty output. A separate rational
dot-product oracle checks both sets of means and all directed pair identities. Portable tests
also check duplicate-key rejection, unmatched trials, singleton/empty inputs, partial failures,
finite-cap nesting and equality of the direct and restored phase-specific results.

```sh
python3 tools/r-parity/generate_repetition.py --eyesim /path/to/eyesim --check
sbt 'designJVM/testOnly eyes4s.design.RepetitionSuite' 'designJS/testOnly eyes4s.design.RepetitionSuite' \
  'ioJVM/testOnly eyes4s.io.RepetitionGuideSuite' 'ioJS/testOnly eyes4s.io.RepetitionGuideSuite'
```

This evidence covers cosine on supplied duration maps, not the whole eyesim method matrix.
Pearson/Spearman/Fisher-z, L1/Jaccard, distance covariance, transport and multiscale aggregation
remain baseline work. For clipped-duration temporal windows use the
[temporal study guide](examples/TemporalStudyGuide.scala);
that workflow is distinct from sampling a static template density along a path.
