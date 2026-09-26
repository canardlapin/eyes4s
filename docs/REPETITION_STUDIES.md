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
[RepetitionSuite](../design/src/test/scala/eyes4s/design/RepetitionSuite.scala). This facade uses the existing sampler and reductions. The separate all-occasion recipe below
persists its registered projections and finite controls; the two-phase format keeps its original meaning.

## What the reference actually measures

At the eyesim revision pinned in [baseline.json](../tools/r-parity/baseline.json),
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

That original fixture covers cosine on supplied duration maps. The additional method and
multiscale evidence below extends it without changing those frozen inputs. For clipped-duration temporal windows use the
[temporal study guide](examples/TemporalStudyGuide.scala);
that workflow is distinct from sampling a static template density along a path.


## Save an all-occasion supplied-map design

`RepetitionPlan` is a separate recipe over `Trials[K, Unit, Mass[U]]`. Its input includes every
supplied map and key, one checked grid, method, relations, control selection and failure policy.
It does not infer KDE from fixation data. Use the existing smoother explicitly before admission.
The executable public consumer example is
[RepetitionPlanSuite](../codec/src/test/scala/eyes4s/codec/RepetitionPlanSuite.scala); its first
case runs the complete save, fresh-registration reopen and rerun workflow on both runtimes.

1. Register a `RepetitionLayout` with distinct versioned participant, stimulus and occasion
   projection identities. The corresponding `Projection[K, P]`, `Projection[K, I]` and
   `Projection[K, O]` keep their actual value types. An occasion may be a product such as
   `(phase, repetition)`; it need not be a display string. The example uses an integer
   participant and a typed occasion product across three occasions.
2. Choose `RepetitionRelations.withinParticipant` for reinstatement, or
   `referenceConditionGrouping(scope)` for the reference condition-grouping estimand. The latter
   matches each trial with every other trial of **its own occasion** and controls it with every
   trial of **another occasion**. It is not a reinstatement contrast: same-stimulus pairs across
   occasions, the pairs reinstatement matches, land among its controls, which is the inverted
   contrast recorded as eyesim issue #28. The `ParticipantScope` argument has no default:
   `WithinParticipant` adds `SameParticipant` to both roles, `AcrossParticipants` adds
   `DifferentParticipant`, and `Pooled` crosses participant boundaries exactly as
   `repetitive_similarity(condition_var = ...)` does. On the wire a plan stores its rule list,
   not the constructor name, so a saved pooled plan (`[SameOccasion]` / `[DifferentOccasion]`)
   decodes unchanged. `RepetitionRelations.of` accepts finite conjunctions
   of same/different participant, stimulus and occasion rules. It rejects empty, duplicate,
   contradictory or potentially overlapping matched/control relations. Both roles are directed
   and exclude full-key self edges. The relation order is preserved in diagnostics and provenance.
3. Choose a `MapSimilarityMethod`, `Selection.All` or checked `Selection.BottomK`, and
   `FailurePolicy`. Matched comparisons are exhaustive. Controls use the existing keyed sampler
   after eligibility and all true-match exclusions; no new RNG or sampler is introduced.
4. Construct with `RepetitionPlan.of`. Every map must agree with the declared nominal grid.
   Empty input and duplicate full keys remain admitted source data so the ordinary pairing
   diagnostics can report them. `run` returns matched/control edge analyses; `contrasts`
   derives per-focal means and differences under the declared failure policy.
5. Create `RepetitionPlanCodec.of` with a distinct recipe schema, typed key codec and
   `RepetitionRegistry`. Save its versioned JSON. A fresh registry with the declared layout
   reconstructs the same keys, eligible/selected directed endpoints, seed, cap, input and plan
   identities, failures, counts and provenance. Unknown schema versions, method revisions,
   projection identities or rule names are typed failures. The wire contains identities and
   finite rule cases, never arbitrary closures or string-valued key maps.

A registration owner must change the appropriate identity/version when projection meaning or
key encoding changes. An identity names registered behavior; it cannot prove that an arbitrary
user-supplied function is pure. The codec checks every declared identity against the chosen typed
registration and recomputes input/plan hashes. Those portable `ContentHash` values are change
identities, not cryptographic authentication.

The [pinned recipe](../codec/src/test/resources/eyes4s/repetition-plan-v1.json) contains the
custom-key example with exact 64-bit seed text. Its portable fixture also pins input hash
`26dd8b9539bed70d`, plan hash `18c9fcde538303e3` and the cap-one selected keys, independently
executed on JVM and Scala.js. Increasing the cap nests selected candidates; reversing source
order preserves membership while the input artifact identity still records source order.
The historical `eyes4s.study@1` fixture remains readable and re-encodes unchanged as a two-phase
study; the new recipe does not reinterpret that schema.

## Method breadth and nested multiscale means

The per-ticket [reference matrix](../tools/r-parity/fixtures/repetition-methods.json) calls
`repetitive_similarity` with pearson, spearman, fisherz, cosine, l1, jaccard, dcov and emd;
pairwise true/false; and multiscale mean/none. It retains duplicate, singleton and empty
outputs. The seven native non-transport methods use the [map comparison contracts](MAP_COMPARISON.md),
including average ties, explicit Fisher endpoints and distance correlation rather than covariance.
EMD remains measured reference evidence with the existing backend qualification and unavailable
native dispatch; multiscale EMD failures are retained, without an approximation alias.

Independent Cartesian enumeration determines directed pairs, self exclusions and same/other
condition denominators. Rational method oracles determine scalar scores and means at absolute
`MeanTolerance = 1e-12`. The condition-only reference crosses participant and stimulus boundaries;
within-participant reinstatement is an explicit different relation. Full-key duplicate exclusion
and missing/failed native results remain intentional differences from duplicated R observations.

For multiscale inputs, the reference first averages available scales **within each comparison**,
then averages those comparison means. It does not pool all scale values across comparisons.
`RepetitionAggregation.scales` and `.comparisons` expose this two-stage operation, with a named
`FailurePolicy` at each level. Every source scale and failure remains accessible; requested,
successful and contributing counts distinguish admission from reduction. `RequireAll` refuses
an incomplete level, while `SuccessfulOnly(minimum)` names the omission/minimum policy. Empty
means are errors. `MapScaleComparison.mean` keeps its original strict policy.

The heterogeneous-scale fixture includes a constant map at one sigma, so different comparisons
have different valid-scale counts; the nested and pooled results differ. Tests explicitly check
that difference, raw retained rows and both denominators. The saved supplied-map recipe represents
one map per trial; multiscale comparisons use the separate typed aggregation API and do not hide
an implicit scale policy inside that recipe.

```sh
python3 tools/r-parity/generate_repetition_methods.py --eyesim /path/to/eyesim --check
sbt 'designJVM/testOnly *RepetitionMethodsSuite' 'designJS/testOnly *RepetitionMethodsSuite' \
    'codecJVM/testOnly *RepetitionPlanSuite' 'codecJS/testOnly *RepetitionPlanSuite'
```
