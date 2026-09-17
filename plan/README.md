# eyes4s-plan

Typed, inspectable descriptions of analyses. `StudyPlan` records fixation-study input
artifact identity, geometry, phases, weighting, estimation scales, failure policy and a typed
comparison definition. Its interpreter reuses occupancy, pairing, reduction and contrast operations.

Plans support structural equality, field-level diffs, prerequisite checks and independent results
for each scale. Custom comparison definitions retain typed parameters, scores and differences.
This module has no JSON, filesystem or streaming dependency.

Concrete recording and temporal routes also exist; see the [saved recording example](../io/src/test/scala/eyes4s/io/PsychologyWorkflowSuite.scala)
and the [temporal guide](../docs/examples/TemporalStudyGuide.scala). They have bounded supported operation sets,
not a promise that every detector, measure or arbitrary function can be persisted.

Start with [saved studies](../docs/SAVED_STUDIES.md) or the [task index](../docs/START_HERE.md).
Arbitrary all-occasion repetition projections and finite-cap repetition selection are not yet
part of the saved study schema. Generic result archives and complete baseline method coverage
remain separate acceptance work.
