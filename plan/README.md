# eyes4s-plan

Typed, inspectable descriptions of matched/control fixation studies. `StudyPlan` records input
artifact identity, geometry, phases, weighting, estimation scales, failure policy and a typed
comparison definition. Its interpreter reuses occupancy, pairing, reduction and contrast operations.

Plans support structural equality, field-level diffs, prerequisite checks and independent results
for each scale. Custom comparison definitions retain typed parameters, scores and differences.
This module has no JSON, filesystem or streaming dependency.

Start with [saved studies](../docs/SAVED_STUDIES.md). General detection, AOI and temporal plan
vocabularies remain future work; this module currently implements the concrete fixation-study path.
