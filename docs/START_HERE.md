# Start with your scientific task

eyes4s separates pure analysis from loading and exporting data. Start with the workflow matching
your input; each guide states what is implemented and what its evidence does not cover.

| Your task | Start here | What you retain |
|---|---|---|
| Compare existing fixation summaries to matched templates and controls | [Fixation studies](FIXATION_STUDIES.md) | Trial keys, admitted/rejected rows, signed contrasts and tidy CSV |
| Test reinstatement across occasions | [Repetition studies](REPETITION_STUDIES.md) | Explicit participant/stimulus relations, edge results and realized denominators |
| Fit fixed template features and evaluate held-out trials | [Training-only template fitting](TEMPLATE_FITTING.md) | Explicit folds, saved recipe, training-bound coefficients and keyed prediction errors |
| Analyze an imported recording through detection and AOIs | [Psychology workflow](../io/src/main/scala/eyes4s/io/PsychologyWorkflow.scala) and [real-data saved recording tests](../io/src/test/scala/eyes4s/io/PsychologyWorkflowSuite.scala) | Synchronization, preprocessing, detection and AOI descriptions and diagnostics |
| Measure changes in duration-weighted maps over time | [Temporal study source](examples/TemporalStudyGuide.scala) | Window identity, clipped durations, missing support and empty results |
| Save, inspect and rerun a fixation study | [Saved studies](SAVED_STUDIES.md) | Artifact identity, versioned parameters, structural differences and prerequisite failures |
| Add a custom typed comparison | [Extending studies](EXTENDING_STUDIES.md) | Typed parameters/results, registration, codecs and published conformance tests |
| Check what transfers from eyesim | [Baseline capabilities](EYESIM_CAPABILITIES.md) | Measured equivalences, deliberate differences and unresolved cases |

For EyeLink ASC input, begin with the [format guide](formats/eyelink-asc.md); licensed EDF/device
certification is separate from the portable parser's tested support. A fixation summary is not
a raw recording and must not acquire fabricated samples or synchronization evidence.

The guides link executable sources and tests. A complete mdoc-built documentation site, packaged
consumer coverage for every journey, all eyesim methods and learned template construction are
still open work; this index does not claim those gates have passed.
