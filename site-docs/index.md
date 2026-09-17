# eyes4s

Eye-movement analysis in Scala 3, with geometry, clocks and scientific choices carried
through the result. Start with imported fixations or raw recordings; retain trial keys,
failed comparisons and excluded observations instead of losing rows along the way.

**Start:** [Run your first study](getting-started.md), then [understand the model](concepts.md).

## Choose a task

- [Compare fixation maps](fixation-studies.md): matched templates, explicit controls and exports.
- [Analyze a raw recording](recordings.md): synchronization, visual angle, detection and AOIs.
- [Test repetition](repetition.md): same-participant reinstatement with visible pair identities.
- [Fit and evaluate templates](templates.md): training-only external fitting and held-out results.
- [Migrate from eyesim](migration.md): supported routes and changes that affect your numbers.
- [Methods and support](reference.md): units, failures, persistence and API documentation.

## Development status

The library targets Scala 3.7.4 on JVM and Scala.js. These guides are compiled and executed
locally and by the configured documentation CI task; that is not a claim of a published release.
The scientific baseline is still incomplete, with unsupported routes called out in the migration
guide. No dataframe dependency or adapter is needed for the current workflows.

Binary EyeLink EDF is not supported: convert to ASC with SR Research's `edf2asc` first.
The library has no UI or general statistical modeling engine.
