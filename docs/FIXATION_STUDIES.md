# From fixation tables to study contrasts

This workflow answers a specific question: is a recall trial's gaze map more similar to its matched
encoding trial than to the other encoding trials from the same participant? The result is matched
cosine minus mean control cosine. A positive value describes that contrast; it is not a significance
test. All eligible controls are used.

The [example table](../tools/r-parity/fixtures/matched-control.csv) has two participants, three images,
two phases and four fixation summaries per trial. It is synthetic, with independently checked
results. It does not invent raw recordings or detector output.

## Run the example

The complete [StudyGuide.scala](examples/StudyGuide.scala) is compiled and exercised on both JVM and
Scala.js. From the repository root, this command runs that same guide and writes a saved plan and CSV:

```sh
sbt -J-Xmx3g "ioJVM/Test/runMain eyes4s.examples.ExportStudy \"$PWD/tools/r-parity/fixtures/matched-control.csv\" /private/tmp/eyes4s-study"
```

The command expands the input to an absolute path because sbt forks the launcher from its module
directory. The launcher writes `study.json` and `contrasts.csv`. Use an output directory you intend to update.
The guide's frame and grid are explicitly two by two pixels for this tiny fixture. Change those
values to your study's declared geometry before using a different recording setup.

## Admit the table

`FixationColumns.of` names the ordinal, x, y, onset, duration and support-count columns.
`FixationKeyReader.study` names participant, stimulus and phase columns. The names are interpreted
only at this raw-file boundary; the library constructs typed `StudyKey` values for analysis.
Custom keys use a typed `FixationKeyReader` and the same importer. The built-in reader assigns
a distinct nominal clock to each trial; it does not assert synchronization between trials.

Pass the source `TimestampUnit` explicitly. Decimal timestamps round to the nearest microsecond,
with ties toward positive infinity; integer microseconds retain their exact 64-bit value. Fixation
ordinals specify order within a trial. The importer rejects duplicate ordinals, overlapping intervals,
non-finite or out-of-frame coordinates, nonpositive durations, invalid support counts and malformed
rows. CSV quoting preserves commas, quotes and newlines in identifiers.

`FixationCsv.read` returns the source rows, accepted scanpaths and rejected rows with their logical
CSV record numbers, raw fields, recoverable key and reason. If a keyed row is invalid, the entire
trial is quarantined. A row whose key cannot be decoded remains an explicit rejection.

`read` is the version-1 admission: a finite position outside the frame rejects its record and
quarantines its trial. `FixationCsv.admit(..., policy)` admits under an explicit `AdmissionPolicy`,
whose default is `OffScreenPolicy.ExcludeRecord`: a record whose finite position lies outside the
frame (off the screen) is admitted, listed in `imported.outsideFrame` and in the ledger, and left
out of every map by the study, which reports it per trial as "outside screen". Unparseable or
non-finite coordinates still reject the record and quarantine the trial under either policy;
`OffScreenPolicy.QuarantineTrial` keeps the version-1 behaviour. The policy also records
coordinate corrections (`Correction.FlipX`, `FlipY` or `Correction.translate(dx, dy)`), each scoped
to all trials, one participant or one typed trial key. A covering rule is applied to the parsed
position before containment is checked; the raw fields and the source digest are untouched, so the
same rows under different corrections have the same `SourceRef` and a different input digest. A
trial that two rules cover is quarantined with `QuarantineCause.CorrectionConflict(first, second)`,
naming both rules; a participant-scoped rule needs a key reader that names the participant
(`FixationKeyReader.study` does, and `FixationKeyReader.withParticipant` for a custom key).
`requireComplete` refuses to produce a study input while any rejected rows remain. The `accepted`
collection is separately available if an analyst deliberately reviews and handles the exclusions.

`FixationEvidence.ledger(label, imported, decision)` turns the import report into a pure
`AdmissionLedger` with exactly one typed disposition per source record, the admission policy and
the admitted records outside the frame. `imported.admitted` links
each accepted row to its typed key and ordinal; row errors become typed `AdmissionReason` values, and
a quarantined trial names its records and a `QuarantineCause`. `AdmissionDecision.RequireComplete`
records the outcome `Refused` when any record was rejected; `ReviewExclusions` records
`ReviewedExclusions`, the explicit decision to proceed with `imported.accepted`. See
[saved studies](SAVED_STUDIES.md) for the input and ledger payloads that reconstruct the study,
with the same input digest and results, without the importer.

## Analysis window and degrees

A study maps the whole admission frame by default (`StudyGeometry.WholeFrame(grid)`, what
`StudyPlan.of` and `StudyPlan.cosine` build). `StudyPlan.configure` takes an explicit geometry:
`StudyGeometry.windowed(window, grid, offWindow)` maps a `Subframe` of the admission frame, for
example a 1024 x 768 image centred on a 1920 x 1080 display (`Subframe.centred`), on a grid over the
window's own frame, whose origin is the window's corner. The window is half-open,
`[x0, x1) x [y0, y1)`, and only a fixation's centre decides. Fixations on the screen but outside
the window are left out of the map under `OffWindowPolicy.Exclude`, or fail their trial under
`FailTrial`; a trial with no fixation inside the window fails at every scale with
`StudyFailure.OffWindow` either way. Fixations off the screen are always left out of the map.

`plan.windowTallies(input)`, `PreparedStudy.windowTallies` and `StudyPreview.windowTallies` give,
per trial in input order, a `WindowTally`: fixations and fixation duration outside the window,
outside the screen, and in total. `windowSummary` totals them ("543 of 11,520 records in 409
trials"). Preflight reports `StudyFinding.OffWindowFixations` and `NoFixationInWindow` as
warnings, and `StudyInspection.windowTallies` gives the same tallies beside a completed result.

Scales can be declared in degrees: `StudyScale.Angular(StudyEstimate.Gaussian(sigmaDeg, edges))`
resolves through the plan's one `LinearAngularScale` (a declared, uniform units-per-degree value on
the admission frame, not a calibration). The description records the degrees and the units per
degree beside the resolved pixel bandwidth. Degrees are measured from the frame centre with `y`
upward (`LinearAngularScale.angular`, and `on(window)` for degrees from the image centre).

## Interpret and export

The guide constructs duration-weighted occupancy on the declared grid, normalizes it to mass,
compares matched and control templates, and reduces with `RequireAll`. The saved/reloaded plan runs
through those same operations. Change a scientific choice in the plan and inspect the resulting diff.

The export has one row per focal key, scale and score component. Scalar contrasts have one component;
MultiMatch contrasts have five named signed components. Columns retain each operand, the difference,
selected/successful/failed/contributing counts, units, typed key JSON, input identity, provenance and
the full saved plan. Failed results keep their row with an explicit status and reason. Empty numeric
fields are missing values; they are never substituted with zero. Excluded phases and scale-level
failures have separate `scope` values.

Read and inspect the exported results in R:

```r
results <- read.csv('/private/tmp/eyes4s-study/contrasts.csv', check.names = FALSE)
results[c('participant', 'stimulus', 'difference', 'status',
          'matched_contributing', 'control_contributing')]
```

The [R acceptance check](../tools/r-parity/check_study_export.R) reads the actual Scala export with
base R and compares it with the pinned eyesim output and rational targets. See [PARITY.md](../PARITY.md)
for the evidence boundary, and [saved studies](SAVED_STUDIES.md) for versions and artifact resolution.
