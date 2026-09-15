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
`requireComplete` refuses to produce a study input while any rejected rows remain. The `accepted`
collection is separately available if an analyst deliberately reviews and handles the exclusions.

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
