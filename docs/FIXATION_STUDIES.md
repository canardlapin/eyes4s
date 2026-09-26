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
Custom keys use a typed `FixationKeyReader` and the same importer. `FixationKeyReader.trial`
reads a `TrialKey`: participant, phase and trial label identify the trial, and its occurrence (1
when no occurrence column is named) and the item it is matched on are attributes of it. Two
retrieval trials may share a display and remember different items, and a retrieval trial may name
an item no encoding trial showed. Records of one trial that name different items, or different
occurrences, quarantine the trial once, under its first key, with `QuarantineCause.ItemConflict(items)`
or `OccurrenceConflict(occurrences)`; the prepared study refuses such keys with
`PlanError.MatchItemConflict`. The built-in reader assigns
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

### Record numbers and lines

Three counting conventions meet at a source file, and each is its own type in `eyes4s-plan`:

| Type | Counts | Example |
|---|---|---|
| `DataRecord` | data records from 1, the header excluded; the number a reader is shown | fixations.csv record 7,214 |
| `CsvRecord` | CSV records from 1, the header being record 1; the convention of ledger, locus and source-link numbers | CSV record 7,215 |
| `SourceLine` | physical lines from 1 under `LineBreak.LineFeed`: a line ends at a line feed, and a lone carriage return does not end one | the record's first line |

`dataRecord.csv` is always the value plus one, and `csvRecord.role` is `RecordRole.Header` for
record 1 and `RecordRole.Data(record)` for every other, so the two convert both ways without loss.
Lines are another matter: a record whose quoted field holds a line break occupies several lines,
so from there on a record's line is not its number plus one. `CsvLayout.scan(text)` (`eyes4s-io`)
reads the records exactly as the importer's decoder does and gives each record's text
(`layout.verbatim(record)`) and a `RecordLines` layout: `span(record)` is the record's
`LineSpan`, and `owner(line)` is the record a line belongs to (a line's place within its record,
from 0, is `line - span.first`). The layout covers the lines from 1 to `lines`, the last line of
the last record; text after it that the decoder reads as no record (an empty quoted field `""`
after the final line feed) is beyond the layout, and `owner` refuses it. Derive line numbers from
the layout, never by splitting the text: Java's `String.lines()` and most editors also end a line
at a lone carriage return, so their numbers can disagree. The ledger still stores `Int`
CSV record numbers; `entry.dataRecord`, `fixationSource.dataRecord`, `sources.entryAt(record)`,
`sources.trialAt(record)` and `sources.fixationAt(key, position)` read them as the typed
identities. A fixation's position in its scanpath is a `ScanpathPosition` (from 0), and its
display number a `FixationNumber` (from 1): fixation 6 is position 5.

### Coordinates and source records

`CoordinateProvenance.of(plan, input, ledger)` gives every admitted fixation's coordinates under
the plan, each in a named frame. `provenance.fixation(key, position)` is a `FixationProvenance`:
the fixation's interval, its source and data record (or the `MissingSource` that says why none is
known), and a `CoordinateTrail`:

| Member | Meaning |
|---|---|
| `recorded` | the source's position fields as text and their parse before correction, in the admission frame; filled by `eyes4s-io` from the source text |
| `correction` | the admission policy's rule that moved the trial's positions (its index and the `Correction`), if any |
| `admitted` | the position the study input holds, in the admission frame (the screen) |
| `window` | the same position in the analysis window's frame (image units), for a windowed plan; positions outside the window keep their window coordinates |
| `placement` | `DroppedInitial`, `OutsideScreen`, `OutsideWindow(policy)`, `TrialFailed(tally)` or `InMap`, decided in that order, as the plan's tallies count them; `TrialFailed` is a fixation in the window of a trial the study fails as a whole (under `OffWindowPolicy.FailTrial`, one of its fixations lies outside the window), so no map is built from it. `CentrePlacement` is the one screen and window classification that both the tallies and the trail use |
| `angular` | degrees under the plan's `AngularReference`: from the centre (`origin`) of the `measured` frame (the window, or the screen for a whole-frame plan), at the declared `unitsPerDegree`, into the `degrees` frame named `<measured>/degrees`, whose `x` runs right and `y` up |

`provenance.records` lists the ledger's source records in record order. Its `total` is the
ledger's record count, known before any page is built; `first(size)` and `page(from, size)`
build the provenance of their own admitted records only, and each page names the data record the
next one starts at. `FixationSourceText.of(text, ledger.source)` (`eyes4s-io`) reads the source
text, refusing one whose decoded records do not have the ledger's source digest; its `first` and
`page` add each record's verbatim text and `LineSpan`, and for an admitted record parse the
position columns as the importer does and check that correcting the parse gives the admitted
position bit for bit. A record that fails that check, or whose position field is not a finite
number, keeps its text and lines and carries its `refusal` (`SourceTextError.RecordedMismatch`
or `Field`, naming the record); the rest of the page is given. A participant-scoped correction
is resolved through a participant projection twice, at admission and in provenance; build a
custom key's reader with `LayoutKeys.reader(layout, columns)(read, clock)` so both use the
layout's.

### From a report to a source record

An application walks the chain *summary > participant > query contrast > pair > map > fixation >
record* one typed step at a time, and back up; a step that cannot be taken is refused with a
typed error naming its operands, never guessed.

| Step | Down | Up |
|---|---|---|
| summary > participant | `ReportNavigation.cells(report)`, `participants(report, cell)` (`eyes4s-results`) | `participant.cell` |
| participant > query contrast | `ReportNavigation.queries(report, participant, layout)` | `ReportNavigation.participantOf(report, cell, contrast, layout)` |
| query contrast > pair | `ResultNavigation.pairs(inspection, contrast, design, offset, size)`: an `OffsetPage` whose `total` is known before any pair is built | `ResultNavigation.queryOf(pair)` |
| pair > map | `ResultNavigation.maps(inspection, pair)`: the query's and the reference's `Estimation` | the pair the path came from |
| map > fixation | `ResultNavigation.fixations(inspection, provenance, map)`: `FixationRef`s, each a `ScanpathPosition` (from 0) with its display `number` (from 1) | the map of the fixation's trial |
| fixation > record | `ResultNavigation.record(provenance, fixation)`: a `DataRecord` | `ResultNavigation.fixationOf(provenance, record)` |

Report references are `ReportRef.Cell` (a scale, group, role and component) and
`ReportRef.Participant` (a cell and a participant name), built through `ReportRef.cell` and
`ReportRef.participant`; refusals are `ReportNavigationError` (`report-navigation`) and
`NavigationError` (`navigation`). A scale's `usedBy(key)` lists the pairs a trial takes part in,
as the query, as the matched reference and as a control reference, and `usedByCounts(key)` counts
them from the stored rows without building a pair; `pairsOfQuery(design, key)` reaches every pair
from its query trial.

Inspection listings are built lazily: a listing checks its references (no reference addresses two
entries) and each reduction's membership counts when it is built, and knows its `total` then, but
builds an entry from its stored row only when a page or lookup asks for it. Pages start at a
reference (`page(ref, size)`) or an offset (`page(ListingOffset, size)`, counted from 0); a page
from the end or beyond is empty.

## Join a trial inventory

A trials table (the inventory) lists every trial the design presented, including trials that
produced no fixation records. `FixationCsv.admitInventory(fixations, table, inventory, frame,
policy)` admits the fixation table against it; `StudioAcceptance` runs it end to end on the
Studio fixture. Both tables are read under declared columns only: a column no declaration names is
never read, and no role is inferred.

- `TrialColumns.of(participant, phase, trial, occurrence)` names the trial identity both tables
  share (occurrence 1 when no column is named). As on the key route, the label (participant,
  phase and trial) identifies a trial and the occurrence is checked against it, not joined on:
  two inventory records of one label with different occurrences are a `Conflict`, and records of
  an inventory label that name another occurrence quarantine the trial with
  `QuarantineCause.OccurrenceConflict(occurrences)`, naming the inventory's and the records'.
- `TrialInventory.read(contents, TrialInventoryColumns.of(trial, item, attributes))` reads the
  inventory. It is a declaration, so any defective record refuses it with a
  `FixationImportError.Inventory(errors)` listing every defect, each an `InventoryError` naming
  its record and column: a wrong width, a blank
  identity field or item, an occurrence that is not a positive integer, an attribute not of its
  declared kind, or two records that declare one participant, phase and trial label with different
  values (`Conflict`, naming the records and the differing columns). Repeated records whose
  parsed values are equal (`1` and `1.0`, occurrence `1` and `01`) collapse into one trial and
  never multiply its fixations.
- `FixationTable.of(trial, ordinal, x, y, TimeColumns(onset, duration, unit), samples, item,
  attributes)` declares the fixation table. The `TimestampUnit` is a required part of
  `TimeColumns`; there is no default and no inference. `SampleCountRule` states the support rule:
  `PositiveColumn(name)` reads counts from a column and rejects a record whose count is not a
  positive integer, 0 included; `DerivedFromDuration(rate)` is for tables without a count
  column and gives each record its duration times the declared rate, rounded up. The rule is
  recorded in the ledger, because the counts are part of the study input's identity.
- Attributes are `AttributeColumn(name, kind)` with kind `Text`, `Integer` or `Number`; an empty
  cell is `AttributeValue.Blank`. Integers and numbers follow a strict decimal grammar (an
  optional sign, digits, and for numbers a fraction and exponent); whitespace, type suffixes,
  hexadecimal and non-finite spellings are refused. Inventory attributes belong to the trial
  (`ledger.inventory.attributes(key)`, `imported.attributes(key)`); fixation-table attributes
  belong to each admitted record (`imported.recordAttributes`). A fixation attribute of the wrong
  kind rejects its record.

The trial's item is the inventory's when the inventory declares an item column, otherwise the one
item its records name; at least one table must declare one (`FixationImportError.NoItemColumn`).
A blank record item cell never detaches its record from its trial: with an inventory item it
takes that item, and without one it rejects the record, which keeps its key and its trial.
Records that name several items quarantine the trial with `QuarantineCause.ItemConflict(items)`;
records that name another item than the inventory's quarantine it with
`InventoryItemConflict(inventory, records)`. Either way the trial's records are reported under the
inventory's item.

Every inventory trial gets exactly one `TrialDisposition`:

| Disposition | When |
|---|---|
| `Absent` | no record names the trial |
| `NoFixations` | it has records and every one was rejected on its own, e.g. all with sample count 0; this takes precedence over `Quarantined(RejectedRecords)` |
| `Quarantined(cause)` | it has an admissible record, but an item conflict or the importer's usual grounds (a rejected record, duplicate ordinals, overlap, a correction conflict) quarantine it |
| `Admitted` | its records build its scanpath |

Records of a trial the inventory does not declare are not dropped: each admissible one is
quarantined with `QuarantineCause.NotInInventory(participant, phase, trial, occurrence)` and the
trial is listed in `imported.unlisted`. A record whose identity cannot be read is a row-level
`Key` rejection, as before.

`FixationEvidence.ledger(label, inventoryLabel, imported, decision)` records all of it: the
fixation ledger joined to an `InventoryLedger` (written as `eyes4s.admission-ledger@3`, see
[saved studies](SAVED_STUDIES.md#input-payloads-and-admission-ledgers)). The join is checked:
`ledger.withInventory` refuses, with `AdmissionError.Inventory(InventoryError…)`, an inventory in
which a keyed record is not listed under its own trial, a trial's disposition contradicts its
records (for example `Quarantined(RejectedRecords)` for a trial whose records were all rejected on
their own), or an admitted record carries another item than its trial's. On the Studio fixture
the ledger reads 960 inventory trials: 937 admitted, 17 quarantined (duplicate ordinals 4, no
fixations 5, overlap 6, rejected records 2) and 6 absent, with 11,311 admitted and 209 rejected
records. The published `InventoryLaws` in `eyes4s-laws` state the partition, precedence and
absence rules over generated studies, for any importer.

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
per trial in input order, a `WindowTally` (fixations and fixation duration outside the window, none
for a whole-frame plan, outside the screen, and in total), or the frame refusal of a trial in
another frame. `windowSummary` totals them, and `WindowSummary.of(tallies, ledger)` adds the
importer's source-record count ("543 of 11,520 records in 409 trials"). Preflight reports `StudyFinding.OffWindowFixations` and `NoFixationInWindow` as
warnings, and `StudyInspection.windowTallies` gives the same tallies beside a completed result.

Scales can be declared in degrees: `StudyScale.Angular(StudyEstimate.Gaussian(sigmaDeg, edges))`
resolves through the plan's one `LinearAngularScale` (a declared, uniform units-per-degree value on
the admission frame, not a calibration). The description records the degrees and the units per
degree beside the resolved pixel bandwidth. Degrees are measured from the frame centre with `y`
upward (`LinearAngularScale.angular`, and `on(window)` for degrees from the image centre).

A flip correction is a `HalfOpenReflection` about the frame's centre line: a position on the
half-open frame stays on it (the lower edge maps just below the upper edge) and one off it stays
off, so a flip never moves a record on or off the screen.

## Initial fixations

`StudyPlan.configure` takes an `InitialFixationPolicy`, applied identically to focal (query) and
reference trials. It defaults to `InitialFixationPolicy.keepAll`, which is what every version-1
and version-2 plan means.

| Policy | Dropped from each trial |
|---|---|
| `keepAll` | nothing |
| `dropFirst` | the first fixation, wherever it lies |
| `dropLeadingInClosedDisc(cross, radiusDegrees)` | the leading run of fixations whose centres lie in the closed disc of `radiusDegrees` around `cross`: every fixation before the first one farther from the cross; a later return to the cross is kept |

The cross is a position in admission-frame units and must lie on the admission frame
(`InitialFixationError.CrossOffFrame`). The distance is `centre.distanceTo(cross) /
unitsPerDegree` through the plan's one `LinearAngularScale`, so this policy needs one
(`InitialFixationError.MissingAngularScale`), and a centre exactly at the radius is within it.
Refusals arrive as `PlanError.InitialFixations`, with the `initial-fixation.*` diagnostic codes and the
remedy `ReviseInitialFixationPolicy` (`ReviseScaleDeclaration` for a missing scale). Preflight does
not report a trial the policy empties as having no fixation in the window.

Dropped fixations are removed before the analysis window and the screen are considered, so every
fixation is exactly one of: dropped, outside the screen, outside the window, or in the map.
`plan.initialFixationTallies(input)`, `PreparedStudy.initialFixationTallies`,
`StudyPreview.initialFixationTallies` and `StudyInspection.initialFixationTally(key)` give, per
trial in input order, an `InitialFixationTally` (dropped and total fixations and their durations,
with `kept = total - dropped`); `initialFixationSummary` totals them. Window tallies count only the
kept fixations. A trial whose fixations are all dropped fails at every scale with
`StudyFailure.InitialFixations(key, InitialFixationError.NoFixationKept(dropped, micros))`; in a
temporal study, every cell applies the base plan's policy before its window.

The policy is a recorded plan field: the description gains
`initialFixations -> [dropFirst]` or `[dropLeadingInClosedDisc, x, y, radius]` when anything is
dropped, `RecipeInspection` explains it, and `policy.methods` gives one sentence a methods section
can cite. The plan codec writes it as `eyes4s.study@3` (see [saved studies](SAVED_STUDIES.md)).

## Repeated items and matched references

When an item is studied more than once, a focal trial can have several matched references.
`StudyPlan.configure` takes a `StudyPairing`; its default, `StudyPairing.default`, requires exactly
one (`MatchedReferences.RequireOne`). The alternatives need a layout with occurrences, such as
`TrialKey.layout`: `SameOccurrence` pairs a retrieval trial of occurrence n with the encoding trial
of occurrence n, and `Select(First | Last | At(n))` keeps one occurrence of each item.
`MeanOfAll`, the version-1 meaning, averages every matched reference and must be chosen explicitly.
The control pool applies the same selection, so each other item contributes one reference per
participant (`ControlReferences.SameSelection`); `ControlReferences.AllOccurrences` uses every
occurrence as a control. A focal trial without a matched reference is reported as no match
(`UnmatchedFocalPolicy.ReportNoMatch`) or refuses the study (`Refuse`).

`PreparedStudy.matchedCardinality` is computed once from the prepared matched schedule: focal
trials with several matched references and their references, reference groups the control pool
cannot reduce to one, unmatched focal trials, and trials whose identity names two items. The same
value produces the preflight findings (`StudyFinding.MatchedCardinality`, a blocker under a
one-reference rule and a warning under `MeanOfAll`; `AmbiguousReferences`;
`UnmatchedFocalRefused`; `MatchItemConflict`, each suggesting `ChooseMatchedReference`,
`SupplyMatchedReference` or `ResolveMatchItemConflict`), refuses execution with
`PlanError.MatchedCardinality`, `UnmatchedFocalRefused` or `MatchItemConflict` naming the trials by
key digest, and appears in `StudyPreview.matchedCardinality`. `StudyPlan.of` and a decoded
`eyes4s.study@1` plan mean `MeanOfAll`, so a custom layout that repeats items now receives a warning
instead of averaging silently; for `StudyKey` nothing changes, because it never has more than one
matched reference.

## Interpret and export

The guide constructs duration-weighted occupancy on the declared grid, normalizes it to mass,
compares matched and control templates, and reduces with `RequireAll`. The saved/reloaded plan runs
through those same operations.

## Compare plan revisions

`plan.structuralDiff(revised)` (`StudyDiff.between`) lists the typed changes between two plans, one
per declared field, in `StudyField` order: input, layout, method (with its parameters), phases,
weight, failure policy, grid, window, off-window policy, scales, units per degree, the three
pairing choices and the initial-fixation policy. Each `StudyChange` carries the field's typed value
before and after (`StudyChange.Scales` also gives `added` and `removed`); layouts and methods are
compared by identity. `StudyDiff.render(changes)` is a default English rendering, for example
"scales +8°" or "matched references policy RequireOne → MeanOfAll"; an application localises from
the field and the values. The diff of a plan with itself is empty, a diff is empty exactly when the
two plans have the same description, and the diff in the other direction is `changes.map(_.inverse)`.

`plan.revise(changes)` applies changes. Each change must start from the plan's current value of its
field (`StudyRevisionError.Stale`, naming both), each field changes once (`DuplicateField`), a
window needs its off-window policy (`IncompleteWindow`), and the revised plan is checked like any
configured plan (`StudyRevisionError.Plan`). Applying `a.structuralDiff(b)` to `a` gives a plan
equal to `b`. The published `StudyDiffLaws` and `InitialFixationLaws` state these rules; the
field-level `plan.diff` over descriptions remains for stored descriptions.

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
