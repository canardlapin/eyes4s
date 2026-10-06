# Native reporting and saved contrast direction

Studio keeps run facts and numerical reports separate. `ResultSummary` carries the
run's revisions, declared scales, pair-row totals and query accounting, including
participant counts. Group means, participant means, group-size ranges and level
contrasts come from `report(run, spec, scale)`. Editing a report reuses the retained
library result; it starts no analysis job.

`ReportingSpec.groupBy` selects a categorical inventory column. Grouping alone
serves its cells and declares no subtraction. To request a within-participant
contrast, save a checked `ReportingContrast.of(minuend, subtrahend)` in the spec's
`contrast` field. The operands are nonblank and distinct. The native library checks
that both belong to the grouping's declared levels. Inventory row order and
alphabetical label order never choose the direction.

The reporting editor exposes both operands with the visible subtraction order and
Apply/Clear controls. Filtering, weighting, minimum edits and Save as preserve an
existing contrast. Changing the grouping clears it; returning to the earlier
grouping does not invent it again.

Participant means support a minimum query count per group. Pooled-query weighting
with an explicit minimum returns a named `ReportRefusal.Spec`, because the native
pooled reduction cannot express that minimum. Studio does not ignore the request.
Minimum exclusions and their source references come from the evaluated library
report. An unevaluated edit displays no guessed exclusion count.

Reports retain missing cells with their library absence. A participant plot's
finite group-tick interface returns `UnscoredGroup` when a group has no estimate;
the report and result table retain that group and its absence. Whole-run plots keep
the native whole-group references under the display label "All queries". Scale
profiles use the reports' explicit run, reporting id and scale indices, preserve
their returned references and refuse missing series cells.

The illustrative `fixture.json` summaries remain frozen display metadata from
Python's `statistics.mean` and two-decimal `round`; grand summaries average already
rounded participant values. Reports instead evaluate the stored query rows directly.
Four participant/group means lie on decimal rounding boundaries (P05 Forgotten
−0.235, P13 Forgotten 0.025, P16 Remembered 0.445 and P24 Forgotten 0.175), where
floating accumulation and display rounding change the final hundredth. Tests retain
the original FIXTURE.md metadata pins, independently average the stored query values
with decimal arithmetic, and pin those four evaluated display cells separately.
These illustrative values do not replace the native golden scientific results.

## Persistence and compatibility

New vocabulary uses the earliest schema version that expresses it:

| Stored value | First version with explicit contrast operands |
|---|---:|
| Studio document | 6 |
| Scientific content/digest | 2 |
| Reporting spec/digest | 2 |
| `PutReporting` journal entry | 3 |

Older grouped specs load with no contrast. Their bytes and digests stay unchanged;
lifting does not synthesize operands. Prior schema writers and readers refuse an
explicit contrast, including a payload placed inside an older schema envelope.
Protocol 1.16 carries the optional contrast, scale-free result facts and optional
matched references. Client and backend use the same exact protocol version.

## Verification

`ReportingContrastSuite` checks smart construction, schema envelopes, reopen,
direction-sensitive digests and older-version refusal. `ReportDirectionSuite`
uses asymmetric scores with Forgotten observed first, permutes declaration and
row order, and checks reversing operands, filtering, minimums and both weighting
policies. Its `ArithmeticTolerance` is `1e-12`.

The deliberate operand-swap mutation made two direction tests fail, including a
filtered contrast changing from +0.2 to −0.2. The original implementation was
restored. Native golden-study regressions construct the library report specification
independently and compare grouped, filtered, minimum and weighted report views;
fixture display means are not evidence for a native scientific result.
