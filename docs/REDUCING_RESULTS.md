# Reducing study results: report specifications

A report reduces the stored rows of a completed study to group estimates: which trials count
(a filter), how they group (by levels, flags or declared bins), how a group's trials become one
estimate (participant means by default), and an optional within-participant contrast between
two levels. It reads what the study stored and never reruns a pair score. The report, its
specification and its tables live in `eyes4s-results`, a pure module (JVM and Scala.js, no JSON
library, no effect system), so an application's presentation layer can hold and render them.
The executed [report page](../site-docs/summaries.md) shows the API; this page states the contract.

## What a report reads

`eyes4s.codec.ReportSources.study(plans, inputs, results)(plan, input, result, ledger,
covariates)` is the public way to obtain a source. It computes the source's binding from the very
values it reads (see [binding](#binding-persistence-and-staleness)), checks that `result` carries
the plan's description and refers to `input` and that the ledger is consistent with `input`,
reads the declared covariates from the ledger's trials table, takes the plan's described score
components, and computes the plan's window tallies over `input`. The constructors in
`eyes4s-results` that take a binding on trust (`ReportSource.study`, `fromQueries`,
`Report.reduce`) are package-private: they exist for the codec and the published law suites, so
a report cannot be bound to documents it did not read. The report types themselves, and the
binding they carry, are all in `eyes4s-results`. For a scale it builds a `QueryTable`: one
`Query` per focal trial (every key of the scale's matched and control reductions and its
contrast, and every focal trial the matched pairing left unmatched), with

- its layout fields (participant, item, phase, occurrence), read through the study layout;
- its covariates, joined by key from a `CovariateTable` (never by row position);
- its window measures from the plan's `WindowTally` (outside-window and outside-screen shares
  and counts, fixation count), a share being undefined for a trial with no fixation duration;
- each role's stored outcome: `Matched` and `Control` from the reductions, `Difference` from the
  contrast row, as the score components the method describes, a stored failure (its diagnostic
  code), or no stored row.

`Report.evaluate(spec, source)` reduces the scale's query table; it refuses a specification
that reads covariates over a source bound to no covariate source (`UnboundCovariates`).

## Covariates

A `CovariateSchema` declares each covariate's `CovariateType`: `Numeric(unit)`, `Ordinal(levels)`
(declared order, lowest first), `Categorical(levels)` or `Binary`. `CovariateTable.fromLedger`
reads the trial inventory an admission ledger carries (the trials table of the trials importer):
every declared covariate must be an attribute column of a kind that can hold it, each trial with
a resolved item is keyed by its `TrialKey`, a blank cell is not recorded and a value outside the
declared type is unparsed. `CovariateTable.forKeys` joins the same table to any study keys
through the layout's trial identity (participant, phase, trial label, occurrence); it is what
`ReportSources.study` uses. `CovariateTable.of` does the same for keyed attributes of any key
type. A report reads a covariate only as its table declares it; a disagreement is refused.

## Values and absences

Every estimate, spread and participant value is a `Value[Double]`: `Present(x)` or
`Missing(absence)`, where the absence says why (`NotRecorded`, `Unparsed`, `Failed(code)`,
`Undefined(reason)`, `EmptyGroup`, `Unpaired`, `BelowMinimum(queries, required)`). A `Value` has no
`Numeric`, `Monoid`, `orZero` or `getOrElse` (compile-time checks in the report suite pin this):
a missing mean is never a zero. In a report table a missing value is a null cell with its
absence named in the adjacent `_absence` column.

## The specification

| Part | Meaning |
|---|---|
| `scale` | the estimation scale the report reads |
| `ReportSelection(roles, components)` | the roles (`Matched`, `Control`, `Difference` = matched minus control) and score components |
| `filter: Option[Predicate]` | three-valued: `Cmp` (numeric terms), `In` (levels), `AtLeast` (ordinal rank), `Is` (flags), `IsMissing`, `And`, `Or`, `Not` |
| `groupBy: Vector[Grouping]` | `ByLevel` (a layout field or a categorical or ordinal covariate), `ByFlag`, `ByBins` (a numeric term, only through declared bins) |
| `reduce` | `ParticipantMeans(minimumQueries)` (the default, minimum one) or `PooledQueries` |
| `contrast: Option[LevelContrast]` | minuend minus subtrahend level of one grouping term, within participant |
| `spread` | `StandardDeviation` (the default) or `StandardDeviationAndError` |

Terms are typed by their values: comparing a layout field numerically, or grouping a numeric term
without bins, does not compile. `ReportSpec.of` refuses an undeclared level, a non-finite
threshold, a covariate declared as two types, and a contrast over a term that is not grouped.

A filter over a missing value is `Unknown` (Kleene logic: `Unknown and False` is `False`,
`Unknown or True` is `True`); only `IsMissing` decides a missing value. A query whose filter is
unknown is excluded and reported (`UnknownPredicate`), never counted as passing or failing.

## Reduction

For each role, group and component:

- a participant's value is the mean of its queries in the group;
- under `ParticipantMeans`, a participant with fewer than `minimumQueries` queries in the group is
  left out of that group only (`BelowMinimum`), and the estimate is the mean of the contributing
  participants' values, each participant weighted equally;
- under `PooledQueries` the estimate is the mean of every query in the group, so a participant
  with more queries weighs more; it is never chosen by default;
- the spread is the count of the averaged values and their sample standard deviation (n - 1), and
  the standard error when asked; with fewer than two values it is undefined. No confidence
  interval is computed: a report states n and spread.

Group levels are the declared ones (a covariate's levels, `false`/`true`, a grouping's bins), or,
for a layout field, the levels observed among the eligible queries; groups are their product in
grouping order. A group with no valued query has the absence `EmptyGroup`. A cell's `queries`
counts its queries with a stored value for the role, and `failed` those assigned to it that
passed the filter but have none.

A `LevelContrast` is taken in each combination of the other groupings' levels (a stratum). Each
participant present at both levels contributes its difference of participant values; the
estimate is their mean and its n the number of paired participants, `|Pa ∩ Pb|`. A participant
present at one level only is unpaired and reported (`UnpairedParticipant`). The strata come from
the specification's levels, so a contrast whose level no query shows is reported as a statistic
missing with `EmptyGroup`, never dropped. Under `PooledQueries`, level contrasts still use
per-participant means: each participant's own mean at each level, since the contrast is within
participant; only the cells pool queries.

## Accounting

For each role, `Accounting` records where every eligible query went, and these identities hold:

- `eligible = kept + filteredOut + unknownPredicate + failed`, where `failed` counts queries that
  passed the filter but have no stored value for the role;
- the role's cells (of any one component) together hold `kept - missingGroupAttribute` queries,
  where `missingGroupAttribute` counts kept queries with a missing grouping value or a value in
  no declared bin;
- `belowMinimum` counts the participant-and-group exclusions.

`Report.reconstruct`, which the codec decodes through, refuses a report whose accounting or cells
break them.

## Tables

`ReportTables.cells`, `participants` and `contrasts` render a report into the `ReportCells`,
`ReportParticipants` and `ReportContrasts` families of the one result-table layer
(`eyes4s.result-table/1`). Every family carries the report's context: reduction and weight,
spread, filter, groupings, contrast sign, binding and accounting. The CSV and Arrow transports of
`eyes4s-io` render them like any other table (see [result exports](RESULT_EXPORTS.md)). Report
cells carry `ResultRef`s (`Reduction` or `ContrastRow` at the report's scale) that
`ResultInspection` resolves to the stored rows.

## Binding, persistence and staleness

A `ReportBinding` names the canonical digests (`CanonicalDigest`, see
[domain codecs](DOMAIN_CODECS.md#separate-identities)) of the plan, the input, the stored result
and, when the report reads covariates, the covariate source (the admission ledger carrying the
trials table). The binding types are pure (`eyes4s-results`); the digests are computed in
`eyes4s-codec`, which holds the JSON codecs, by `ReportSources.study` from the values the source
reads. `Report.checkCurrent(binding)` and
`Report.evaluate(spec, source, expected)` refuse a stale binding, naming the field that changed.

The codecs are `eyes4s.covariate-schema@1`, `eyes4s.report-spec@1` and `eyes4s.report@1`
(`ReportCodecs`, identities in `ReportCodecDefinitions`), with pinned fixtures
`covariate-schema-v1.json`, `report-spec-v1.json` and `report-v1.json`. In a saved study the roles
`report-spec` and `report` and the relation `report-of(report, spec, result, input, covariates)`
join a report to what it was evaluated over: the resolver (with `ArtifactDecoders.withReports`)
checks that the report's specification is the stored one (`ReportSpec`), that its input is the
one its result was computed on (`ReportInput`), that its ledger is a ledger of that input
(`ReportLedger`), that every cell member is a trial the result estimated at the report's scale
(`ReportMembers`), and that its binding names the stored result, input, the result's plan and the
ledger (`ReportBinding(field, bound, stored)`).

## Diagnostics

Import `eyes4s.results.ResultsDiagnostics.given` for `Diagnostic.of`. The `report` family holds
the findings, as warnings keyed to trials (`Locus.Trial`), participants (`Locus.Participant`) and
groups (`Locus.Group`): `EmptyGroup`, `UnpairedParticipant`, `MissingCovariate`,
`UnknownPredicate`, `BelowMinimum`, `UndefinedWindowShare` and `CovariateType`. Refusals are the
`report-error`, `report-spec`, `covariate` and `result-table` families (see
[diagnostics](DIAGNOSTICS.md)).

## Evidence

`ReportLaws` in `eyes4s-laws` (published) states, over `ReportGenerators` cases with missing and
unparsed covariates, undefined shares, failed and unstored rows and every specification form:
equal weight under within-participant duplication (with a pooled witness showing the law
discriminates), order and participant-relabel invariance, filter/group commutation, the
accounting identities, paired n, that a missing value is never a zero (in cells and tables), and
agreement with an exact rational oracle, within the named tolerance `ReportLaws.arithmetic`. The
oracle decides each cell's membership on its own (its own three-valued filter, comparisons and
bin edges) and checks it against the cell's members, queries and failures; the generators put
ratings on every threshold and bin edge.
`ReportLawSuite` also runs the three codec round trips and kills reduction mutants from a fixed
seed. The report suite of `eyes4s-results` covers hand-computed estimates, a stored study read by
key with its members resolved through `ResultInspection`, and the compile-time refusals.

Not provided: an inferential interval (a t-based interval may be added later as a named,
non-default option); grouping by raw numeric values; methods text generated from a report.
