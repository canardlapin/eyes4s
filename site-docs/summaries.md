# Summarize a study with a report

A report reduces a completed study's stored rows to group estimates without rerunning a single
comparison. It filters trials with a three-valued predicate, groups them by levels or declared
bins, averages each participant's trials and then weighs every participant equally, and can take
a within-participant contrast between two levels. Every exclusion is counted and every missing
value says why; a missing mean is never a zero. The
[contract](https://github.com/canardlapin/eyes4s/blob/main/docs/REDUCING_RESULTS.md) states the
rules and the evidence.

For a stored study, `ReportSource.study(plan, input, result, covariates, binding)` reads each
focal trial's matched, control and difference values by key. Here the same query table is written
out by hand: three participants, a memory covariate, and one trial whose window share is
undefined.

```scala mdoc:silent
import eyes4s.plan.StudyKey
import eyes4s.results.*

def get[E, A](e: Either[E, A]): A = e.fold(x => sys.error(x.toString), identity)

val memory = get(CovariateName.of("memory"))
val levels = get(Levels.of(Vector("Remembered", "Forgotten")))
val schema = get(CovariateSchema.of(Vector(Covariate(memory, CovariateType.Categorical(levels)))))
val share  = NumericTerm.Window(WindowMeasure.OutsideWindowShare)

def trial(p: String, item: String, remembered: Boolean, outside: Option[Double], d: Double) =
  get(Query.of(
    StudyKey(p, item, "recall"), p, item, "recall", 1,
    Vector(memory -> Value.Present(
      CovariateValue.Level(if remembered then "Remembered" else "Forgotten"))),
    Vector.empty,
    Vector(WindowMeasure.OutsideWindowShare -> outside.fold(
      Value.Missing(Absence.Undefined(UndefinedReason.ZeroDuration)))(Value.Present(_))),
    RoleOutcome.NotStored, RoleOutcome.NotStored, RoleOutcome.Scored(Vector(d))))

val table = get(QueryTable.of(0, Vector("value"), schema, Vector(
  trial("p1", "a", true, Some(0.0), 0.30), trial("p1", "b", true, Some(0.1), 0.10),
  trial("p1", "c", false, Some(0.0), -0.10),
  trial("p2", "a", true, Some(0.0), 0.50), trial("p2", "b", false, None, 0.20),
  trial("p3", "a", false, Some(0.5), 0.00))))

val memoryTerm = LevelTerm.Categorical(memory, levels)
val spec = get(ReportSpec.of(
  get(ReportId.of("memory")), 0,
  get(ReportSelection.of(Vector(Role.Difference), Vector("value"))),
  filter   = Some(Predicate.Cmp(share, Comparison.LessOrEqual, 0.25)),
  groupBy  = Vector(Grouping.ByLevel(memoryTerm)),
  contrast = Some(LevelContrast(memoryTerm, "Remembered", "Forgotten"))))

val binding = ReportBinding(
  get(BindingDigest.parse("plan", "1" * 64)), get(BindingDigest.parse("input", "2" * 64)),
  get(BindingDigest.parse("result", "3" * 64)), None)
val report = get(Report.reduce(spec, table, binding))
```

Participant p1 averages its two remembered trials (0.2); p2 has one (0.5); the remembered
estimate weighs both participants equally. p2's forgotten trial has no defined window share, so
the filter is unknown for it: it is excluded and reported, not counted as filtered out. p3's
trial lies outside the window and is filtered out.

```scala mdoc
val remembered = report.cell(GroupKey(Vector("covariate:memory" -> "Remembered")),
  Role.Difference, "value").get
remembered.estimate
remembered.perParticipant
report.accountingOf(Role.Difference)
report.contrasts.map(c => (c.estimate, c.paired.map(_.participant), c.unpaired))
report.findings.collect { case f @ ReportFinding.UnknownPredicate(_, _) => f }
```

```scala mdoc:silent
assert(remembered.estimate.toOption.exists(v => math.abs(v - (0.2 + 0.5) / 2) < 1e-12))
assert(report.accountingOf(Role.Difference).exists(a =>
  a.eligible == 6 && a.kept == 4 && a.filteredOut == 1 && a.unknownPredicate == 1))
```

The report is one set of result tables, which the CSV and Arrow writers of `eyes4s-io` render
like any other export; a group with no trial is a null estimate with its absence named.

```scala mdoc
get(ReportTables.cells(report)).csvHeader
```

A report is bound to the canonical digests of the plan, input, result and covariate source it
was evaluated over; `report.checkCurrent(current)` refuses it once any of them changes, and the
codecs `eyes4s.report-spec@1` and `eyes4s.report@1` save both halves of the pair.
