# Summarize a study with a report

A report reduces a completed study's stored rows to group estimates without rerunning a single
comparison. It filters trials with a three-valued predicate, groups them by levels or declared
bins, averages each participant's trials and then weighs every participant equally, and can take
a within-participant contrast between two levels. Every exclusion is counted and every missing
value says why; a missing mean is never a zero. The
[contract](https://github.com/canardlapin/eyes4s/blob/main/docs/REDUCING_RESULTS.md) states the
rules and the evidence.

A small study first: two participants recall items `a` and `b` they encoded earlier.

```scala mdoc:silent
import eyes4s.codec.*
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import eyes4s.results.*

def get[E, A](e: Either[E, A]): A = e.fold(x => sys.error(x.toString), identity)

val frame = get(Frame.screen("screen", 2, 2))
def trial(p: String, item: String, phase: String, xs: Vector[(Double, Double)]) =
  val key   = StudyKey(p, item, phase)
  val clock = ClockId(s"$p/$item/$phase")
  val fixes = xs.zipWithIndex.map { case ((x, y), i) =>
    get(Event.Fixation.withoutDispersion(
      get(Interval.of(clock, Instant.micros(i * 1000L), Instant.micros(i * 1000L + 1000L))),
      Pt[Px](x, y), 1))
  }
  Trial(key, (), get(Scanpath.of(frame, clock, IArray.from(fixes))))

val input = StudyInput(Trials(Vector(
  trial("p1", "a", "recall", Vector(0.5 -> 0.5, 1.5 -> 0.5)),
  trial("p1", "a", "encode", Vector(0.5 -> 0.5, 0.5 -> 1.5)),
  trial("p1", "b", "recall", Vector(1.5 -> 1.5, 1.5 -> 0.5)),
  trial("p1", "b", "encode", Vector(1.5 -> 1.5, 0.5 -> 0.5)),
  trial("p2", "a", "recall", Vector(0.5 -> 0.5, 0.5 -> 0.5)),
  trial("p2", "a", "encode", Vector(0.5 -> 0.5, 1.5 -> 0.5)),
  trial("p2", "b", "recall", Vector(1.5 -> 0.5, 1.5 -> 1.5)),
  trial("p2", "b", "encode", Vector(1.5 -> 0.5, 0.5 -> 1.5)))))
val plan = get(StudyPlan.cosine(input.reference, get(Grid.over(frame, 2, 2)), "recall",
  "encode", Weight.Duration, Vector(StudyEstimate.Binned()), FailurePolicy.RequireAll))
val result = get(plan.run(input))
```

`ReportSources.study` reads the stored result through the plan's layout and binds the source to
the canonical digests of the very plan, input and result it reads (and of the admission ledger,
when the report reads covariates from its trials table). The report groups trials by item and
contrasts item `a` with item `b` within each participant.

```scala mdoc:silent
val source = get(ReportSources.study(StudyCodecs.cosine[Px], StudyInputCodecs.study[Px],
  StudyResultCodecs.cosine[Px])(plan, input, result, None, CovariateSchema.empty))

val item = LevelTerm.Layout(LayoutField.Item)
val spec = get(ReportSpec.of(
  get(ReportId.of("by item")), 0,
  get(ReportSelection.of(Vector(Role.Difference), Vector("value"))),
  groupBy  = Vector(Grouping.ByLevel(item)),
  contrast = Some(LevelContrast(item, "a", "b"))))

val report = get(Report.evaluate(spec, source))
```

Each cell averages each participant's trials first; the contrast pairs the participants who have
both items; the accounting says where every eligible trial went.

```scala mdoc
report.cells.map(c => (c.group.render, c.estimate, c.participants, c.queries))
report.contrasts.map(c => (c.estimate, c.paired.map(_.participant), c.unpaired))
report.accountingOf(Role.Difference)
```

```scala mdoc:silent
assert(report.accountingOf(Role.Difference).exists(a => a.eligible == 4 && a.kept == 4))
assert(report.contrasts.head.paired.map(_.participant) == Vector("p1", "p2"))
assert(report.binding == source.binding)
```

The report is one set of result tables, which the CSV and Arrow writers of `eyes4s-io` render
like any other export; a group with no trial is a null estimate with its absence named.

```scala mdoc
get(ReportTables.cells(report)).csvHeader
```

A report is bound to the canonical digests of the plan, input, result and covariate source it
was evaluated over; `report.checkCurrent(current)` refuses it once any of them changes, and the
codecs `eyes4s.report-spec@1` and `eyes4s.report@1` save both halves of the pair.
