/*
 * Copyright 2026 canardlapin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package example

import eyes4s.codec.*
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.io.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.laws.Tolerance
import eyes4s.plan.*
import eyes4s.results.*
import io.circe.Json

/** The summaries and exports pages from a fresh consumer: a completed study is
  * reduced by `Report.evaluate` without rerunning a comparison, the report is
  * checked against the same reduction composed explicitly from the stored
  * contrast rows, saved and reloaded through its codecs, refused once its
  * source changes, and exported through `ReportTables` and `ResultExports`.
  * Runs on the JVM and Scala.js.
  */
class ReportJourneySuite extends munit.FunSuite:
  private val Exact = Tolerance(absolute = 1e-15, relative = 0)

  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)

  private val frame = get(Frame.screen("report-journey", 2, 2))

  private def trial(p: String, item: String, phase: String, xs: Vector[(Double, Double)]) =
    val key   = StudyKey(p, item, phase)
    val clock = ClockId(s"$p/$item/$phase")
    val fixes = xs.zipWithIndex.map { case ((x, y), i) =>
      get(
        Event.Fixation.withoutDispersion(
          get(Interval.of(clock, Instant.micros(i * 1000L), Instant.micros(i * 1000L + 1000L))),
          Pt[Px](x, y),
          1
        )
      )
    }
    Trial(key, (), get(Scanpath.of(frame, clock, IArray.from(fixes))))

  /** Three participants recall items `a` and `b` they encoded earlier; `p1` also
    * recalls a third item `c`, so participants contribute unequal numbers of trials.
    */
  private def input(lastRecall: Vector[(Double, Double)] = Vector(0.5 -> 1.5, 1.5 -> 1.5)) =
    StudyInput(
      Trials(
        Vector(
          trial("p1", "a", "recall", Vector(0.5 -> 0.5, 1.5 -> 0.5)),
          trial("p1", "a", "encode", Vector(0.5 -> 0.5, 0.5 -> 1.5)),
          trial("p1", "b", "recall", Vector(1.5 -> 1.5, 1.5 -> 0.5)),
          trial("p1", "b", "encode", Vector(1.5 -> 1.5, 0.5 -> 0.5)),
          trial("p1", "c", "recall", Vector(1.5 -> 0.5, 1.5 -> 0.5)),
          trial("p1", "c", "encode", Vector(1.5 -> 0.5, 0.5 -> 1.5)),
          trial("p2", "a", "recall", Vector(0.5 -> 0.5, 0.5 -> 0.5)),
          trial("p2", "a", "encode", Vector(0.5 -> 0.5, 1.5 -> 0.5)),
          trial("p2", "b", "recall", Vector(1.5 -> 0.5, 1.5 -> 1.5)),
          trial("p2", "b", "encode", Vector(1.5 -> 0.5, 0.5 -> 1.5)),
          trial("p3", "a", "recall", Vector(1.5 -> 0.5, 0.5 -> 0.5, 0.5 -> 0.5)),
          trial("p3", "a", "encode", Vector(0.5 -> 0.5, 1.5 -> 1.5)),
          trial("p3", "b", "recall", lastRecall),
          trial("p3", "b", "encode", Vector(1.5 -> 1.5, 0.5 -> 1.5))
        )
      )
    )

  private final case class Completed(
      input: StudyInput[StudyKey, Px],
      plan: StudyPlan[StudyKey, Px, Unit, eyes4s.compare.Similarity, SignedDifference],
      result: StudyResult[StudyKey, Px, eyes4s.compare.Similarity, SignedDifference],
      source: ReportSource[StudyKey]
  )

  private def complete(in: StudyInput[StudyKey, Px] = input()): Completed =
    val plan = get(
      StudyPlan.cosine(
        in.reference,
        get(Grid.over(frame, 2, 2)),
        "recall",
        "encode",
        Weight.Duration,
        Vector(StudyEstimate.Binned()),
        FailurePolicy.RequireAll
      )
    )
    val result = get(plan.run(in))
    val source = get(
      ReportSources.study(
        StudyCodecs.cosine[Px],
        StudyInputCodecs.study[Px],
        StudyResultCodecs.cosine[Px]
      )(plan, in, result, None, CovariateSchema.empty)
    )
    Completed(in, plan, result, source)

  private val item = LevelTerm.Layout(LayoutField.Item)

  private def spec(id: String, grouped: Boolean) = get(
    ReportSpec.of(
      get(ReportId.of(id)),
      0,
      get(ReportSelection.of(Vector(Role.Difference, Role.Matched), Vector("value"))),
      groupBy = if grouped then Vector(Grouping.ByLevel(item)) else Vector.empty,
      contrast = if grouped then Some(LevelContrast(item, "a", "b")) else None
    )
  )

  private def estimate(value: Value[Double]): Double = value match
    case Value.Present(v) => v
    case Value.Missing(r) => fail(s"missing estimate: $r")

  /** The stored per-trial values of one role, read from the result itself. */
  private def stored(c: Completed, role: Role): Map[StudyKey, Double] =
    val contrast = get(c.result.scales.head.contrast)
    contrast.rows.flatMap { row =>
      role match
        case Role.Difference => row.difference.toOption.map(d => row.key -> d.value)
        case Role.Matched    =>
          row.matched.flatMap(_.result.toOption).map(s => row.key -> s.value)
        case Role.Control =>
          row.control.flatMap(_.result.toOption).map(s => row.key -> s.value)
    }.toMap

  private def mean(xs: Iterable[Double]): Double = xs.sum / xs.size

  test(
    "report cells and the level contrast equal the explicit participant-weighted reduction"
  ) {
    val c      = complete()
    val report = get(Report.evaluate(spec("by item", grouped = true), c.source))
    for role <- Vector(Role.Difference, Role.Matched) do
      val values = stored(c, role)
      assertEquals(values.size, 7)
      for level <- Vector("a", "b") do
        val cell = report.cells
          .find(c => c.role == role && c.group.levels.map(_._2) == Vector(level))
          .getOrElse(fail(s"no $role cell for $level"))
        // One recall trial per participant and item: each participant mean is that trial.
        val explicit = mean(values.collect { case (k, v) if k.stimulus == level => v })
        assert(Exact.approxEquals(estimate(cell.estimate), explicit), s"$role $level")
        assertEquals((cell.participants, cell.queries, cell.failed), (3, 3, 0))
      val contrast = report.contrasts.find(_.role == role).getOrElse(fail(s"no $role contrast"))
      val paired   = Vector("p1", "p2", "p3").map { p =>
        values(StudyKey(p, "a", "recall")) - values(StudyKey(p, "b", "recall"))
      }
      assert(Exact.approxEquals(estimate(contrast.estimate), mean(paired)), s"$role contrast")
      assertEquals(contrast.paired.map(_.participant), Vector("p1", "p2", "p3"))
      assertEquals(contrast.unpaired, Vector.empty)
    assert(report.accountingOf(Role.Difference).exists(a => a.eligible == 7 && a.kept == 7))
  }

  test("an ungrouped report weighs participants equally, not trials") {
    val c             = complete()
    val report        = get(Report.evaluate(spec("overall", grouped = false), c.source))
    val values        = stored(c, Role.Difference)
    val byParticipant =
      values.groupBy(_._1.participant).values.map(trials => mean(trials.values))
    val byTrial = mean(values.values)
    // p1 has three trials and the others two, so the two weightings must differ here.
    assert(math.abs(mean(byParticipant) - byTrial) > 1e-6, s"${mean(byParticipant)} $byTrial")
    val cell = report.cell(GroupKey.all, Role.Difference, "value").getOrElse(fail("no cell"))
    assert(Exact.approxEquals(estimate(cell.estimate), mean(byParticipant)))
    assert(!Exact.approxEquals(estimate(cell.estimate), byTrial))
    assertEquals(cell.perParticipant.map(_.participant), Vector("p1", "p2", "p3"))
  }

  test("reports save, reload and refuse a changed source; tables export what was reduced") {
    val c        = complete()
    val reportOf = spec("by item", grouped = true)
    val report   = get(Report.evaluate(reportOf, c.source))
    val specJson = get(ReportCodecs.reportSpec.encode(reportOf))
    assertEquals(get(ReportCodecs.reportSpec.parse(specJson.noSpaces)), reportOf)
    val codec  = ReportCodecs.report(StudyCodecs.cosine[Px].keys)
    val json   = get(codec.encode(report))
    val reread = get(codec.parse(json.noSpaces))
    assertEquals(reread, report)
    assertEquals(get(Report.evaluate(reportOf, c.source, report.binding)), report)
    assertEquals(report.checkCurrent(c.source.binding), Right(()))

    // Moving one fixation changes the input, the plan's reference and the result.
    val changed = complete(input(lastRecall = Vector(0.5 -> 1.5, 0.5 -> 0.5)))
    assert(report.checkCurrent(changed.source.binding).isLeft)
    assert(Report.evaluate(reportOf, changed.source, report.binding).isLeft)

    val cells = get(ReportTables.cells(report))
    assertEquals(cells.rows.size, report.cells.size)
    val study = get(
      ResultExports.study(c.plan, c.result, StudyCodecs.cosine[Px], ScoreColumns.similarity)
    )
    assertEquals(study.family, ResultFamily.StudyContrasts)
    assertEquals(study.rows.size, get(c.result.scales.head.contrast).rows.size)
    val header     = study.csvHeader
    val difference = header.indexOf("difference")
    assert(difference >= 0, s"no difference column in $header")
    val exported = study.csvRows.map(_(difference).toDouble).sorted
    assertEquals(exported, stored(c, Role.Difference).values.toVector.sorted)

    val runtime = if System.getProperty("java.vm.name") == "Scala.js" then "js" else "jvm"
    println(
      "EYES4S_REPORT_JOURNEY=" + Json
        .obj(
          "runtime"       -> Json.fromString(runtime),
          "report"        -> json,
          "cells_sha256"  -> Json.fromString(cells.identity.hex),
          "study_sha256"  -> Json.fromString(study.identity.hex),
          "contrast_bits" -> Json.arr(
            report.contrasts.map(s =>
              Json.fromString(
                java.lang.Double.doubleToLongBits(estimate(s.estimate)).toHexString
              )
            )*
          )
        )
        .noSpaces
    )
  }
