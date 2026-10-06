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

package eyes4s.studio.app.plot

import eyes4s.studio.core.backend.{ReportRole, Response}
import eyes4s.studio.core.selection.StudioRef
import scala.concurrent.ExecutionContext

/** Participant plotting reads scale-bound native reports and keeps every
  * served reference, including whole-run and missing-value cells.
  */
class ParticipantMeansSuite extends munit.FunSuite:
  private given ExecutionContext           = ExecutionContext.global
  private def ok[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)
  private val columns                      = ok(ParticipantColumns.standard)

  test(
    "native fixture participant cells and grand means preserve the exact report values and refs"
  ) {
    PlotReports.read.map { (summary, reports, _) =>
      val report = reports(2)
      val means  = ok(ParticipantMeans.of(report, summary.scales(2)))
      assertEquals(means.scale.value, 2)
      assertEquals(means.groups.map(_.group), Vector(Response.Remembered, Response.Forgotten))
      assertEquals(
        means.groups.map(_.d),
        report.cells.filter(_.role == ReportRole.Difference).flatMap(_.estimate)
      )
      assertEquals(
        means.groups.map(_.ref),
        report.cells.filter(_.role == ReportRole.Difference).map(_.ref)
      )
      val participants = report.participants.filter(_.role == ReportRole.Difference)
      assertEquals(means.cells.map(_.ref), participants.map(_.ref))
      assertEquals(means.cells.map(_.d), participants.map(_.value))
      assertEquals(means.cells.map(_.n), participants.map(p => Some(p.queries)))
      assertEquals(means.cells.size, 48)
      val source = ok(ParticipantMeans.source(means, columns))
      assertEquals(source.rows.size, 50)
      assertEquals(
        source.rows.map(_.ref),
        means.groups.flatMap(g => means.cells.filter(_.group == g.group).map(_.ref) :+ g.ref)
      )
    }
  }

  test("an ungrouped report plots All queries and preserves whole-run references") {
    val report = PlotReports.synthetic(group = None, id = PlotReports.overallId)
    val means  = ok(ParticipantMeans.of(report, "2°"))
    assertEquals(means.groups.map(_.group.label), Vector("All queries"))
    assertEquals(means.groups.head.ref, report.cells.head.ref)
    assertEquals(means.cells.map(_.ref), report.participants.map(_.ref))
    assert(means.cells.forall(_.ref.isInstanceOf[StudioRef.ReportParticipant]))
    val source = ok(ParticipantMeans.source(means, columns))
    assertEquals(source.value(0, columns.d), Some(PlotValue.Number(0.0)))
    assertEquals(source.value(1, columns.d), Some(PlotValue.Missing))
  }

  test(
    "an absent group estimate is a typed plot refusal, with its original report/table cell intact"
  ) {
    val report = PlotReports.synthetic(estimate = None)
    assertEquals(
      ParticipantMeans.of(report, "2°"),
      Left(
        ParticipantMeansError.UnscoredGroup(
          report.run,
          Response.Remembered,
          Some(eyes4s.studio.core.backend.ReportAbsence.EmptyGroup)
        )
      )
    )
    assertEquals(report.cells.size, 1)
    assertEquals(report.cells.head.estimate, None)
  }

  test("duplicate cells and participants, and an undeclared group, are refused") {
    val report = PlotReports.synthetic()
    assertEquals(
      ParticipantMeans.of(report.copy(cells = report.cells ++ report.cells), "2°"),
      Left(ParticipantMeansError.DuplicateGroup(report.run, Response.Remembered))
    )
    assertEquals(
      ParticipantMeans.of(
        report.copy(participants = report.participants ++ report.participants.take(1)),
        "2°"
      ),
      Left(ParticipantMeansError.DuplicateParticipant(report.run, "A"))
    )
    val unknown = report.copy(participants =
      report.participants.map(_.copy(group = Some(Response.Forgotten)))
    )
    assertEquals(
      ParticipantMeans.of(unknown, "2°"),
      Left(
        ParticipantMeansError.UnknownGroup(
          report.run,
          "A",
          Response.Forgotten,
          Vector(Response.Remembered)
        )
      )
    )
  }
