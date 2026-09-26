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

package eyes4s.results

import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

/** The top of the provenance chain on a stored study: the summary's cells, a
  * cell's participants, a participant's query contrasts, and back up; and
  * every refusal naming its operands.
  */
class ReportNavigationSuite extends munit.FunSuite:
  import ResultsDiagnostics.given

  private def get[E, A](e: Either[E, A]): A = e.fold(error => fail(s"$error"), identity)
  private def hex(c: Char)                  = Vector.fill(64)(c).mkString

  private val frame = get(Frame.screen("report", 2, 2))
  private val grid  = get(Grid.over(frame, 2, 2))

  private def trial(key: StudyKey, points: (Double, Double)*) =
    val clock = ClockId(s"${key.participant}/${key.stimulus}/${key.phase}")
    val fixes = points.zipWithIndex.map { case ((x, y), i) =>
      get(
        Event.Fixation.withoutDispersion(
          get(Interval.of(clock, Instant.micros(i * 1000L), Instant.micros(i * 1000L + 500L))),
          Pt[Px](x, y),
          1
        )
      )
    }
    Trial(key, (), get(Scanpath.of(frame, clock, IArray.from(fixes))))

  private def k(p: String, i: String) = StudyKey(p, i, "recall")
  private val input                   = StudyInput(
    Trials(
      for
        p     <- Vector("P17", "P18")
        i     <- Vector("a", "b", "c")
        phase <- Vector("recall", "encode")
      yield trial(StudyKey(p, i, phase), (0.5, 0.5), (1.5, if i == "a" then 0.5 else 1.5))
    )
  )
  private val plan = get(
    StudyPlan.cosine(
      input.reference,
      grid,
      "recall",
      "encode",
      Weight.Duration,
      Vector(StudyEstimate.Binned()),
      FailurePolicy.RequireAll
    )
  )
  private val layout  = plan.layout
  private val result  = get(plan.run(input))
  private val binding = ReportBinding(
    get(BindingDigest.parse("plan", hex('a'))),
    get(BindingDigest.parse("input", hex('b'))),
    get(BindingDigest.parse("result", hex('c'))),
    None
  )
  private val byParticipant = Grouping.ByLevel(LevelTerm.Layout(LayoutField.Participant))
  private val report        = get(
    Report.evaluate(
      get(
        ReportSpec.of(
          get(ReportId.of("by participant")),
          0,
          get(ReportSelection.of(Vector(Role.Difference), Vector("value"))),
          None,
          Vector(byParticipant),
          ReducePolicy.default,
          None,
          Spread.StandardDeviation
        )
      ),
      get(ReportSource.study(plan, input, result, None, binding))
    )
  )
  private val p17  = GroupKey(Vector("participant" -> "P17"))
  private val cell = get(ReportRef.cell(0, p17, Role.Difference, "value"))

  test("the summary's cells, a cell's participant, and its query contrasts, in order") {
    assertEquals(
      ReportNavigation.cells(report),
      Vector("P17", "P18").map(p =>
        get(ReportRef.cell(0, GroupKey(Vector("participant" -> p)), Role.Difference, "value"))
      )
    )
    val participants = get(ReportNavigation.participants(report, cell))
    assertEquals(participants, Vector(get(ReportRef.participant(cell, "P17"))))
    assertEquals(participants.head.cell, cell)
    val queries = get(ReportNavigation.queries(report, participants.head, layout))
    assertEquals(queries, Vector("a", "b", "c").map(i => ResultRef.ContrastRow(0, k("P17", i))))
    queries.foreach(q =>
      assertEquals(
        ReportNavigation.participantOf(report, cell, q, layout),
        Right(participants.head)
      )
    )
  }

  test("refusals name their operands, and project to coded diagnostics") {
    val elsewhere =
      get(ReportRef.cell(0, GroupKey(Vector("participant" -> "P99")), Role.Difference, "value"))
    assertEquals(
      ReportNavigation.participants(report, elsewhere),
      Left(ReportNavigationError.UnknownCell(elsewhere.group, Role.Difference, "value"))
    )
    val later = get(ReportRef.cell(2, p17, Role.Difference, "value"))
    assertEquals(
      ReportNavigation.participants(report, later),
      Left(ReportNavigationError.ScaleMismatch(2, 0))
    )
    val stranger = get(ReportRef.participant(cell, "P18"))
    assertEquals(
      ReportNavigation.queries(report, stranger, layout),
      Left(ReportNavigationError.NotInCell("P18", p17, Role.Difference, "value"))
    )
    val theirs = ResultRef.ContrastRow(0, k("P18", "a"))
    assertEquals(
      ReportNavigation.participantOf(report, cell, theirs, layout),
      Left(ReportNavigationError.NotAMember(k("P18", "a"), p17, Role.Difference, "value"))
    )
    assertEquals(
      ReportNavigation
        .participantOf(report, cell, ResultRef.ContrastRow(1, k("P17", "a")), layout),
      Left(ReportNavigationError.ScaleMismatch(1, 0))
    )
    val map   = ResultRef.Estimation(0, k("P17", "a"))
    val wrong = ReportNavigation.participantOf(report, cell, map, layout)
    assertEquals(
      wrong,
      Left(ReportNavigationError.WrongLevel(map, NavigationLevel.QueryContrast))
    )
    assertEquals(
      ReportRef.cell(-1, p17, Role.Matched, "v"),
      Left(ReportNavigationError.NegativeScale(-1))
    )
    assertEquals(
      ReportRef.cell(0, p17, Role.Matched, " "),
      Left(ReportNavigationError.BlankComponent(" "))
    )
    assertEquals(
      ReportRef.participant(cell, ""),
      Left(ReportNavigationError.BlankParticipant(""))
    )
    val messages = Vector(
      ReportNavigationError.NegativeScale(-1)    -> "not -1",
      ReportNavigationError.BlankComponent(" ")  -> "names a component",
      ReportNavigationError.BlankParticipant("") -> "is named",
      ReportNavigationError.ScaleMismatch(2, 0)  -> "scale 2; the report is at scale 0",
      ReportNavigationError
        .UnknownCell(p17, Role.Matched, "v") -> "no cell for participant=P17",
      ReportNavigationError
        .NotInCell("P18", p17, Role.Matched, "v") -> "Participant P18 has no value",
      ReportNavigationError
        .WrongLevel(map, NavigationLevel.QueryContrast) -> "is not a QueryContrast",
      ReportNavigationError.NotAMember(
        k("P18", "a"),
        p17,
        Role.Matched,
        "v"
      ) -> "is not a member"
    )
    messages.foreach((error, text) => assert(error.message.contains(text), error.message))
    val diagnostic = Diagnostic.of(
      ReportNavigationError.NotAMember(k("P18", "a"), p17, Role.Difference, "value")
    )
    assertEquals(diagnostic.code.render, "report-navigation.not-a-member")
    assertEquals(diagnostic.keys, Vector(k("P18", "a")))
  }
