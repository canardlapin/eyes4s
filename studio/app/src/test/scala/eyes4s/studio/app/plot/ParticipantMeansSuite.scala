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

import eyes4s.studio.app.text.{Format, ParticipantText, ParticipantTextId}
import eyes4s.studio.core.backend.{
  GroupMeans,
  GroupSummary,
  ParticipantSummary,
  ResultSummary,
  Response,
  RunId,
  ScoreMeans
}
import eyes4s.studio.core.document.ReportingId
import eyes4s.studio.core.fixture.{MockStudy, StoryMoment}
import eyes4s.studio.core.headless.HeadlessSession
import eyes4s.studio.core.selection.{ScaleIndex, StudioRef}
import io.circe.Json

import scala.concurrent.{ExecutionContext, Future}

/** The participant plot's values (ticket S4.5c): read from the fake
  * backend's run summary and written as the plot's and table's one value
  * source. Every participant's group means are checked against
  * fixture.json itself, read here independently of the backend's decoding.
  */
class ParticipantMeansSuite extends munit.FunSuite:

  private given ExecutionContext = ExecutionContext.global

  private val run = RunId(7)

  private def right[E, A](either: Either[E, A]): A =
    either.fold(e => fail(s"unexpected Left: $e"), identity)

  private val reporting = right(ReportingId.of("by-retrieval-response"))
  private val columns   = right(ParticipantColumns.standard)
  private val at2       = right(ScaleIndex.of(2))

  private def summary: Future[ResultSummary] =
    HeadlessSession
      .open(StoryMoment.T2)
      .flatMap(s => s.result(run).transformWith(r => s.close.transform(_ => r)))
      .map(right)

  // fixture.json's summary, read without the backend.
  private lazy val fixture: Json =
    val json = right(io.circe.parser.parse(MockStudy.fixtureText))
    json.hcursor.downField("summary").focus.getOrElse(json)

  private def fixtureParticipants: Vector[Json] =
    right(fixture.hcursor.downField("participants").as[Vector[Json]])

  test("the fixture's 24 participants and both grand means are the summary's, at 2°") {
    summary.map { s =>
      val means = right(ParticipantMeans.of(s, reporting, at2))
      assertEquals(means.scaleLabel, "2°")
      assertEquals(means.groups.map(_.group), Vector(Response.Remembered, Response.Forgotten))
      Vector("Remembered", "Forgotten").zip(means.groups).foreach { (label, g) =>
        assertEquals(g.d, right(fixture.hcursor.get[Double](s"grand_D_$label")))
        assertEquals(g.n, right(fixture.hcursor.get[Int](s"n_$label")))
        assertEquals(g.ref, StudioRef.GroupCell(run, reporting, at2, Response(label)))
      }
      assertEquals(means.cells.size, 48)
      for
        p     <- fixtureParticipants
        label <- Vector("Remembered", "Forgotten")
      do
        val id   = right(p.hcursor.get[String]("id"))
        val cell = means.cells
          .find(c => c.participant == id && c.group.label == label)
          .getOrElse(fail(s"no cell for $id in $label"))
        val g = p.hcursor.downField(label)
        assertEquals(cell.d, Some(right(g.get[Double]("D"))), s"$id $label")
        assertEquals(cell.n, Some(right(g.get[Int]("n"))), s"$id $label")
        assertEquals(
          cell.ref,
          StudioRef.ParticipantSummary(run, reporting, at2, Some(Response(label)), id)
        )
      // The board's per-group n range, 2 to 17 queries per participant.
      val ns = means.cells.flatMap(_.n)
      assertEquals((ns.min, ns.max), (2, 17))
    }
  }

  test(
    "the source lists each group's participants, then its grand mean, as the table writes them"
  ) {
    summary.map { s =>
      val means  = right(ParticipantMeans.of(s, reporting, at2))
      val source = right(ParticipantMeans.source(means, columns))
      assertEquals(source.caption, ParticipantText(ParticipantTextId.Caption, "2°"))
      assertEquals(source.rows.size, 50)
      assertEquals(
        source.rows.map(_.ref),
        means.groups.flatMap(g => means.cells.filter(_.group == g.group).map(_.ref) :+ g.ref)
      )
      def cells(ref: StudioRef) = source.cells(right(source.rowOf(ref).toRight(ref))).get
      val p17                   = means.cells.filter(_.participant == "P17")
      assertEquals(
        p17.map(c => cells(c.ref)),
        Vector(
          Vector("Remembered", "P17", "+0.38", "17 queries"),
          Vector("Forgotten", "P17", "+0.32", "2 queries")
        )
      )
      assertEquals(
        means.groups.map(g => cells(g.ref)),
        Vector(
          Vector("Remembered", "all participants", "+0.30", "24 participants"),
          Vector("Forgotten", "all participants", "+0.15", "24 participants")
        )
      )
      // P05's Forgotten mean is negative, written with U+2212.
      val p05 = means.cells.find(c => c.participant == "P05" && c.group == Response.Forgotten)
      assertEquals(p05.map(c => cells(c.ref)(2)), Some(Format.signed(-0.23, 2)))
    }
  }

  // --- Missing means and refusals ------------------------------------------------------

  private val remembered = Response.Remembered
  private val forgotten  = Response.Forgotten
  private val scoreMeans = ScoreMeans(0.5, 0.3, 0.2, Vector(0.1, 0.2))

  private def participant(id: String, groups: GroupMeans*): ParticipantSummary =
    ParticipantSummary(id, 4, 4, 0, 0, 0, scoreMeans, groups.toVector)

  private val twoGroups = Vector(
    GroupSummary("response", remembered, 3, 0.2, Vector(0.1, 0.2)),
    GroupSummary("response", forgotten, 2, 0.1, Vector(0.05, 0.1))
  )

  // The fixture's summary with two scales and these groups and participants.
  private def synthetic(
      participants: Vector[ParticipantSummary],
      groups: Vector[GroupSummary] = twoGroups
  ): Future[ResultSummary] =
    summary.map(
      _.copy(scales = Vector("1°", "2°"), groups = groups, participants = participants)
    )

  private val at1 = right(ScaleIndex.of(1))

  test("a group without queries, or with means of none, is missing, never zero") {
    synthetic(
      Vector(
        participant(
          "A",
          GroupMeans(remembered, 3, 0.6, 0.6, 0.0),
          GroupMeans(forgotten, 1, 0.4, 0.3, 0.1)
        ),
        participant("B", GroupMeans(remembered, 2, 0.5, 0.3, 0.2)),
        participant(
          "C",
          GroupMeans(remembered, 4, 0.5, 0.3, 0.2),
          GroupMeans(forgotten, 0, 0.0, 0.0, 0.0)
        )
      )
    ).map { s =>
      val means                        = right(ParticipantMeans.of(s, reporting, at1))
      def cell(p: String, g: Response) =
        means.cells.find(c => c.participant == p && c.group == g).getOrElse(fail(s"$p $g"))
      // A zero mean is a value.
      assertEquals((cell("A", remembered).d, cell("A", remembered).n), (Some(0.0), Some(3)))
      // No means listed: no D and no n.
      assertEquals((cell("B", forgotten).d, cell("B", forgotten).n), (None, None))
      // Means of no queries: n 0 and no D.
      assertEquals((cell("C", forgotten).d, cell("C", forgotten).n), (None, Some(0)))
      val source                = right(ParticipantMeans.source(means, columns))
      def d(c: ParticipantCell) =
        source.value(right(source.rowOf(c.ref).toRight(c.ref)), columns.d)
      assertEquals(d(cell("A", remembered)), Some(PlotValue.Number(0.0)))
      assertEquals(d(cell("B", forgotten)), Some(PlotValue.Missing))
      assertEquals(d(cell("C", forgotten)), Some(PlotValue.Missing))
      val row = right(source.rowOf(cell("B", forgotten).ref).toRight("B"))
      assertEquals(
        source.cells(row).map(_.drop(2)),
        Some(Vector(PlotSource.MissingText, PlotSource.MissingText))
      )
    }
  }

  test("a scale the run lacks, or not the summary's, is refused, naming it") {
    synthetic(Vector(participant("A", GroupMeans(remembered, 3, 0.6, 0.4, 0.2)))).map { s =>
      val at5 = right(ScaleIndex.of(5))
      assertEquals(
        ParticipantMeans.of(s, reporting, at5),
        Left(ParticipantMeansError.Scale(run, at5, Vector("1°", "2°")))
      )
      val at0 = right(ScaleIndex.of(0))
      val e   = ParticipantMeans.of(s, reporting, at0)
      assertEquals(e, Left(ParticipantMeansError.OtherScale(run, remembered, at0, 0.2, 0.1)))
      assert(e.left.toOption.get.message.contains("Remembered"), e)
    }
  }

  test("duplicate groups or participants, and means in no group, are refused") {
    val a = participant("A", GroupMeans(remembered, 3, 0.6, 0.4, 0.2))
    for
      dg <- synthetic(Vector(a), twoGroups :+ twoGroups.head)
      dp <- synthetic(Vector(a, a))
      ug <- synthetic(
        Vector(participant("A", GroupMeans(Response("Unsure"), 1, 0.5, 0.4, 0.1)))
      )
      tw <- synthetic(
        Vector(
          participant(
            "A",
            GroupMeans(remembered, 1, 0.5, 0.4, 0.1),
            GroupMeans(remembered, 1, 0.5, 0.4, 0.1)
          )
        )
      )
    yield
      assertEquals(
        ParticipantMeans.of(dg, reporting, at1),
        Left(ParticipantMeansError.DuplicateGroup(run, remembered))
      )
      assertEquals(
        ParticipantMeans.of(dp, reporting, at1),
        Left(ParticipantMeansError.DuplicateParticipant(run, "A"))
      )
      val groups = Vector(remembered, forgotten)
      assertEquals(
        ParticipantMeans.of(ug, reporting, at1),
        Left(ParticipantMeansError.UnknownGroup(run, "A", Response("Unsure"), groups))
      )
      assertEquals(
        ParticipantMeans.of(tw, reporting, at1),
        Left(ParticipantMeansError.UnknownGroup(run, "A", remembered, groups))
      )
  }
