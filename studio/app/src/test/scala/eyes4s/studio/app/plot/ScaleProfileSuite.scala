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

import eyes4s.studio.core.backend.{ResultSummary, Response, RunId}
import eyes4s.studio.core.document.{ReportingId, ScaleSet, Sigma}
import eyes4s.studio.core.fixture.{MockStudy, StoryMoment, StoryMoments}
import eyes4s.studio.core.headless.HeadlessSession
import eyes4s.studio.core.selection.{ScaleIndex, StudioRef}
import io.circe.Json

import scala.concurrent.{ExecutionContext, Future}

/** The scale profile's values (ticket S4.5d): read from the fake backend's
  * run summary at the run's declared scales and written as the plot's and
  * table's one value source. Every group's and participant's D by scale is
  * checked against fixture.json itself, read here independently of the
  * backend's decoding.
  */
class ScaleProfileSuite extends munit.FunSuite:

  private given ExecutionContext = ExecutionContext.global

  private val run = RunId(7)

  private def right[E, A](either: Either[E, A]): A =
    either.fold(e => fail(s"unexpected Left: $e"), identity)

  private val reporting = right(ReportingId.of("by-retrieval-response"))
  private val columns   = right(ProfileColumns.standard)

  /** Run 7's declared scales: its analysis revision's recipe. */
  private lazy val declared: ScaleSet =
    val doc = right(StoryMoments.t2)
    right(
      doc
        .run(run)
        .flatMap(r => doc.analysis(r.analysis))
        .map(_.recipe.scales)
        .toRight("no run 7")
    )

  private def summary: Future[ResultSummary] =
    HeadlessSession
      .open(StoryMoment.T2)
      .flatMap(s => s.result(run).transformWith(r => s.close.transform(_ => r)))
      .map(right)

  private lazy val fixture: Json =
    val json = right(io.circe.parser.parse(MockStudy.fixtureText))
    json.hcursor.downField("summary").focus.getOrElse(json)

  private def scale(i: Int) = right(ScaleIndex.of(i))

  test("each group's and participant's D at each declared scale is fixture.json's") {
    summary.map { s =>
      val profile = right(ScaleProfile.of(s, reporting, declared))
      assertEquals(declared.values.map(_.degrees), Vector(0.5, 1.0, 2.0, 4.0))
      assertEquals(profile.groups.map(_.name), Vector("Remembered", "Forgotten"))
      profile.groups.foreach { g =>
        assertEquals(
          g.points.flatMap(_.d),
          right(fixture.hcursor.get[Vector[Double]](s"grand_D_by_scale_${g.name}"))
        )
        assertEquals(g.n, "24 participants")
        assertEquals(
          g.points.map(_.ref),
          Vector.tabulate(4)(i =>
            StudioRef.GroupCell(run, reporting, scale(i), Response(g.name))
          )
        )
        assertEquals(g.points.map(_.label), Vector("0.5°", "1°", "2°", "4°"))
      }
      val participants = right(fixture.hcursor.downField("participants").as[Vector[Json]])
      assertEquals(profile.participants.size, 24)
      participants.foreach { p =>
        val id     = right(p.hcursor.get[String]("id"))
        val series = profile.participants.find(_.name == id).getOrElse(fail(id))
        assertEquals(
          series.points.flatMap(_.d),
          right(p.hcursor.downField("all").get[Vector[Double]]("D_by_scale")),
          id
        )
        assertEquals(series.n, s"${right(p.hcursor.get[Int]("contributing"))} queries")
        assertEquals(
          series.points.map(_.ref),
          Vector.tabulate(4)(i =>
            StudioRef.ParticipantSummary(run, reporting, scale(i), None, id)
          )
        )
      }
    }
  }

  test("the source lists the groups' points, then the participants', in scale order") {
    summary.map { s =>
      val profile = right(ScaleProfile.of(s, reporting, declared))
      val source  = right(ScaleProfile.source(profile, columns))
      assertEquals(source.rows.size, (2 + 24) * 4)
      assertEquals(
        source.rows.map(_.ref),
        (profile.groups ++ profile.participants).flatMap(_.points.map(_.ref))
      )
      val p17 = profile.participants.find(_.name == "P17").getOrElse(fail("P17"))
      assertEquals(
        p17.points.map(p => source.cells(right(source.rowOf(p.ref).toRight(p.ref))).get),
        Vector(
          Vector("P17", "0.5°", "0.50", "+0.19", "19 queries"),
          Vector("P17", "1°", "1.00", "+0.29", "19 queries"),
          Vector("P17", "2°", "2.00", "+0.38", "19 queries"),
          Vector("P17", "4°", "4.00", "+0.23", "19 queries")
        )
      )
    }
  }

  test("a participant without contributing queries, or a scale not served, is missing") {
    summary.map { s =>
      val p   = s.participants.head
      val cut = s.copy(
        participants = Vector(
          p.copy(contributing = 0),
          p.copy(participant = "PX", all = p.all.copy(dByScale = p.all.dByScale.take(2)))
        ),
        groups = s.groups.map(g => g.copy(dByScale = g.dByScale.take(3)))
      )
      val profile = right(ScaleProfile.of(cut, reporting, declared))
      assertEquals(profile.participants.head.points.map(_.d), Vector.fill(4)(None))
      assertEquals(
        profile.participants(1).points.map(_.d.isDefined),
        Vector(true, true, false, false)
      )
      assertEquals(
        profile.groups.head.points.map(_.d.isDefined),
        Vector(true, true, true, false)
      )
      val source = right(ScaleProfile.source(profile, columns))
      val row    = right(source.rowOf(profile.participants.head.points.head.ref).toRight("row"))
      assertEquals(source.value(row, columns.d), Some(PlotValue.Missing))
    }
  }

  test("scales that are not the summary's, and repeated groups or participants, are refused") {
    summary.map { s =>
      val three = right(ScaleSet.of(declared.values.take(3)).left.map(_.message))
      assertEquals(
        ScaleProfile.of(s, reporting, three),
        Left(ProfileError.ScaleCount(run, 3, 4))
      )
      val swapped = right(
        ScaleSet
          .of(Vector(0.5, 1.0, 4.0, 2.0).map(d => right(Sigma.of(d).left.map(_.message))))
          .left
          .map(_.message)
      )
      ScaleProfile.of(s, reporting, swapped) match
        case Left(e @ ProfileError.ScaleLabel(_, 2, "2°", sigma)) =>
          assertEquals(sigma.degrees, 4.0)
          assert(e.message.contains("2°"), e.message)
        case other => fail(s"$other")
      val g = s.groups.head
      assertEquals(
        ScaleProfile.of(s.copy(groups = s.groups :+ g), reporting, declared),
        Left(ProfileError.DuplicateGroup(run, g.label))
      )
      val p = s.participants.head
      assertEquals(
        ScaleProfile.of(s.copy(participants = s.participants :+ p), reporting, declared),
        Left(ProfileError.DuplicateParticipant(run, p.participant))
      )
    }
  }
