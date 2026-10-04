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

package eyes4s.studio.app.compare

import eyes4s.studio.app.layout.{CompareLayout, StudioLayouts}
import eyes4s.studio.app.nav.Place
import eyes4s.studio.app.plot.{ParticipantColumns, PlotSource, PlotValue}
import eyes4s.studio.app.text.Format
import eyes4s.studio.app.{AppModel, StoryModels}
import eyes4s.studio.core.backend.{PageRequest, QueryRow, ResultSummary, Response}
import eyes4s.studio.core.fixture.{StoryMoment, StoryMoments}
import eyes4s.studio.core.headless.HeadlessSession
import eyes4s.studio.core.selection.{ScaleIndex, StudioRef}

import scala.concurrent.{ExecutionContext, Future}

/** Compare's summary layout headlessly (ticket S8.6): the fake backend's
  * run 7 as the Results board shows it. Every number is FIXTURE.md's
  * participant table, typed in below; Explain keeps the spec and the group;
  * the σ selector offers only the scale the means are served at; the
  * newer run's freshness shows while it runs.
  */
class CompareSummarySuite extends munit.FunSuite:
  import StoryModels.*

  private given ExecutionContext = ExecutionContext.global

  private val run7       = StoryMoments.run7
  private val remembered = Response.Remembered

  /** FIXTURE.md, participant table (σ 2°), as written there. */
  private val fixtureTable: Vector[Vector[String]] = Vector(
    "P01 | 20 | 19 | 0 | 0 | 1 | 0.52 | 0.35 | +0.17 | +0.26 (11) | +0.06 (8)",
    "P02 | 20 | 19 | 0 | 0 | 1 | 0.58 | 0.36 | +0.22 | +0.27 (13) | +0.12 (6)",
    "P03 | 20 | 19 | 0 | 1 | 0 | 0.68 | 0.35 | +0.33 | +0.38 (15) | +0.15 (4)",
    "P04 | 20 | 19 | 0 | 0 | 1 | 0.65 | 0.36 | +0.29 | +0.36 (12) | +0.18 (7)",
    "P05 | 20 | 17 | 3 | 0 | 0 | 0.27 | 0.34 | -0.08 | -0.03 (13) | -0.23 (4)",
    "P06 | 20 | 19 | 0 | 0 | 1 | 0.59 | 0.34 | +0.25 | +0.30 (11) | +0.17 (8)",
    "P07 | 20 | 19 | 0 | 1 | 0 | 0.62 | 0.35 | +0.27 | +0.32 (15) | +0.10 (4)",
    "P08 | 20 | 19 | 0 | 1 | 0 | 0.61 | 0.35 | +0.25 | +0.31 (12) | +0.15 (7)",
    "P09 | 20 | 19 | 0 | 0 | 1 | 0.67 | 0.35 | +0.32 | +0.34 (15) | +0.24 (4)",
    "P10 | 20 | 19 | 0 | 1 | 0 | 0.78 | 0.35 | +0.43 | +0.45 (16) | +0.33 (3)",
    "P11 | 20 | 19 | 0 | 1 | 0 | 0.57 | 0.35 | +0.22 | +0.28 (13) | +0.11 (6)",
    "P12 | 20 | 19 | 0 | 0 | 1 | 0.64 | 0.35 | +0.28 | +0.34 (12) | +0.19 (7)",
    "P13 | 20 | 19 | 0 | 0 | 1 | 0.47 | 0.36 | +0.11 | +0.17 (11) | +0.03 (8)",
    "P14 | 20 | 19 | 0 | 1 | 0 | 0.62 | 0.35 | +0.27 | +0.29 (14) | +0.22 (5)",
    "P15 | 20 | 19 | 0 | 1 | 0 | 0.45 | 0.35 | +0.10 | +0.18 (13) | -0.08 (6)",
    "P16 | 20 | 19 | 0 | 0 | 1 | 0.74 | 0.35 | +0.38 | +0.45 (12) | +0.28 (7)",
    "P17 | 20 | 19 | 0 | 0 | 1 | 0.73 | 0.35 | +0.38 | +0.38 (17) | +0.32 (2)",
    "P18 | 20 | 19 | 0 | 1 | 0 | 0.76 | 0.35 | +0.41 | +0.42 (16) | +0.37 (3)",
    "P19 | 20 | 19 | 0 | 0 | 1 | 0.59 | 0.35 | +0.24 | +0.29 (15) | +0.06 (4)",
    "P20 | 20 | 19 | 0 | 0 | 1 | 0.59 | 0.37 | +0.22 | +0.26 (14) | +0.12 (5)",
    "P21 | 20 | 19 | 0 | 1 | 0 | 0.67 | 0.35 | +0.31 | +0.32 (17) | +0.27 (2)",
    "P22 | 20 | 19 | 0 | 0 | 1 | 0.54 | 0.35 | +0.19 | +0.27 (12) | +0.06 (7)",
    "P23 | 20 | 19 | 0 | 0 | 1 | 0.64 | 0.34 | +0.30 | +0.34 (14) | +0.21 (5)",
    "P24 | 20 | 19 | 0 | 0 | 1 | 0.65 | 0.34 | +0.30 | +0.36 (13) | +0.17 (6)"
  ).map(_.split('|').toVector.map(_.trim))

  // FIXTURE.md writes minus as '-'; studio writes U+2212.
  private def ascii(s: String): String = s.replace(Format.Minus, "-")

  private def right[E, A](e: Either[E, A]): A =
    e.fold(x => fail(s"unexpected Left: $x"), identity)

  private def read: Future[(ResultSummary, Vector[QueryRow])] =
    HeadlessSession.open(StoryMoment.T3).flatMap { s =>
      val page = right(PageRequest.first(PageRequest.MaximumSize))
      (for
        r <- s.result(run7)
        q <- s.queries(run7, page)
      yield (right(r), right(q).rows)).transformWith(x => s.close.transform(_ => x))
    }

  /** The summary layout of `m` once run 7's summary and queries are read. */
  private def loaded(m: AppModel): Future[CompareSummary] =
    read.map { (r, q) =>
      val (s0, asks) = CompareSummary.sync(CompareSummary.empty, m)
      assertEquals(
        asks,
        Vector(SummaryEffect.RequestSummary(run7), SummaryEffect.RequestQueries(run7))
      )
      val s1 =
        CompareSummary.update(s0, SummaryIntent.SummaryRead(run7, SummaryAnswer.Answered(r)))._1
      CompareSummary.update(s1, SummaryIntent.QueriesRead(run7, QueriesAnswer.Answered(q)))._1
    }

  private def cells(source: PlotSource): Vector[Vector[String]] =
    source.rows.map(source.cellsOf).map(_.map(ascii))

  test("the participant table is FIXTURE.md's, number for number") {
    loaded(t3Summary).map { s =>
      val vm    = CompareSummaryVM.of(s, t3Summary)
      val table = right(vm.participants.getOrElse(fail("no participant table")))
      assertEquals(table.columns.size, 11)
      assertEquals(cells(table), fixtureTable)
      assertEquals(table.caption, "Participant summary at σ 2°, run 7")
      // Every row is the participant's summary ref, at 2° in run 7.
      assertEquals(
        table.rows.map(_.ref),
        fixtureTable.map(row =>
          StudioRef.ParticipantSummary(run7, reporting, sigma2, None, row.head): StudioRef
        )
      )
    }
  }

  test("the plot's grand means, the notes and the σ selector are the fixture's") {
    loaded(t3Summary).map { s =>
      val vm    = CompareSummaryVM.of(s, t3Summary)
      val plot  = right(vm.participantPlot.getOrElse(fail("no plot")))
      val cols  = right(ParticipantColumns.standard)
      val di    = plot.indexOf(cols.d).get
      val grand = plot.rows.indices.collect {
        case i if plot.rows(i).ref.isInstanceOf[StudioRef.GroupCell] =>
          (plot.text(i, 0).get, plot.text(i, di).get, plot.text(i, 3).get)
      }
      assertEquals(
        grand.toVector,
        Vector(
          ("Remembered", "+0.30", "24 participants"),
          ("Forgotten", "+0.15", "24 participants")
        )
      )
      // 24 participants in each group.
      assertEquals(plot.rows.count(_.ref.isInstanceOf[StudioRef.ParticipantSummary]), 48)
      assertEquals(
        vm.notes,
        Vector(
          "Paired n = 24 participants",
          "Means: equal participant weight",
          "Unit below each dot: query trial",
          "Per-group n: 2–17 queries per participant"
        )
      )
      // The means are served at 2° only: the other scales cannot be chosen.
      assertEquals(
        vm.scales.map(c => (c.label, c.available, c.chosen)),
        Vector(
          ("σ 0.5°", false, false),
          ("σ 1°", false, false),
          ("σ 2°", true, true),
          ("σ 4°", false, false)
        )
      )
      val s0 = CompareSummary.update(s, SummaryIntent.ChooseScale(right(ScaleIndex.of(0))))._1
      assertEquals(s0.shown, Some(sigma2))
      // The scale profile is at every protocol scale, the query table every query.
      val profile = right(vm.profile.getOrElse(fail("no profile")))
      assertEquals(profile.rows.size, (2 + 24) * 4)
      val queries = right(vm.queries.getOrElse(fail("no queries")))
      assertEquals(queries.rows.size, 480)
      // The newer run's freshness, while run 8 runs.
      assertEquals(vm.freshness, Some("Rev 5 · run 8 running · 48%"))
    }
  }

  test("a P05 query that failed has no value, never zero, in the query table") {
    loaded(t3Summary).map { s =>
      val queries = right(CompareSummaryVM.of(s, t3Summary).queries.get)
      val failed  = queries.rows.indices.filter(i => queries.text(i, 3).contains("failed"))
      assertEquals(failed.size, 3)
      failed.foreach { i =>
        assert(queries.text(i, 0).exists(_.startsWith("P05")), queries.text(i, 0))
        Vector(4, 5, 6).foreach(c => assertEquals(queries.rows(i).values(c), PlotValue.Missing))
        assertEquals(queries.text(i, 6), Some(PlotSource.MissingText))
      }
    }
  }

  test("a group mean of no queries is missing in the table and the plot, never zero") {
    read.map { (r, _) =>
      val p   = r.participants.head
      val cut = r.copy(participants =
        p.copy(groups =
          p.groups.map(g => if g.label == remembered then g.copy(n = 0) else g)
        ) +:
          r.participants.tail
      )
      val s = CompareSummary(
        Some(run7),
        Some(reporting),
        Some(SummaryAnswer.Answered(cut)),
        None,
        None
      )
      val vm    = CompareSummaryVM.of(s, t3Summary)
      val table = right(vm.participants.get)
      assertEquals(table.text(0, 9), Some(PlotSource.MissingText))
      val plot = right(vm.participantPlot.get)
      val row  = plot.rows.indexWhere(
        _.ref ==
          StudioRef.ParticipantSummary(run7, reporting, sigma2, Some(remembered), p.participant)
      )
      assertEquals(plot.rows(row).values(2), PlotValue.Missing)
    }
  }

  test("Explain P17 carries the spec and the group into the query layout") {
    val p17 = StudioRef.ParticipantSummary(run7, reporting, sigma2, Some(remembered), "P17")
    val m   =
      AppModel.run(t3Summary, Vector(select(t3Summary, "compare.participant-plot", p17)))._1
    loaded(m).map { s =>
      val vm      = CompareSummaryVM.of(s, m)
      val explain = vm.explain.getOrElse(fail("no Explain"))
      assertEquals(explain.label, "Explain P17 →")
      assertEquals(explain.keeps, "keeps: by retrieval response › Remembered › P17")
      val (after, _) = AppModel.run(m, explain.intents)
      val trail      = after.location.trail
      assertEquals(
        trail.takeRight(3),
        Vector(Place.Summary(reporting), Place.Group(reporting, remembered), Place.At(p17))
      )
      assertEquals(StudioLayouts.compareLayout(trail), CompareLayout.Query)
    }
  }

  test("an answer for a run no longer shown is dropped; another run is read afresh") {
    read.map { (r, _) =>
      val (s0, _) = CompareSummary.sync(CompareSummary.empty, t3Summary)
      val stale   = CompareSummary
        .update(s0, SummaryIntent.SummaryRead(StoryMoments.run8, SummaryAnswer.Answered(r)))
        ._1
      assertEquals(stale.summary, None)
      assertEquals(CompareSummaryVM.of(stale, t3Summary).status, Some("Reading run 7…"))
      assertEquals(CompareSummaryVM.of(CompareSummary.empty, t3Summary).status.isDefined, true)
    }
  }
