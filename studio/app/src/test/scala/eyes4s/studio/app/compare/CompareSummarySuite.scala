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
import eyes4s.studio.app.text.{SummaryText, SummaryTextId}
import eyes4s.studio.app.text.Format
import eyes4s.studio.app.{AppModel, StoryModels}
import eyes4s.studio.core.backend.{PageRequest, QueryRow, ReportRole, ResultSummary, Response}
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
  val fixtureTable: Vector[Vector[String]] = Vector(
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
    HeadlessSession.open(StoryMoment.T3).flatMap { session =>
      val page = right(PageRequest.first(PageRequest.MaximumSize))
      (for
        r0 <- session.result(run7)
        q0 <- session.queries(run7, page)
        r          = right(r0)
        q          = right(q0).rows
        (s0, asks) = CompareSummary.sync(CompareSummary.empty, m)
        _          = assertEquals(
          asks,
          Vector(SummaryEffect.RequestSummary(run7), SummaryEffect.RequestQueries(run7))
        )
        (s1, reportReads) = CompareSummary.update(
          s0,
          SummaryIntent.SummaryRead(run7, SummaryAnswer.Answered(r))
        )
        reports <- Future.sequence(reportReads.collect {
          case SummaryEffect.RequestReport(_, spec, scale, whole) =>
            session
              .report(run7, spec, scale.value)
              .map(answer =>
                (scale, whole, answer.fold(ReportAnswer.Refused(_), ReportAnswer.Answered(_)))
              )
        })
      yield
        val withReports = reports.foldLeft(s1) { case (state, (scale, whole, answer)) =>
          CompareSummary.update(state, SummaryIntent.ReportRead(run7, scale, whole, answer))._1
        }
        CompareSummary
          .update(withReports, SummaryIntent.QueriesRead(run7, QueriesAnswer.Answered(q)))
          ._1
      ).transformWith(result => session.close.transform(_ => result))
    }

  def cells(source: PlotSource): Vector[Vector[String]] =
    source.rows.map(source.cellsOf).map(_.map(ascii))

  private def report(s: CompareSummary, overall: Boolean = false) =
    s.shown
      .flatMap(scale => s.reports.get((scale, overall)))
      .collect { case ReportAnswer.Answered(view) => view }
      .getOrElse(fail("no served report"))

  test("the participant table takes every M/B/D and group cell from its served reports") {
    loaded(t3Summary).map { s =>
      val vm    = CompareSummaryVM.of(s, t3Summary)
      val table = right(vm.participants.getOrElse(fail("no participant table")))
      val shown = report(s)
      val all   = report(s, overall = true)
      assertEquals(table.columns.size, 11)
      val ids = all.participants.collect {
        case p if p.role == ReportRole.Difference && p.group.isEmpty => p.participant
      }.distinct
      assertEquals(
        table.rows.map(_.ref),
        ids.map(id => all.participant(None, ReportRole.Difference, id).get.ref)
      )
      ids.zipWithIndex.foreach { (id, row) =>
        Vector(ReportRole.Matched, ReportRole.Control, ReportRole.Difference)
          .zip(Vector(6, 7, 8))
          .foreach { (role, column) =>
            assertEquals(
              table.value(row, table.columns(column).id),
              all.participant(None, role, id).flatMap(_.value).map(PlotValue.Number(_))
            )
          }
        shown.cells
          .collect {
            case c if c.role == ReportRole.Difference && c.group.nonEmpty => c.group.get
          }
          .distinct
          .zipWithIndex
          .foreach { (group, i) =>
            val served =
              shown.participant(Some(group), ReportRole.Difference, id).flatMap(_.value)
            assertEquals(
              table.value(row, table.columns(9 + i).id),
              served.map(v =>
                PlotValue.Text(
                  SummaryText(
                    SummaryTextId.GroupCell,
                    Format.signed(v, 2),
                    shown
                      .participant(Some(group), ReportRole.Difference, id)
                      .get
                      .queries
                      .toString
                  )
                )
              )
            )
          }
      }
    }
  }

  test("the plot, notes and selector take their values and refs from served reports") {
    loaded(t3Summary).map { s =>
      val vm     = CompareSummaryVM.of(s, t3Summary)
      val plot   = right(vm.participantPlot.getOrElse(fail("no plot")))
      val cols   = right(ParticipantColumns.standard)
      val di     = plot.indexOf(cols.d).get
      val served = report(s)
      val grand  = served.cells.filter(c =>
        c.role == ReportRole.Difference && c.group.nonEmpty && c.estimate.nonEmpty
      )
      assertEquals(
        plot.rows.filter(_.ref.isInstanceOf[StudioRef.ReportCell]).map(_.ref).toSet,
        grand.map(_.ref).toSet
      )
      grand.foreach(c =>
        assert(
          plot.rows.exists(r =>
            r.ref == c.ref && r.values(di) == PlotValue.Number(c.estimate.get)
          )
        )
      )
      val contrast = served.contrast(ReportRole.Difference).get
      val range    = served.queryRange(ReportRole.Difference).get
      assertEquals(vm.notes.head, SummaryText(SummaryTextId.PairedN, contrast.pairedN.toString))
      assertEquals(
        vm.notes.last,
        SummaryText(SummaryTextId.GroupRange, range.fewest.toString, range.most.toString)
      )
      assert(vm.scales.forall(_.available))
      val s0 = CompareSummary.update(s, SummaryIntent.ChooseScale(right(ScaleIndex.of(0))))._1
      assertEquals(s0.shown, Some(right(ScaleIndex.of(0))))
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

  test("a missing served report value is written as missing, never a zero") {
    loaded(t3Summary).map { s =>
      val served  = report(s)
      val missing = served.participants.find(p =>
        p.role == ReportRole.Difference && p.group.nonEmpty && p.value.isEmpty
      )
      missing.foreach { p =>
        val plot = right(CompareSummaryVM.of(s, t3Summary).participantPlot.get)
        val row  = right(plot.rowOf(p.ref).toRight(p.ref))
        assertEquals(plot.rows(row).values(2), PlotValue.Missing)
      }
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

  test("the scale profile's group rows and participant dots have report refs and values") {
    loaded(t3Summary).map { s =>
      val vm      = CompareSummaryVM.of(s, t3Summary)
      val profile = right(vm.profile.getOrElse(fail("no profile")))
      // FIXTURE.md: grand D by scale (0.5/1/2/4°) by group.
      def byScale(group: String) =
        profile.rows.indices
          .filter(i =>
            profile.rows(i).ref.isInstanceOf[StudioRef.ReportCell] && profile
              .text(i, 0)
              .contains(group)
          )
          .map(i => ascii(profile.text(i, 3).get))
          .toVector
      def served(group: Response) = Vector.tabulate(4) { i =>
        s.reports
          .get((right(ScaleIndex.of(i)), false))
          .collect { case ReportAnswer.Answered(view) => view }
          .flatMap(_.cell(Some(group), ReportRole.Difference))
          .flatMap(_.estimate)
          .map(Format.signed(_, 2))
          .getOrElse(PlotSource.MissingText)
      }
      assertEquals(byScale("Remembered"), served(Response.Remembered))
      assertEquals(byScale("Forgotten"), served(Response.Forgotten))
      // Participant dots use the served participant ref and value.
      val plot                        = right(vm.participantPlot.get)
      val cols                        = right(ParticipantColumns.standard)
      def dot(p: String, g: Response) =
        val ref = report(s).participant(Some(g), ReportRole.Difference, p).get.ref
        plot.rowOf(ref).flatMap(i => plot.text(i, plot.indexOf(cols.d).get)).map(ascii)
      Vector(("P17", remembered), ("P05", Response.Forgotten), ("P15", Response.Forgotten))
        .foreach { (p, g) =>
          assertEquals(
            dot(p, g),
            report(s)
              .participant(Some(g), ReportRole.Difference, p)
              .flatMap(_.value)
              .map(v => ascii(Format.signed(v, 2)))
          )
        }
      // The query table remains the run's served query scores at the selected scale.
      val queries  = right(vm.queries.get)
      val scale    = s.shown.get
      val queryKey = query match
        case StudioRef.QueryContrast(_, _, key) => key
        case other => fail(s"expected query contrast ref, got $other")
      val ret07 =
        queries.rows.indexWhere(_.ref == StudioRef.QueryContrast(run7, scale, queryKey))
      val expected = s.queries.collect { case QueriesAnswer.Answered(rows) =>
        rows
          .find(_.query == queryKey)
          .flatMap(_.status match
            case eyes4s.studio.core.backend.QueryStatus.Contributing(m, b, d) =>
              for
                mm <- m.lift(scale.value)
                bb <- b.lift(scale.value)
                dd <- d.lift(scale.value)
              yield Vector(
                "contributing",
                Format.decimal(mm, 2),
                Format.decimal(bb, 2),
                Format.signed(dd, 2)
              )
            case _ => None)
      }.flatten
      assertEquals(
        queries.cells(ret07).map(_.drop(3).map(ascii)),
        expected
      )
      assert(vm.scales.forall(_.unavailable.isEmpty))
    }
  }

  test("Explain a Forgotten mean keeps its group; a table row, with no group, keeps the spec") {
    val p21 =
      StudioRef.ParticipantSummary(run7, reporting, sigma2, Some(Response.Forgotten), "P21")
    val m21 =
      AppModel.run(t3Summary, Vector(select(t3Summary, "compare.participant-plot", p21)))._1
    // t3's own selection is P17's table row: a participant summary with no group.
    loaded(t3Summary).flatMap { s0 =>
      val row =
        CompareSummaryVM.of(s0, t3Summary).explain.getOrElse(fail("no Explain for the row"))
      assertEquals(row.keeps, "keeps: by retrieval response › P17")
      val (afterRow, _) = AppModel.run(t3Summary, row.intents)
      val rowTrail      = afterRow.location.trail
      assertEquals(rowTrail.head, Place.Summary(reporting))
      assertEquals(rowTrail.last, Place.At(p17Summary))
      assert(!rowTrail.exists(_.isInstanceOf[Place.Group]), rowTrail)
      assertEquals(StudioLayouts.compareLayout(rowTrail), CompareLayout.Query)
      loaded(m21).map { s =>
        val explain = CompareSummaryVM.of(s, m21).explain.getOrElse(fail("no Explain"))
        assertEquals(explain.keeps, "keeps: by retrieval response › Forgotten › P21")
        val trail = AppModel.run(m21, explain.intents)._1.location.trail
        assertEquals(
          trail.takeRight(3),
          Vector(
            Place.Summary(reporting),
            Place.Group(reporting, Response.Forgotten),
            Place.At(p21)
          )
        )
      }
    }
  }

  test("an answer for a run no longer shown is dropped; another run is read afresh") {
    read.map { (r, q) =>
      val (s0, _) = CompareSummary.sync(CompareSummary.empty, t3Summary)
      val stale   = CompareSummary
        .update(
          s0,
          SummaryIntent.SummaryRead(StoryMoments.run8, SummaryAnswer.Answered(r))
        )
        ._1
      assertEquals(stale.summary, None)
      assertEquals(CompareSummaryVM.of(stale, t3Summary).status, Some("Reading run 7…"))
      // Run 7 read, then run 5 shown: run 5 is asked for and nothing of run 7 remains.
      val s7 = CompareSummary
        .update(
          CompareSummary
            .update(s0, SummaryIntent.SummaryRead(run7, SummaryAnswer.Answered(r)))
            ._1,
          SummaryIntent.QueriesRead(run7, QueriesAnswer.Answered(q))
        )
        ._1
      val m5 = AppModel
        .update(
          t3Summary,
          eyes4s.studio.app.Intent.Dispatch(
            eyes4s.studio.core.command.Command.ShowRun(Some(StoryMoments.run5))
          )
        )
        ._1
      assertEquals(CompareSummary.shownRun(m5), Some(StoryMoments.run5))
      val (s5, asks) = CompareSummary.sync(s7, m5)
      assertEquals(
        asks,
        Vector(
          SummaryEffect.RequestSummary(StoryMoments.run5),
          SummaryEffect.RequestQueries(StoryMoments.run5)
        )
      )
      assertEquals((s5.run, s5.summary, s5.queries), (Some(StoryMoments.run5), None, None))
      val vm = CompareSummaryVM.of(s5, m5)
      assertEquals(vm.status, Some("Reading run 5…"))
      assertEquals((vm.participantPlot, vm.participants, vm.queries), (None, None, None))
      // Run 7's late answer is dropped.
      val late =
        CompareSummary.update(s5, SummaryIntent.SummaryRead(run7, SummaryAnswer.Answered(r)))._1
      assertEquals(late.summary, None)
    }
  }
