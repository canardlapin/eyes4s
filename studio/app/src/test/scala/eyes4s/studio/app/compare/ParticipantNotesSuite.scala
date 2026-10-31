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

import eyes4s.studio.app.plot.{
  GroupGrandMean,
  NoteRole,
  NoteTone,
  ParticipantCell,
  ParticipantMeans,
  ParticipantNotes,
  PlotReports
}
import eyes4s.studio.app.{AppModel, StoryModels}
import eyes4s.studio.core.backend.{PageRequest, ParticipantCounts, ReportRole, Response}
import eyes4s.studio.core.fixture.{StoryMoment, StoryMoments}
import eyes4s.studio.core.headless.HeadlessSession
import eyes4s.studio.core.selection.{ReportGroup, ScaleIndex, StudioRef}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.chaining.*

/** The participant plot's annotation column as the app composes it (review
  * ticket 26, slice b): at t3, with P17 selected, exactly the Results
  * board's two blocks, every number the participant table's own; the
  * overflow rule; and the groups' short names.
  */
class ParticipantNotesSuite extends munit.FunSuite:
  private given ExecutionContext = ExecutionContext.global

  private val run7 = StoryMoments.run7

  private def right[E, A](e: Either[E, A]): A =
    e.fold(x => fail(s"unexpected Left: $x"), identity)

  /** The summary layout of `m` at σ 2° once run 7's summary, reports and
    * queries are read from the headless t3 backend.
    */
  private def loaded(m: AppModel): Future[CompareSummary] =
    HeadlessSession.open(StoryMoment.T3).flatMap { session =>
      val page = right(PageRequest.first(PageRequest.MaximumSize))
      (for
        r0 <- session.result(run7)
        q0 <- session.queries(run7, page)
        (s0, _)           = CompareSummary.sync(CompareSummary.empty, m)
        (s1, reportReads) = CompareSummary.update(
          s0,
          SummaryIntent.SummaryRead(run7, SummaryAnswer.Answered(right(r0)))
        )
        reports <- Future.sequence(reportReads.collect {
          case SummaryEffect.RequestReport(_, spec, scale, whole) =>
            session
              .report(run7, spec, scale.value)
              .map(a =>
                (spec, scale, whole, a.fold(ReportAnswer.Refused(_), ReportAnswer.Answered(_)))
              )
        })
      yield
        val withReports = reports.foldLeft(s1) { case (state, (spec, scale, whole, answer)) =>
          CompareSummary
            .update(state, SummaryIntent.ReportRead(run7, spec, scale, whole, answer))
            ._1
        }
        CompareSummary
          .update(
            withReports,
            SummaryIntent.QueriesRead(run7, QueriesAnswer.Answered(right(q0).rows))
          )
          ._1
          // The board's σ 2°, once every σ is served.
          .pipe(CompareSummary.update(_, SummaryIntent.ChooseScale(StoryModels.sigma2))._1)
      ).transformWith(result => session.close.transform(_ => result))
    }

  private def lines(notes: ParticipantNotes): Vector[Vector[String]] =
    notes.blocks.map(_.lines.map(_.text))

  // The board's participant D plot notes (Results.dc.html, the annotation
  // column): the served group means of SCORES.json's query scores, rounded
  // half up as the participant table writes them (CompareSummaryFxSuite).
  private val boardP17 = Vector(
    "P17 · selected",
    "19 of 20 contributing",
    "Rem +0.40 (n 17)",
    "Forg +0.22 (n 2)"
  )
  private val boardP05 = Vector(
    "P05 · 17 of 20 contributing",
    "Rem +0.39 (n 13)",
    "Forg +0.22 (n 4)",
    "3 failed queries:",
    "2 Remembered, 1 Forgotten",
    "no value, not zero"
  )

  test("t3, P17 selected: exactly the board's two blocks, joined to their Forgotten marks") {
    loaded(StoryModels.t3Summary).map { s =>
      val vm    = CompareSummaryVM.of(s, StoryModels.t3Summary)
      val notes = vm.plotNotes
      assertEquals(s.shown, Some(StoryModels.sigma2))
      assertEquals(lines(notes), Vector(boardP17, boardP05))
      assertEquals(notes.omitted, 0)
      assertEquals(
        notes.blocks.map(b => b.lines.map(l => (l.role, l.tone))),
        Vector(
          Vector(
            (NoteRole.Heading, NoteTone.Primary),
            (NoteRole.Fact, NoteTone.Secondary),
            (NoteRole.Fact, NoteTone.Secondary),
            (NoteRole.Fact, NoteTone.Secondary)
          ),
          Vector(
            (NoteRole.Heading, NoteTone.Failure),
            (NoteRole.Fact, NoteTone.Secondary),
            (NoteRole.Fact, NoteTone.Secondary),
            (NoteRole.Fact, NoteTone.Primary),
            (NoteRole.Fact, NoteTone.Primary),
            (NoteRole.Fact, NoteTone.Failure)
          )
        )
      )
      // Each block is joined to its participant's Forgotten mean, the
      // column nearest the notes, as the board's leaders are.
      notes.blocks.foreach { b =>
        b.anchor match
          case StudioRef.ReportParticipant(`run7`, _, _, ReportGroup.Level(g), _, p) =>
            assertEquals((g, p), (Response.Forgotten, b.participant))
          case other => fail(s"anchor $other")
      }
      // The groups are spoken by their whole names.
      assertEquals(
        notes.blocks.head.lines.map(_.spoken).drop(2),
        Vector("Remembered +0.40, n 17", "Forgotten +0.22, n 2")
      )
    }
  }

  test("every number is the participant table's own: counts, group means and their n") {
    loaded(StoryModels.t3Summary).map { s =>
      val vm    = CompareSummaryVM.of(s, StoryModels.t3Summary)
      val table = vm.participants.getOrElse(fail("no table")).getOrElse(fail("no table"))
      def row(p: String) =
        table.rows.map(table.cellsOf).find(_.headOption.contains(p)).getOrElse(fail(p))
      vm.plotNotes.blocks.foreach { b =>
        val cells = row(b.participant)
        // participant, queries (contributing), Rem D, n Rem, Forg D, n Forg, Δ…
        val con  = cells(1)
        val said = b.lines.map(_.text)
        assert(said.exists(_.contains(s"$con of ")), said)
        // "+0.40" and "17" in the table are "Rem +0.40 (n 17)" in the notes.
        def note(short: String, d: String, n: String) = s"$short $d (n $n)"
        assert(said.contains(note("Rem", cells(2), cells(3))), s"$said vs ${cells.take(4)}")
        assert(said.contains(note("Forg", cells(4), cells(5))), s"$said vs ${cells.take(6)}")
        val failed = said
          .find(_.endsWith("failed queries:"))
          .map(_.takeWhile(_ != ' '))
          .getOrElse("0")
        if failed != "0" then assert(said.contains(s"$failed failed queries:"), said)
      }
    }
  }

  test("with nothing selected only the participants with failed queries are noted") {
    val t3   = StoryModels.t3Summary
    val none = AppModel.update(t3, StoryModels.select(t3, "compare.participant-plot"))._1
    assertEquals(none.selection.selected, Vector.empty)
    loaded(none).map { s =>
      assertEquals(lines(CompareSummaryVM.of(s, none).plotNotes), Vector(boardP05))
    }
  }

  test(
    "t3 in a short frame: the view-model's slots keep the selected block and say what is left"
  ) {
    loaded(StoryModels.t3Summary).map { s =>
      def at(slots: Int) =
        CompareSummaryVM.of(s, StoryModels.t3Summary, noteSlots = slots).plotNotes
      // Both blocks and the gap between them: 4 + 1 + 6 = 11 slots.
      assertEquals(lines(at(11)), Vector(boardP17, boardP05))
      assertEquals(at(11).omitted, 0)
      // One slot fewer: P17 is first though P05 has the failures, and its
      // block ends with the line for the block left out.
      Vector(10, 5).foreach { slots =>
        assertEquals(lines(at(slots)), Vector(boardP17 :+ "+1 more"), s"$slots slots")
        assertEquals(at(slots).omitted, 1)
      }
      assertEquals(at(10).blocks.head.lines.last.spoken, "1 more participant not annotated")
      // Not even P17 with its "+1 more" fits: no notes at all.
      assertEquals(at(4), ParticipantNotes.empty)
      assertEquals(at(0), ParticipantNotes.empty)
    }
  }

  test("failures by group are written only when the query rows add up to the served count") {
    loaded(StoryModels.t3Summary).map { s =>
      val summary =
        s.summary.collect { case SummaryAnswer.Answered(r) => r }.getOrElse(fail("summary"))
      val rows =
        s.queries.collect { case QueriesAnswer.Answered(q) => q }.getOrElse(fail("rows"))
      val report = s.reports
        .get((StoryModels.sigma2, false))
        .collect { case ReportAnswer.Answered(v) => v }
        .getOrElse(fail("report"))
      val ms = right(ParticipantMeans.of(report, "2°"))
      def p05(queries: Option[Vector[eyes4s.studio.core.backend.QueryRow]]) =
        ParticipantNotes
          .of(ms, summary.participants, queries, None, 100)
          .blocks
          .head
          .lines
          .map(_.text)
      assert(p05(Some(rows)).contains("2 Remembered, 1 Forgotten"), p05(Some(rows)))
      // One failed row fewer than the three served: no by-group line, the total stays.
      val dropped =
        rows.diff(rows.find(q => q.query.participant == "P05" && q.status.isFailed).toVector)
      val whole = p05(Some(rows)).filterNot(_ == "2 Remembered, 1 Forgotten")
      assertEquals(p05(Some(dropped)), whole)
      assert(whole.contains("3 failed queries:"), whole)
      // Rows not yet read: likewise.
      assertEquals(p05(None), whole)
    }
  }

  // --- Synthetic means --------------------------------------------------------------

  private val at     = right(ScaleIndex.of(2))
  private val groups = Vector(Response.Remembered, Response.Forgotten)

  private def ref(g: Response, p: String): StudioRef =
    StudioRef.ReportParticipant(
      run7,
      PlotReports.reporting,
      at,
      ReportGroup.Level(g),
      ReportRole.Difference,
      p
    )

  /** Participants with their Forgotten mean and failed queries. */
  private def means(people: (String, Option[Double])*): ParticipantMeans =
    ParticipantMeans(
      run7,
      PlotReports.reporting,
      at,
      "2°",
      groups.map(g =>
        GroupGrandMean(
          StudioRef.ReportCell(
            run7,
            PlotReports.reporting,
            at,
            ReportGroup.Level(g),
            ReportRole.Difference
          ),
          g,
          0.1,
          people.size
        )
      ),
      groups.flatMap(g =>
        people.map((p, d) => ParticipantCell(ref(g, p), g, p, d, d.map(_ => 4)))
      )
    )

  private def counts(failed: (String, Int)*): Vector[ParticipantCounts] =
    failed.toVector.map((p, f) => ParticipantCounts(p, 20, 20 - f, f, 0, 0))

  private def slotsOf(n: ParticipantNotes): Int =
    n.blocks.map(_.lines.size).sum + ParticipantNotes.BlockGapSlots * (n.blocks.size - 1)

  test("overflow keeps the selected first, then failures by count, and says how many more") {
    val ms = means(
      "P01" -> Some(0.1),
      "P02" -> Some(0.3),
      "P03" -> Some(0.2),
      "P04" -> Some(0.4),
      "P05" -> Some(0.5)
    )
    val cs = counts("P01" -> 0, "P02" -> 1, "P03" -> 4, "P04" -> 2, "P05" -> 1)
    // Unbounded: the selected P01, then P03 (4), P04 (2), P02 and P05 (1 each, plot order).
    val all = ParticipantNotes.of(ms, cs, None, Some("P01"), 1000)
    assertEquals(all.blocks.map(_.participant), Vector("P01", "P03", "P04", "P02", "P05"))
    assertEquals(all.omitted, 0)
    assert(!all.blocks.flatMap(_.lines).exists(_.text.startsWith("+")))
    // Selected: heading, contributing, two means = 4 lines; a failure block:
    // heading, two means, "n failed", "no value" = 5 lines (no query rows,
    // so no by-group line). P01 + P03 + P04 = 4 + 1 + 5 + 1 + 5 = 16 slots,
    // and one more line for "+2 more" makes 17.
    val bounded = ParticipantNotes.of(ms, cs, None, Some("P01"), 17)
    assertEquals(bounded.blocks.map(_.participant), Vector("P01", "P03", "P04"))
    assertEquals(bounded.omitted, 2)
    assertEquals(slotsOf(bounded), 17)
    // The "+2 more" line ends the block drawn lowest: P01, D +0.1.
    val more = bounded.blocks
      .flatMap(b => b.lines.map(b.participant -> _.text))
      .filter(_._2.startsWith("+"))
    assertEquals(more, Vector("P01" -> "+2 more"))
    assertEquals(
      bounded.blocks.head.lines.last.spoken,
      "2 more participants not annotated"
    )
    // One slot fewer drops P04 as well.
    val tighter = ParticipantNotes.of(ms, cs, None, Some("P01"), 16)
    assertEquals(tighter.blocks.map(_.participant), Vector("P01", "P03"))
    assertEquals(tighter.omitted, 3)
    assert(slotsOf(tighter) <= 16)
    // Not even the selected block with its "+n more" fits: no notes.
    assertEquals(ParticipantNotes.of(ms, cs, None, Some("P01"), 4), ParticipantNotes.empty)
  }

  test("at every capacity the notes take at most the slots they are given") {
    val ms = means(
      "P01" -> Some(0.1),
      "P02" -> Some(0.3),
      "P03" -> Some(0.2),
      "P04" -> Some(0.4),
      "P05" -> Some(0.5)
    )
    val cs = counts("P01" -> 0, "P02" -> 1, "P03" -> 4, "P04" -> 2, "P05" -> 1)
    (0 to 30).foreach { slots =>
      val n = ParticipantNotes.of(ms, cs, None, Some("P01"), slots)
      assert(n.isEmpty || slotsOf(n) <= slots, s"$slots slots: ${lines(n)}")
      // The selected participant leads whatever fits, and a block left out is counted.
      assert(n.isEmpty || n.blocks.head.participant == "P01", s"$slots slots: ${lines(n)}")
      assertEquals(n.blocks.size + n.omitted, if n.isEmpty then 0 else 5, s"$slots slots")
    }
    // The fewest slots any notes take: P01's 4 lines and "+4 more".
    assertEquals(ParticipantNotes.of(ms, cs, None, Some("P01"), 4), ParticipantNotes.empty)
    val five = ParticipantNotes.of(ms, cs, None, Some("P01"), 5)
    assertEquals(five.blocks.map(_.participant), Vector("P01"))
    assertEquals(five.blocks.head.lines.map(_.text).last, "+4 more")
  }

  test("a missing mean is drawn lowest, so it takes the '+n more' line") {
    val ms = means("P01" -> Some(0.1), "P02" -> None, "P03" -> Some(0.2))
    val cs = counts("P01" -> 2, "P02" -> 1, "P03" -> 1)
    val n  = ParticipantNotes.of(ms, cs, None, None, 12)
    assertEquals(n.blocks.map(_.participant), Vector("P01", "P02"))
    assertEquals(n.blocks(1).lines.map(_.text).last, "+1 more")
    assert(n.blocks(1).lines.map(_.text).contains("Rem no value"), n.blocks(1).lines)
  }

  test("a selected participant with failures keeps its heading and adds the failure lines") {
    val ms = means("P01" -> Some(0.1))
    val n  = ParticipantNotes.of(ms, counts("P01" -> 1), None, Some("P01"), 100)
    assertEquals(
      n.blocks.map(_.lines.map(l => (l.text, l.tone))),
      Vector(
        Vector(
          ("P01 · selected", NoteTone.Primary),
          ("19 of 20 contributing", NoteTone.Failure),
          ("Rem +0.10 (n 4)", NoteTone.Secondary),
          ("Forg +0.10 (n 4)", NoteTone.Secondary),
          ("1 failed query:", NoteTone.Primary),
          ("no value, not zero", NoteTone.Failure)
        )
      )
    )
    // A selection of a participant the plot does not draw adds nothing.
    assertEquals(
      ParticipantNotes.of(ms, counts("P01" -> 0), None, Some("P99"), 100).blocks,
      Vector.empty
    )
  }

  test("short names are one closed syllable, unless two groups would share one") {
    assertEquals(
      ParticipantNotes.shortNames(groups),
      Map(Response.Remembered -> "Rem", Response.Forgotten -> "Forg")
    )
    val clash = Vector(Response("Forgotten"), Response("Forgiven"))
    assertEquals(
      ParticipantNotes.shortNames(clash),
      Map(Response("Forgotten") -> "Forgotten", Response("Forgiven") -> "Forgiven")
    )
  }

  test("the selection names its participant on this run only") {
    val p17 = StudioRef.ParticipantSummary(run7, PlotReports.reporting, at, None, "P17")
    assertEquals(ParticipantNotes.selectedParticipant(Vector(p17), run7), Some("P17"))
    assertEquals(ParticipantNotes.selectedParticipant(Vector(p17), StoryMoments.run8), None)
    assertEquals(
      ParticipantNotes.selectedParticipant(Vector(StudioRef.Participant("P05")), run7),
      Some("P05")
    )
  }
