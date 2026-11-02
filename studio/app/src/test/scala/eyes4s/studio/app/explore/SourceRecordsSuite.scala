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

package eyes4s.studio.app.explore

import eyes4s.studio.app.nav.Place
import eyes4s.studio.app.plot.ViewSelection
import eyes4s.studio.app.{AppModel, Intent, StoryModels}
import eyes4s.plan.MapPlacement
import eyes4s.studio.core.backend.{
  AdmittedFixation,
  AnalysisRevision,
  DatasetRevision,
  TrialFixations
}
import eyes4s.studio.core.document.SourceRole
import eyes4s.studio.core.fixture.{MockStudy, StoryMoment}
import eyes4s.studio.core.headless.HeadlessSession
import eyes4s.studio.core.selection.{
  FixationIndex,
  InputCause,
  InputStamp,
  RecordNumber,
  SelectionInput,
  SelectionMode,
  StudioRef,
  ViewId
}

import scala.concurrent.{ExecutionContext, Future}

/** Explore's source records table headlessly (ticket S6.4): pages are read
  * around the viewport and at most KeptPages are held while scrolling all
  * 11,520 records; stale pages are dropped; the cursor follows a selected
  * record or fixation (whose record comes from the trial's admitted
  * fixations); Enter selects the row's fixation; and 'Show raw record'
  * gives the verbatim line.
  */
class SourceRecordsSuite extends munit.FunSuite:

  private given ExecutionContext = ExecutionContext.global

  private def right[E, A](e: Either[E, A]): A =
    e.fold(x => fail(s"unexpected Left: $x"), identity)

  private val rev4  = AnalysisRevision(4)
  private val total = 11520
  private val view  = right(ViewId.of("explore.source-records"))
  private val enc03 = MockStudy.key("P17", "enc_03")

  // t2's Explore with nothing selected; t2 itself selects fixation 6 of enc_03.
  private lazy val model: AppModel = selecting(StoryModels.t2Explore, Vector.empty)

  /** Synthetic record `n`: record 7,214 is P17 enc_03's sixth fixation. */
  private def row(n: Int): SourceRecordRow =
    val focus = n >= 7209 && n <= 7221
    val trial = if focus then enc03 else MockStudy.key("P01", "enc_01")
    val index = if focus then n - 7208 else 1
    SourceRecordRow(
      right(RecordNumber.of(n)),
      StudioRef.SourceRecord(
        trial,
        Some(right(FixationIndex.of(index))),
        SourceRole.Fixations,
        right(RecordNumber.of(n))
      ),
      Some(right(FixationIndex.of(index))),
      trial,
      Some(index),
      Some(2160.0),
      Some(412.0),
      Some(FramePosition(1148.0, 456.0)),
      Some(FramePosition(700.0, 300.0)),
      Some(FramePosition(5.4, 2.4)),
      Some(206),
      RecordPlace.Inside,
      s"P17,Encoding,enc_03,1,$index,1148.0,456.0,2160,412,206 #$n"
    )

  private def answer(page: Int): SourceRecordsIntent =
    val from = page * SourceRecords.PageSize
    val to   = math.min(total, from + SourceRecords.PageSize)
    SourceRecordsIntent.PageRead(
      rev4,
      page,
      1,
      Right(
        BackendAnswer.Answered(
          SourceRecordPage(from, total, (from until to).map(i => row(i + 1)).toVector)
        )
      )
    )

  private def located(trial: eyes4s.studio.core.backend.TrialKey, record: Int): TrialFixations =
    val fixations = Vector.tabulate(6) { i =>
      right(
        AdmittedFixation.of(
          StudioRef.Fixation(trial, right(FixationIndex.of(i + 1))),
          record + i,
          1.0,
          1.0,
          0.0,
          1.0,
          MapPlacement.InWindow
        )
      )
    }
    right(TrialFixations.of(rev4, DatasetRevision(3), trial, fixations))

  private def start: (SourceRecords, Vector[SourceRecordsEffect]) =
    SourceRecords.sync(
      SourceRecords.initial(ViewSelection.initial(view, model.selection)),
      model
    )

  /** Performs every page request at once with synthetic pages. */
  private def settle(s: SourceRecords, effects: Vector[SourceRecordsEffect]): SourceRecords =
    effects.foldLeft(s) {
      case (acc, SourceRecordsEffect.RequestPage(_, page, _, _, _)) =>
        val (next, more) = SourceRecords.update(acc, answer(page), model)
        settle(next, more)
      case (acc, SourceRecordsEffect.Locate(r, trial, ask)) =>
        val record       = if trial == enc03 then 7209 else 1
        val (next, more) = SourceRecords.update(
          acc,
          SourceRecordsIntent
            .Located(r, trial, ask, Right(BackendAnswer.Answered(located(trial, record)))),
          model
        )
        settle(next, more)
      case (acc, _) => acc
    }

  test("the first page is asked for under the shown revision, and gives the total") {
    val (s, effects) = start
    assertEquals(s.revision, Some(rev4))
    assertEquals(effects, Vector(SourceRecordsEffect.Locate(rev4, enc03, 1)))
    assertEquals(
      SourceRecords.status(s),
      Some("Reading the whole source file for trial P17 · enc_03…")
    )
    val read = settle(s, effects)
    assertEquals(read.total, Some(total))
    assertEquals(
      SourceRecords.status(read),
      Some("Whole source file; following trial P17 · enc_03")
    )
    assertEquals(
      SourceRecords.rowVM(read, 7208).asInstanceOf[SourceRowVM.Shown].cells(0),
      "7,209"
    )
    assertEquals(SourceRecords.rowVM(read, 0), SourceRowVM.Reading)
    assertEquals(SourceRecords.rowVM(read, 500), SourceRowVM.Reading)
  }

  test("scrolling through every record holds at most KeptPages pages") {
    val (s0, e0) = start
    var s        = settle(s0, e0)
    var asked    = 0
    (0 until total by 25).foreach { first =>
      val (next, effects) =
        SourceRecords.update(s, SourceRecordsIntent.Viewport(first, 30), model)
      asked += effects.size
      s = settle(next, effects)
      assert(s.pages.size <= SourceRecords.KeptPages, s.pages.keys)
      (first until math.min(total, first + 30)).foreach(i =>
        assert(SourceRecords.rowVM(s, i).isInstanceOf[SourceRowVM.Shown], i)
      )
    }
    // Each page was read about once on the way down.
    assert(asked <= total / SourceRecords.PageSize + 2, asked)
  }

  test("a stale page and another revision's page are dropped") {
    val (s, _) = start
    val stale  = SourceRecords
      .update(s, answer(0).asInstanceOf[SourceRecordsIntent.PageRead].copy(ask = 0), model)
      ._1
    assertEquals(stale, s)
    val other = SourceRecords
      .update(
        s,
        answer(0)
          .asInstanceOf[SourceRecordsIntent.PageRead]
          .copy(revision = AnalysisRevision(3)),
        model
      )
      ._1
    assertEquals(other, s)
    // A page not asked for is not installed either.
    assertEquals(SourceRecords.update(s, answer(40), model)._1, s)
  }

  private def selecting(m: AppModel, refs: Vector[StudioRef]): AppModel =
    val other = right(ViewId.of("explore.trial-view"))
    val next  = m.selection.delivered.get(other).fold(0L)(_ + 1L)
    val stamp = InputStamp(m.selection.context, other, next, InputCause.Pointer)
    AppModel
      .run(m, Vector(Intent.Select(SelectionInput(stamp, SelectionMode.Replace, refs))))
      ._1

  test("a later selection within the displayed trial moves the whole-source cursor") {
    val (s0, e0) = start
    val s        = settle(s0, e0)
    val record   =
      StudioRef.SourceRecord(enc03, None, SourceRole.Fixations, right(RecordNumber.of(7214)))
    val (onRecord, e1) = SourceRecords.sync(s, selecting(model, Vector(record)))
    assertEquals(onRecord.cursor, Some(7213))
    assertEquals(e1, Vector.empty)
    val fixation         = StudioRef.Fixation(enc03, right(FixationIndex.of(6)))
    val (onFixation, e2) = SourceRecords.sync(s, selecting(model, Vector(fixation)))
    assertEquals(onFixation.cursor, Some(7213))
    assertEquals(e2, Vector.empty)
  }

  test("Enter selects the row's fixation; 'Show raw record' gives the verbatim line") {
    val (s0, e0)   = start
    val s          = settle(s0, e0)
    val (first, _) = SourceRecords.update(s, SourceRecordsIntent.Move(RecordMove.Down), model)
    val (moved, _) =
      SourceRecords.update(first, SourceRecordsIntent.Move(RecordMove.Down), model)
    assertEquals(moved.cursor, Some(7210))
    val (_, effects) = SourceRecords.update(moved, SourceRecordsIntent.Activate, model)
    effects match
      case Vector(SourceRecordsEffect.App(Intent.Select(input))) =>
        assertEquals(
          input.refs,
          Vector(StudioRef.Fixation(enc03, right(FixationIndex.of(3))))
        )
      case other => fail(s"expected one selection, got $other")
    assertEquals(SourceRecords.rawLine(moved), None)
    val raw = SourceRecords.update(moved, SourceRecordsIntent.ShowRaw(true), model)._1
    assertEquals(SourceRecords.rawLine(raw), Some(row(7211).raw))
    // The ends and a page.
    assertEquals(
      SourceRecords.update(moved, SourceRecordsIntent.Move(RecordMove.Last), model)._1.cursor,
      Some(total - 1)
    )
    assertEquals(
      SourceRecords.update(moved, SourceRecordsIntent.Move(RecordMove.Up), model)._1.cursor,
      Some(7209)
    )
    assertEquals(
      SourceRecords.update(moved, SourceRecordsIntent.Move(RecordMove.Up), model)._1.cursor,
      Some(7209)
    )
  }

  test("a row's cells are the backend's values, formatted") {
    assertEquals(
      SourceRecords.cellsOf(row(7214)),
      Vector(
        "7,214",
        "6",
        "2,160",
        "412",
        "1148.0, 456.0",
        "700, 300",
        "+5.4°, +2.4°",
        "206",
        "inside"
      )
    )
    assertEquals(
      SourceRecords.headers,
      Vector(
        "Record",
        "Fixation",
        "Onset ms",
        "Dur ms",
        "Screen px",
        "Image px",
        "Degrees",
        "Samples",
        "Window"
      )
    )
    val unplaced =
      row(1).copy(image = None, degrees = None, samples = None, place = RecordPlace.NotAdmitted)
    assertEquals(SourceRecords.cellsOf(unplaced).drop(5), Vector("—", "—", "—", "not admitted"))
  }

  test("at t2 the displayed trial anchors the cursor at its first admitted source record") {
    val t2           = StoryModels.t2Explore
    val (s, effects) =
      SourceRecords.sync(SourceRecords.initial(ViewSelection.initial(view, t2.selection)), t2)
    // Source-record selection does not redefine the table's displayed-trial context.
    assertEquals(t2.selection.selected, Vector(StoryModels.record))
    assertEquals(s.cursor, None)
    assertEquals(effects, Vector(SourceRecordsEffect.Locate(rev4, enc03, 1)))
  }

  test("a record of the trial inventory does not move the fixations cursor") {
    val (s0, e0) = start
    val s        = settle(s0, e0)
    val trials   =
      StudioRef.SourceRecord(enc03, None, SourceRole.Trials, right(RecordNumber.of(12)))
    val (after, effects) = SourceRecords.sync(s, selecting(model, Vector(trials)))
    assertEquals(after.cursor, s.cursor)
    assertEquals(
      effects.collect { case l @ SourceRecordsEffect.Locate(_, _, _) => l },
      Vector.empty
    )
  }

  test(
    "a click is a pointer input; Enter a keyboard one; Down advances from the trial anchor"
  ) {
    val (s0, e0)  = start
    val s         = settle(s0, e0)
    val (down, _) = SourceRecords.update(s, SourceRecordsIntent.Move(RecordMove.Down), model)
    assertEquals(down.cursor, Some(7209))
    def cause(effects: Vector[SourceRecordsEffect]) = effects.collect {
      case SourceRecordsEffect.App(Intent.Select(input)) => input.stamp.cause
    }
    assertEquals(
      cause(SourceRecords.update(s, SourceRecordsIntent.Click(7208), model)._2),
      Vector(InputCause.Pointer)
    )
    assertEquals(
      cause(SourceRecords.update(down, SourceRecordsIntent.Activate, model)._2),
      Vector(InputCause.Keyboard)
    )
  }

  test("an empty table says so; a misaligned page is refused by name") {
    val (unread, asks) = start
    val read           = settle(unread, asks)
    val (s0, _)        = SourceRecords.update(read, SourceRecordsIntent.Viewport(0, 1), model)
    val empty          = SourceRecords
      .update(
        s0,
        SourceRecordsIntent.PageRead(
          rev4,
          0,
          1,
          Right(BackendAnswer.Answered(SourceRecordPage(0, 0, Vector.empty)))
        ),
        model
      )
      ._1
    assertEquals(
      SourceRecords.status(empty),
      Some("Whole source file; following trial P17 · enc_03. The fixation table has no records")
    )
    val shifted = SourceRecords
      .update(
        s0,
        SourceRecordsIntent.PageRead(
          rev4,
          0,
          1,
          Right(BackendAnswer.Answered(SourceRecordPage(5, total, Vector(row(6)))))
        ),
        model
      )
      ._1
    assertEquals(
      SourceRecords.rowVM(shifted, 0),
      SourceRowVM.Failed("Page 0 of the source records starts at row 5, not row 0")
    )
  }

  test("a trial switch clears old rows and fences a late locate reply") {
    val (onEnc03, first) = start
    assertEquals(first, Vector(SourceRecordsEffect.Locate(rev4, enc03, 1)))
    val loaded = settle(onEnc03, first)
    assert(loaded.pages.nonEmpty)
    assertEquals(loaded.viewport, (7208, 1))
    val ret07 = StoryModels.p17ret07
    val other = AppModel.update(model, Intent.Explain(Place.At(StudioRef.Trial(ret07))))._1
    val (onRet07, second) = SourceRecords.sync(loaded, other)
    assertEquals(second, Vector(SourceRecordsEffect.Locate(rev4, ret07, 2)))
    assertEquals(onRet07.pages, Map.empty)
    assertEquals(onRet07.total, None)
    assertEquals(onRet07.cursor, None)
    val (backOnEnc03, third) = SourceRecords.sync(onRet07, model)
    assertEquals(third, Vector(SourceRecordsEffect.Locate(rev4, enc03, 3)))
    val (afterLate, lateEffects) = SourceRecords.update(
      backOnEnc03,
      SourceRecordsIntent.Located(rev4, enc03, 1, Left("late enc_03 response")),
      model
    )
    assertEquals(afterLate, backOnEnc03)
    assertEquals(lateEffects, Vector.empty)
  }

  test("a displayed trial without admitted fixations is named while the whole source loads") {
    val (waiting, effects) = start
    assertEquals(effects, Vector(SourceRecordsEffect.Locate(rev4, enc03, 1)))
    val empty = right(TrialFixations.of(rev4, DatasetRevision(3), enc03, Vector.empty))
    val (reading, pageEffects) = SourceRecords.update(
      waiting,
      SourceRecordsIntent.Located(rev4, enc03, 1, Right(BackendAnswer.Answered(empty))),
      model
    )
    assertEquals(
      SourceRecords.status(reading),
      Some("Trial P17 · enc_03 has no admitted fixations; reading the whole source file…")
    )
    assertEquals(
      pageEffects,
      Vector(SourceRecordsEffect.RequestPage(rev4, 0, 0, SourceRecords.PageSize, 1))
    )
  }

  test("a repeated sync leaves an in-flight locate waiting until its anchor arrives") {
    val (waiting, effects) = start
    assertEquals(effects, Vector(SourceRecordsEffect.Locate(rev4, enc03, 1)))
    val (stillWaiting, more) = SourceRecords.sync(waiting, model)
    assertEquals(stillWaiting, waiting)
    assertEquals(more, Vector.empty)
    val (anchored, pageEffects) = SourceRecords.update(
      stillWaiting,
      SourceRecordsIntent.Located(
        rev4,
        enc03,
        1,
        Right(BackendAnswer.Answered(located(enc03, 7209)))
      ),
      model
    )
    assertEquals(anchored.cursor, Some(7208))
    assertEquals(anchored.viewport, (7208, 1))
    assertEquals(
      pageEffects,
      Vector(SourceRecordsEffect.RequestPage(rev4, 7208 / SourceRecords.PageSize, 7168, 128, 1))
    )
  }

  test(
    "the window's adapter: protocol 1.7's records in the table's terms, every place mapped"
  ) {
    HeadlessSession.open(StoryMoment.T2).flatMap { h =>
      h.sourceRecords(rev4, 7001, 500)
        .map { a =>
          val wirePage = a.fold(e => fail(e.message), identity)
          val page     = SourceRecords.served(wirePage)
          assertEquals((page.from, page.total, page.rows.size), (7000, 11520, 500))
          assertEquals(page.scale.map(_.pixelsPerDegree), Some(wirePage.pixelsPerDegree))
          val r = page.rows(213)
          assertEquals(
            SourceRecords.cellsOf(r),
            Vector(
              "7,214",
              "6",
              "2,160",
              "412",
              "1148.0, 456.0",
              "700, 300",
              "+5.4°, +2.4°",
              "206",
              "inside"
            )
          )
          assertEquals(r.raw, "P17,Encoding,enc_03,1,6,1148.0,456.0,2160,412,206")
        }
        .transformWith(x => h.close.transform(_ => x))
    }
  }

  test("every record's served placement becomes its place, over the whole file") {
    import eyes4s.plan.MapPlacement
    def expected(p: Option[MapPlacement]): RecordPlace = p match
      case None                                => RecordPlace.NotAdmitted
      case Some(MapPlacement.InWindow)         => RecordPlace.Inside
      case Some(MapPlacement.DroppedInitial)   => RecordPlace.DroppedInitial
      case Some(MapPlacement.OutsideWindow(_)) => RecordPlace.Outside
      case Some(MapPlacement.OutsideScreen)    => RecordPlace.OffScreen
      case Some(MapPlacement.TrialFailed(t))   =>
        RecordPlace.InsideTrialFails(t.outsideWindow, t.total)
    val tally = eyes4s.plan.WindowTally
      .of(
        1,
        2,
        9,
        eyes4s.kernel.Span.micros(100L),
        eyes4s.kernel.Span.micros(300L),
        eyes4s.kernel.Span.micros(2000L)
      )
      .fold(e => fail(e.message), identity)
    HeadlessSession.open(StoryMoment.T2).flatMap { h =>
      val pages = (1 to 11520 by 500).toVector.map(from => h.sourceRecords(rev4, from, 500))
      Future
        .sequence(pages)
        .map { all =>
          val rows = all.flatMap { a =>
            val w = a.fold(e => fail(e.message), identity)
            w.rows.zip(SourceRecords.served(w).rows)
          }
          assertEquals(rows.size, 11520)
          rows.foreach((w, a) => assertEquals(a.place, expected(w.placement), w.line))
          // The fixture places none dropped or off screen: record 7,214
          // served under each placement covers those.
          val w   = all(14).fold(e => fail(e.message), identity)
          val one = w.rows.find(_.record == 7214).get
          Vector(
            MapPlacement.DroppedInitial,
            MapPlacement.OutsideScreen,
            MapPlacement.InWindow,
            MapPlacement.OutsideWindow(eyes4s.plan.OffWindowPolicy.Exclude),
            MapPlacement.OutsideWindow(eyes4s.plan.OffWindowPolicy.FailTrial),
            MapPlacement.TrialFailed(tally)
          ).foreach { p =>
            val row = right(
              eyes4s.studio.core.backend.SourceRecordRow.of(
                one.ref,
                one.ordinal,
                one.onsetMs,
                one.durationMs,
                one.samples,
                one.screen,
                one.image,
                one.degrees,
                Some(p),
                one.line
              )
            )
            val page = right(
              eyes4s.studio.core.backend.SourceRecordPage
                .of(
                  w.revision,
                  w.dataset,
                  w.source,
                  35.0,
                  w.scaleSource,
                  11520,
                  7214,
                  1,
                  Vector(row)
                )
            )
            assertEquals(
              SourceRecords.served(page).rows.map(_.place),
              Vector(expected(Some(p)))
            )
          }
        }
        .transformWith(x => h.close.transform(_ => x))
    }
  }
