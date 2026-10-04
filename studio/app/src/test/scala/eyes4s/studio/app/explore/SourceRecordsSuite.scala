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

import eyes4s.studio.app.geometry.Loading
import eyes4s.studio.app.plot.ViewSelection
import eyes4s.studio.app.{AppModel, Intent, StoryModels}
import eyes4s.studio.core.backend.AnalysisRevision
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

import scala.concurrent.ExecutionContext

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
      index,
      2160.0,
      412.0,
      "1148.0",
      "456.0",
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
      case (acc, _) => acc
    }

  test("the first page is asked for under the shown revision, and gives the total") {
    val (s, effects) = start
    assertEquals(s.revision, Some(rev4))
    assertEquals(
      effects,
      Vector(SourceRecordsEffect.RequestPage(rev4, 0, 0, SourceRecords.PageSize, 1))
    )
    assertEquals(SourceRecords.status(s), Some("Reading the source records…"))
    val read = settle(s, effects)
    assertEquals(read.total, Some(total))
    assertEquals(SourceRecords.status(read), None)
    assertEquals(SourceRecords.rowVM(read, 0).asInstanceOf[SourceRowVM.Shown].cells(0), "1")
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

  test("the cursor follows a selected record, and a fixation through its trial's records") {
    val (s0, e0) = start
    val s        = settle(s0, e0)
    val record   =
      StudioRef.SourceRecord(enc03, None, SourceRole.Fixations, right(RecordNumber.of(7214)))
    val (onRecord, e1) = SourceRecords.sync(s, selecting(model, Vector(record)))
    assertEquals(onRecord.cursor, Some(7213))
    assert(
      e1.exists {
        case SourceRecordsEffect.RequestPage(_, p, _, _, _) => p == 7213 / 128; case _ => false
      },
      e1
    )
    // A fixation: its record comes from the trial's admitted fixations.
    val fixation     = StudioRef.Fixation(enc03, right(FixationIndex.of(6)))
    val chosen       = selecting(model, Vector(fixation))
    val (asking, e2) = SourceRecords.sync(s, chosen)
    assertEquals(
      e2.collect { case l @ SourceRecordsEffect.Locate(_, _) => l },
      Vector(SourceRecordsEffect.Locate(rev4, enc03))
    )
    assertEquals(asking.cursor, None)
    HeadlessSession
      .open(StoryMoment.T2)
      .flatMap { h =>
        h.trialFixations(rev4, enc03).transformWith(x => h.close.transform(_ => x))
      }
      .map { fixations =>
        val located = SourceRecords
          .update(
            asking,
            SourceRecordsIntent
              .Located(rev4, enc03, Right(BackendAnswer.Answered(right(fixations)))),
            chosen
          )
          ._1
        // FIXTURE.md: fixation 6 of enc_03 is record 7,214.
        assertEquals(located.cursor, Some(7213))
        assertEquals(
          located.located.get(enc03).map(_.isInstanceOf[Loading.Ready[?]]),
          Some(true)
        )
      }
  }

  test("Enter selects the row's fixation; 'Show raw record' gives the verbatim line") {
    val (s0, e0)   = start
    val s          = settle(s0, e0)
    val (moved, _) = SourceRecords.update(s, SourceRecordsIntent.Move(RecordMove.Down), model)
    assertEquals(moved.cursor, Some(1))
    val (_, effects) = SourceRecords.update(moved, SourceRecordsIntent.Activate, model)
    effects match
      case Vector(SourceRecordsEffect.App(Intent.Select(input))) =>
        assertEquals(
          input.refs,
          Vector(StudioRef.Fixation(MockStudy.key("P01", "enc_01"), right(FixationIndex.of(1))))
        )
      case other => fail(s"expected one selection, got $other")
    assertEquals(SourceRecords.rawLine(moved), None)
    val raw = SourceRecords.update(moved, SourceRecordsIntent.ShowRaw(true), model)._1
    assertEquals(SourceRecords.rawLine(raw), Some(row(2).raw))
    // The ends and a page.
    assertEquals(
      SourceRecords.update(moved, SourceRecordsIntent.Move(RecordMove.Last), model)._1.cursor,
      Some(total - 1)
    )
    assertEquals(
      SourceRecords.update(moved, SourceRecordsIntent.Move(RecordMove.Up), model)._1.cursor,
      Some(0)
    )
    assertEquals(
      SourceRecords.update(moved, SourceRecordsIntent.Move(RecordMove.Up), model)._1.cursor,
      Some(0)
    )
  }

  test("a row's cells are the backend's values, formatted") {
    assertEquals(
      SourceRecords.cellsOf(row(7214)),
      Vector(
        "7,214",
        "enc_03",
        "6",
        "2,160",
        "412",
        "1148.0,456.0",
        "700,300",
        "+5.4°,+2.4°",
        "206",
        "inside"
      )
    )
    assertEquals(
      SourceRecords.headers,
      Vector(
        "Record",
        "Trial",
        "Ordinal",
        "Onset ms",
        "Dur ms",
        "Screen x,y",
        "Image x,y",
        "Degrees x,y",
        "Samples",
        "Window"
      )
    )
    val unplaced =
      row(1).copy(image = None, degrees = None, samples = None, place = RecordPlace.NotAdmitted)
    assertEquals(SourceRecords.cellsOf(unplaced).drop(6), Vector("—", "—", "—", "not admitted"))
  }

  test("at t2 the story's selected fixation puts the cursor on record 7,214") {
    val t2           = StoryModels.t2Explore
    val (s, effects) =
      SourceRecords.sync(SourceRecords.initial(ViewSelection.initial(view, t2.selection)), t2)
    // The story selects record 7,214 (StoryModels.record), whose row is 7,213.
    assertEquals(t2.selection.selected, Vector(StoryModels.record))
    assertEquals(s.cursor, Some(7213))
    assert(
      effects.contains(
        SourceRecordsEffect.RequestPage(rev4, 7213 / 128, 7213 / 128 * 128, 128, 1)
      ),
      effects
    )
  }
