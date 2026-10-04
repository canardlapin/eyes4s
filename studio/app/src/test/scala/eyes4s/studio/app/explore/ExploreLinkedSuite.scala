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

import eyes4s.studio.app.nav.{Location, Place}
import eyes4s.studio.app.{AppModel, Intent, StoryModels}
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.{Perspective, SourceRole}
import eyes4s.studio.core.fixture.StoryMoment
import eyes4s.studio.core.headless.HeadlessSession
import eyes4s.studio.core.selection.{FixationIndex, RecordNumber, StudioRef}

import scala.concurrent.{ExecutionContext, Future}

/** Linked selection in Explore, headless (ticket S6.6; Explore.dc.html,
  * interactive): Prev and Next step through P17 enc_03's 13 fixations, and
  * the trail follows the selected fixation to its fixation and record
  * crumbs (fixation 6 is record 7,214; records count up with the scanpath).
  */
class ExploreLinkedSuite extends munit.FunSuite:

  private given ExecutionContext = ExecutionContext.global

  private def ok[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)

  private val rev4  = AnalysisRevision(4)
  private val enc03 = StoryModels.p17enc03

  private def fixation(i: Int): StudioRef.Fixation =
    StudioRef.Fixation(enc03, ok(FixationIndex.of(i)))

  private def record(i: Int): StudioRef.SourceRecord =
    StudioRef.SourceRecord(
      enc03,
      Some(ok(FixationIndex.of(i))),
      SourceRole.Fixations,
      ok(RecordNumber.of(7208 + i))
    )

  /** t2's trial view with P17 enc_03's fixations read. */
  private def view(m: AppModel): Future[ExploreTrialView] =
    for
      session <- HeadlessSession.open(StoryMoment.T2)
      fs      <- session.trialFixations(rev4, enc03)
      _       <- session.close
    yield
      val (synced, _) = ExploreTrialView.sync(ExploreTrialView.empty, m)
      val answer      = Right(
        fs.fold(e => BackendAnswer.Refused(e.message), BackendAnswer.Answered(_))
      )
      ExploreTrialView.update(synced, TrialViewIntent.FixationsRead(rev4, enc03, 1, answer))._1

  private def selecting(m: AppModel, view: String, refs: StudioRef*): AppModel =
    AppModel.update(m, StoryModels.select(m, view, refs*))._1

  test("t2 selects record 7,214, fixation 6; Prev and Next step through the scanpath") {
    val m = StoryModels.t2Explore
    view(m).map { v =>
      assertEquals(ExploreLinked.selected(v, m), Some(fixation(6)))
      assertEquals(ExploreLinked.step(v, m, 1), Some(fixation(7)))
      assertEquals(ExploreLinked.step(v, m, -1), Some(fixation(5)))
      // At the ends there is no step.
      val last = selecting(m, "explore.timeline", fixation(13))
      assertEquals(
        (ExploreLinked.step(v, last, 1), ExploreLinked.canStep(v, last, -1)),
        (None, true)
      )
      val first = selecting(m, "explore.timeline", fixation(1))
      assertEquals(ExploreLinked.canStep(v, first, -1), false)
      // With nothing selected, Next starts at the first and Prev at the last.
      val none = selecting(m, "explore.timeline")
      assertEquals(
        (ExploreLinked.step(v, none, 1), ExploreLinked.step(v, none, -1)),
        (Some(fixation(1)), Some(fixation(13)))
      )
      // Another trial's fixation is not this trial's.
      val other = selecting(
        m,
        "explore.timeline",
        StudioRef.Fixation(enc03.copy(trial = "enc_04"), ok(FixationIndex.of(2)))
      )
      assertEquals(ExploreLinked.selected(v, other), None)
    }
  }

  test("the trail follows the selected fixation to its fixation and record crumbs") {
    val m = StoryModels.t2Explore
    view(m).map { v =>
      // t2's trail already ends at fixation 6's record.
      assertEquals(
        m.location.trail.takeRight(2),
        Vector(Place.At(fixation(6)), Place.At(record(6)))
      )
      assertEquals(ExploreLinked.follow(v, m), None)
      // A timeline bar selects fixation 8: the trail moves to it and record 7,216.
      val m8     = selecting(m, "explore.timeline", fixation(8))
      val follow = ExploreLinked.follow(v, m8)
      assertEquals(follow, Some(Intent.Explain(Place.At(record(8)))))
      val moved = AppModel.update(m8, follow.get)._1
      assertEquals(
        moved.location.trail.takeRight(2),
        Vector(Place.At(fixation(8)), Place.At(record(8)))
      )
      // The query trail before it is kept, and the selection is unchanged.
      assertEquals(moved.location.trail.dropRight(2), m.location.trail.dropRight(2))
      assertEquals(moved.selection.selected, Vector(fixation(8)))
      assertEquals(ExploreLinked.follow(v, moved), None)
      // A record selects its fixation too.
      val r3m = selecting(m, "explore.source-records", record(3))
      assertEquals(ExploreLinked.follow(v, r3m), Some(Intent.Explain(Place.At(record(3)))))
      // Outside Explore the trail is not moved.
      val compare = AppModel
        .update(m8, Intent.Navigate(Location(Perspective.Compare, StoryModels.queryTrail)))
        ._1
      assertEquals(ExploreLinked.follow(v, compare), None)
    }
  }

  test("the trail follows a change of the selection once, not the selection itself") {
    val m = StoryModels.t2Explore
    view(m).map { v =>
      val start = TrailFollow.initial(m)
      // Nothing changed: nothing to ask, even with fixation 6 selected.
      assertEquals(TrailFollow.step(start, v, m), (start, None))
      // A crumb above the fixation (the pair) is not undone by the selection.
      val up = AppModel.update(m, Intent.Explain(m.location.trail(4)))._1
      assertEquals(TrailFollow.step(start, v, up)._2, None)
      // A new selection is asked for once.
      val m8           = selecting(m, "explore.timeline", fixation(8))
      val (asked, ask) = TrailFollow.step(start, v, m8)
      assertEquals(ask, Some(Intent.Explain(Place.At(record(8)))))
      assertEquals(TrailFollow.step(asked, v, m8), (asked, None))
      // Before the fixations are read it waits, then asks.
      val (unread, _)     = ExploreTrialView.sync(ExploreTrialView.empty, m8)
      val (waiting, none) = TrailFollow.step(start, unread, m8)
      assertEquals((none, waiting.pending), (None, true))
      assertEquals(
        TrailFollow.step(waiting, v, m8)._2,
        Some(Intent.Explain(Place.At(record(8))))
      )
    }
  }
