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

import eyes4s.studio.app.plot.{HalfOpenSpan, PlotBrush, Timeline, TimelineColumns}
import eyes4s.studio.app.{AppModel, StoryModels}
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.StudioDocument
import eyes4s.studio.core.fixture.StoryMoment
import eyes4s.studio.core.headless.HeadlessSession
import eyes4s.studio.core.selection.{FixationIndex, StudioRef}

import scala.concurrent.{ExecutionContext, Future}

/** Explore's timeline, headless (ticket S6.3; Explore.dc.html, timeline), on
  * P17 enc_03's 13 admitted fixations from the fake backend at t2 (onsets
  * 60 … 4688 ms, the last ending at 5038 ms): play, pause, ticks at each
  * speed, steps between onsets, and a brush that selects the overlapping
  * fixations without touching the analysis document.
  */
class ExploreTimelineSuite extends munit.FunSuite:

  private given ExecutionContext = ExecutionContext.global

  private def ok[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)

  private val enc03 = StoryModels.p17enc03

  /** P17 enc_03's timeline as the trial view reads it. */
  private def timeline: Future[Timeline] =
    for
      session <- HeadlessSession.open(StoryMoment.T2)
      fs      <- session.trialFixations(AnalysisRevision(4), enc03)
      _       <- session.close
    yield ok(ExploreTimeline.timeline(ok(fs))).timeline

  private def start = ExploreTimeline.sync(ExploreTimeline.empty, Some(enc03))

  private def step(t: Timeline)(s: ExploreTimeline, i: TimelineIntent) =
    ExploreTimeline.update(s, Some(t), i)

  test("the fixture's intervals: 13 bars, to the millisecond, ending at 5038 ms") {
    timeline.map { t =>
      assertEquals(t.fixations.size, 13)
      assertEquals(t.fixations.head.onsetMs, 60L)
      assertEquals((t.fixations(5).onsetMs, t.fixations(5).durationMs), (2160L, 412L))
      assertEquals(ExploreTimeline.endMs(t), 5038.0)
      val vm = ExploreTimelineVM.of(start, Right(Some(TimelineRead(t, Vector.empty))))
      assertEquals(vm.status, "playhead 0.00 s")
      assertEquals(vm.disclaimer, "Brushing highlights; it does not crop the analysis")
      assertEquals(vm.source.map(_.rows.size), Some(13))
      assertEquals(
        vm.speeds.map((_, label, on) => (label, on)),
        Vector(("0.5×", false), ("1×", true), ("2×", false))
      )
      assertEquals((vm.playLabel, vm.enabled), ("Play", true))
    }
  }

  test("playback: ticks advance the playhead by speed, and stop at the trial's end") {
    timeline.map { t =>
      val s       = step(t)
      val playing = s(start, TimelineIntent.Play)
      assert(playing.playing)
      assertEquals(
        ExploreTimelineVM.of(playing, Right(Some(TimelineRead(t, Vector.empty)))).playLabel,
        "Pause"
      )
      val one = s(playing, TimelineIntent.Tick(1000.0))
      assertEquals(one.playheadMs, 1000.0)
      val twice =
        s(s(one, TimelineIntent.SetSpeed(PlaybackSpeed.Two)), TimelineIntent.Tick(500.0))
      assertEquals(twice.playheadMs, 2000.0)
      val half =
        s(s(twice, TimelineIntent.SetSpeed(PlaybackSpeed.Half)), TimelineIntent.Tick(320.0))
      assertEquals(half.playheadMs, 2160.0)
      assertEquals(
        ExploreTimelineVM.of(half, Right(Some(TimelineRead(t, Vector.empty)))).status,
        "playhead 2.16 s"
      )
      val paused = s(half, TimelineIntent.Pause)
      assertEquals(s(paused, TimelineIntent.Tick(1000.0)).playheadMs, 2160.0)
      // The end stops playback; playing again starts over.
      val ended = s(s(paused, TimelineIntent.Play), TimelineIntent.Tick(1e6))
      assertEquals((ended.playheadMs, ended.playing), (5038.0, false))
      assertEquals(s(ended, TimelineIntent.Play).playheadMs, 0.0)
      // Nothing plays before the fixations are read.
      assert(!ExploreTimeline.update(start, None, TimelineIntent.Play).playing)
    }
  }

  test("steps move between fixation onsets and pause") {
    timeline.map { t =>
      val s     = step(t)
      val first = s(start, TimelineIntent.StepForward)
      assertEquals(first.playheadMs, 60.0)
      val sixth = (1 to 5).foldLeft(first)((a, _) => s(a, TimelineIntent.StepForward))
      assertEquals(sixth.playheadMs, 2160.0)
      assertEquals(s(sixth, TimelineIntent.StepBack).playheadMs, 1740.0)
      val midway = s(s(sixth, TimelineIntent.Play), TimelineIntent.Tick(100.0))
      assertEquals(
        s(midway, TimelineIntent.StepBack),
        midway.copy(playheadMs = 2160.0, playing = false)
      )
      assertEquals(s(start, TimelineIntent.StepBack), start)
    }
  }

  test("the brush selects the overlapping fixations; the analysis document is untouched") {
    timeline.map { t =>
      val span    = HalfOpenSpan.between(1200.0, 2800.0).get
      val brushed = step(t)(start, TimelineIntent.Brushed(Some(span)))
      val vm      = ExploreTimelineVM.of(brushed, Right(Some(TimelineRead(t, Vector.empty))))
      assertEquals(vm.status, "playhead 0.00 s · brush 1.20–2.80 s")
      assertEquals(vm.brush, Some(span))
      val cols = ok(TimelineColumns.standard)
      val refs = PlotBrush.rows(vm.source.get, TimelineColumns.brushRule(cols), span)
      // Fixations 3 (870 to 1220, which reaches into the span) to 7 (from 2622).
      assertEquals(
        refs,
        (3 to 7).toVector.map(i => StudioRef.Fixation(enc03, ok(FixationIndex.of(i))))
      )
      // Brushing selects through the shared selection, as every view's brush
      // does: the document, and its hash, do not change.
      val model    = StoryModels.t2Explore
      val selected =
        AppModel.update(model, StoryModels.select(model, "explore.timeline", refs*))._1
      assertEquals(selected.selection.selected, refs)
      assertEquals(selected.document, model.document)
      assertEquals(
        StudioDocument.encode(selected.document),
        StudioDocument.encode(model.document)
      )
      // Clearing it leaves no span.
      assertEquals(step(t)(brushed, TimelineIntent.Brushed(None)).brush, None)
    }
  }

  test("a new trial starts paused at its beginning, unbrushed; the speed stays") {
    val other = enc03.copy(trial = "enc_04")
    val tuned = start.copy(
      playheadMs = 900.0,
      playing = true,
      speed = PlaybackSpeed.Two,
      brush = HalfOpenSpan.between(100.0, 400.0)
    )
    val moved = ExploreTimeline.sync(tuned, Some(other))
    assertEquals(
      moved,
      ExploreTimeline.empty.copy(trial = Some(other), speed = PlaybackSpeed.Two)
    )
    assertEquals(ExploreTimeline.sync(tuned, Some(enc03)), tuned)
    val none = ExploreTimelineVM.of(ExploreTimeline.empty, Right(None))
    assertEquals((none.enabled, none.source, none.playheadMs), (false, None, None))
  }

  test(
    "non-integer times: ends are rounded, so meeting fixations still meet; too short is reported"
  ) {
    import eyes4s.plan.MapPlacement
    def fix(i: Int, onset: Double, duration: Double) =
      ok(
        AdmittedFixation.of(
          StudioRef.Fixation(enc03, ok(FixationIndex.of(i))),
          7208 + i,
          960.5,
          540.5,
          onset,
          duration,
          MapPlacement.InWindow
        )
      )
    // A 2 kHz recording: fixation 1 ends at 103.0 ms, where fixation 2 starts.
    val fixations = ok(
      TrialFixations.of(
        AnalysisRevision(4),
        DatasetRevision(3),
        enc03,
        Vector(fix(1, 100.5, 2.5), fix(2, 103.0, 4.2), fix(3, 107.2, 0.2), fix(4, 110.4, 10.0))
      )
    )
    val read = ok(ExploreTimeline.timeline(fixations))
    assertEquals(
      read.timeline.fixations.map(f => (f.index.value, f.onsetMs, f.durationMs)),
      // Onset and end rounded; the duration is their difference.
      Vector((1, 101L, 2L), (2, 103L, 4L), (4, 110L, 10L))
    )
    // Fixation 3 (107.2 to 107.4 ms) has no whole millisecond: it is not drawn, and said so.
    assertEquals(read.skipped.map(_.ref.index.value), Vector(3))
    val vm = ExploreTimelineVM.of(
      ExploreTimeline.sync(ExploreTimeline.empty, Some(enc03)),
      Right(Some(read))
    )
    assertEquals(
      vm.skipped,
      Vector("Fixation 3 lasts 0.2 ms, too short to draw; it is not shown.")
    )
    // A brush just after fixation 1 ends picks only fixation 2, as in the data.
    val span = HalfOpenSpan.between(103.0, 103.5).get
    val refs = PlotBrush.rows(
      vm.source.get,
      TimelineColumns.brushRule(ok(TimelineColumns.standard)),
      span
    )
    assertEquals(refs, Vector(StudioRef.Fixation(enc03, ok(FixationIndex.of(2)))))
  }

  test("a tick that reaches the end exactly stops playback there") {
    timeline.map { t =>
      val s    = step(t)
      val near = s(start.copy(playheadMs = 4938.0), TimelineIntent.Play)
      val at   = s(near, TimelineIntent.Tick(100.0))
      assertEquals((at.playheadMs, at.playing), (5038.0, false))
      // Just short of it plays on.
      assertEquals(s(near, TimelineIntent.Tick(99.5)).playing, true)
    }
  }

  test("a trial change while playing stops playback and starts the new trial at 0") {
    val other   = enc03.copy(trial = "enc_04")
    val playing = start.copy(playheadMs = 1200.0, playing = true)
    val moved   = ExploreTimeline.sync(playing, Some(other))
    assertEquals((moved.trial, moved.playing, moved.playheadMs), (Some(other), false, 0.0))
  }
