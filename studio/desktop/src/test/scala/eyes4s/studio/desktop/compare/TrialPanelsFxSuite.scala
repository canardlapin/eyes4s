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

package eyes4s.studio.desktop.compare

import eyes4s.studio.app.nav.Place
import eyes4s.studio.app.{Intent, StoryModels}
import eyes4s.studio.core.backend.PairDesign
import eyes4s.studio.core.fixture.{MockStudy, StoryMoment, StoryMoments}
import eyes4s.studio.core.selection.StudioRef
import eyes4s.studio.desktop.StudioWindow
import eyes4s.studio.desktop.harness.{FxStage, StudioTheme}
import eyes4s.studio.desktop.plot.PlotHostStatus
import eyes4s.studio.desktop.shell.ShellFxSuite
import eyes4s.studio.desktop.trial.{GoldenTrials, StimulusSource, TrialView, TrialViewStatus}
import eyes4s.studio.viz.trial.{MarkStyle, TrialExtent, TrialRole, TrialScene}

import scala.concurrent.duration.Duration

/** Compare's query and reference trial panels in the studio window (ticket
  * S8.2; Main.dc.html, panels) at t2: the query panel is P17 ret_07 with its
  * contrast, the reference panel its matched enc_03 with the inspected pair
  * score; choosing a control switches the reference panel to 'Control ·
  * item' while the matched reference stays named, and Back returns to it.
  */
class TrialPanelsFxSuite extends ShellFxSuite:

  override val munitTimeout: Duration = Duration(180, "s")

  private val control: StudioRef = StudioRef.Pair(
    StoryMoments.run7,
    StoryModels.sigma2,
    PairDesign.Control,
    StoryModels.p17ret07,
    MockStudy.key("P17", "enc_08")
  )

  private def scored(fx: FxStage, w: StudioWindow, title: String): Unit =
    eventually(fx, s"the reference panel shows $title, scored") {
      val shown = w.summary.panels.shown
      shown(1)._2 == title && !shown(1)._3.endsWith("reading…")
    }

  fxStage.test(
    "at t2 the query panel is ret_07 with its contrast, the reference matched enc_03"
  ) { fx =>
    val w = boot(fx, StoryModels.t2Compare, StoryMoment.T2)
    scored(fx, w, "P17 · enc_03 · beach-042")
    val Vector(q, r) = runOnFx(w.summary.panels.shown): @unchecked
    assertEquals(q, ("Query", "P17 · ret_07 · beach-042", "Query contrast D · σ 2°: +0.38"))
    assertEquals(r._1, "Matched")
    assert(r._3.startsWith("Inspected pair score · σ 2°: "), r._3)
    assertEquals(runOnFx(w.summary.panels.backOffered), None)
    assertEquals(
      runOnFx(w.summary.panels.matchedNote),
      "Matched reference: P17 · enc_03 · beach-042"
    )
    fx.snapshot(StudioTheme.Light)
  }

  fxStage.test("a control switches the reference panel to 'Control · item'; Back returns") {
    fx =>
      val w = boot(fx, StoryModels.t2Compare, StoryMoment.T2)
      scored(fx, w, "P17 · enc_03 · beach-042")
      val before = runOnFx(w.summary.panels.shown)
      runOnFx(w.runtime.dispatch(Intent.Explain(Place.At(control))))
      scored(fx, w, "P17 · enc_08 · dog-077")
      val Vector(q, r) = runOnFx(w.summary.panels.shown): @unchecked
      assertEquals(r._1, "Control")
      // The query and its contrast did not move; the score is the control pair's.
      assertEquals(q, before(0))
      assertNotEquals(r._3, before(1)._3)
      // The matched identity stays named and reachable.
      assertEquals(
        runOnFx(w.summary.panels.matchedNote),
        "Matched reference: P17 · enc_03 · beach-042"
      )
      assertEquals(runOnFx(w.summary.panels.backOffered), Some("Back to matched reference"))
      fx.snapshot(StudioTheme.Light)
      runOnFx(w.summary.panels.pressBack())
      scored(fx, w, "P17 · enc_03 · beach-042")
      assertEquals(runOnFx(w.summary.panels.shown)(1), before(1))
      assertEquals(runOnFx(w.runtime.model.location.trail.last), Place.At(StoryModels.pair))
  }

  // --- The stages, with fixtures/studio-golden behind the content port -------------

  private val golden = PanelSources(
    GoldenTrials.contentSource,
    StimulusSource.directory(GoldenTrials.stimuli)
  )

  private def scene(v: TrialView): Option[TrialScene] = v.status.get match
    case TrialViewStatus.Shown(s) => Some(s)
    case _                        => None

  fxStage.test("the stages draw the query and its reference in their roles, with captions") {
    fx =>
      val w = boot(fx, StoryModels.t2Compare, StoryMoment.T2, panels = golden)
      scored(fx, w, "P17 · enc_03 · beach-042")
      def painted(v: TrialView) =
        scene(v).nonEmpty && (v.plotHost.status.get match
          case PlotHostStatus.Drawn(_) => true
          case _                       => false)
      eventually(fx, "both stages are drawn") {
        painted(w.summary.panels.queryView) && painted(w.summary.panels.referenceView)
      }
      val q = runOnFx(scene(w.summary.panels.queryView)).get
      val r = runOnFx(scene(w.summary.panels.referenceView)).get
      assertEquals(q.marks.size, GoldenTrials.fixations("P17", "ret_07").size)
      assertEquals(r.marks.size, GoldenTrials.fixations("P17", "enc_03").size)
      assert(q.caption.startsWith("Displayed: blank + fixation cross"), q.caption)
      assert(r.caption.startsWith("Displayed: image"), r.caption)
      // The query's remembered image is not shown until the user asks.
      assertEquals(q.disclosure, Some("Remembered image not shown"))
      assertEquals(
        runOnFx(w.summary.panels.queryView.input.map(_.marks)),
        Some(MarkStyle.Role(TrialRole.Query))
      )
      assertEquals(
        runOnFx(w.summary.panels.referenceView.input.map(_.marks)),
        Some(MarkStyle.Role(TrialRole.Matched))
      )
      val counts = runOnFx(w.summary.panels.counts)
      assertEquals(counts._1, s"${q.marks.size} fix")
      // The Table tabs list each trial's fixations, one row per mark.
      eventually(fx, "the Table tabs are filled") {
        w.summary.queryTrialTable.source.exists(_.rows.size == q.marks.size) &&
        w.summary.referenceTrialTable.source.exists(_.rows.size == r.marks.size)
      }
      fx.snapshot(StudioTheme.Light)
      // Underlay on: the query's scene underlays the matched image and says so.
      runOnFx(w.summary.panels.toggleUnderlay())
      eventually(fx, "the underlay is disclosed") {
        scene(w.summary.panels.queryView)
          .flatMap(_.disclosure)
          .contains("Reference image — not displayed during this trial")
      }
      // A control: the reference stage redraws in the control role.
      runOnFx(w.runtime.dispatch(Intent.Explain(Place.At(control))))
      eventually(fx, "the reference stage shows enc_08 as a control") {
        w.summary.panels.referenceView.input.exists(i =>
          i.display.trial == MockStudy.key("P17", "enc_08") &&
            i.marks == MarkStyle.Role(TrialRole.Control)
        ) && scene(w.summary.panels.referenceView).nonEmpty
      }
  }

  fxStage.test("with no content served, each stage says so and draws nothing") { fx =>
    val w = boot(fx, StoryModels.t2Compare, StoryMoment.T2)
    eventually(fx, "the stages say why they are empty") {
      w.summary.panels.stageNotes._1.nonEmpty && w.summary.panels.stageNotes._2.nonEmpty
    }
    assertEquals(
      runOnFx(w.summary.panels.stageNotes),
      (
        "The content of P17 · ret_07 is not served.",
        "The content of P17 · enc_03 is not served."
      )
    )
    assertEquals(runOnFx(scene(w.summary.panels.queryView)), None)
  }

  private def painted(v: TrialView): Boolean =
    scene(v).nonEmpty && (v.plotHost.status.get match
      case PlotHostStatus.Drawn(_) => true
      case _                       => false)

  fxStage.test("both stages share one covering extent; leaving the query clears both panels") {
    fx =>
      val w = boot(fx, StoryModels.t2Compare, StoryMoment.T2, panels = golden)
      eventually(fx, "both stages are drawn") {
        painted(w.summary.panels.queryView) && painted(w.summary.panels.referenceView)
      }
      val (qi, ri) =
        runOnFx(
          (w.summary.panels.queryView.input.get, w.summary.panels.referenceView.input.get)
        )
      val shared = TrialScene
        .sharedExtent(
          Vector(qi, ri).map(i => i.copy(options = i.options.copy(extent = TrialExtent.Gaze)))
        )
        .getOrElse(fail("no shared extent"))
      assertEquals(qi.options.extent, TrialExtent.Covering(shared))
      assertEquals(ri.options.extent, TrialExtent.Covering(shared))
      // It covers each trial's own gaze extent.
      Vector(qi, ri).foreach { i =>
        val own =
          TrialScene.extentOf(i.copy(options = i.options.copy(extent = TrialExtent.Gaze)))
        assert(
          shared.left <= own.left && shared.top <= own.top && shared.right >= own.right &&
            shared.bottom >= own.bottom,
          (own, shared)
        )
      }
      // The trail leaves the query: no panel keeps the earlier trial.
      runOnFx(
        w.runtime.dispatch(
          Intent.Explain(Place.Summary(StoryModels.reporting))
        )
      )
      eventually(fx, "both panels are cleared") {
        w.summary.panels.queryView.input.isEmpty && w.summary.panels.referenceView.input.isEmpty
      }
      val Vector(q, r) = runOnFx(w.summary.panels.shown): @unchecked
      assertEquals(q, ("", "Choose a query in the Queries navigator", ""))
      assertEquals(r, ("", "", ""))
      assertEquals(runOnFx(w.summary.panels.counts), ("", ""))
      assertEquals(runOnFx(w.summary.panels.referenceNode.getAccessibleText), null)
      assertEquals(runOnFx(w.summary.panels.queryView.status.get), TrialViewStatus.Empty)
      assertEquals(runOnFx(w.summary.panels.referenceView.status.get), TrialViewStatus.Empty)
  }

  fxStage.test(
    "Retry reads unreadable content again; no remembered image says it is not shown"
  ) { fx =>
    val first = java.util.concurrent.atomic.AtomicBoolean(true)
    // ret_07 is unreadable the first time; enc_03 is never served.
    val content: eyes4s.studio.app.compare.TrialContentSource = (rev, key, done) =>
      if key.trial == "enc_03" then
        done(Left(eyes4s.studio.app.compare.ContentError.NotServed(key)))
      else if key.trial == "ret_07" && first.getAndSet(false) then
        done(Left(eyes4s.studio.app.compare.ContentError.Unreadable(key, "busy")))
      else GoldenTrials.contentSource.content(rev, key, done)
    val w = boot(
      fx,
      StoryModels.t2Compare,
      StoryMoment.T2,
      panels = PanelSources(content, StimulusSource.directory(GoldenTrials.stimuli))
    )
    eventually(fx, "Retry is offered")(w.summary.panels.retryOffered)
    assertEquals(
      runOnFx(w.summary.panels.stageNotes._1),
      "The content of P17 · ret_07 could not be read: busy"
    )
    runOnFx(w.summary.panels.pressRetry())
    eventually(fx, "the query stage is drawn")(painted(w.summary.panels.queryView))
    // enc_03 is not served, so the underlay has no image: the note says so.
    assertEquals(runOnFx(w.summary.panels.rememberedNote), "")
    runOnFx(w.summary.panels.toggleUnderlay())
    eventually(fx, "the note says the remembered image is not shown") {
      w.summary.panels.rememberedNote == "Remembered image not shown"
    }
  }
