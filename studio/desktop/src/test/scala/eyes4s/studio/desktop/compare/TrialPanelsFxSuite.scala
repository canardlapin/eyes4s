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
import eyes4s.studio.desktop.shell.ShellFxSuite

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
