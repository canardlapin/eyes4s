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

import eyes4s.studio.app.StoryModels
import eyes4s.studio.core.backend.PairDesign
import eyes4s.studio.core.fixture.StoryMoment
import eyes4s.studio.core.selection.StudioRef
import eyes4s.studio.desktop.StudioWindow
import eyes4s.studio.desktop.harness.{FxStage, StudioTheme}
import eyes4s.studio.desktop.plot.{PlotHostStatus, PlotTwinStatus}
import eyes4s.studio.desktop.shell.ShellFxSuite
import javafx.scene.input.KeyCode

import scala.concurrent.duration.Duration

/** Compare's contrast pane in the studio window (ticket S8.3; Main.dc.html,
  * contrast) at t2: the scale ladder is drawn and the readout is FIXTURE.md's
  * focus (2° M 0.73, B 0.35 of 19 controls, D +0.38) with the confound
  * sentence verbatim; Next and Prev walk the references by cosine and the
  * reference panel follows; picking a control in the ladder switches the
  * reference panel to it.
  */
class ScaleLadderFxSuite extends ShellFxSuite:

  override val munitTimeout: Duration = Duration(180, "s")

  private def ready(fx: FxStage, w: StudioWindow): Unit =
    eventually(fx, "the ladder is drawn and the readout filled") {
      w.summary.readout.lines.headOption.exists(_.nonEmpty) &&
      (w.summary.ladder.status.get match
        case PlotTwinStatus.Shown(_) => true
        case _                       => false) &&
      (w.summary.ladder.plotHost.status.get match
        case PlotHostStatus.Drawn(_) => true
        case _                       => false)
    }

  private def reference(w: StudioWindow): (String, String) =
    val r = runOnFx(w.summary.panels.shown)(1)
    (r._1, r._2)

  fxStage.test("the readout is FIXTURE.md's focus at 2°, with the confound sentence verbatim") {
    fx =>
      val w = boot(fx, StoryModels.t2Compare, StoryMoment.T2)
      ready(fx, w)
      assertEquals(
        runOnFx(w.summary.readout.lines),
        Vector(
          "ret_07 contrast · σ 2°",
          "+0.38 D",
          "M matched 0.73",
          "B mean of 19 controls 0.35",
          "Spatial correspondence, not replay. D does not separate participant-specific " +
            "reinstatement from item-driven salience common to all viewers of that image, " +
            "and may retain residual centre bias.",
          "Matched · beach-042",
          "0.73",
          "The matched pair. Its cosine is M.",
          "matched · then 19 controls"
        )
      )
      assertEquals(runOnFx(w.summary.readout.steps), (false, true))
      fx.snapshot(StudioTheme.Light)
  }

  fxStage.test("Next and Prev walk the references by cosine; the reference panel follows") {
    fx =>
      val w = boot(fx, StoryModels.t2Compare, StoryMoment.T2)
      ready(fx, w)
      runOnFx(w.summary.readout.pressNext())
      eventually(fx, "the reference panel shows the first control") {
        reference(w) == ("Control", "P17 · enc_01 · street-112")
      }
      val lines = runOnFx(w.summary.readout.lines)
      assertEquals(
        lines.takeRight(4),
        Vector(
          "Control · street-112",
          "0.61",
          "One of 19 controls. It enters B; it is not M.",
          "control 1 of 19 by cosine"
        )
      )
      // D, M and B are the query's; they do not move with the inspected pair.
      assertEquals(
        lines.take(4).drop(1),
        Vector("+0.38 D", "M matched 0.73", "B mean of 19 controls 0.35")
      )
      runOnFx(w.summary.readout.pressPrev())
      eventually(fx, "the reference panel is back on the matched reference") {
        reference(w) == ("Matched", "P17 · enc_03 · beach-042")
      }
  }

  fxStage.test("picking a control in the ladder switches the reference panel to it") { fx =>
    val w = boot(fx, StoryModels.t2Compare, StoryMoment.T2)
    ready(fx, w)
    val host = runOnFx(w.summary.ladder.plotHost)
    runOnFx(host.requestFocus())
    fx.awaitLayout()
    def pickedControl: Option[StudioRef] = runOnFx(w.runtime.model.selection.selected) match
      case Vector(ref @ StudioRef.Pair(_, _, PairDesign.Control, _, _)) => Some(ref)
      case _                                                            => None
    var steps = 0
    while pickedControl.isEmpty && steps < 40 do
      fx.robot.press(KeyCode.RIGHT)
      fx.robot.press(KeyCode.ENTER)
      fx.awaitLayout()
      steps += 1
    val ref = pickedControl.getOrElse(fail(s"no control picked in $steps steps"))
    val StudioRef.Pair(_, _, _, _, key) = ref: @unchecked
    eventually(fx, s"the reference panel shows ${key.trial}") {
      val (role, title) = reference(w)
      role == "Control" && title.startsWith(s"P17 · ${key.trial}")
    }
    assertEquals(runOnFx(w.summary.panels.backOffered), Some("Back to matched reference"))
  }
