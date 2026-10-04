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
import eyes4s.studio.app.maps.LimitsScope
import eyes4s.studio.app.tokens.StageVariant
import eyes4s.studio.core.document.{Perspective, StageAppearance}
import eyes4s.studio.core.fixture.StoryMoment
import eyes4s.studio.desktop.harness.StudioTheme
import eyes4s.studio.desktop.shell.ShellFxSuite
import eyes4s.studio.desktop.trial.GoldenTrials

import scala.concurrent.duration.Duration

/** Compare's inspector in the studio window (ticket S8.4; Main.dc.html,
  * inspector) at t2: it binds the explanation and the read-only sections of
  * [[eyes4s.studio.app.compare.WhyReference]], the stage control changes the
  * document's stage and the panels' surround, and Edit opens Analysis.
  */
class CompareInspectorFxSuite extends ShellFxSuite:

  override val munitTimeout: Duration = Duration(180, "s")

  private val golden = PanelSources(
    GoldenTrials.contentSource,
    eyes4s.studio.desktop.trial.StimulusSource.directory(GoldenTrials.stimuli)
  )

  fxStage.test("t2: the inspector explains the matched reference; the stage control acts") {
    fx =>
      val w = boot(fx, StoryModels.t2Compare, StoryMoment.T2, panels = golden)
      val i = w.compareInspector
      eventually(fx, "the explanation and the ledger") {
        i.explanationText.startsWith("Matched reference:") &&
        i.facts.exists(_ == ("Excluded candidates", "none of 20 Encoding trials"))
      }
      assertEquals(
        runOnFx(i.explanationText),
        "Matched reference: the only admitted Encoding trial of P17 whose match item is " +
          "beach-042, the item this retrieval trial cued."
      )
      val facts = runOnFx(i.facts).toMap
      assertEquals(facts("Participant"), "P17, same as query")
      assertEquals(facts("Grid"), "64×48 · cell 0.46°")
      assertEquals(facts("Min queries per group"), "Off · n per group 2–17")
      assertEquals(
        runOnFx(i.chosen),
        (Some(StageAppearance.Dark), Some(LimitsScope.Shared), 0.6)
      )
      fx.snapshot(StudioTheme.Light)

      runOnFx(i.chooseStage(StageAppearance.Light))
      eventually(fx, "the light stage") {
        w.runtime.model.document.presentation.stage == StageAppearance.Light &&
        w.summary.panels.queryView.input.exists(_.stage == StageVariant.Light)
      }
      assertEquals(runOnFx(i.chosen._1), Some(StageAppearance.Light))
      runOnFx(i.setOpacity(0.4))
      eventually(fx, "opacity 0.4")(
        w.runtime.model.document.presentation.mapOpacity.value == 0.4
      )
      runOnFx(i.chooseLimits(LimitsScope.PerPanel))
      assertEquals(runOnFx(i.chosen._2), Some(LimitsScope.PerPanel))

      runOnFx(i.pressEdit())
      eventually(fx, "Analysis")(w.runtime.model.perspective == Perspective.Analysis)
  }
