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

package eyes4s.studio.desktop.shell

import eyes4s.studio.app.{AppModel, Intent, StoryModels}
import eyes4s.studio.app.tokens.Theme
import eyes4s.studio.core.document.Perspective
import eyes4s.studio.core.fixture.StoryMoment
import eyes4s.studio.desktop.StudioStyles
import eyes4s.studio.desktop.harness.StudioTheme

/** The finished shell chrome (S1.6–S1.9, S1.11) at every perspective of the
  * three story moments (the S0.8 seeds): the drawn texts equal the
  * view-models', and each state is snapshotted in light, plus dark for
  * Compare, to `target/studio-snapshots/ShellMomentsFxSuite/<test>/`.
  */
class ShellMomentsFxSuite extends ShellFxSuite:

  private val moments: Vector[(String, () => AppModel, StoryMoment)] = Vector(
    ("t1", () => StoryModels.t1Data, StoryMoment.T1),
    ("t2", () => StoryModels.t2Compare, StoryMoment.T2),
    ("t3", () => StoryModels.t3Summary, StoryMoment.T3)
  )

  for (moment, base, story) <- moments; p <- Perspective.values do
    fxStage.test(s"$moment · ${p.label}: the shell reads as its view-models; snapshot") { fx =>
      assumeFullStage(fx)
      val model = at(base(), Intent.SwitchPerspective(p))
      val w     = boot(fx, model, story)
      assertEquals(runOnFx(w.runtime.model.perspective), p)
      assertNoDiff(texts(w).mkString("\n"), expected(w).mkString("\n"))
      fx.snapshot(StudioTheme.Light)
      if p == Perspective.Compare then
        val sheets = StudioStyles.stylesheets(Theme.Dark).fold(e => fail(e.message), identity)
        runOnFx(w.root.getStylesheets.setAll(sheets*))
        fx.snapshot(StudioTheme.Dark)
        assertNoDiff(texts(w).mkString("\n"), expected(w).mkString("\n"))
    }
