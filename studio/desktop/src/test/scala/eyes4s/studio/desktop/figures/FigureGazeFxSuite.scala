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

package eyes4s.studio.desktop.figures

import eyes4s.studio.app.StoryModels
import eyes4s.studio.core.document.{FigureId, PanelLetter}
import eyes4s.studio.core.fixture.StoryMoment
import eyes4s.studio.desktop.shell.ShellFxSuite
import eyes4s.studio.viz.trial.{MarkStyle, TrialRole}

import scala.concurrent.duration.Duration

/** Panels A and B on the page in the studio window (follow-up
  * bd-01M42K7ZNCC5J9R9RPR7H6ZHNT to S9.2a) at story moment t2: each draws
  * its trial with the fixations trialFixations serves, marked by role.
  */
class FigureGazeFxSuite extends ShellFxSuite:

  override val munitTimeout: Duration = Duration(180, "s")

  private def ok[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)
  private val figure1                      = ok(FigureId.of(1))
  private def letter(l: String)            = ok(PanelLetter.of(l))

  fxStage.test("panels A and B show their trials' fixations, A as the matched reference") {
    fx =>
      val w                = boot(fx, StoryModels.t2Figures, StoryMoment.T2)
      def drawn(l: String) =
        runOnFx(w.figures.gaze(figure1, letter(l)).flatMap(_.input))
      eventually(fx, "both gaze panels are drawn") {
        drawn("A").isDefined && drawn("B").isDefined
      }
      val a = drawn("A").get
      val b = drawn("B").get
      assertEquals((a.display.trial.trial, a.fixations.size), ("enc_03", 13))
      assertEquals((b.display.trial.trial, b.fixations.size), ("ret_07", 12))
      assertEquals(a.marks, MarkStyle.Role(TrialRole.Matched))
      assertEquals(b.marks, MarkStyle.Role(TrialRole.Query))
  }
