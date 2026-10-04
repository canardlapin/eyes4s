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

package eyes4s.studio.app.appearance

import eyes4s.studio.app.vm.Menus
import eyes4s.studio.app.{AppModel, Intent, StoryModels}
import eyes4s.studio.core.command.HistoryStack
import eyes4s.studio.core.document.Theme

/** The S1.10 review's probe, kept as its regression tests (finding 1): under
  * View › Appearance › System a platform scheme change is shown and recorded
  * nowhere (no edit, no undo step), and Undo after a chosen theme leaves the
  * menu and the window agreeing.
  */
class ReviewProbeSuite extends munit.FunSuite:

  private val model: AppModel = StoryModels.t2Compare

  private def run(m: AppModel, intents: Intent*): AppModel =
    AppModel.run(m, intents.toVector)._1

  private def theme(m: AppModel): Theme = m.theme

  private def checked(m: AppModel): Vector[(String, Boolean)] =
    Menus
      .bar(m)
      .flatMap(_.items)
      .filter(_.submenu.contains("Appearance"))
      .map(i => i.label -> i.checked.getOrElse(false))

  test("under System a platform change is shown, but is no edit and no undo step") {
    val system = run(model, Intent.SetAppearance(Appearance.System))
    val dark   = run(system, Intent.SystemTheme(Theme.Dark))
    assertEquals(theme(dark), Theme.Dark)
    assertEquals(dark.document, model.document)
    assertEquals(dark.save.edited, model.save.edited)
    assertEquals(dark.history.stack(HistoryStack.Presentation).canUndo, false)
    assertEquals(checked(dark).filter(_._2).map(_._1), Vector("System"))
    // An undo now undoes nothing of the platform's.
    val undone = run(dark, Intent.Undo(HistoryStack.Presentation))
    assertEquals(
      (theme(undone), checked(undone).filter(_._2).map(_._1)),
      (Theme.Dark, Vector("System"))
    )
  }

  test("undo after a chosen Light → Dark leaves the menu and the window agreeing") {
    val dark = run(model, Intent.SetAppearance(Appearance.Dark))
    assertEquals(dark.history.stack(HistoryStack.Presentation).canUndo, true)
    val undone = run(dark, Intent.Undo(HistoryStack.Presentation))
    assertEquals(
      (theme(undone), checked(undone).filter(_._2).map(_._1)),
      (Theme.Light, Vector("Light"))
    )
    // The same after a spell under System: leaving it for Dark is one undo step.
    val viaSystem = run(
      model,
      Intent.SetAppearance(Appearance.System),
      Intent.SystemTheme(Theme.Dark),
      Intent.SetAppearance(Appearance.Dark)
    )
    val back = run(viaSystem, Intent.Undo(HistoryStack.Presentation))
    assertEquals(
      (theme(back), checked(back).filter(_._2).map(_._1)),
      (Theme.Light, Vector("Light"))
    )
  }
