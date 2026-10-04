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

import eyes4s.studio.app.keys.CommandRegistry
import eyes4s.studio.app.vm.Menus
import eyes4s.studio.app.{AppModel, Intent, StoryModels}
import eyes4s.studio.core.command.HistoryStack
import eyes4s.studio.core.document.Theme

/** View › Appearance headlessly (ticket S1.10): Light and Dark set the
  * document's theme as a view-only change, System follows the platform's
  * reports, and a switch changes the document's theme and nothing else.
  */
class AppearanceSuite extends munit.FunSuite:

  private val model: AppModel = StoryModels.t2Compare

  private def run(m: AppModel, intents: Intent*): AppModel =
    AppModel.run(m, intents.toVector)._1

  private def theme(m: AppModel): Theme = m.document.presentation.theme

  private def checked(m: AppModel): Vector[(String, Boolean)] =
    Menus
      .bar(m)
      .flatMap(_.items)
      .filter(_.submenu.contains("Appearance"))
      .map(i => i.label -> i.checked.getOrElse(false))

  test("Dark and Light set the document's theme, undoably as a view change") {
    assertEquals(theme(model), Theme.Light)
    val dark = run(model, Intent.SetAppearance(Appearance.Dark))
    assertEquals(theme(dark), Theme.Dark)
    assertEquals(checked(dark), Vector("Light" -> false, "Dark" -> true, "System" -> false))
    val back = run(dark, Intent.Undo(HistoryStack.Presentation))
    assertEquals(theme(back), Theme.Light)
    assertEquals(checked(back).filter(_._2).map(_._1), Vector("Light"))
    // Choosing the theme already shown records nothing.
    assertEquals(run(model, Intent.SetAppearance(Appearance.Light)).document, model.document)
  }

  test("System follows the platform's theme until a fixed theme is chosen") {
    val system =
      run(model, Intent.SystemTheme(Theme.Dark), Intent.SetAppearance(Appearance.System))
    assertEquals(theme(system), Theme.Dark)
    assertEquals(checked(system).filter(_._2).map(_._1), Vector("System"))
    assertEquals(theme(run(system, Intent.SystemTheme(Theme.Light))), Theme.Light)
    val fixed =
      run(system, Intent.SetAppearance(Appearance.Dark), Intent.SystemTheme(Theme.Light))
    assertEquals(theme(fixed), Theme.Dark)
    // A platform report while a fixed theme is chosen changes nothing shown.
    assertEquals(run(model, Intent.SystemTheme(Theme.Dark)).document, model.document)
  }

  test("a switch changes the theme and no other value of the document") {
    val dark  = run(model, Intent.SetAppearance(Appearance.Dark))
    val light = run(dark, Intent.SetAppearance(Appearance.Light))
    assertNotEquals(dark.document, model.document)
    assertEquals(light.document, model.document)
    assertEquals((dark.selection, dark.location), (model.selection, model.location))
  }

  test("the three choices are View-menu commands in an Appearance submenu") {
    val ids = Vector(
      CommandRegistry.appearanceLight,
      CommandRegistry.appearanceDark,
      CommandRegistry.appearanceSystem
    ).map(_.id.value)
    assertEquals(
      ids,
      Vector("view.appearance-light", "view.appearance-dark", "view.appearance-system")
    )
    val view = Menus.bar(model).find(_.title == "View").getOrElse(fail("no View menu"))
    assertEquals(view.items.filter(_.submenu.isDefined).map(_.command.value), ids)
  }
