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

import eyes4s.studio.app.{AppModel, Intent, StoryModels}
import eyes4s.studio.core.document.Theme
import eyes4s.studio.core.preferences.{AppearanceChoice, UserPreferences}

/** The appearance preference headlessly (ticket S2.8 with S1.10; the lead's
  * decision on the bead): System is a preference only, applied when a
  * window opens and never written into the document; Light or Dark is the
  * theme a new project starts with, never applied to a project that opens;
  * only the user's explicit choice is a preference to save.
  */
class AppearancePreferenceSuite extends munit.FunSuite:

  private val t2                         = StoryModels.t2Compare
  private def prefs(c: AppearanceChoice) = UserPreferences.defaults.withAppearance(c)

  private def run(m: AppModel, intents: Intent*): AppModel =
    intents.foldLeft(m)((acc, i) => AppModel.update(acc, i)._1)

  test("the preference and S1.10's appearance map one to one") {
    Appearance.values.foreach(a =>
      assertEquals(AppearancePreference.fromChoice(AppearancePreference.toChoice(a)), a)
    )
    AppearanceChoice.values.foreach(c =>
      assertEquals(AppearancePreference.toChoice(AppearancePreference.fromChoice(c)), c)
    )
  }

  test("at boot, a System preference follows the platform; the document keeps its theme") {
    val intents = AppearancePreference.atBoot(prefs(AppearanceChoice.System), t2)
    assertEquals(intents, Vector(Intent.SetAppearance(Appearance.System)))
    val booted = run(t2, (Intent.SystemTheme(Theme.Dark) +: intents)*)
    assert(booted.appearance.followSystem)
    assertEquals(booted.theme, Theme.Dark)
    // Following the platform is never a document field.
    assertEquals(booted.document, t2.document)
    // Already following: nothing more to do.
    assertEquals(
      AppearancePreference.atBoot(prefs(AppearanceChoice.System), booted),
      Vector.empty
    )
  }

  test("at boot, a fixed preference leaves the document's own theme") {
    Vector(AppearanceChoice.Light, AppearanceChoice.Dark).foreach(c =>
      assertEquals(AppearancePreference.atBoot(prefs(c), t2), Vector.empty, c.toString)
    )
  }

  test("only an explicit appearance choice is a preference to save") {
    val dark = prefs(AppearanceChoice.Dark)
    // A Dark preference meets a Light document: nothing is chosen, nothing saved.
    assertEquals(t2.document.presentation.theme, Theme.Light)
    assertEquals(AppearancePreference.atBoot(dark, t2), Vector.empty)
    Vector(
      Intent.SystemTheme(Theme.Dark),
      Intent.Undo(eyes4s.studio.core.command.HistoryStack.Presentation),
      Intent.SwitchPerspective(eyes4s.studio.core.document.Perspective.Data)
    ).foreach(i => assertEquals(AppearancePreference.chosen(dark, i), None, i.toString))
    // The user's choice is the preference, the rest kept.
    assertEquals(
      AppearancePreference.chosen(dark, Intent.SetAppearance(Appearance.Light)),
      Some(dark.withAppearance(AppearanceChoice.Light))
    )
    assertEquals(
      AppearancePreference
        .chosen(dark, Intent.SetAppearance(Appearance.System))
        .map(_.appearance),
      Some(AppearanceChoice.System)
    )
  }

  test("a new project starts with the fixed preference, or the platform's theme") {
    assertEquals(
      AppearancePreference.newProjectTheme(prefs(AppearanceChoice.Dark), Theme.Light),
      Theme.Dark
    )
    assertEquals(
      AppearancePreference.newProjectTheme(prefs(AppearanceChoice.Light), Theme.Dark),
      Theme.Light
    )
    assertEquals(
      AppearancePreference.newProjectTheme(prefs(AppearanceChoice.System), Theme.Dark),
      Theme.Dark
    )
  }
