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

import eyes4s.studio.app.about.AboutBox
import eyes4s.studio.app.text.Messages
import eyes4s.studio.app.{Intent, PlatformDialog, StoryModels}
import eyes4s.studio.core.engine.StudioBuild
import eyes4s.studio.desktop.StudioWindow

import scala.jdk.CollectionConverters.*

/** The About box in the studio (ticket S1.14): Help › About opens a window
  * listing the build, the running Java and JavaFX, and every component of
  * the generated notices.
  */
class AboutFxSuite extends ShellFxSuite:

  fxStage.test("Help › About opens the About box with every bundled component") { fx =>
    val dialogs         = Dialogs(None)
    val w: StudioWindow = boot(fx, StoryModels.t2Compare, dialogs = dialogs)
    val facts           = DesktopAbout.facts
    val listed          = facts.components.fold(fail(_), identity)
    val vm              = AboutBox.vm(facts)
    val view            = runOnFx(AboutView(vm, () => ()))
    assertEquals(runOnFx(view.shownRows), listed.size + 1)
    assertEquals(
      runOnFx(view.shownLines).map(_._1),
      Vector("Version", "Commit", "Backend protocol", "Java", "JavaFX")
    )
    // The running JavaFX is the one the build compiles against.
    assert(
      runOnFx(view.shownLines).last._2.startsWith(StudioBuild.javaFxVersion),
      runOnFx(view.shownLines).last._2
    )
    // Help › About asks the shell for the About dialog, which opens its window.
    runOnFx(w.runtime.dispatch(Intent.ShowAbout))
    eventually(fx, "the About dialog asked")(dialogs.asked.contains(PlatformDialog.About))
    runOnFx(
      StudioWindow
        .fxDialogs(() => w.runtime.model, Messages.english)
        .open(PlatformDialog.About, _ => ())
    )
    eventually(fx, "the About window") {
      javafx.stage.Window.getWindows.asScala.exists {
        case s: javafx.stage.Stage => s.getTitle == "About Eyes Studio" && s.isShowing
        case _                     => false
      }
    }
    runOnFx(
      javafx.stage.Window.getWindows.asScala.toVector
        .collect {
          case s: javafx.stage.Stage if s.getTitle == "About Eyes Studio" => s
        }
        .foreach(_.close())
    )
  }
