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

package eyes4s.studio.desktop

import eyes4s.studio.app.AppModel
import eyes4s.studio.viz.StudioScenes
import intaglio.javafx.JavaFxRenderer
import scaladock.dsl.*
import scaladock.fx.{Dock, PaneFactories}
import scaladock.{LayoutSettings, LayoutState, PaneCodec, PaneType}

/** The pinned scaladock and Intaglio JavaFX backend link against the shell's
  * JavaFX without starting the toolkit (S0.3). No node is constructed.
  */
class PinnedProvidersSuite extends munit.FunSuite:

  test("a scaladock layout is built from the pinned core model") {
    val note   = PaneType[ujson.Value]("eyes4s.studio.note")(using PaneCodec.json)
    val layout = LayoutState.of(
      row(
        note(ujson.Null).titled("Left") sized 1.fr,
        note(ujson.Null).titled("Right") sized 1.fr
      )
    )
    assertEquals(layout.panes.map(_.title), Vector("Left", "Right"))
  }

  test("the studio's 28px tab header is a scaladock layout setting") {
    // DESIGN_SPEC section 2 item 5; scaladock's default is 32.
    val settings = LayoutSettings(headerPx = 28)
    assertEquals(settings.headerPx, 28.0)
    assertEquals(settings.dividerPx, LayoutSettings.default.dividerPx)
    // Typechecks only if Dock takes the settings at construction.
    val make: LayoutSettings => Dock = s => Dock(PaneFactories.empty, settings = s)
    assert(make ne null)
  }

  test("scaladock-fx types resolve against the shell's JavaFX") {
    // Typechecks only if Dock's view is a JavaFX Parent on this classpath.
    val view: Dock => javafx.scene.Parent = _.view
    assertEquals(classOf[Dock].getName, "scaladock.fx.Dock")
    assert(PaneFactories.empty ne null)
    assert(view ne null)
  }

  test("the Intaglio JavaFX backend compiles a studio scene off the FX thread") {
    val program = StudioScenes.intents(AppModel.initial).flatMap(JavaFxRenderer.compile(_))
    assert(program.isRight, program.left.map(_.message))
  }
