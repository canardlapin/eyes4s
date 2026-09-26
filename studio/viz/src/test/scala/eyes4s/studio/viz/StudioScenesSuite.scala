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

package eyes4s.studio.viz

import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.app.nav.{Location, Place}
import eyes4s.studio.core.document.Perspective
import intaglio.svg.SvgRenderer

/** The pinned Intaglio core, interaction and svg link on this platform. */
class StudioScenesSuite extends munit.FunSuite:

  test("the intents scene compiles and renders to SVG, including the initial model") {
    val initial = AppModel.newProject.fold(e => fail(e.message), identity)
    val moved   = AppModel
      .update(initial, Intent.Navigate(Location(Perspective.Figures, Vector(Place.Figures))))
      ._1
    Seq(initial, moved).foreach { model =>
      val svg = StudioScenes.intents(model).flatMap(SvgRenderer.render(_)).map(_.value)
      assert(svg.exists(_.contains("<svg")), svg.left.map(_.message))
    }
  }

  test("intent keys and an empty selection come from intaglio-interaction") {
    assert(StudioScenes.intentKeys.isRight)
    assertEquals(StudioScenes.noSelection.size, 0)
  }
