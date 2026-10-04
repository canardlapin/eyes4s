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

package eyes4s.studio.viz.geometry

import eyes4s.studio.app.geometry.*
import eyes4s.studio.app.tokens.{StageVariant, Theme}
import eyes4s.studio.core.backend.{Phase, TrialKey}
import intaglio.{Grob, YDirection, value}

/** The placement pictures as Intaglio scenes (ticket S5.5): screen-pixel
  * data coordinates with y down, the image frame and its outline, the
  * records drawn where eyes4s placed them (off-screen ones only counted),
  * and the density's non-empty cells at the map opacity.
  */
class GeometrySceneSuite extends munit.FunSuite:

  private val frame = FramePicture(1920, 1080, 448, 156, 1024, 768)
  private val trial = TrialKey("P05", Phase.Retrieval, "ret_04", 1)

  private def get[A](e: Either[eyes4s.studio.viz.plot.PlotSceneError, A]): A =
    e.fold(err => fail(err.message), identity)

  private def names(g: Grob): Vector[String] = g match
    case Grob.Group(children, _, name) => name.map(_.value).toVector ++ children.flatMap(names)
    case other                         => other.name.map(_.value).toVector

  test("a thumbnail draws the screen, the image frame, its outline and the on-screen records") {
    val picture = ThumbnailPicture(
      trial,
      3,
      1,
      1,
      Vector(
        MarkPicture(700.0, 420.0, MarkPlace.Inside, corrected = false),
        MarkPicture(260.0, 120.0, MarkPlace.OutsideWindow, corrected = true),
        MarkPicture(2000.0, 420.0, MarkPlace.OutsideScreen, corrected = false)
      )
    )
    val scene = get(
      GeometryScene.thumbnail(
        "geometry.thumb.0",
        Theme.Light,
        StageVariant.Dark,
        frame,
        picture
      )
    )
    assertEquals(scene.panel.xDomain.upper, 1920.0)
    assertEquals(scene.panel.yDomain.upper, 1080.0)
    assertEquals(scene.panel.yDirection, YDirection.Down)
    assertEquals(
      scene.scene.grobs.flatMap(names),
      Vector("image-frame", "records", "window-outline")
    )
    val batch = scene.scene.grobs.flatMap {
      case Grob.Group(children, _, _) => children.collect { case b: Grob.PointBatch => b }
      case _                          => Vector.empty
    }
    // The off-screen record is counted by the label, not drawn.
    assertEquals(batch.map(_.points.size), Vector(2))
  }

  test("a thumbnail whose records are all off screen still draws its frames") {
    val picture = ThumbnailPicture(
      trial,
      1,
      0,
      1,
      Vector(MarkPicture(-50.0, 20.0, MarkPlace.OutsideScreen, corrected = false))
    )
    val scene = get(
      GeometryScene.thumbnail("geometry.thumb.1", Theme.Dark, StageVariant.Mid, frame, picture)
    )
    assertEquals(scene.scene.grobs.flatMap(names), Vector("image-frame", "window-outline"))
  }

  test("the density draws one rect per non-empty cell, at the map opacity") {
    val levels =
      Vector.tabulate(64 * 36)(i => if i == 15 * 64 + 38 then 4 else if i == 0 then 1 else 0)
    val picture = DensityPicture(64, 36, levels, 2)
    val scene   =
      get(GeometryScene.density("geometry.density.0", StageVariant.Dark, frame, picture))
    val cells = scene.scene.grobs.flatMap {
      case Grob.Group(children, _, _) =>
        children.collect {
          case Grob.Group(rects, _, Some(n)) if n.value == "density" => rects
        }.flatten
      case _ => Vector.empty
    }
    assertEquals(cells.size, 2)
    cells.foreach {
      case r: Grob.Rect => assertEquals(r.gp.alpha, GeometryScene.DensityOpacity)
      case other        => fail(s"expected a rect, got $other")
    }
  }
