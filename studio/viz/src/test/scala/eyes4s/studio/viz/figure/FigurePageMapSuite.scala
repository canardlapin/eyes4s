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

package eyes4s.studio.viz.figure

import eyes4s.studio.app.figures.*
import eyes4s.studio.app.maps.{
  ColourLimits,
  Isolines,
  MapColours,
  MapGrid,
  MapId,
  MapPalette,
  MapStyle,
  RowOrder
}
import eyes4s.studio.core.backend.{Phase, RunId, ScreenRegion, TrialKey}
import eyes4s.studio.core.document.{FigureId, PanelLetter}
import eyes4s.studio.core.selection.{ScaleIndex, StudioRef}
import eyes4s.studio.viz.maps.MapTileScene
import intaglio.{ExtentExpr, Grob, LengthExpr, alpha, blue, green, red, value}

/** Figure export keeps panel C's served map cells, limits and isolines rather
  * than reducing a drawn tile to its score label.
  */
class FigurePageMapSuite extends munit.FunSuite:

  private def right[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)

  private val trial  = TrialKey("P17", Phase.Retrieval, "ret_07", 1)
  private val scale  = right(ScaleIndex.of(2))
  private val region = right(ScreenRegion.of(trial, 100.0, 200.0, 500.0, 400.0))
  private val grid   = right(
    MapGrid.of(
      MapId(RunId(7), trial, scale),
      columns = 2,
      rows = 2,
      RowOrder.TopFirst,
      Vector(Some(0.0), Some(1.0), Some(0.25), Some(0.75)),
      Vector(0.5)
    )
  )
  // The full shared limit is deliberately above the served map's peak.
  private val style = right(
    ColourLimits.sequential(0.0, 2.0).flatMap(MapStyle.of(MapPalette.Mass, _))
  )

  private val page =
    PageVM(
      right(FigureId.of(1)),
      "Figure 1",
      PageWidth.SingleColumn,
      "Page 89 mm · one-column",
      "100%",
      3.5,
      Vector(
        PanelVM(
          right(PanelLetter.of("C")),
          "Density maps",
          selected = true,
          widthMm = 89,
          PanelBody.Maps(
            DensityMapsVM(
              Vector(
                MapTileVM(
                  "Query ret_07",
                  "P17 · 2°",
                  StudioRef.Trial(trial),
                  trial,
                  TileMap.Drawn(grid, style, region)
                )
              ),
              "caption",
              "shared limits"
            )
          ),
          StudioRef.Trial(trial)
        )
      ),
      ComposerTab.Figure,
      None,
      "Open in Compare",
      "caption",
      "stamp",
      7,
      AppearanceVM(Vector.empty, Vector.empty, None, ("images", true)),
      greyscale = false,
      ExportVM(Vector.empty, "Export", None),
      AddPanelVM("Add panel", Vector.empty)
    )

  private def grobs(g: Grob): Vector[Grob] = g +: g.children.flatMap(grobs)

  private def native(e: LengthExpr): Double = e match
    case LengthExpr.Const(length) => length.value
    case other                    => fail(s"not a native length: $other")

  private def extent(e: ExtentExpr): Double = e match
    case ExtentExpr(LengthExpr.Const(length)) => length.value
    case other                                => fail(s"not a native extent: $other")

  test("a drawn density tile exports its served cells, shared style and served contour") {
    val all = right(FigurePage.build(page)).scene.grobs.flatMap(grobs)
    val map = all
      .collectFirst {
        case image: Grob.Image if image.name.exists(_.value == MapTileScene.MapName) =>
          image.image
      }
      .getOrElse(fail("no exported map raster"))
    assertEquals((map.width, map.height), (grid.columns, grid.rows))
    def argb(x: Int, y: Int) =
      val pixel = map.pixelUnsafe(x, y)
      (pixel.red << 16) | (pixel.green << 8) | pixel.blue | (pixel.alpha << 24)
    assertEquals(argb(0, 0), MapColours.argb(style, Some(0.0)))
    assertEquals(argb(1, 0), MapColours.argb(style, Some(1.0)))
    assertEquals(argb(0, 1), MapColours.argb(style, Some(0.25)))
    assertEquals(argb(1, 1), MapColours.argb(style, Some(0.75)))
    assertNotEquals(argb(1, 0), MapColours.argb(style, Some(2.0)))

    val contours = all
      .collectFirst {
        case lines: Grob.Segments if lines.name.exists(_.value == MapTileScene.IsolinesName) =>
          lines
      }
      .getOrElse(fail("no exported served contour"))
    assertEquals(contours.segments.size, Isolines.of(grid).flatMap(_.segments).size)

    val tile = all
      .collectFirst {
        case group: Grob.Group
            if group.viewport.nonEmpty && group.children.exists(_.isInstanceOf[Grob.Group]) &&
              group.children.flatMap(grobs).exists {
                case image: Grob.Image => image.name.exists(_.value == MapTileScene.MapName)
                case _                 => false
              } =>
          group
      }
      .getOrElse(fail("no map export viewport"))
    val viewport = tile.viewport.getOrElse(fail("map group has no viewport"))
    assertEquals(
      extent(viewport.size.width) / extent(viewport.size.height),
      region.width / region.height
    )

    val score = all
      .collectFirst { case text: Grob.Text if text.label == "P17 · 2°" => text }
      .getOrElse(fail("no map score label"))
    val box = all
      .collectFirst { case rect: Grob.Rect if rect.gp.stroke.nonEmpty => rect }
      .getOrElse(fail("no map tile box"))
    val scoreY = native(score.at.y)
    assertEquals(scoreY, native(viewport.origin.y))
    assert(
      scoreY <= native(box.center.y) && scoreY >= native(box.center.y) - extent(box.size.height)
    )
  }
