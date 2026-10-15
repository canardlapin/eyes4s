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

import eyes4s.studio.app.maps.{ColourLimits, MapGrid, MapId, MapPalette, RowOrder}
import eyes4s.studio.app.tokens.{Colour, PaletteToken, StageVariant, Tokens}
import eyes4s.studio.core.backend.{Phase, RunId, ScreenRegion, TrialKey}
import eyes4s.studio.core.selection.ScaleIndex
import eyes4s.studio.desktop.harness.{SnapshotScale, StudioFxSuite, StudioTheme}
import eyes4s.studio.desktop.plot.{CanvasPlotHost, PlotFrame, PlotHostStatus}
import eyes4s.studio.viz.maps.MapTileScene
import eyes4s.studio.viz.plot.DataPoint
import javafx.geometry.Point2D
import javafx.scene.layout.StackPane

import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import scala.concurrent.duration.Duration

/** A figure map tile's cells without coverage on real JavaFX (slice S27;
  * DESIGN_SPEC section 15: never transparent): a block of missing cells many
  * rows tall, whose row edges are a fractional number of device pixels
  * apart, is opaque `fig-hatch` stripes on `fig-hatch-ground` throughout, at
  * two tile sizes and at output scales 1 and 2. Drawn as separate rects per
  * row, the stage showed through the seams.
  */
class MapTileHatchFxSuite extends StudioFxSuite:

  override val munitTimeout: Duration = Duration(120, "s")

  private val TimeoutMillis = 30000L

  /** How far, per channel, a pixel of the hatch may lie from an opaque mix
    * of its two inks: the canvas's rounding of an antialiased stripe edge
    * (as `MissingAssetHatchFxSuite`). A seam the stage showed through lay
    * far off it.
    */
  private val InkMixTolerance = 6

  /** Device pixels left out inside the missing block's edge, where its
    * antialiased boundary meets the covered cells around it.
    */
  private val EdgeInsetPx = 2

  private def right[E, A](either: Either[E, A]): A =
    either.fold(e => fail(s"unexpected Left: $e"), identity)

  private val Columns = 64
  private val Rows    = 48
  private val trial   = TrialKey("P17", Phase.Retrieval, "ret_07", 1)
  private val region  = right(ScreenRegion.of(trial, 448.0, 156.0, 1472.0, 924.0))

  // The missing block: columns 16 until 40, rows 12 until 32 from the top.
  private val (fromX, untilX, fromY, untilY) = (16, 40, 12, 32)

  /** Test values, not results: mass rising to the right, no levels (so no
    * isoline crosses the probe), with the block uncovered.
    */
  private val grid: MapGrid =
    right(
      MapGrid.of(
        MapId(RunId(7), trial, right(ScaleIndex.of(2))),
        Columns,
        Rows,
        RowOrder.TopFirst,
        Vector.tabulate(Columns * Rows) { i =>
          val (x, y) = (i % Columns, i / Columns)
          if x >= fromX && x < untilX && y >= fromY && y < untilY then None
          else Some(x / 63.0)
        },
        Vector.empty
      )
    )

  private val tile = right(
    MapTileScene.of(
      "figures.map.hatch",
      grid,
      ColourLimits.spanning(MapPalette.Mass, Vector(grid)),
      region,
      StageVariant.Dark
    )
  )

  private def awaitFrame(host: CanvasPlotHost, scale: Double): PlotFrame =
    val deadline = System.currentTimeMillis + TimeoutMillis
    def drawn    = runOnFx {
      host.status.get match
        case PlotHostStatus.Drawn(f)
            if f.surface.deviceScale == scale && f.surface.logicalWidth == host.getWidth &&
              f.surface.logicalHeight == host.getHeight =>
          Some(f)
        case _ => None
    }
    var result = drawn
    while result.isEmpty do
      if System.currentTimeMillis > deadline then
        fail(s"not drawn within $TimeoutMillis ms: ${runOnFx(host.status.get)}")
      Thread.sleep(2)
      result = drawn
    result.get

  // The snapshot pixel under screen position (x, y) of the tile.
  private def pixelAt(host: CanvasPlotHost, f: PlotFrame, x: Double, y: Double, k: Int) =
    val c = right(f.transform.dataToCanvas(DataPoint(x, y)))
    val s = runOnFx(host.localToScene(Point2D(c.x, c.y)))
    (math.floor(s.getX * k).toInt, math.floor(s.getY * k).toInt)

  private def rgb(image: BufferedImage, x: Int, y: Int): (Int, Int, Int) =
    val argb = image.getRGB(x, y)
    ((argb >> 16) & 0xff, (argb >> 8) & 0xff, argb & 0xff)

  // How far `c` lies from the nearest opaque mix of `a` and `b`, per channel.
  private def offMix(c: (Int, Int, Int), a: Colour, b: Colour): Double =
    val p = Vector(c._1, c._2, c._3)
    val q = Vector(a.red, a.green, a.blue)
    val d = Vector(b.red - a.red, b.green - a.green, b.blue - a.blue).map(_.toDouble)
    val t = (p.indices.map(i => (p(i) - q(i)) * d(i)).sum / d.map(e => e * e).sum)
      .max(0.0)
      .min(1.0)
    p.indices.map(i => math.abs(p(i) - (q(i) + t * d(i)))).max

  private def near(c: (Int, Int, Int), t: Colour, tol: Int) =
    math.abs(c._1 - t.red) <= tol && math.abs(c._2 - t.green) <= tol &&
      math.abs(c._3 - t.blue) <= tol

  fxStage.test("a tile's cells without coverage are opaque hatch at two sizes and scales") {
    fx =>
      val stripes = Tokens.palette(PaletteToken.FigHatch)
      val ground  = Tokens.palette(PaletteToken.FigHatchGround)
      val cellW   = region.width / Columns
      val cellH   = region.height / Rows
      // A desktop figure tile and a wider one; cells 2.8 and 5.2 layout
      // pixels, so the block's row edges fall inside device pixels.
      for
        w <- Vector(180.0, 333.0)
        k <- Vector(1, 2)
      do
        val h    = w * region.height / region.width
        val host = runOnFx {
          val host = CanvasPlotHost()
          host.setMaxSize(w, h)
          host
        }
        fx.show(runOnFx(StackPane(host)))
        runOnFx {
          host.setOutputScaleOverride(Some(k.toDouble))
          host.show(tile)
        }
        val f        = awaitFrame(host, k.toDouble)
        val scale    = if k == 1 then SnapshotScale.X1 else SnapshotScale.X2
        val img      = ImageIO.read(fx.snapshot(StudioTheme.Light, List(scale)).head.toFile)
        val (x0, y0) =
          pixelAt(host, f, region.left + fromX * cellW, region.top + fromY * cellH, k)
        val (x1, y1) =
          pixelAt(host, f, region.left + untilX * cellW, region.top + untilY * cellH, k)
        // Every device pixel inside the block: stripes and ground, never the
        // stage or the raster's transparent cell showing through a seam.
        val inside = for
          py <- (y0 + EdgeInsetPx) to (y1 - EdgeInsetPx)
          px <- (x0 + EdgeInsetPx) to (x1 - EdgeInsetPx)
        yield (px, py) -> rgb(img, px, py)
        val at   = s"${w.toInt} wide at scale $k"
        val seen = inside.map(_._2).distinct
        val off  = inside.filter((_, c) => offMix(c, ground, stripes) > InkMixTolerance)
        assert(inside.size > 100, s"$at: too few pixels inside the block: ${inside.size}")
        assert(off.isEmpty, s"$at: the hatch is not opaque at ${off.take(8)}")
        assert(seen.exists(near(_, stripes, 40)), s"$at: no hatch stripe: $seen")
        assert(seen.exists(near(_, ground, 24)), s"$at: no hatch ground: $seen")
        runOnFx(host.dispose())
  }
