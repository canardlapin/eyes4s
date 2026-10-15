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

package eyes4s.studio.desktop.plot

import eyes4s.studio.app.geometry.{FramePicture, FrameState, ThumbnailPicture}
import eyes4s.studio.app.tokens.{Colour, StageVariant, Theme, ThemedToken, Tokens}
import eyes4s.studio.core.assets.AssetFile
import eyes4s.studio.core.backend.{Phase, TrialKey}
import eyes4s.studio.desktop.harness.{SnapshotScale, StudioFxSuite, StudioTheme}
import eyes4s.studio.viz.geometry.GeometryScene
import eyes4s.studio.viz.plot.DataPoint
import javafx.geometry.Point2D
import javafx.scene.layout.StackPane

import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import scala.concurrent.duration.Duration

/** The missing-asset hatch on real JavaFX (DESIGN_SPEC section 9: a missing
  * asset is hatched, never blank): the Data board's placement thumbnail of a
  * missing image, whose hatch tile is a fractional number of device pixels,
  * is opaque warn-text stripes on warn-soft at every thumbnail size and at
  * output scales 1 and 2. The trial map's hatch is probed in
  * `TrialViewMapFxSuite`.
  */
class MissingAssetHatchFxSuite extends StudioFxSuite:

  override val munitTimeout: Duration = Duration(120, "s")

  private val TimeoutMillis = 30000L

  /** How far, per channel, a pixel of the hatch may lie from an opaque mix
    * of its two inks: the canvas's rounding of an antialiased stripe edge. A
    * hatch the screen showed through at 75% lay about 25 off it.
    */
  private val InkMixTolerance = 6

  /** Device pixels left out inside the image frame's edge, where its dashed
    * outline is drawn.
    */
  private val OutlineInsetPx = 3

  private def right[E, A](either: Either[E, A]): A =
    either.fold(e => fail(s"unexpected Left: $e"), identity)

  private val frame  = FramePicture(1920, 1080, 448, 156, 1024, 768)
  private val forest = right(AssetFile.of("forest-044.png"))
  private val trial  = TrialKey("P01", Phase.Encoding, "enc_08", 1)

  private def missing(theme: Theme) =
    right(
      GeometryScene.thumbnail(
        "geometry.thumb.hatch",
        theme,
        StageVariant.Dark,
        frame,
        ThumbnailPicture(trial, 0, 0, 0, Vector.empty, FrameState.Missing(forest))
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

  // The snapshot pixel under screen position (x, y) of the thumbnail.
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

  List(StudioTheme.Light -> Theme.Light, StudioTheme.Dark -> Theme.Dark).foreach {
    (studioTheme, theme) =>
      fxStage.test(
        s"a missing image's thumbnail hatch is opaque at every size and scale (${studioTheme.id})"
      ) { fx =>
        val warnText = Tokens.themed(theme, ThemedToken.WarnText)
        val warnSoft = Tokens.themed(theme, ThemedToken.WarnSoft)
        // The Data board's 164 × 92 thumbnail and sizes around it.
        for
          (w, h) <- Vector((164.0, 92.0), (140.0, 79.0), (237.0, 133.0), (411.0, 231.0))
          k      <- Vector(1, 2)
        do
          val host = runOnFx {
            val host = CanvasPlotHost()
            host.setMaxSize(w, h)
            host
          }
          fx.show(runOnFx(StackPane(host)))
          runOnFx {
            host.setOutputScaleOverride(Some(k.toDouble))
            host.show(missing(theme))
          }
          val f        = awaitFrame(host, k.toDouble)
          val scale    = if k == 1 then SnapshotScale.X1 else SnapshotScale.X2
          val img      = ImageIO.read(fx.snapshot(studioTheme, List(scale)).head.toFile)
          val (x0, y0) = pixelAt(host, f, frame.left.toDouble, frame.top.toDouble, k)
          val (x1, y1) = pixelAt(
            host,
            f,
            (frame.left + frame.width).toDouble,
            (frame.top + frame.height).toDouble,
            k
          )
          // Every device pixel inside the frame: stripes, ground and the X,
          // never the screen or the stage showing through.
          val inside = for
            py <- (y0 + OutlineInsetPx) to (y1 - OutlineInsetPx)
            px <- (x0 + OutlineInsetPx) to (x1 - OutlineInsetPx)
          yield (px, py) -> rgb(img, px, py)
          val at   = s"${w.toInt} × ${h.toInt} at scale $k"
          val seen = inside.map(_._2).distinct
          val off  = inside.filter((_, c) => offMix(c, warnSoft, warnText) > InkMixTolerance)
          assert(inside.nonEmpty, s"$at: no pixel inside the frame")
          assert(off.isEmpty, s"$at: the hatch is not opaque at ${off.take(8)}")
          assert(seen.exists(near(_, warnText, 40)), s"$at: no hatch stroke: $seen")
          assert(seen.exists(near(_, warnSoft, 24)), s"$at: no hatch ground: $seen")
          runOnFx(host.dispose())
      }
  }
