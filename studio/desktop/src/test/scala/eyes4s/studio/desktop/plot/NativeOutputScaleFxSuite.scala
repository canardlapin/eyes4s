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

import cats.effect.unsafe.implicits.global
import eyes4s.studio.app.tokens.{Colour, Theme, ThemedToken, Tokens}
import eyes4s.studio.desktop.StudioMain
import eyes4s.studio.desktop.platform.JavaFxFonts
import eyes4s.studio.desktop.harness.{
  FxStage,
  SnapshotScale,
  StageSize,
  StudioFxSuite,
  StudioTheme
}
import eyes4s.studio.viz.plot.*
import javafx.scene.canvas.GraphicsContext
import javafx.scene.layout.StackPane
import javafx.scene.paint.Color
import javax.imageio.ImageIO
import java.nio.file.Files

/** The native window's scale, never a host override or a larger snapshot alone. */
class NativeOutputScaleFxSuite extends StudioFxSuite:
  override protected def stageSize: StageSize = StageSize(256, 192)

  private val ProbeColumns                = Vector(32, 64, 96)
  private val ProbeTop                    = 32
  private val ProbeHeight                 = 96
  private val MaxRasterDifferenceFraction = 0.005

  override def beforeAll(): Unit =
    super.beforeAll()
    val errors = StudioMain.loadFonts(JavaFxFonts).unsafeRunSync()
    assert(errors.isEmpty, errors.map(_.message).mkString("\n"))

  private def right[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private def rgb(c: Colour): Int                 = (c.red << 16) | (c.green << 8) | c.blue
  private def paint(c: Colour): Color             =
    Color.rgb(c.red, c.green, c.blue, c.alphaPercent / 100.0)

  private def awaitFrame(fx: FxStage, host: CanvasPlotHost, scale: Double): PlotFrame =
    def read: Option[PlotFrame] = runOnFx {
      host.status.get match
        case PlotHostStatus.Drawn(frame) if frame.surface.deviceScale == scale => Some(frame)
        case PlotHostStatus.Failed(error) => fail(error.toString)
        case _                            => None
    }
    val deadline = System.nanoTime() + java.util.concurrent.TimeUnit.SECONDS.toNanos(30)
    var answer   = read
    while answer.isEmpty && System.nanoTime() < deadline do
      fx.awaitLayout()
      answer = read
    answer.getOrElse(fail(s"no frame at native scale $scale: ${runOnFx(host.status.get)}"))

  fxStage.test("native output scale keeps one-device-pixel feedback sharp") { fx =>
    val native = runOnFx((fx.stage.getOutputScaleX, fx.stage.getOutputScaleY))
    sys.props.get("eyes4s.studio.fx.expectedOutputScale").foreach { requested =>
      val expected = requested.toDouble
      assertEquals(
        native,
        (expected, expected),
        "the native window did not acquire the required output scale"
      )
    }
    assert(
      Set(1.0, 2.0)(native._1),
      s"this raster fixture needs a 1x or 2x window, got $native"
    )
    assertEquals(native._1, native._2)
    val scale = if native._1 == 2.0 then SnapshotScale.X2 else SnapshotScale.X1
    val host  = runOnFx(CanvasPlotHost())
    try
      fx.show(runOnFx(StackPane(host)))
      runOnFx(host.show(right(ReferenceScene(Theme.Light))))
      val frame = awaitFrame(fx, host, native._1)
      assertEquals(runOnFx(host.outputScale), Some(native._1))
      assertEquals(
        runOnFx(host.canvasTexture),
        (frame.surface.deviceWidth, frame.surface.deviceHeight)
      )
      val origin = runOnFx(host.localToScene(0.0, 0.0))
      assertEquals((origin.getX, origin.getY), (0.0, 0.0))
      val background = Tokens.themed(Theme.Light, ThemedToken.Surface)
      val ink        = Tokens.themed(Theme.Light, ThemedToken.Ink)
      runOnFx(host.setOverlay(Some(new PlotOverlay:
        def paint(gc: GraphicsContext, frame: PlotFrame): Unit =
          gc.setFill(NativeOutputScaleFxSuite.this.paint(background))
          gc.fillRect(
            0.0,
            0.0,
            frame.surface.deviceWidth.toDouble,
            frame.surface.deviceHeight.toDouble
          )
          gc.setFill(NativeOutputScaleFxSuite.this.paint(ink))
          ProbeColumns.foreach(x =>
            gc.fillRect(x.toDouble, ProbeTop.toDouble, 1.0, ProbeHeight.toDouble)
          ))))
      fx.awaitLayout()
      val file  = fx.snapshot(StudioTheme.Light, List(scale)).head
      val image = ImageIO.read(file.toFile)
      assertEquals(
        (image.getWidth, image.getHeight),
        (frame.surface.deviceWidth, frame.surface.deviceHeight)
      )
      val row         = ProbeTop + ProbeHeight / 2
      val darkColumns = (0 until image.getWidth)
        .filter(x => (image.getRGB(x, row) & 0xffffff) == rgb(ink))
        .toVector
      assertEquals(
        darkColumns,
        ProbeColumns,
        "a device-pixel stripe was blurred, widened or misplaced"
      )
      ProbeColumns.foreach { x =>
        assertEquals(image.getRGB(x - 1, row) & 0xffffff, rgb(background))
        assertEquals(image.getRGB(x + 1, row) & 0xffffff, rgb(background))
      }
      var changed = 0L
      for y <- 0 until image.getHeight; x <- 0 until image.getWidth do
        val expected =
          if ProbeColumns.contains(x) && y >= ProbeTop && y < ProbeTop + ProbeHeight then
            rgb(ink)
          else rgb(background)
        if (image.getRGB(x, y) & 0xffffff) != expected then changed += 1
      val difference = changed.toDouble / (image.getWidth.toLong * image.getHeight).toDouble
      assert(
        difference <= MaxRasterDifferenceFraction,
        s"analytic raster difference $difference exceeds $MaxRasterDifferenceFraction"
      )
      val receipt =
        s"""{"nativeScaleX":${native._1},"nativeScaleY":${native._2},"requiredScale":${sys.props
            .getOrElse(
              "eyes4s.studio.fx.expectedOutputScale",
              "null"
            )},"hostOverride":false,"deviceWidth":${image.getWidth},"deviceHeight":${image.getHeight},"stripeWidthDevicePixels":1,"rasterDifferenceFraction":$difference,"maximumDifferenceFraction":$MaxRasterDifferenceFraction}
"""
      Files.writeString(file.getParent.resolve("native-output-scale.json"), receipt): Unit
    finally runOnFx(host.dispose())
  }
