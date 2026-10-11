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

import eyes4s.studio.app.tokens.Greyscale
import javafx.application.Platform
import javafx.beans.value.ChangeListener
import javafx.geometry.Rectangle2D
import javafx.scene.image.{ImageView, PixelFormat, WritableImage}
import javafx.scene.layout.Region
import javafx.scene.paint.Color
import javafx.scene.transform.Transform
import javafx.scene.{Scene, SnapshotParameters}

/** The greyscale check of the paper (S9.3): an image of `target` in
  * Rec. 709 greys, computed in linear light ([[Greyscale]]), drawn over it.
  *
  * JavaFX's `ColorAdjust` desaturates in HSB, which keeps roughly the
  * largest channel, so the figure's matched and control marks would print as
  * nearly one grey. No built-in effect decodes sRGB, so the check renders
  * the target and converts its pixels.
  *
  * [[node]] is laid over the target and lets every mouse event through to
  * it, so the page stays live under the check. While the check is on, it
  * follows the target: after every layout pulse it renders the target again
  * and replaces the image when a pixel changed (a plot drawn, a stimulus
  * read), so a quiet page is not redrawn.
  */
final class GreyscaleProof(target: Region):

  /** The greys, over the target; empty and hidden while the check is off. */
  val node: ImageView = ImageView()
  node.setMouseTransparent(true)
  node.setManaged(false)
  node.setSmooth(true)
  node.setVisible(false)
  node.layoutXProperty.bind(target.layoutXProperty)
  node.layoutYProperty.bind(target.layoutYProperty)
  node.fitWidthProperty.bind(target.widthProperty)
  node.fitHeightProperty.bind(target.heightProperty)

  private var on: Boolean                         = false
  private var queued: Boolean                     = false
  private var greys: Array[Int]                   = Array.emptyIntArray
  private var size: (Int, Int)                    = (0, 0)
  private var scene: Option[Scene]                = None
  private var redrawn: Int                        = 0
  private val afterLayout: Runnable               = () => onLayout()
  private val sceneChanged: ChangeListener[Scene] = (_, _, now) => follow(Option(now))
  target.sceneProperty.addListener(sceneChanged)
  follow(Option(target.getScene))

  /** True while the check is shown. */
  def isOn: Boolean = on

  /** How many times the image has been replaced; for tests. */
  private[figures] def redraws: Int = redrawn

  /** Turns the check on or off. Call on the FX thread. */
  def show(greyscale: Boolean): Unit =
    if greyscale != on then
      on = greyscale
      node.setVisible(on)
      if on then refresh()
      else
        node.setImage(null)
        greys = Array.emptyIntArray
        size = (0, 0)

  /** Stops following the target. Idempotent. */
  def dispose(): Unit =
    show(false)
    target.sceneProperty.removeListener(sceneChanged)
    follow(None)

  // Converting during the pulse would render inside it: the refresh runs
  // after it, once however many pulses passed meanwhile.
  private def onLayout(): Unit =
    if on && !queued then
      queued = true
      Platform.runLater { () =>
        queued = false
        refresh()
      }

  private def follow(now: Option[Scene]): Unit =
    scene.foreach(_.removePostLayoutPulseListener(afterLayout))
    scene = now
    scene.foreach(_.addPostLayoutPulseListener(afterLayout))

  /** Renders the target and converts it; replaces the image only on a change. */
  private def refresh(): Unit =
    val w = target.getWidth
    val h = target.getHeight
    if on && target.getScene != null && w > 0 && h > 0 then
      val scale  = Option(target.getScene.getWindow).fold(1.0)(_.getOutputScaleX)
      val pw     = math.max(1, math.ceil(w * scale).toInt)
      val ph     = math.max(1, math.ceil(h * scale).toInt)
      val params = SnapshotParameters()
      params.setTransform(Transform.scale(scale, scale))
      params.setViewport(Rectangle2D(0, 0, pw.toDouble, ph.toDouble))
      params.setFill(Color.TRANSPARENT)
      val shot   = target.snapshot(params, null)
      val sw     = shot.getWidth.toInt
      val sh     = shot.getHeight.toInt
      val pixels = new Array[Int](sw * sh)
      shot.getPixelReader.getPixels(0, 0, sw, sh, PixelFormat.getIntArgbInstance, pixels, 0, sw)
      var i = 0
      while i < pixels.length do
        pixels(i) = Greyscale.argb(pixels(i))
        i += 1
      if size != (sw, sh) || !java.util.Arrays.equals(pixels, greys) then
        val image = WritableImage(sw, sh)
        image.getPixelWriter.setPixels(
          0,
          0,
          sw,
          sh,
          PixelFormat.getIntArgbInstance,
          pixels,
          0,
          sw
        )
        greys = pixels
        size = (sw, sh)
        redrawn += 1
        node.setImage(image)
