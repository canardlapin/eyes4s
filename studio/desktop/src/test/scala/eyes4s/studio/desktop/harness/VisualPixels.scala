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

package eyes4s.studio.desktop.harness

import java.awt.image.BufferedImage
import java.nio.file.{Files, Path}
import javax.imageio.ImageIO

/** A bounded raster-rounding policy, separate from scientific numerical tolerances. */
object VisualPixels:
  val MaximumChannelDelta: Int       = 1
  val MaximumChangedFraction: Double = 0.001
  final case class Difference(width: Int, height: Int, changed: Long, maximumChannelDelta: Int):
    def changedFraction: Double = changed.toDouble / (width.toLong * height).toDouble
    def accepted: Boolean       =
      maximumChannelDelta <= MaximumChannelDelta && changedFraction <= MaximumChangedFraction
    def json: ujson.Value = ujson.Obj(
      "width"               -> width,
      "height"              -> height,
      "changedPixels"       -> changed.toDouble,
      "maximumChannelDelta" -> maximumChannelDelta,
      "changedFraction"     -> changedFraction,
      "accepted"            -> accepted
    )

  def read(path: Path): BufferedImage =
    val image = ImageIO.read(path.toFile)
    if image == null then throw AssertionError(s"Unreadable PNG: $path")
    image

  def compare(a: BufferedImage, b: BufferedImage): Either[String, Difference] =
    if a.getWidth != b.getWidth || a.getHeight != b.getHeight then
      Left(
        s"Wrong PNG dimensions: ${a.getWidth}x${a.getHeight} versus ${b.getWidth}x${b.getHeight}"
      )
    else
      var changed = 0L
      var maximum = 0
      var y       = 0
      while y < a.getHeight do
        var x = 0
        while x < a.getWidth do
          val p     = a.getRGB(x, y)
          val q     = b.getRGB(x, y)
          var delta = 0
          var shift = 0
          while shift < 32 do
            delta = math.max(delta, math.abs(((p >>> shift) & 255) - ((q >>> shift) & 255)))
            shift += 8
          if delta > 0 then changed += 1
          maximum = math.max(maximum, delta)
          x += 1
        y += 1
      Right(Difference(a.getWidth, a.getHeight, changed, maximum))

  def writeDiff(a: BufferedImage, b: BufferedImage, path: Path): Unit =
    val diff = BufferedImage(a.getWidth, a.getHeight, BufferedImage.TYPE_INT_ARGB)
    for y <- 0 until a.getHeight; x <- 0 until a.getWidth do
      diff.setRGB(x, y, if a.getRGB(x, y) == b.getRGB(x, y) then 0xff202020 else 0xffff3040)
    Files.createDirectories(path.getParent)
    if !ImageIO.write(diff, "png", path.toFile) then
      throw AssertionError(s"No PNG writer for $path")
