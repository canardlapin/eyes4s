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

import intaglio.{FontWeight, HJust, LineCap, LineJoin, PatternPaint, RasterImage, VJust}
import intaglio.javafx.{JavaFxColor, JavaFxGraphicsContext}

import java.util.Locale
import scala.collection.mutable

/** A drawing context that records every call as one line of text: the op log.
  *
  * Numbers are written to four decimals in the root locale, so the log is the
  * same on every platform and JDK.
  */
final class RecordingContext extends JavaFxGraphicsContext:
  private val lines = mutable.ArrayBuffer.empty[String]

  /** The calls so far, one per line. */
  def log: Vector[String] = lines.toVector

  private def op(name: String, args: Any*): Unit =
    lines += args.map(RecordingContext.show).mkString(s"$name(", ", ", ")")

  override def save(): Unit                                           = op("save")
  override def restore(): Unit                                        = op("restore")
  override def translate(x: Double, y: Double): Unit                  = op("translate", x, y)
  override def rotateDegrees(degrees: Double): Unit                   = op("rotate", degrees)
  override def beginPath(): Unit                                      = op("beginPath")
  override def moveTo(x: Double, y: Double): Unit                     = op("moveTo", x, y)
  override def lineTo(x: Double, y: Double): Unit                     = op("lineTo", x, y)
  override def closePath(): Unit                                      = op("closePath")
  override def rect(x: Double, y: Double, w: Double, h: Double): Unit = op("rect", x, y, w, h)
  override def arcTo(x1: Double, y1: Double, x2: Double, y2: Double, r: Double): Unit =
    op("arcTo", x1, y1, x2, y2, r)
  override def clip(): Unit                                               = op("clip")
  override def fillPath(): Unit                                           = op("fill")
  override def strokePath(): Unit                                         = op("stroke")
  override def fillOval(x: Double, y: Double, w: Double, h: Double): Unit =
    op("fillOval", x, y, w, h)
  override def strokeOval(x: Double, y: Double, w: Double, h: Double): Unit =
    op("strokeOval", x, y, w, h)
  override def setFill(c: JavaFxColor): Unit                  = op("setFill", c)
  override def setPatternFill(pattern: PatternPaint): Boolean =
    op("setPatternFill", pattern.toString)
    false
  override def setStroke(c: JavaFxColor): Unit              = op("setStroke", c)
  override def setLineWidth(width: Double): Unit            = op("setLineWidth", width)
  override def setLineCap(cap: LineCap): Unit               = op("setLineCap", cap.toString)
  override def setLineJoin(join: LineJoin): Unit            = op("setLineJoin", join.toString)
  override def setLineDashes(pattern: Vector[Double]): Unit = op("setLineDashes", pattern*)
  override def setFont(
      family: Option[String],
      sizePx: Double,
      weight: Option[FontWeight]
  ): Unit =
    op("setFont", family.getOrElse("-"), sizePx, weight.fold("-")(_.toString))
  override def setTextAlign(h: HJust): Unit    = op("setTextAlign", h.toString)
  override def setTextBaseline(v: VJust): Unit = op("setTextBaseline", v.toString)
  override def fillText(label: String, x: Double, y: Double): Unit =
    op("fillText", s"\"$label\"", x, y)
  override def setGlobalAlpha(alpha: Double): Unit       = op("setGlobalAlpha", alpha)
  override def setImageSmoothing(enabled: Boolean): Unit = op("setImageSmoothing", enabled)
  override def drawImage(image: RasterImage, x: Double, y: Double, w: Double, h: Double): Unit =
    op("drawImage", s"${image.width}x${image.height}", x, y, w, h)

object RecordingContext:
  private def show(value: Any): String = value match
    case d: Double =>
      val s = String.format(Locale.ROOT, "%.4f", d)
      if s == "-0.0000" then "0.0000" else s
    case c: JavaFxColor => s"rgba(${c.red},${c.green},${c.blue},${show(c.alpha)})"
    case other          => other.toString
