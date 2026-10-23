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

package eyes4s.studio.desktop.trial

import eyes4s.studio.app.text.{TrialText, TrialTextId}
import eyes4s.studio.app.tokens.{FontFace, TypeSize}
import eyes4s.studio.viz.trial.{FrameArt, TrialScene, TrialSceneInput}
import javafx.scene.text.{Font, Text}

/** Choose the compact two-column caption only when actual font metrics
  * leave a gap. Narrow panels wrap every clause, including the placement
  * and remembered-image disclosure, into separate Intaglio text lines.
  */
private[trial] object TrialCaptions:
  def lines(input: TrialSceneInput, width: Double): Option[Vector[String]] =
    if width <= 16.0 || !input.options.captionBand then None
    else
      val text = Text()
      text.setFont(Font.font(FontFace.SansRegular.javaFxFamily, TypeSize.T11.px.toDouble))
      def measure(s: String): Double =
        text.setText(s)
        text.getLayoutBounds.getWidth
      val caption = TrialScene.captionOf(input, TrialScene.frameArtOf(input))
      val area    = Option.when(input.options.points && input.fixations.nonEmpty)(
        TrialText(TrialTextId.MarkerArea)
      )
      val order = Option.when(input.options.order && input.fixations.size >= 2)(
        TrialText(TrialTextId.OrderNote)
      )
      val remembered                                          = TrialScene.disclosureOf(input)
      val available                                           = width - 16.0
      def fits(a: Option[String], b: Option[String]): Boolean =
        a.fold(0.0)(measure) + b.fold(0.0)(measure) + (if a.nonEmpty && b.nonEmpty then 8.0
                                                       else 0.0) <= available
      if fits(Some(caption), area) && fits(order, remembered) then None
      else
        def wrap(s: String): Vector[String] =
          val lines = Vector.newBuilder[String]
          var line  = ""
          s.split(" ").foreach { word =>
            val combined = if line.isEmpty then word else s"$line $word"
            if measure(combined) <= available then line = combined
            else
              if line.nonEmpty then lines += line
              line = ""
              word.codePoints().toArray.foreach { code =>
                val ch = String(Character.toChars(code))
                if line.nonEmpty && measure(line + ch) > available then
                  lines += line
                  line = ch
                else line += ch
              }
          }
          if line.nonEmpty then lines += line
          lines.result()
        val art    = TrialScene.frameArtOf(input)
        val window = Option.when(
          input.options.windowOutline && art != FrameArt.Unknown &&
            !art.isInstanceOf[FrameArt.Loading]
        ) {
          TrialText(
            TrialTextId.WindowLabel,
            input.display.placement.width.toString,
            input.display.placement.height.toString
          )
        }
        Some((Vector(caption) ++ window ++ area ++ order ++ remembered).flatMap(wrap))
