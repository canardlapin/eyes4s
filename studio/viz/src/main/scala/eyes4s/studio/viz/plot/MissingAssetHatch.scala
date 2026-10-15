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

package eyes4s.studio.viz.plot

import eyes4s.studio.app.tokens.{PaletteToken, Theme, ThemedToken, Tokens}
import intaglio.{
  GraphicParams,
  GraphicsError,
  GraphicsName,
  Grob,
  PatternPaint,
  PatternRecipe,
  Point,
  Rgba,
  StrokeUnit
}

/** The missing-asset hatch (DESIGN_SPEC section 9: a missing asset is
  * hatched, never blank): warn-text stripes on warn-soft, with a warn-text X
  * across the frame. A trial's missing or unreadable image frame, a map's
  * uncovered cells and a placement thumbnail's missing frame all wear it,
  * each at its own [[MissingAssetHatch.Scale]]; a figure's map tile wears it
  * in the paper palette ([[MissingAssetHatch.paper]]).
  */
object MissingAssetHatch:

  /** The hatch's tile side and stripe width, in layout pixels. */
  final case class Scale(spacing: Double, lineWidth: Double) derives CanEqual

  /** The trial view's hatch (the Explore and Compare boards' frames). */
  val Trial: Scale = Scale(9.0, 3.0)

  /** The Data board draws a placement thumbnail's 1920-pixel screen 140
    * layout pixels wide.
    */
  val ThumbnailScale: Double = 140.0 / 1920.0

  /** The Data board's thumbnail hatch: an 80-pixel tile and 14-pixel
    * stripes of that screen, at [[ThumbnailScale]].
    */
  val Thumbnail: Scale = Scale(80.0 * ThumbnailScale, 14.0 * ThumbnailScale)

  /** The hatch's fill: 45° warn-text stripes over warn-soft. */
  def paint(theme: Theme, scale: Scale): Either[GraphicsError, GraphicParams] =
    painted(
      IntaglioColours.themed(theme, ThemedToken.WarnText),
      IntaglioColours.themed(theme, ThemedToken.WarnSoft),
      scale
    )

  /** The hatch's fill on paper, where a figure is drawn without an app
    * theme: 45° `fig-hatch` stripes over `fig-hatch-ground` (slice S27).
    */
  def paper(scale: Scale): Either[GraphicsError, GraphicParams] =
    painted(
      IntaglioColours.toIntaglio(Tokens.palette(PaletteToken.FigHatch)),
      IntaglioColours.toIntaglio(Tokens.palette(PaletteToken.FigHatchGround)),
      scale
    )

  private def painted(
      stripes: Rgba,
      ground: Rgba,
      scale: Scale
  ): Either[GraphicsError, GraphicParams] =
    for
      recipe <- PatternRecipe.angledHatch(45.0, scale.spacing, scale.lineWidth)
      base   <- GraphicParams.checked(stroke = None, fill = Some(ground))
    yield base.withPatternFill(PatternPaint(recipe, stripes, Some(ground)))

  /** The X: two warn-text strokes `widthPt` points wide crossing at
    * (`x`, `y`), each arm reaching `arm` native units from it along both
    * axes.
    */
  def cross(
      theme: Theme,
      x: Double,
      y: Double,
      arm: Double,
      widthPt: Double,
      name: Option[GraphicsName] = None
  ): Either[GraphicsError, Grob] =
    for
      gp <- GraphicParams.checked(
        stroke = Some(IntaglioColours.themed(theme, ThemedToken.WarnText)),
        fill = None,
        lineWidth = widthPt,
        lineWidthUnit = StrokeUnit.Point
      )
      a1    <- Point.native(x - arm, y - arm)
      b1    <- Point.native(x + arm, y + arm)
      a2    <- Point.native(x + arm, y - arm)
      b2    <- Point.native(x - arm, y + arm)
      cross <- Grob.segments(Vector(a1 -> b1, a2 -> b2), gp = gp, name = name)
    yield cross
