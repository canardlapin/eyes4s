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

package eyes4s.studio.app.maps

import eyes4s.studio.app.geometry.DensityPicture
import eyes4s.studio.app.tokens.{
  Colour,
  Ground,
  PaletteToken,
  StageToken,
  StageVariant,
  Theme,
  TokenUsage,
  Tokens,
  Wcag
}

/** What a placement density cell's edge lies on: the stage outside the image
  * frame, or the frame's blank screen field (`--screen`) inside it. The
  * density picture draws no stimulus and no map under its cells.
  */
enum DensityGround(val where: String) derives CanEqual:
  case Stage  extends DensityGround("the stage")
  case Screen extends DensityGround("the blank screen")

  /** This ground's colour on `stage`. */
  def colour(stage: StageVariant): Colour = this match
    case Stage  => Tokens.staged(stage, StageToken.Stage)
    case Screen => Tokens.palette(PaletteToken.Screen)

/** The one band a cased density edge wears: the stage `--halo`, or the inner
  * casing ([[TokenUsage.innerCasing]]).
  */
enum DensityBand derives CanEqual:
  case Halo, InnerCasing

  /** This band's colour on `stage`. */
  def colour(stage: StageVariant): Colour = this match
    case Halo => Tokens.staged(stage, StageToken.Halo)
    // The inner casing is a palette token: every theme resolves it alike.
    case InnerCasing => Tokens.resolve(TokenUsage.innerCasing(stage), Theme.Light, stage)

/** Where the placement density's cells wear a casing band, and which
  * (DESIGN_SPEC section 15.1, "Contrast"; owner decisions 2026-10-15 and
  * 2026-10-16, option B of mote bd-01M4SJ6T6MMGAW1PRGREGVSX50, quiet
  * variant).
  *
  * Density level `level` (1 to [[DensityPicture.Levels]]) is ramp stop
  * `level − 1`, drawn opaque. An edge between a level's occupied cell and an
  * empty one lies on one [[DensityGround]]. It is cased only where the
  * level's fill is below the 3:1 non-text minimum against that ground, and
  * then with one band, 1 px outside the cell: the band of [[DensityBand]]
  * that reaches 3:1 against the ground, the higher contrast if both do.
  *
  * The rule is derived from the contrast computation, the same way as
  * [[StageCasing]], and not tabulated, so a token change re-derives it.
  */
object DensityCasing:

  /** The ramp stop of density level `level`, 1 to [[DensityPicture.Levels]];
    * a level outside that range takes the nearest stop.
    */
  def fill(level: Int): PaletteToken =
    Ramp((level - 1).max(0).min(Ramp.size - 1))

  private val Ramp: Vector[PaletteToken] =
    Vector(PaletteToken.Ramp0, PaletteToken.Ramp1, PaletteToken.Ramp2, PaletteToken.Ramp3)

  private def on(colour: Colour, stage: StageVariant, ground: DensityGround): Double =
    Ground.of(ground.colour(stage)).fold(1.0)(Wcag.contrastOn(colour, _))

  /** The contrast of level `level`'s opaque fill against `ground` on `stage`. */
  def fillContrast(level: Int, stage: StageVariant, ground: DensityGround): Double =
    on(Tokens.palette(fill(level)), stage, ground)

  /** The contrast of `band` against `ground` on `stage`. */
  def bandContrast(band: DensityBand, stage: StageVariant, ground: DensityGround): Double =
    on(band.colour(stage), stage, ground)

  // The band of a level whose fill fails against a ground: the one that
  // carries the contrast, the higher if both do; if neither does, the higher
  // still (TokenContrastSuite rejects that).
  private def derive(
      level: Int,
      stage: StageVariant,
      ground: DensityGround
  ): Option[DensityBand] =
    Option.when(fillContrast(level, stage, ground) < Wcag.NonTextMinimum)(
      DensityBand.values.maxBy(bandContrast(_, stage, ground))
    )

  // Every answer, computed once.
  private lazy val table: Map[(Int, StageVariant, DensityGround), Option[DensityBand]] =
    (for
      level  <- (1 to DensityPicture.Levels).toList
      stage  <- StageVariant.values.toList
      ground <- DensityGround.values.toList
    yield (level, stage, ground) -> derive(level, stage, ground)).toMap

  /** The band level `level`'s edges against `ground` on `stage` wear, if any. */
  def band(level: Int, stage: StageVariant, ground: DensityGround): Option[DensityBand] =
    table.getOrElse((level, stage, ground), derive(level, stage, ground))

  /** Every level's band against `ground` on `stage`, lowest level first. */
  def levels(stage: StageVariant, ground: DensityGround): Vector[Option[DensityBand]] =
    Vector.tabulate(DensityPicture.Levels)(i => band(i + 1, stage, ground))
