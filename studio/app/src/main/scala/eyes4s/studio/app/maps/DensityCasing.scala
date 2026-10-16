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
import eyes4s.studio.app.tokens.{Ground, PaletteToken, StageToken, StageVariant, Tokens, Wcag}

/** Where the placement density's cells wear the two-band stage casing
  * (DESIGN_SPEC section 15.1, "Contrast"; owner decision 2026-10-15, option
  * B of mote bd-01M4SJ6T6MMGAW1PRGREGVSX50): the stage `--halo` outside and
  * the inner casing ([[eyes4s.studio.app.tokens.TokenUsage.innerCasing]])
  * inside it, around the boundary between a level's occupied cells and empty
  * ones, only where it is needed.
  *
  * Density level `level` (1 to [[DensityPicture.Levels]]) is ramp stop
  * `level − 1`, drawn opaque. Its cells lie on the stage or on the image
  * frame's blank screen field ([[grounds]]). The rule is derived the same
  * way as [[StageCasing]], from the contrast computation and not tabulated,
  * so a token change re-derives it: a level on a stage is cased when its
  * fill falls below the 3:1 non-text minimum on some ground of that stage.
  */
object DensityCasing:

  /** The ramp stop of density level `level`, 1 to [[DensityPicture.Levels]];
    * a level outside that range takes the nearest stop.
    */
  def fill(level: Int): PaletteToken =
    Ramp((level - 1).max(0).min(Ramp.size - 1))

  private val Ramp: Vector[PaletteToken] =
    Vector(PaletteToken.Ramp0, PaletteToken.Ramp1, PaletteToken.Ramp2, PaletteToken.Ramp3)

  /** What a density cell on `stage` is drawn over: the stage around the image
    * frame, and the blank screen field inside it. The density picture draws
    * no stimulus and no map under its cells.
    */
  def grounds(stage: StageVariant): Vector[(String, Ground)] =
    Vector(
      "the stage"        -> Tokens.staged(stage, StageToken.Stage),
      "the blank screen" -> Tokens.palette(PaletteToken.Screen)
    ).flatMap((where, colour) => Ground.of(colour).map(where -> _))

  /** The first ground, if any, on which level `level`'s opaque fill on
    * `stage` falls below 3:1.
    */
  def failingGround(level: Int, stage: StageVariant): Option[String] =
    val colour = Tokens.palette(fill(level))
    grounds(stage).collectFirst {
      case (where, ground) if Wcag.contrastOn(colour, ground) < Wcag.NonTextMinimum => where
    }

  // Every level's answer on every stage, computed once.
  private lazy val table: Map[(Int, StageVariant), Boolean] =
    (for
      level <- (1 to DensityPicture.Levels).toList
      stage <- StageVariant.values.toList
    yield (level, stage) -> failingGround(level, stage).nonEmpty).toMap

  /** Whether level `level`'s cells on `stage` wear the two-band casing. */
  def needed(level: Int, stage: StageVariant): Boolean =
    table.getOrElse((level, stage), failingGround(level, stage).nonEmpty)

  /** Every level's answer on `stage`, lowest level first. */
  def levels(stage: StageVariant): Vector[Boolean] =
    Vector.tabulate(DensityPicture.Levels)(i => needed(i + 1, stage))
