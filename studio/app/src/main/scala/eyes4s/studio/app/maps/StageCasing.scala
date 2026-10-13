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

import eyes4s.studio.app.tokens.{
  Ground,
  PaletteToken,
  StageToken,
  StageVariant,
  Theme,
  TokenRef,
  TokenUsage,
  Tokens,
  Wcag
}

/** Where a stage mark wears its inner casing ([[TokenUsage.innerCasing]];
  * DESIGN_SPEC section 15, "Stage casing"; review ticket 17): only where it
  * is needed (owner decision 2026-10-13), so the boards' lighter marks stay
  * wherever contrast allows.
  *
  * The rule is derived from the contrast computation, not tabulated, so a
  * token change re-derives it: a mark in a theme on a stage wears the inner
  * casing when, on some [[frameGrounds]] ground, neither the mark nor the
  * stage halo reaches the 3:1 non-text minimum. One answer per mark token,
  * theme and stage: a scene draws one mark style, so it is one style per
  * scene.
  */
object StageCasing:

  /** Steps along the mass ramp, from zero to the upper limit. */
  val RampSteps: Int = 100

  // The mass ramp over the unit range, as a trial map draws it.
  private val unit: MapStyle = MapStyle(MapPalette.Mass, ColourLimits.Sequential(0.0, 1.0))

  /** What a mark on `stage` can be drawn over inside the image frame and
    * around it: the blank screen and the stage itself, each bare and under
    * every mass-map colour in [[RampSteps]] steps along the limits, faded
    * in by the alpha floor and drawn at [[MapOpacity.Default]].
    */
  def frameGrounds(stage: StageVariant): Vector[(String, Ground)] =
    val bases = Vector(
      "the blank screen" -> Tokens.palette(PaletteToken.Screen),
      "the stage"        -> Tokens.staged(stage, StageToken.Stage)
    )
    for
      (where, colour) <- bases
      base            <- Ground.of(colour).toVector
      step            <- (-1 to RampSteps).toVector
    yield
      if step < 0 then where -> base
      else
        val t    = step.toDouble / RampSteps
        val cell = MapOpacity.Default.over(MapColours.argb(unit, Some(t)))
        f"$where under the map at $t%.2f" -> base.under(
          ((cell >> 16) & 0xff).toDouble,
          ((cell >> 8) & 0xff).toDouble,
          (cell & 0xff).toDouble,
          ((cell >>> 24) & 0xff) / 255.0
        )

  /** The first ground, if any, on which `mark` in `theme` on `stage` falls
    * below 3:1 with only its halo around it.
    */
  def failingGround(mark: TokenRef, theme: Theme, stage: StageVariant): Option[String] =
    val fill = Tokens.resolve(mark, theme, stage)
    val halo = Tokens.staged(stage, StageToken.Halo)
    frameGrounds(stage).collectFirst {
      case (where, ground)
          if Wcag.contrastOn(fill, ground) < Wcag.NonTextMinimum &&
            Wcag.contrastOn(halo, ground) < Wcag.NonTextMinimum =>
        where
    }

  // Every stage mark's answer, computed once.
  private lazy val table: Map[(TokenRef, Theme, StageVariant), Boolean] =
    (for
      mark  <- TokenUsage.innerCased
      theme <- Theme.values.toList
      stage <- StageVariant.values.toList
    yield (mark, theme, stage) -> failingGround(mark, theme, stage).nonEmpty).toMap

  /** Whether `mark` in `theme` on `stage` wears the inner casing. */
  def needed(mark: TokenRef, theme: Theme, stage: StageVariant): Boolean =
    table.getOrElse((mark, theme, stage), failingGround(mark, theme, stage).nonEmpty)
