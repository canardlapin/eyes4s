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

package eyes4s.plan

import eyes4s.kernel.*

/** A value along each frame axis. */
final case class PerAxis(x: Double, y: Double) derives CanEqual:
  def min: Double                       = math.min(x, y)
  def max: Double                       = math.max(x, y)
  def map(f: Double => Double): PerAxis = PerAxis(f(x), f(y))

/** Derived facts of one estimation scale. Absent for a binned scale, which
  * has no bandwidth.
  *
  *  - `sigma`: the standard deviation in frame units, along each axis;
  *  - `degrees`: the same in degrees, when the plan declares units per degree;
  *  - `cells`: the same in grid cells;
  *  - `extent`: the same as a fraction of the mapped region's width and height.
  */
final case class ScaleFacts(
    index: Int,
    sigma: PerAxis,
    degrees: Option[PerAxis],
    cells: PerAxis,
    extent: PerAxis
) derives CanEqual

/** Read-only facts a study plan implies, for a form to show beside its fields:
  * the grid cell in frame units and in degrees, and each scale's bandwidth in
  * frame units, degrees, cells and as a fraction of the mapped region.
  */
final case class StudyFacts(
    unit: PlanarUnit,
    cell: PerAxis,
    cellDegrees: Option[PerAxis],
    scales: Vector[ScaleFacts]
) derives CanEqual

/** A warning about a study recipe that is valid but likely unintended, keyed
  * by the [[StudyField]] it concerns and naming its operands.
  */
enum StudyAdvisory derives CanEqual:
  /** The scale's bandwidth spans fewer grid cells than `minimumCells` on some
    * axis, so the grid undersamples the kernel.
    */
  case SigmaBelowCells(scale: Int, cells: Double, minimumCells: Double)

  /** The scale's bandwidth is at least `limit` of the mapped region's extent
    * on some axis, so the maps are close to uniform.
    */
  case SigmaNearUniform(scale: Int, fraction: Double, limit: Double)

  def field: StudyField = StudyField.Scales

  def message: String = this match
    case SigmaBelowCells(i, cells, min) =>
      s"Scale $i: sigma spans ${Display.decimals(cells, 2)} grid cells, fewer than ${Display.decimals(min, 2)}; the grid undersamples it."
    case SigmaNearUniform(i, fraction, limit) =>
      s"Scale $i: sigma is ${Display.decimals(fraction, 2)} of the mapped region, at least ${Display.decimals(limit, 2)}; maps are close to uniform."

/** Derived facts and advisories of a study plan. `MinimumSigmaCells` is the
  * Studio design's rule (DESIGN_SPEC section 9: warn when sigma is under 2
  * cells; on the fixture's 64x48 grid over a 1024x768 px image at 35 px per
  * degree, 0.5 degrees is 1.1 cells). `NearUniformFraction` is eyes4s's
  * choice: the design gives only the example of 8 degrees (280 px, 0.36 of
  * the image height) as near-uniform, and 0.25 is chosen so that example
  * warns while 4 degrees (0.18) does not.
  */
object StudyAdvice:
  /** Warn when a bandwidth spans fewer grid cells than this on some axis. */
  val MinimumSigmaCells: Double = 2.0

  /** Warn when a bandwidth is at least this fraction of the mapped region's
    * width or height.
    */
  val NearUniformFraction: Double = 0.25

  def facts[K, U <: Unit2D, P, S, D](plan: StudyPlan[K, U, P, S, D])(using
      u: UnitLabel[U]
  ): StudyFacts =
    val frame               = plan.grid.frame
    val cell                = PerAxis(frame.width / plan.grid.nx, frame.height / plan.grid.ny)
    def degrees(v: PerAxis) = plan.angularScale.map(s => v.map(_ / s.unitsPerDegree))
    val scales              = plan.estimates.zipWithIndex.flatMap { (estimate, i) =>
      val sigma = estimate match
        case StudyEstimate.Binned()             => None
        case StudyEstimate.Gaussian(s, _)       => Some(PerAxis(s.value, s.value))
        case StudyEstimate.Anisotropic(x, y, _) => Some(PerAxis(x.value, y.value))
      sigma.map(s =>
        ScaleFacts(
          i,
          s,
          degrees(s),
          PerAxis(s.x / cell.x, s.y / cell.y),
          PerAxis(s.x / frame.width, s.y / frame.height)
        )
      )
    }
    StudyFacts(u.planar, cell, degrees(cell), scales)

  def advisories[K, U <: Unit2D: UnitLabel, P, S, D](
      plan: StudyPlan[K, U, P, S, D]
  ): Vector[StudyAdvisory] =
    facts(plan).scales.flatMap { s =>
      Option
        .when(s.cells.min < MinimumSigmaCells)(
          StudyAdvisory.SigmaBelowCells(s.index, s.cells.min, MinimumSigmaCells)
        )
        .toVector ++
        Option
          .when(s.extent.max >= NearUniformFraction)(
            StudyAdvisory.SigmaNearUniform(s.index, s.extent.max, NearUniformFraction)
          )
          .toVector
    }

/** Rounded decimal text for prose, identical on the JVM and Scala.js. */
private[plan] object Display:
  /** `x` rounded half-even to `places` decimals, without trailing zeros. */
  def decimals(x: Double, places: Int): String =
    val rounded =
      new java.math.BigDecimal(Numeral.real.write(x))
        .setScale(places, java.math.RoundingMode.HALF_EVEN)
        .stripTrailingZeros
    if rounded.signum == 0 then "0" else rounded.toPlainString
