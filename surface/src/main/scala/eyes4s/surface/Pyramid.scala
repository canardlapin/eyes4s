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

package eyes4s.surface

import eyes4s.kernel.*

/** The same measure smoothed at several bandwidths.
  *
  * ==The scales are in the value==
  *
  * `eyesim` represents this as a bare list with the bandwidths in an attribute,
  * which its own operations then lose: the latent transforms silently take
  * element one and discard the rest, and comparison matches scales by looking
  * up a parallel vector that may have been filtered. Here a scale and its
  * surface cannot be separated, so neither can happen.
  *
  * Scales are held in ascending order, and two pyramids compare only when their
  * scale sets agree exactly -- a comparison across mismatched bandwidths is a
  * different quantity, not a slightly noisier version of the same one.
  */
final class Pyramid[U <: Unit2D] private (
    val grid: Grid[U],
    val levels: Vector[(Sigma[U], Mass[U])]
):

  def scales: Vector[Sigma[U]]  = levels.map(_._1)
  def surfaces: Vector[Mass[U]] = levels.map(_._2)
  def size: Int                 = levels.length

  def at(s: Sigma[U]): Option[Mass[U]] = levels.find(_._1.value == s.value).map(_._2)

  def finest: Mass[U]   = levels.head._2
  def coarsest: Mass[U] = levels.last._2

  /** Scales shared with another pyramid, in order. */
  def commonScales(that: Pyramid[U]): Vector[Sigma[U]] =
    scales.filter(s => that.scales.exists(_.value == s.value))

  def render: String =
    s"pyramid(${levels.length} scales ${scales.map(_.value).mkString(", ")} over ${grid.id})"

end Pyramid

object Pyramid:

  /** Build by smoothing one measure at several bandwidths.
    *
    * Duplicate bandwidths are rejected rather than deduplicated: a caller who
    * asked for the same scale twice has a bug upstream, and quietly collapsing
    * it hides that.
    */
  def of[U <: Unit2D](
      m: PointMeasure[U],
      g: Grid[U],
      sigmas: Vector[Sigma[U]],
      edges: EdgePolicy
  ): Either[EstimateError, Pyramid[U]] =
    if sigmas.isEmpty then Left(EstimateError.NoMass)
    else if sigmas.map(_.value).distinct.length != sigmas.length then
      Left(EstimateError.DegenerateBandwidth(sigmas.head.value, 0.0))
    else
      val ordered = sigmas.sortBy(_.value)
      ordered
        .foldLeft[Either[EstimateError, Vector[(Sigma[U], Mass[U])]]](Right(Vector.empty)) {
          (acc, s) =>
            for
              done <- acc
              mass <- Smoother.gaussian(s, edges).density(m, g)
            yield done :+ (s -> mass)
        }
        .map(new Pyramid(g, _))

end Pyramid
