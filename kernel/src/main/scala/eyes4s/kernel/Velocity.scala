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

package eyes4s.kernel

/** A speed, in frame units per second.
  *
  * Carries its unit for the same reason [[Sigma]] does. A velocity threshold of
  * thirty is a statement about visual angle or about nothing: thirty pixels per
  * second is a different physical claim on every display, and the type refuses
  * to let one stand in for the other.
  */
opaque type Velocity[U <: Unit2D] = Double

object Velocity:

  def perSecond[U <: Unit2D](v: Double): Either[GeometryError, Velocity[U]] =
    if !v.isFinite then Left(GeometryError.NonFiniteVelocity(v))
    else if v < 0.0 then Left(GeometryError.NegativeVelocity(v))
    else Right(v)

  /** Degrees of visual angle per second, the conventional unit for oculomotor
    * thresholds.
    */
  def degPerSecond(v: Double): Either[GeometryError, Velocity[Unit2D.Deg]] =
    perSecond[Unit2D.Deg](v)

  def zero[U <: Unit2D]: Velocity[U] = 0.0

  /** The speed implied by covering a displacement in a span. */
  def over[U <: Unit2D](d: Vec2[U], t: Span): Option[Velocity[U]] =
    val s = t.toSeconds
    if s <= 0.0 then None else Some(d.norm / s)

  extension [U <: Unit2D](v: Velocity[U])
    def value: Double                         = v
    def render(using u: UnitLabel[U]): String = f"$v%.2f${u.symbol}/s"

  given [U <: Unit2D]: cats.kernel.Order[Velocity[U]] =
    cats.kernel.Order.from((a, b) => java.lang.Double.compare(a, b))

end Velocity

/** A non-negative length in frame units.
  *
  * Distinct from [[Extent]], which is a width and a height, and from [[Sigma]],
  * which is a standard deviation. This is a plain separation: a merge radius, a
  * saccade amplitude, a distance between two positions.
  *
  * Carries its unit for the usual reason. "Merge fixations closer than one" is
  * a different instruction in degrees and in pixels, and the two are not
  * convertible without the viewing geometry.
  */
opaque type Distance[U <: Unit2D] = Double

object Distance:

  def of[U <: Unit2D](v: Double): Either[GeometryError, Distance[U]] =
    if !v.isFinite then Left(GeometryError.NonFiniteDistance(v))
    else if v < 0.0 then Left(GeometryError.NegativeDistance(v))
    else Right(v)

  def deg(v: Double): Either[GeometryError, Distance[Unit2D.Deg]] = of[Unit2D.Deg](v)
  def px(v: Double): Either[GeometryError, Distance[Unit2D.Px]]   = of[Unit2D.Px](v)

  val zero: Distance[Nothing] = 0.0

  /** The separation between two positions. Always defined and non-negative. */
  def between[U <: Unit2D](a: Pt[U], b: Pt[U]): Distance[U] = a.distanceTo(b)

  extension [U <: Unit2D](d: Distance[U])
    def value: Double                         = d
    def <(that: Distance[U]): Boolean         = d < that
    def <=(that: Distance[U]): Boolean        = d <= that
    def render(using u: UnitLabel[U]): String = f"$d%.3g${u.symbol}"

  given [U <: Unit2D]: cats.kernel.Order[Distance[U]] =
    cats.kernel.Order.from((a, b) => java.lang.Double.compare(a, b))

end Distance
