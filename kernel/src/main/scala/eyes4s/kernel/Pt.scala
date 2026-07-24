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

/** A position in the plane, tagged with the unit it is expressed in.
  *
  * ==Why the unit is on the point==
  *
  * The architecture note said points would be bare and frames would live on
  * collections, on the grounds that per-point frames are slow. That reasoning
  * applies to storing a `Frame` *reference* per point; it does not apply to a
  * phantom type parameter, which is erased and costs nothing at runtime. Since
  * it is free, tagging closes a real hole: `Region[Deg].contains` would
  * otherwise accept a pixel position without complaint.
  *
  * Frame *identity* still lives on collections, exactly as designed. `Pt[Deg]`
  * says "this is in degrees"; it does not say which degrees.
  *
  * ==Points and displacements are different things==
  *
  * `Pt` and [[Vec2]] form an affine space: the difference of two positions is a
  * displacement, a position plus a displacement is a position, and two
  * positions cannot be added at all. Summing two gaze positions is meaningless,
  * and here it does not compile. Saccade amplitude and direction are properties
  * of the [[Vec2]] between two fixations, which is where they belong.
  */
final case class Pt[U <: Unit2D](x: Double, y: Double) derives CanEqual:

  def +(v: Vec2[U]): Pt[U] = Pt(x + v.dx, y + v.dy)
  def -(v: Vec2[U]): Pt[U] = Pt(x - v.dx, y - v.dy)

  /** The displacement from this position to `that`. */
  def vectorTo(that: Pt[U]): Vec2[U] = Vec2(that.x - x, that.y - y)

  def distanceTo(that: Pt[U]): Double = vectorTo(that).norm

  def midpoint(that: Pt[U]): Pt[U] = Pt((x + that.x) / 2.0, (y + that.y) / 2.0)

  def isFinite: Boolean = x.isFinite && y.isFinite

  def render(using u: UnitLabel[U]): String = f"($x%.3f, $y%.3f)${u.symbol}"

end Pt

object Pt:
  def origin[U <: Unit2D]: Pt[U] = Pt(0.0, 0.0)

/** A displacement in the plane, tagged with its unit.
  *
  * Distinct from [[Pt]] so that the affine discipline holds: displacements add
  * and scale, positions do not.
  */
final case class Vec2[U <: Unit2D](dx: Double, dy: Double) derives CanEqual:

  def +(w: Vec2[U]): Vec2[U] = Vec2(dx + w.dx, dy + w.dy)
  def -(w: Vec2[U]): Vec2[U] = Vec2(dx - w.dx, dy - w.dy)
  def *(k: Double): Vec2[U]  = Vec2(dx * k, dy * k)
  def unary_- : Vec2[U]      = Vec2(-dx, -dy)

  /** Euclidean length. For a saccade, this is its amplitude. */
  def norm: Double = math.hypot(dx, dy)

  def normSquared: Double = dx * dx + dy * dy

  /** Direction, measured counter-clockwise from the positive x-axis.
    *
    * Note that "counter-clockwise" is a statement about the coordinate system,
    * not about the screen: under [[YAxis.Down]] this appears clockwise to a
    * viewer. That is why [[Frame]] records its axis direction rather than
    * assuming one.
    */
  def angle: Angle = Angle.radians(math.atan2(dy, dx))

  def isFinite: Boolean = dx.isFinite && dy.isFinite

  def render(using u: UnitLabel[U]): String = f"<$dx%.3f, $dy%.3f>${u.symbol}"

end Vec2

object Vec2:
  def zero[U <: Unit2D]: Vec2[U] = Vec2(0.0, 0.0)

  def polar[U <: Unit2D](magnitude: Double, direction: Angle): Vec2[U] =
    Vec2(magnitude * math.cos(direction.toRadians), magnitude * math.sin(direction.toRadians))

/** A planar angle, stored in radians.
  *
  * Unit-agnostic on purpose: the angle between two displacements is the same
  * number whether they were measured in pixels or degrees of visual angle.
  */
opaque type Angle = Double

object Angle:
  val zero: Angle = 0.0

  def radians(r: Double): Angle = r
  def degrees(d: Double): Angle = d * math.Pi / 180.0

  extension (a: Angle)
    def toRadians: Double = a
    def toDegrees: Double = a * 180.0 / math.Pi

    /** Fold into `(-pi, pi]`. */
    def normalised: Angle =
      val twoPi = 2 * math.Pi
      val r     = ((a % twoPi) + twoPi) % twoPi
      if r > math.Pi then r - twoPi else r

    /** The smaller of the two arcs between the directions, in `[0, pi]`.
      *
      * Wrapping is the whole difficulty here: two directions three degrees
      * either side of straight-up are six degrees apart, not 354.
      */
    def separationFrom(b: Angle): Angle =
      math.abs((Angle.radians(a.toRadians - b.toRadians)).normalised.toRadians)

    def +(b: Angle): Angle = a + b
    def -(b: Angle): Angle = a - b

    def render: String = f"${a.toDegrees}%.2f°"

  given cats.kernel.Order[Angle] =
    cats.kernel.Order.from((x, y) => java.lang.Double.compare(x, y))

end Angle
