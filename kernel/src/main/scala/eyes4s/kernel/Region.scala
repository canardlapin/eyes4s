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

/** A subset of the plane, in a known unit.
  *
  * ==Exact membership, sampled area== (bead `q-region-exact`)
  *
  * `contains` is exact and resolution-independent: it answers from the shape's
  * own definition, not from a rasterisation. `area` takes a [[Grid]] and counts
  * cells, because exact area over arbitrary unions, intersections and
  * complements of polygons needs a polygon-clipping implementation -- delicate,
  * bug-prone work for a number most analyses never read. Membership is what
  * dwell time actually needs, and membership is the half that is free.
  *
  * The signature is the honest part: `area(g: Grid[U])` cannot be mistaken for
  * an exact answer the way a bare `area: Double` could.
  *
  * ==A Boolean algebra==
  *
  * Regions form a lattice with complement, so overlapping areas of interest,
  * exclusions and "anything but" are expressible without special cases. The
  * laws are tested observationally through `contains` in `eyes4s-laws`, which
  * is the only honest way to test them: `Union(a, a)` is a different *value*
  * from `a` and must remain so, since the tree records how the region was
  * built.
  */
sealed trait Region[U <: Unit2D]:

  /** Exact membership. Half-open at the upper edges, matching [[Bounds]] and
    * [[Interval]], so tiled regions partition rather than overlap.
    */
  def contains(p: Pt[U]): Boolean

  /** Area estimated by counting cells whose centre is inside.
    *
    * At the grid's resolution, and stated as such. The error is proportional to
    * the boundary length over the cell size.
    */
  def area(g: Grid[U]): Double =
    var n = 0
    var i = 0
    while i < g.size do
      if contains(g.cellCentre(i)) then n += 1
      i += 1
    n * g.cellArea

  /** Membership evaluated at every cell centre. */
  def rasterise(g: Grid[U]): IArray[Boolean] =
    IArray.tabulate(g.size)(i => contains(g.cellCentre(i)))

  def render: String

end Region

object Region:

  final case class Rect[U <: Unit2D] private[kernel] (lo: Pt[U], hi: Pt[U]) extends Region[U]:
    def contains(p: Pt[U]): Boolean =
      p.x >= lo.x && p.x < hi.x && p.y >= lo.y && p.y < hi.y
    override def area(g: Grid[U]): Double = (hi.x - lo.x) * (hi.y - lo.y)
    def render: String = f"rect(${lo.x}%.2f,${lo.y}%.2f -> ${hi.x}%.2f,${hi.y}%.2f)"

  final case class Ellipse[U <: Unit2D] private[kernel] (
      centre: Pt[U],
      rx: Double,
      ry: Double
  ) extends Region[U]:
    def contains(p: Pt[U]): Boolean =
      val dx = (p.x - centre.x) / rx
      val dy = (p.y - centre.y) / ry
      dx * dx + dy * dy <= 1.0
    override def area(g: Grid[U]): Double = math.Pi * rx * ry
    def render: String = f"ellipse(${centre.x}%.2f,${centre.y}%.2f r=$rx%.2f,$ry%.2f)"

  /** A simple polygon, by even-odd ray casting.
    *
    * Even-odd rather than winding number: for the self-intersecting shapes a
    * hand-drawn area of interest can produce, the two disagree, and even-odd
    * matches what a user drawing an outline sees on screen.
    */
  final case class Polygon[U <: Unit2D] private[kernel] (vertices: Vector[Pt[U]])
      extends Region[U]:
    def contains(p: Pt[U]): Boolean =
      var inside = false
      var j      = vertices.length - 1
      var i      = 0
      while i < vertices.length do
        val a = vertices(i)
        val b = vertices(j)
        if (a.y > p.y) != (b.y > p.y) &&
          p.x < (b.x - a.x) * (p.y - a.y) / (b.y - a.y) + a.x
        then inside = !inside
        j = i
        i += 1
      inside
    def render: String = s"polygon(${vertices.length} vertices)"

  final case class Union[U <: Unit2D] private[kernel] (a: Region[U], b: Region[U])
      extends Region[U]:
    def contains(p: Pt[U]): Boolean = a.contains(p) || b.contains(p)
    def render: String              = s"(${a.render} or ${b.render})"

  final case class Intersect[U <: Unit2D] private[kernel] (a: Region[U], b: Region[U])
      extends Region[U]:
    def contains(p: Pt[U]): Boolean = a.contains(p) && b.contains(p)
    def render: String              = s"(${a.render} and ${b.render})"

  final case class Complement[U <: Unit2D] private[kernel] (a: Region[U]) extends Region[U]:
    def contains(p: Pt[U]): Boolean = !a.contains(p)
    def render: String              = s"not(${a.render})"

  final case class Empty[U <: Unit2D] private[kernel] () extends Region[U]:
    def contains(p: Pt[U]): Boolean       = false
    override def area(g: Grid[U]): Double = 0.0
    def render: String                    = "empty"

  final case class Everything[U <: Unit2D] private[kernel] () extends Region[U]:
    def contains(p: Pt[U]): Boolean = true
    def render: String              = "everything"

  // -------------------------------------------------------------------------
  // Smart constructors
  // -------------------------------------------------------------------------

  def empty[U <: Unit2D]: Region[U]      = Empty()
  def everything[U <: Unit2D]: Region[U] = Everything()

  def rect[U <: Unit2D](lo: Pt[U], hi: Pt[U]): Either[GeometryError, Region[U]] =
    if !(lo.isFinite && hi.isFinite) then
      Left(GeometryError.NonFiniteBounds(lo.x, lo.y, hi.x, hi.y))
    else if hi.x <= lo.x || hi.y <= lo.y then
      Left(GeometryError.DegenerateBounds(lo.x, lo.y, hi.x, hi.y))
    else Right(Rect(lo, hi))

  def fromBounds[U <: Unit2D](b: Bounds[U]): Region[U] =
    Rect(Pt(b.xMin, b.yMin), Pt(b.xMax, b.yMax))

  def ellipse[U <: Unit2D](
      centre: Pt[U],
      rx: Double,
      ry: Double
  ): Either[GeometryError, Region[U]] =
    if !(centre.isFinite && rx.isFinite && ry.isFinite) then
      Left(GeometryError.NonFiniteRegion("ellipse"))
    else if rx <= 0.0 || ry <= 0.0 then Left(GeometryError.DegenerateEllipse(rx, ry))
    else Right(Ellipse(centre, rx, ry))

  def circle[U <: Unit2D](centre: Pt[U], r: Double): Either[GeometryError, Region[U]] =
    ellipse(centre, r, r)

  def polygon[U <: Unit2D](vertices: Vector[Pt[U]]): Either[GeometryError, Region[U]] =
    if vertices.length < 3 then Left(GeometryError.DegeneratePolygon(vertices.length))
    else if !vertices.forall(_.isFinite) then Left(GeometryError.NonFiniteRegion("polygon"))
    else Right(Polygon(vertices))

  extension [U <: Unit2D](a: Region[U])
    def ||(b: Region[U]): Region[U] = Union(a, b)
    def &&(b: Region[U]): Region[U] = Intersect(a, b)
    def unary_! : Region[U]         = Complement(a)

    /** Set difference: in `a` but not in `b`. */
    def \(b: Region[U]): Region[U] = Intersect(a, Complement(b))

end Region
