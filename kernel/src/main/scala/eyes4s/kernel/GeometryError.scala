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

/** Recoverable failures in the geometry layer.
  *
  * As with [[TimeError]], every case names its operands so that an application
  * can point at what failed (PRD APP-14).
  */
enum GeometryError derives CanEqual:

  case DegenerateBounds(xMin: Double, yMin: Double, xMax: Double, yMax: Double)

  case NonFiniteBounds(xMin: Double, yMin: Double, xMax: Double, yMax: Double)

  /** Two values in the same unit but different frames were combined.
    *
    * This is the failure the unit parameter cannot catch. `Pt[Deg]` proves a
    * position is in degrees; it does not prove it is in the *same* degrees as
    * another. A region drawn on one stimulus and a path recorded over a
    * different one are both `Deg`, and combining them silently is precisely the
    * bug class this library exists to prevent.
    */
  case FrameMismatch(left: FrameId, right: FrameId)

  case NonFinitePerspective(distanceMm: Double, widthMm: Double, heightMm: Double)

  case NonPositivePerspective(distanceMm: Double, widthMm: Double, heightMm: Double)

  /** A matrix offered as affine whose bottom row is not `(0, 0, 1)`. */
  case NotAffine(matrix: String)

  case NonFiniteSigma(value: Double)
  case NonPositiveSigma(value: Double)
  case DegenerateGrid(nx: Int, ny: Int)
  case DegenerateEllipse(rx: Double, ry: Double)
  case DegeneratePolygon(vertices: Int)
  case NonFiniteRegion(shape: String)

  def message: String = this match
    case DegenerateBounds(x0, y0, x1, y1) =>
      s"Bounds must have positive extent in both axes, got " +
        s"[$x0, $x1) x [$y0, $y1). If the vertical pair is inverted, set " +
        "YAxis explicitly rather than encoding the flip in the bounds."
    case NonFiniteBounds(x0, y0, x1, y1) =>
      s"Bounds must be finite, got [$x0, $x1) x [$y0, $y1)."
    case FrameMismatch(l, r) =>
      s"Cannot combine values from different frames: '$l' and '$r'. " +
        "Both are in the same unit, but not in the same coordinate system; " +
        "convert one with a Warp before combining them."
    case NonFinitePerspective(d, w, h) =>
      s"Perspective must be finite, got distance=${d}mm, surface=${w}x${h}mm."
    case NonPositivePerspective(d, w, h) =>
      s"Perspective needs a positive distance and surface size, got " +
        s"distance=${d}mm, surface=${w}x${h}mm."
    case NotAffine(m) =>
      s"Expected an affine matrix with bottom row (0, 0, 1), got $m. " +
        "Use Warp.homography for a full projective transform."
    case NonFiniteSigma(v) =>
      s"A bandwidth must be finite, was $v."
    case NonPositiveSigma(v) =>
      s"A bandwidth is a standard deviation and must be positive, was $v. " +
        "If a backend expresses it as a full or quarter width, convert at " +
        "that backend's boundary rather than passing its convention here."
    case DegenerateGrid(nx, ny) =>
      s"A grid needs at least one cell in each axis, got ${nx}x$ny."
    case DegenerateEllipse(rx, ry) =>
      s"An ellipse needs positive radii, got rx=$rx, ry=$ry."
    case DegeneratePolygon(n) =>
      s"A polygon needs at least 3 vertices, got $n."
    case NonFiniteRegion(shape) =>
      s"A $shape region was given non-finite coordinates."

end GeometryError
