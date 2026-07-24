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

end GeometryError
