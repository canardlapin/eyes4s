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

/** A physical length, stored in millimetres. */
opaque type Length = Double

object Length:
  def mm(v: Double): Length = v
  def cm(v: Double): Length = v * 10.0
  def m(v: Double): Length  = v * 1000.0

  extension (l: Length)
    def toMm: Double   = l
    def toCm: Double   = l / 10.0
    def toM: Double    = l / 1000.0
    def render: String = f"${l.toMm}%.1fmm"

  given cats.kernel.Order[Length] =
    cats.kernel.Order.from((a, b) => java.lang.Double.compare(a, b))

end Length

/** The physical arrangement relating a flat surface to a viewpoint: how far
  * away the surface is, and how large it physically is.
  *
  * ==Why this is in the kernel and not called `Viewing`==
  *
  * Converting a linear unit to an angular one is geometry, not physiology. "How
  * large does a rectangle of known physical size appear from a known distance"
  * is the same question whether the viewpoint is an eye, a camera, or a
  * simulated observer in a virtual environment. The kernel therefore owns the
  * projection and names it neutrally; `eyes4s-core` wraps it as `Viewing`, with
  * the observer-specific documentation and constructors that belong there.
  *
  * This split was forced by `checkKernelPurity`, which rejected `Viewing` in the
  * kernel. The rule turned out to be pointing at something real rather than
  * merely being inconvenient.
  *
  * @param distance     from the viewpoint to the centre of the surface
  * @param surfaceWidth  physical width of the whole surface
  * @param surfaceHeight physical height of the whole surface
  */
final case class Perspective private (
    distance: Length,
    surfaceWidth: Length,
    surfaceHeight: Length
) derives CanEqual:

  /** Total horizontal angle subtended by the surface. */
  def horizontalExtent: Angle =
    Angle.radians(2.0 * math.atan((surfaceWidth.toMm / 2.0) / distance.toMm))

  /** Total vertical angle subtended by the surface. */
  def verticalExtent: Angle =
    Angle.radians(2.0 * math.atan((surfaceHeight.toMm / 2.0) / distance.toMm))

  def render: String =
    s"${distance.render} from ${surfaceWidth.render} x ${surfaceHeight.render}"

end Perspective

object Perspective:

  def of(
      distance: Length,
      surfaceWidth: Length,
      surfaceHeight: Length
  ): Either[GeometryError, Perspective] =
    val ds = List(distance.toMm, surfaceWidth.toMm, surfaceHeight.toMm)
    if !ds.forall(_.isFinite) then
      Left(
        GeometryError.NonFinitePerspective(distance.toMm, surfaceWidth.toMm, surfaceHeight.toMm)
      )
    else if ds.exists(_ <= 0.0) then
      Left(
        GeometryError.NonPositivePerspective(
          distance.toMm,
          surfaceWidth.toMm,
          surfaceHeight.toMm
        )
      )
    else Right(Perspective(distance, surfaceWidth, surfaceHeight))

end Perspective
