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

/** A 3x3 matrix in row-major order, for planar transforms in homogeneous
  * coordinates.
  *
  * Deliberately hand-rolled and tiny rather than pulled from a linear-algebra
  * library. The kernel's only external dependency is cats-core, and a 3x3
  * inverse by cofactors is a dozen lines. `gale` enters later for the
  * genuinely heavy work -- SVD for the latent adapters -- not for this.
  */
final case class Mat3(
    m00: Double,
    m01: Double,
    m02: Double,
    m10: Double,
    m11: Double,
    m12: Double,
    m20: Double,
    m21: Double,
    m22: Double
) derives CanEqual:

  def *(that: Mat3): Mat3 = Mat3(
    m00 * that.m00 + m01 * that.m10 + m02 * that.m20,
    m00 * that.m01 + m01 * that.m11 + m02 * that.m21,
    m00 * that.m02 + m01 * that.m12 + m02 * that.m22,
    m10 * that.m00 + m11 * that.m10 + m12 * that.m20,
    m10 * that.m01 + m11 * that.m11 + m12 * that.m21,
    m10 * that.m02 + m11 * that.m12 + m12 * that.m22,
    m20 * that.m00 + m21 * that.m10 + m22 * that.m20,
    m20 * that.m01 + m21 * that.m11 + m22 * that.m21,
    m20 * that.m02 + m21 * that.m12 + m22 * that.m22
  )

  def determinant: Double =
    m00 * (m11 * m22 - m12 * m21) -
      m01 * (m10 * m22 - m12 * m20) +
      m02 * (m10 * m21 - m11 * m20)

  /** Inverse by cofactors, or `None` when numerically singular.
    *
    * Singularity is a `None` rather than an exception because a non-invertible
    * transform is a legitimate thing to hold -- a projection that collapses an
    * axis is still a transform. It is only asking for its inverse that fails.
    */
  def inverse: Option[Mat3] =
    val det = determinant
    if !det.isFinite || math.abs(det) < 1e-12 then None
    else
      val d = 1.0 / det
      Some(
        Mat3(
          (m11 * m22 - m12 * m21) * d,
          (m02 * m21 - m01 * m22) * d,
          (m01 * m12 - m02 * m11) * d,
          (m12 * m20 - m10 * m22) * d,
          (m00 * m22 - m02 * m20) * d,
          (m02 * m10 - m00 * m12) * d,
          (m10 * m21 - m11 * m20) * d,
          (m01 * m20 - m00 * m21) * d,
          (m00 * m11 - m01 * m10) * d
        )
      )

  /** Apply to a planar position in homogeneous coordinates.
    *
    * Returns `None` when the result lies on the line at infinity, which a
    * homography can produce for points behind the projection plane.
    */
  def applyTo[U <: Unit2D, V <: Unit2D](p: Pt[U]): Option[Pt[V]] =
    val x = m00 * p.x + m01 * p.y + m02
    val y = m10 * p.x + m11 * p.y + m12
    val w = m20 * p.x + m21 * p.y + m22
    if !w.isFinite || math.abs(w) < 1e-12 then None
    else
      val r = Pt[V](x / w, y / w)
      if r.isFinite then Some(r) else None

  /** True when the bottom row is `(0, 0, 1)`, i.e. this is affine. */
  def isAffine: Boolean =
    m20 == 0.0 && m21 == 0.0 && m22 == 1.0

  def render: String =
    f"[[$m00%.4f $m01%.4f $m02%.4f] [$m10%.4f $m11%.4f $m12%.4f] [$m20%.4f $m21%.4f $m22%.4f]]"

end Mat3

object Mat3:

  val identity: Mat3 = Mat3(1, 0, 0, 0, 1, 0, 0, 0, 1)

  /** An affine transform from its six meaningful coefficients. */
  def affine(a: Double, b: Double, tx: Double, c: Double, d: Double, ty: Double): Mat3 =
    Mat3(a, b, tx, c, d, ty, 0, 0, 1)

  def translation(tx: Double, ty: Double): Mat3 = affine(1, 0, tx, 0, 1, ty)

  def scaling(sx: Double, sy: Double): Mat3 = affine(sx, 0, 0, 0, sy, 0)

  def rotation(theta: Angle): Mat3 =
    val c = math.cos(theta.toRadians)
    val s = math.sin(theta.toRadians)
    affine(c, -s, 0, s, c, 0)

end Mat3
