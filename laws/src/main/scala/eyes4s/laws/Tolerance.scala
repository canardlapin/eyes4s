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

package eyes4s.laws

import eyes4s.kernel.*

/** How close two floating-point results must be to count as equal.
  *
  * A named, passed value rather than a constant buried in a comparison. PRD V-2
  * requires every numerical law to state its tolerance, because a hidden epsilon
  * is how a law suite quietly stops testing anything: widen it once to make a
  * failure go away and nothing ever tells you it is now vacuous.
  *
  * @param absolute added to the relative term, so that comparisons near zero do
  *                 not demand impossible precision
  * @param relative fraction of the larger magnitude
  */
final case class Tolerance(absolute: Double, relative: Double):

  def approxEquals(a: Double, b: Double): Boolean =
    if a == b then true
    else if !a.isFinite || !b.isFinite then false
    else math.abs(a - b) <= absolute + relative * math.max(math.abs(a), math.abs(b))

  def approxEquals[U <: Unit2D](a: Pt[U], b: Pt[U]): Boolean =
    approxEquals(a.x, b.x) && approxEquals(a.y, b.y)

object Tolerance:

  /** For transforms composed of a handful of multiplications and additions. */
  val exactish: Tolerance = Tolerance(absolute = 1e-9, relative = 1e-12)

  /** For round trips through a transcendental function, where the inverse
    * cannot recover the last bits.
    */
  val roundTrip: Tolerance = Tolerance(absolute = 1e-7, relative = 1e-10)
