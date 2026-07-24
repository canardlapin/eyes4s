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

/** The unit a planar coordinate is expressed in.
  *
  * ==Static unit, nominal frame==
  *
  * This is the half of the geometry discipline that is decided at compile time.
  * Pixels versus degrees is knowable statically and is the error that actually
  * happens, so it is a type parameter. *Which* screen or which stimulus is a
  * runtime value with nominal identity ([[FrameId]]), because no compiler can
  * know which of two hundred images a given trial belongs to.
  *
  * The subtypes are phantom: they are never instantiated and carry no data.
  * Runtime information about a unit -- its symbol, for rendering and for codecs
  * -- comes from the [[UnitLabel]] instance instead.
  */
sealed trait Unit2D

object Unit2D:

  /** Device or screen pixels. */
  sealed trait Px extends Unit2D

  /** Degrees of visual angle. Reaching this unit requires stating the viewing
    * geometry, which is what makes degree-based thresholds trustworthy.
    */
  sealed trait Deg extends Unit2D

  /** Stimulus-normalised coordinates, conventionally the unit square. */
  sealed trait Norm extends Unit2D

  /** Physical millimetres on the display surface. */
  sealed trait Mm extends Unit2D

/** Runtime witness for a [[Unit2D]].
  *
  * Needed because a phantom type cannot render itself, and both display
  * (PRD APP-15) and serialisation (PRD APP-5) must name the unit.
  */
trait UnitLabel[U <: Unit2D]:
  def symbol: String
  def name: String

object UnitLabel:
  def apply[U <: Unit2D](using u: UnitLabel[U]): UnitLabel[U] = u

  private def of[U <: Unit2D](sym: String, nm: String): UnitLabel[U] =
    new UnitLabel[U]:
      val symbol = sym
      val name   = nm

  given UnitLabel[Unit2D.Px]   = of("px", "pixels")
  given UnitLabel[Unit2D.Deg]  = of("deg", "degrees of visual angle")
  given UnitLabel[Unit2D.Norm] = of("norm", "normalised stimulus coordinates")
  given UnitLabel[Unit2D.Mm]   = of("mm", "millimetres")

end UnitLabel
