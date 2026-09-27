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

/** A [[Unit2D]] as a runtime value: one case per phantom unit.
  *
  * The phantom type proves a coordinate's unit at compile time; this names it
  * at run time, so a descriptor or a codec can say "degrees" without a string.
  * Every [[UnitLabel]] instance is built from one of these cases, so the two
  * cannot disagree.
  */
enum PlanarUnit derives CanEqual:
  case Px, Deg, Norm, Mm

  /** The short symbol: `px`, `deg`, `norm`, `mm`. */
  def symbol: String = this match
    case Px   => "px"
    case Deg  => "deg"
    case Norm => "norm"
    case Mm   => "mm"

  /** The unit's name in English. */
  def name: String = this match
    case Px   => "pixels"
    case Deg  => "degrees of visual angle"
    case Norm => "normalised stimulus coordinates"
    case Mm   => "millimetres"

object PlanarUnit:
  /** The unit of `U`. */
  def of[U <: Unit2D](using u: UnitLabel[U]): PlanarUnit = u.planar
