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

package eyes4s.studio.app.maps

import eyes4s.studio.app.tokens.{Colour, Tokens}

/** How a map's values are coloured (ticket S4.4; DESIGN_SPEC section 6, the
  * System board's map ramps).
  */
enum MapPalette derives CanEqual:

  /** Mass maps: the magenta sequential ramp, light to dark from the lower
    * limit to the upper, opaque per cell.
    */
  case Mass

  /** Difference maps only: the BlueRust diverging ramp, its middle colour
    * pinned to zero.
    */
  case Difference

/** The values a palette's ends stand for. */
enum ColourLimits derives CanEqual:

  /** A sequential ramp from `lower` (its lightest stop) to `upper` (its
    * darkest); `lower < upper`.
    */
  case Sequential(lower: Double, upper: Double)

  /** A diverging ramp from `−extent` to `+extent`, zero at its middle;
    * `extent > 0`.
    */
  case Symmetric(extent: Double)

/** Whether panels shown together share their colour limits (one scale for
  * all, so equal colours mean equal values) or each has its own.
  */
enum LimitsScope derives CanEqual:
  case Shared, PerPanel

object ColourLimits:

  /** The limits of `palette` that span every value of `grids`: zero to the
    * largest value for mass maps (mass is never negative; an all-zero map
    * gets the unit range), and the largest magnitude either side of zero
    * for difference maps. Missing cells are ignored. These are display
    * limits, not results.
    */
  def spanning(palette: MapPalette, grids: Vector[MapGrid]): ColourLimits =
    val values = grids.flatMap(_.cells.flatten)
    palette match
      case MapPalette.Mass =>
        val hi = (values :+ 0.0).max
        Sequential(0.0, if hi > 0.0 then hi else 1.0)
      case MapPalette.Difference =>
        val extent = values.map(math.abs).maxOption.getOrElse(0.0)
        Symmetric(if extent > 0.0 then extent else 1.0)

  /** The limits of each of `grids`, drawn together under `scope`: one set
    * spanning all of them when shared, each its own otherwise.
    */
  def forPanels(
      palette: MapPalette,
      scope: LimitsScope,
      grids: Vector[MapGrid]
  ): Vector[ColourLimits] =
    scope match
      case LimitsScope.Shared =>
        val shared = spanning(palette, grids)
        grids.map(_ => shared)
      case LimitsScope.PerPanel => grids.map(g => spanning(palette, Vector(g)))

/** The colour of one map value (ticket S4.4): every colour is a token's or
  * lies between two of a ramp's stops; a missing value is fully transparent,
  * never a ramp colour, so missing is drawn differently from zero. Colours
  * are packed as ARGB with an opaque alpha; the one global map opacity is
  * applied when the raster is drawn, not here.
  */
object MapColours:

  /** Fully transparent: a cell without a value. */
  val Transparent: Int = 0

  /** The ARGB of `value` under `palette` and `limits`. Values beyond the
    * limits take the end colour. A palette given limits of the other kind
    * reads them as its own: a sequential range as a symmetric extent of its
    * larger magnitude, a symmetric extent as `0` to the extent.
    */
  def argb(palette: MapPalette, limits: ColourLimits, value: Option[Double]): Int =
    value.fold(Transparent) { v =>
      palette match
        case MapPalette.Mass =>
          val (lo, hi) = limits match
            case ColourLimits.Sequential(l, u) => (l, u)
            case ColourLimits.Symmetric(e)     => (0.0, e)
          along(Tokens.massRamp.stops.toVector, fraction(v, lo, hi))
        case MapPalette.Difference =>
          val extent = limits match
            case ColourLimits.Symmetric(e)     => e
            case ColourLimits.Sequential(l, u) => math.max(math.abs(l), math.abs(u))
          val ramp = Tokens.differenceRamp
          // Zero falls at the middle stop, whatever the extent: it is pinned.
          along(Vector(ramp.negative, ramp.zero, ramp.positive), fraction(v, -extent, extent))
    }

  /** Where `v` lies from `lo` (0) to `hi` (1), clamped. */
  private def fraction(v: Double, lo: Double, hi: Double): Double =
    if hi <= lo then 0.0 else math.max(0.0, math.min(1.0, (v - lo) / (hi - lo)))

  /** The colour at fraction `t` along evenly spaced `stops`, interpolated
    * channel by channel in sRGB between the two stops around it.
    */
  private def along(stops: Vector[Colour], t: Double): Int =
    val segments = stops.size - 1
    if segments <= 0 then packed(stops.head)
    else
      val at                       = t * segments
      val k                        = math.min(segments - 1, at.toInt)
      val f                        = at - k
      val a                        = stops(k)
      val b                        = stops(k + 1)
      def mix(x: Int, y: Int): Int = math.round(x + (y - x) * f).toInt
      pack(mix(a.red, b.red), mix(a.green, b.green), mix(a.blue, b.blue))

  private def packed(c: Colour): Int = pack(c.red, c.green, c.blue)

  private def pack(r: Int, g: Int, b: Int): Int = (0xff << 24) | (r << 16) | (g << 8) | b

/** The global opacity every map raster is drawn with (default 0.6,
  * DESIGN_SPEC section 6), between 0 and 1. Changing it redraws; it never
  * changes a stored value or a cached raster.
  */
final case class MapOpacity private (value: Double) derives CanEqual

object MapOpacity:
  val Default: MapOpacity = MapOpacity(0.6)

  def of(value: Double): Option[MapOpacity] =
    Option.when(value.isFinite && value >= 0.0 && value <= 1.0)(MapOpacity(value))
