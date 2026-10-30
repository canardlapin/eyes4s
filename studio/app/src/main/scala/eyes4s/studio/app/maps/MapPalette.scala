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
    * limit to the upper, each cell fading in from transparent at the lower
    * limit to opaque at [[MapColours.MassAlphaFloor]] of the range, so the
    * stimulus and stage show through low mass. Its limits are
    * [[ColourLimits.Sequential]].
    */
  case Mass

  /** Difference maps only: the BlueRust diverging ramp, its middle colour
    * pinned to zero, opaque per cell (zero sits mid-ramp, so no value fades).
    * Its limits are [[ColourLimits.Symmetric]].
    */
  case Difference

/** Why colour limits, or a palette with limits, were refused. Every case
  * names its operands.
  */
enum LimitsError derives CanEqual:

  /** A limit is not a finite number. */
  case NotFinite(limit: String, value: Double)

  /** A sequential range runs from `lower` to an `upper` not above it. */
  case Unordered(lower: Double, upper: Double)

  /** A symmetric extent is not above zero. */
  case NotPositive(extent: Double)

  /** `palette` is not drawn between limits like `limits`. */
  case PaletteMismatch(palette: MapPalette, limits: ColourLimits)

  def message: String = this match
    case NotFinite(l, v)   => s"Colour limit $l is $v, which is not a finite number."
    case Unordered(lo, hi) =>
      s"Colour limits run from $lo to $hi; the upper must be above the lower."
    case NotPositive(e)        => s"A symmetric colour extent of $e is not above zero."
    case PaletteMismatch(p, l) =>
      s"The ${p.productPrefix} palette is not drawn between limits like $l."

/** The values a palette's ends stand for, made only through
  * [[ColourLimits.sequential]] and [[ColourLimits.symmetric]].
  */
sealed trait ColourLimits derives CanEqual

object ColourLimits:

  object Sequential:
    private[maps] def apply(lower: Double, upper: Double): Sequential =
      new Sequential(lower, upper) {}

  /** A sequential ramp from `lower` (its lightest stop) to `upper` (its
    * darkest), `lower < upper`, both finite.
    */
  sealed abstract case class Sequential private[maps] (lower: Double, upper: Double)
      extends ColourLimits

  object Symmetric:
    private[maps] def apply(extent: Double): Symmetric =
      new Symmetric(extent) {}

  /** A diverging ramp from `−extent` to `+extent`, zero at its middle,
    * `extent` finite and above zero.
    */
  sealed abstract case class Symmetric private[maps] (extent: Double) extends ColourLimits

  def sequential(lower: Double, upper: Double): Either[LimitsError, ColourLimits] =
    if !lower.isFinite then Left(LimitsError.NotFinite("lower", lower))
    else if !upper.isFinite then Left(LimitsError.NotFinite("upper", upper))
    else if upper <= lower then Left(LimitsError.Unordered(lower, upper))
    else Right(Sequential(lower, upper))

  def symmetric(extent: Double): Either[LimitsError, ColourLimits] =
    if !extent.isFinite then Left(LimitsError.NotFinite("extent", extent))
    else if extent <= 0.0 then Left(LimitsError.NotPositive(extent))
    else Right(Symmetric(extent))

  /** The limits of `palette` that span every value of `grids`: zero to the
    * largest value for mass maps (mass is never negative; an all-zero map
    * gets the unit range), and the largest magnitude either side of zero
    * for difference maps. Missing cells are ignored. These are display
    * limits, not results.
    */
  def spanning(palette: MapPalette, grids: Vector[MapGrid]): MapStyle =
    val values = grids.flatMap(_.cells.flatten)
    palette match
      case MapPalette.Mass =>
        val hi = (values :+ 0.0).max
        MapStyle(palette, Sequential(0.0, if hi > 0.0 then hi else 1.0))
      case MapPalette.Difference =>
        val extent = values.map(math.abs).maxOption.getOrElse(0.0)
        MapStyle(palette, Symmetric(if extent > 0.0 then extent else 1.0))

  /** The style of each of `grids`, drawn together under `scope`: one set of
    * limits spanning all of them when shared, each its own otherwise.
    */
  def forPanels(
      palette: MapPalette,
      scope: LimitsScope,
      grids: Vector[MapGrid]
  ): Vector[MapStyle] =
    scope match
      case LimitsScope.Shared =>
        val shared = spanning(palette, grids)
        grids.map(_ => shared)
      case LimitsScope.PerPanel => grids.map(g => spanning(palette, Vector(g)))

/** Whether panels shown together share their colour limits (one scale for
  * all, so equal colours mean equal values) or each has its own.
  */
enum LimitsScope derives CanEqual:
  case Shared, PerPanel

/** A palette with limits of its own kind: mass between sequential limits,
  * difference between symmetric ones. Made only through [[MapStyle.of]] or
  * [[ColourLimits.spanning]], so a palette never reads another's limits.
  */
sealed abstract case class MapStyle private[maps] (palette: MapPalette, limits: ColourLimits)
    derives CanEqual

object MapStyle:

  private[maps] def apply(palette: MapPalette, limits: ColourLimits): MapStyle =
    new MapStyle(palette, limits) {}

  def of(palette: MapPalette, limits: ColourLimits): Either[LimitsError, MapStyle] =
    (palette, limits) match
      case (MapPalette.Mass, ColourLimits.Sequential(_, _)) |
          (MapPalette.Difference, ColourLimits.Symmetric(_)) =>
        Right(MapStyle(palette, limits))
      case _ => Left(LimitsError.PaletteMismatch(palette, limits))

/** The colour of one map value (ticket S4.4): every colour is a token's or
  * lies between two of a ramp's stops, packed as ARGB, opaque except where
  * a mass map's alpha floor fades it in (review ticket 8; DESIGN_SPEC
  * section 15). A missing value is [[NoCoverage]]: the scenes draw it with
  * the missing-asset hatch, so missing is drawn differently from zero. The
  * one global map opacity is applied when the raster is drawn
  * ([[MapOpacity.over]]), not here.
  */
object MapColours:

  /** A pixel that covers nothing: alpha zero. It is the absence of a
    * colour, not a colour of any palette, so it has no token.
    */
  val NoCoverage: Int = 0

  /** The fraction of a mass map's limits at which a cell becomes opaque:
    * below it the cell's alpha rises linearly from zero, as
    * `min(1, t / MassAlphaFloor)` for `t` the fraction along the limits.
    */
  val MassAlphaFloor: Double = 0.25

  // The ramps' stops, read once from the tokens.
  private val massStops: Vector[Colour]       = Tokens.massRamp.stops.toVector
  private val differenceStops: Vector[Colour] =
    val ramp = Tokens.differenceRamp
    Vector(ramp.negative, ramp.zero, ramp.positive)

  /** The ARGB of `value` in `style`. Values beyond the limits take the end
    * colour; a mass value below the alpha floor keeps its colour and fades.
    */
  def argb(style: MapStyle, value: Option[Double]): Int =
    value.fold(NoCoverage) { v =>
      style.limits match
        case ColourLimits.Sequential(lo, hi) =>
          val t = fraction(v, lo, hi)
          (floorAlpha(t) << 24) | (along(massStops, t) & MapOpacity.ChannelBits)
        // Zero falls at the middle stop, whatever the extent: it is pinned.
        case ColourLimits.Symmetric(extent) =>
          along(differenceStops, fraction(v, -extent, extent))
    }

  /** The alpha, 0 to 255, of a mass cell at fraction `t` along its limits. */
  private def floorAlpha(t: Double): Int =
    math.round(255 * math.min(1.0, t / MassAlphaFloor)).toInt

  /** Where `v` lies from `lo` (0) to `hi` (1), clamped; `lo < hi`. */
  private def fraction(v: Double, lo: Double, hi: Double): Double =
    math.max(0.0, math.min(1.0, (v - lo) / (hi - lo)))

  /** The opaque colour at fraction `t` along evenly spaced `stops`,
    * interpolated channel by channel in sRGB between the two stops around it.
    */
  private def along(stops: Vector[Colour], t: Double): Int =
    val segments                 = stops.size - 1
    val at                       = t * segments
    val k                        = math.min(segments - 1, at.toInt)
    val f                        = at - k
    val a                        = stops(k)
    val b                        = stops(k + 1)
    def mix(x: Int, y: Int): Int = math.round(x + (y - x) * f).toInt
    (0xff << 24) | (mix(a.red, b.red) << 16) | (mix(a.green, b.green) << 8) | mix(
      a.blue,
      b.blue
    )

/** The global opacity every map raster is drawn with (default 0.85,
  * DESIGN_SPEC section 6), between 0 and 1. Changing it redraws; it never
  * changes a stored value or a cached raster.
  */
sealed abstract case class MapOpacity private (value: Double) derives CanEqual:

  /** A stored pixel as drawn at this opacity: its alpha scaled, its colour
    * kept. The draw-time path; the stored pixel is unchanged.
    */
  def over(argb: Int): Int =
    val alpha = math.round(((argb >>> 24) & 0xff) * value).toInt
    (alpha << 24) | (argb & MapOpacity.ChannelBits)

object MapOpacity:
  private def apply(value: Double): MapOpacity =
    new MapOpacity(value) {}

  val Default: MapOpacity = MapOpacity(0.85)

  /** The low 24 bits of a packed pixel: its colour channels, without alpha. */
  private[maps] val ChannelBits: Int = (1 << 24) - 1

  def of(value: Double): Option[MapOpacity] =
    Option.when(value.isFinite && value >= 0.0 && value <= 1.0)(MapOpacity(value))
