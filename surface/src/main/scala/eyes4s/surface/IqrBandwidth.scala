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

package eyes4s.surface

import eyes4s.kernel.*

enum IqrBandwidthClamp derives CanEqual:
  case Unclamped, DisplayOneToFifteenPercent

enum IqrBandwidthError derives CanEqual:
  case TooFewPoints(frame: FrameId, count: Int)
  case Sigma(frame: FrameId, underlying: GeometryError)
  def message: String = this match
    case TooFewPoints(f, n) =>
      s"IQR bandwidth in frame=$f requires at least two points; count=$n."
    case Sigma(f, e) => s"IQR bandwidth in frame=$f: ${e.message}"

/** Unweighted type-7 quartiles and sqrt((IQRx^2+IQRy^2)/2) / 1.349 * n^(-1/6).
  * This pinned eyesim convention is separate from Bandwidth.silverman and scott.
  * The optional clamp uses the mean frame width/height. No implicit unit conversion.
  */
object IqrBandwidth:
  def suggest[U <: Unit2D](
      measure: PointMeasure[U],
      clamp: IqrBandwidthClamp
  ): Either[IqrBandwidthError, Sigma[U]] =
    if measure.size < 2 then
      Left(IqrBandwidthError.TooFewPoints(measure.frame.id, measure.size))
    else
      def iqr(values: Vector[Double]): Double =
        val sorted                      = values.sorted
        def quantile(p: Double): Double =
          val index    = (sorted.size - 1) * p
          val lower    = math.floor(index).toInt
          val upper    = math.ceil(index).toInt
          val fraction = index - lower
          sorted(lower) * (1 - fraction) + sorted(upper) * fraction
        quantile(0.75) - quantile(0.25)
      val x   = iqr(measure.positions.toVector.map(_.x))
      val y   = iqr(measure.positions.toVector.map(_.y))
      val raw =
        math.hypot(x, y) / math.sqrt(2.0) / 1.349 * math.pow(measure.size.toDouble, -1.0 / 6.0)
      val value = clamp match
        case IqrBandwidthClamp.Unclamped                  => raw
        case IqrBandwidthClamp.DisplayOneToFifteenPercent =>
          val display = measure.frame.bounds.width / 2 + measure.frame.bounds.height / 2
          math.min(math.max(raw, display * 0.01), display * 0.15)
      Sigma.of[U](value).left.map(IqrBandwidthError.Sigma(measure.frame.id, _))
