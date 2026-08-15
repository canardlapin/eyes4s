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

package eyes4s.detect

import eyes4s.core.Sample
import eyes4s.kernel.{Instant, Span, Unit2D, Velocity}
import eyes4s.kernel.Unit2D.Deg

/** A rejected filter or detector parameter.
  *
  * Each case retains the named operand that failed so an application can point
  * to the exact field without parsing [[message]].
  */
enum ConfigurationError derives CanEqual:
  case NonPositiveWindowHalfWidth(halfWidth: Int)
  case InsufficientRegularSamples(sampleCount: Int)
  case NonPositiveSamplingInterval(index: Int, interval: Span)
  case IrregularSamplingInterval(index: Int, expected: Span, observed: Span)
  case NegativeMissingPadding(pad: Span)
  case NegativeInterpolationGap(maxGap: Span)
  case NegativeMaximumMergeGap(maxGap: Span)
  case NonPositiveMinimumEventDuration(minDuration: Span)
  case NonPositiveIvtThreshold(thresholdDegPerSecond: Double)
  case InvalidEkThresholds(etaXDegPerSecond: Double, etaYDegPerSecond: Double)
  case InvalidEkMultiplier(lambda: Double)
  case NonPositiveEkMinimumSamples(minSamples: Int)

  def message: String = this match
    case NonPositiveWindowHalfWidth(halfWidth) =>
      s"A centred-filter half-width must be positive, got halfWidth=$halfWidth."
    case InsufficientRegularSamples(sampleCount) =>
      s"Regular sampling needs at least two timestamps, got sampleCount=$sampleCount."
    case NonPositiveSamplingInterval(index, interval) =>
      s"Regular sampling needs a positive interval before sample[$index], got interval=${interval.render}."
    case IrregularSamplingInterval(index, expected, observed) =>
      s"Regular sampling expected interval=${expected.render} before sample[$index], got observed=${observed.render}."
    case NegativeMissingPadding(pad) =>
      s"Missing-data padding cannot be negative, got pad=${pad.render}."
    case NegativeInterpolationGap(maxGap) =>
      s"The interpolation gap limit cannot be negative, got maxGap=${maxGap.render}."
    case NegativeMaximumMergeGap(maxGap) =>
      s"The adjacent-fixation merge gap cannot be negative, got maxGap=${maxGap.render}."
    case NonPositiveMinimumEventDuration(minDuration) =>
      s"An event minimum duration must be positive, got minDuration=${minDuration.render}."
    case NonPositiveIvtThreshold(threshold) =>
      s"An I-VT threshold must be positive, got threshold=$threshold deg/s."
    case InvalidEkThresholds(etaX, etaY) =>
      s"Engbert-Kliegl thresholds must be finite and positive, got etaX=$etaX deg/s and etaY=$etaY deg/s."
    case InvalidEkMultiplier(lambda) =>
      s"The Engbert-Kliegl threshold multiplier must be finite and positive, got lambda=$lambda."
    case NonPositiveEkMinimumSamples(minSamples) =>
      s"An Engbert-Kliegl run must contain at least one sample, got minSamples=$minSamples."

end ConfigurationError

/** Positive radius on each side of a centred filter window. */
opaque type WindowHalfWidth = Int

object WindowHalfWidth:
  def of(halfWidth: Int): Either[ConfigurationError, WindowHalfWidth] =
    if halfWidth > 0 then Right(halfWidth)
    else Left(ConfigurationError.NonPositiveWindowHalfWidth(halfWidth))

  extension (halfWidth: WindowHalfWidth) def value: Int = halfWidth

/** Exact regular timestamp spacing proved from a finite sample sequence. */
final class RegularSampling private (
    val period: Span,
    val first: Instant,
    val last: Instant,
    val sampleCount: Int
) derives CanEqual

object RegularSampling:
  def from[U <: Unit2D](
      samples: Iterable[Sample[U]]
  ): Either[ConfigurationError, RegularSampling] =
    val timestamps = samples.iterator.map(_.t).toVector
    if timestamps.length < 2 then
      Left(ConfigurationError.InsufficientRegularSamples(timestamps.length))
    else
      val expected = timestamps.head.until(timestamps(1))
      if expected.isNegative || expected.isZero then
        Left(ConfigurationError.NonPositiveSamplingInterval(1, expected))
      else
        timestamps.indices
          .drop(2)
          .find(i => timestamps(i - 1).until(timestamps(i)) != expected)
          .fold[Either[ConfigurationError, RegularSampling]](
            Right(
              new RegularSampling(expected, timestamps.head, timestamps.last, timestamps.length)
            )
          ) { index =>
            Left(
              ConfigurationError.IrregularSamplingInterval(
                index,
                expected,
                timestamps(index - 1).until(timestamps(index))
              )
            )
          }

/** Non-negative time condemned on either side of a missing observation. */
opaque type MissingPadding = Span

object MissingPadding:
  val none: MissingPadding = Span.zero

  def of(pad: Span): Either[ConfigurationError, MissingPadding] =
    if !pad.isNegative then Right(pad)
    else Left(ConfigurationError.NegativeMissingPadding(pad))

  extension (pad: MissingPadding) def span: Span = pad

/** Non-negative largest missing interval eligible for interpolation. */
opaque type InterpolationGap = Span

object InterpolationGap:
  val none: InterpolationGap = Span.zero

  def of(maxGap: Span): Either[ConfigurationError, InterpolationGap] =
    if !maxGap.isNegative then Right(maxGap)
    else Left(ConfigurationError.NegativeInterpolationGap(maxGap))

  extension (maxGap: InterpolationGap) def span: Span = maxGap

/** Non-negative largest interval across which fixation fragments may merge. */
opaque type MaximumMergeGap = Span

object MaximumMergeGap:
  val none: MaximumMergeGap = Span.zero

  def of(maxGap: Span): Either[ConfigurationError, MaximumMergeGap] =
    if !maxGap.isNegative then Right(maxGap)
    else Left(ConfigurationError.NegativeMaximumMergeGap(maxGap))

  extension (maxGap: MaximumMergeGap) def span: Span = maxGap

/** Strictly positive duration required before a candidate becomes an event. */
opaque type MinimumEventDuration = Span

object MinimumEventDuration:
  def of(minDuration: Span): Either[ConfigurationError, MinimumEventDuration] =
    if !minDuration.isNegative && !minDuration.isZero then Right(minDuration)
    else Left(ConfigurationError.NonPositiveMinimumEventDuration(minDuration))

  extension (minDuration: MinimumEventDuration) def span: Span = minDuration

/** Strictly positive physical threshold used by I-VT. */
opaque type IvtThreshold = Velocity[Deg]

object IvtThreshold:
  def of(threshold: Velocity[Deg]): Either[ConfigurationError, IvtThreshold] =
    if threshold.value > 0.0 then Right(threshold)
    else Left(ConfigurationError.NonPositiveIvtThreshold(threshold.value))

  extension (threshold: IvtThreshold) def velocity: Velocity[Deg] = threshold

/** Finite positive multiplier for Engbert-Kliegl threshold estimation. */
opaque type EkMultiplier = Double

object EkMultiplier:
  def of(lambda: Double): Either[ConfigurationError, EkMultiplier] =
    if lambda.isFinite && lambda > 0.0 then Right(lambda)
    else Left(ConfigurationError.InvalidEkMultiplier(lambda))

  extension (lambda: EkMultiplier) def value: Double = lambda

/** Positive number of consecutive samples required for a microsaccade. */
opaque type EkMinimumSamples = Int

object EkMinimumSamples:
  def of(minSamples: Int): Either[ConfigurationError, EkMinimumSamples] =
    if minSamples > 0 then Right(minSamples)
    else Left(ConfigurationError.NonPositiveEkMinimumSamples(minSamples))

  extension (minSamples: EkMinimumSamples) def value: Int = minSamples
