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

package eyes4s.core

import eyes4s.kernel.*

/** Largest inter-observation interval that may be represented as gaze time. */
sealed trait MaximumSupportGap derives CanEqual:
  private[core] def retain(gap: Span): Span
  def render: String

object MaximumSupportGap:
  case object Unlimited extends MaximumSupportGap:
    private[core] def retain(gap: Span): Span = gap
    def render: String                        = "unlimited"

  final case class AtMost private[core] (span: Span) extends MaximumSupportGap:
    private[core] def retain(gap: Span): Span =
      if gap.toMicros <= span.toMicros then gap else span
    def render: String = s"at-most(${span.render})"

  def atMost(span: Span): Either[TemporalSupportError, MaximumSupportGap] =
    if span.isNegative then Left(TemporalSupportError.NegativeMaximumGap(span))
    else Right(AtMost(span))

/** How much time beyond the final timestamp belongs to its observation. */
sealed trait EdgeSupport derives CanEqual:
  def render: String

object EdgeSupport:
  case object Censored extends EdgeSupport:
    def render: String = "censored"

  case object PreviousInterval extends EdgeSupport:
    def render: String = "previous-interval"

  case object MedianInterval extends EdgeSupport:
    def render: String = "median-interval"

  final case class Fixed private[core] (span: Span) extends EdgeSupport:
    def render: String = s"fixed(${span.render})"

  def fixed(span: Span): Either[TemporalSupportError, EdgeSupport] =
    if span.isNegative then Left(TemporalSupportError.NegativeEdgeSupport(span))
    else Right(Fixed(span))

/** The temporal support assigned to recording samples before order is lost. */
sealed trait TemporalSupport derives CanEqual:
  def render: String

object TemporalSupport:
  final case class Fixed private[core] (period: Span) extends TemporalSupport:
    def render: String = s"fixed(period=${period.render})"

  final case class Voronoi(
      maxGap: MaximumSupportGap,
      edge: EdgeSupport
  ) extends TemporalSupport:
    def render: String = s"voronoi(maxGap=${maxGap.render}, edge=${edge.render})"

  final case class ForwardHold(
      maxGap: MaximumSupportGap,
      edge: EdgeSupport
  ) extends TemporalSupport:
    def render: String = s"forward-hold(maxGap=${maxGap.render}, edge=${edge.render})"

  def fixed(period: Span): Either[TemporalSupportError, TemporalSupport] =
    if period.isNegative || period.isZero then
      Left(TemporalSupportError.NonPositiveFixedPeriod(period))
    else Right(Fixed(period))

  private[core] def default(rate: Rate): TemporalSupport = rate match
    case Rate.Fixed(hz) => new Fixed(hz.period)
    case Rate.Irregular => Voronoi(MaximumSupportGap.Unlimited, EdgeSupport.MedianInterval)

/** A temporal-support configuration was not locally meaningful. */
enum TemporalSupportError derives CanEqual:
  case NonPositiveFixedPeriod(period: Span)
  case NegativeMaximumGap(maxGap: Span)
  case NegativeEdgeSupport(edgeSupport: Span)

  def message: String = this match
    case NonPositiveFixedPeriod(period) =>
      s"Fixed temporal support needs a positive period, got period=${period.render}."
    case NegativeMaximumGap(maxGap) =>
      s"Temporal support maximum gap cannot be negative, got maxGap=${maxGap.render}."
    case NegativeEdgeSupport(edgeSupport) =>
      s"Temporal edge support cannot be negative, got edgeSupport=${edgeSupport.render}."

/** Per-sample temporal support before spatial or AOI membership is applied.
  *
  * The constructor remains in `eyes4s-core` so occupancy and downstream
  * measures cannot silently implement different time-weighting conventions.
  */
final class SampleSupportLedger private[core] (
    val policy: TemporalSupport,
    private val durations: IArray[Span],
    val censoredTime: Span
):
  def size: Int                     = durations.length
  def get(index: Int): Option[Span] = durations.lift(index)
  def toVector: Vector[Span]        = durations.toVector
  def assignedTime: Span            = Span.micros(durations.foldLeft(0L)(_ + _.toMicros))
  def representedTime: Span         = assignedTime + censoredTime
  def isNonNegative: Boolean = durations.forall(!_.isNegative) && !censoredTime.isNegative

  private[eyes4s] def durationAtKnownIndex(index: Int): Span = durations(index)

end SampleSupportLedger

/** Spatial occupancy plus a complete account of represented and excluded time.
  *
  * Only a validated recording can construct this result, so its durations are
  * non-negative, its sample count is in range, and its measure mass agrees with
  * analysable time.
  */
final class OccupancyResult[U <: Unit2D] private (
    val measure: PointMeasure[U],
    val analysableTime: Span,
    val censoredTime: Span,
    val excludedSamples: Int,
    val policy: TemporalSupport
):
  def representedTime: Span = analysableTime + censoredTime

object OccupancyResult:
  private[core] def validated[U <: Unit2D](
      measure: PointMeasure[U],
      analysableTime: Span,
      censoredTime: Span,
      excludedSamples: Int,
      policy: TemporalSupport
  ): OccupancyResult[U] =
    new OccupancyResult(measure, analysableTime, censoredTime, excludedSamples, policy)

/** A validated temporal-support policy could not produce its spatial measure. */
enum OccupancyError derives CanEqual:
  case Measure(policy: TemporalSupport, underlying: SurfaceError)

  def message: String = this match
    case Measure(policy, underlying) =>
      s"Occupancy under policy=${policy.render} could not construct its measure: ${underlying.message}"
