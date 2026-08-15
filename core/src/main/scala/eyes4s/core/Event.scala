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

/** Something the eye did over an extent of time.
  *
  * ==Events have extent; samples do not==
  *
  * This is why [[Overlap]] exists and why `Recording.within` takes no policy
  * while event selection does. A sample is an instant and either falls in a
  * window or does not. An event straddling the boundary genuinely admits three
  * answers, and which one an analysis takes changes its results.
  *
  * ==The interval is stored, not implied==
  *
  * `eyesim` represents a fixation as an onset plus a duration and has no offset
  * column anywhere in the package, so every window filter it performs tests the
  * onset alone: a fixation beginning one millisecond before a window closes is
  * counted in full, however long it ran on. Here the extent is an [[Interval]]
  * and the straddling policy is named.
  */
sealed trait Event[U <: Unit2D] derives CanEqual:
  def span: Interval
  def duration: Span = span.duration

  /** Where the event happened, for the events that have a single place. */
  def location: Option[Pt[U]] = this match
    case f: Event.Fixation[U] => Some(f.centre)
    case _                    => None

/** The statistic represented by a fixation's spatial spread. */
enum DispersionMethod derives CanEqual:
  case RmsRadius
  case BoundingBoxWidth
  case BoundingBoxDiagonal
  case MedianAbsoluteDeviation

/** A finite, non-negative spatial spread with an explicit statistic. */
final class Dispersion[U <: Unit2D] private (
    val radius: Distance[U],
    val method: DispersionMethod
) derives CanEqual:
  def value: Double = radius.value

  override def equals(other: Any): Boolean = other match
    case that: Dispersion[?] => radius.value == that.radius.value && method == that.method
    case _                   => false

  override def hashCode: Int = 31 * radius.value.hashCode + method.hashCode

  override def toString: String = s"Dispersion(${radius.value},$method)"

object Dispersion:
  def of[U <: Unit2D](
      value: Double,
      method: DispersionMethod
  ): Either[EventError, Dispersion[U]] =
    Distance
      .of[U](value)
      .left
      .map(_ => EventError.InvalidDispersion(value, method))
      .map(new Dispersion(_, method))

/** Structural evidence for the exact spatial transform used to rebuild an
  * event summary. This mirrors the closed [[Warp]] algebra rather than using a
  * rendered string as scientific identity.
  */
enum SpatialTransform derives CanEqual:
  case Identity
  case Affine(matrix: Mat3)
  case Homography(matrix: Mat3)
  case Tangent(perspective: Perspective, sense: Warp.Sense)
  case Then(first: SpatialTransform, second: SpatialTransform)

object SpatialTransform:
  def of[U <: Unit2D, V <: Unit2D](warp: Warp[U, V]): SpatialTransform = warp match
    case _: Warp.Id[?]                     => SpatialTransform.Identity
    case affine: Warp.Affine[?, ?]         => SpatialTransform.Affine(affine.m)
    case projective: Warp.Homography[?, ?] =>
      SpatialTransform.Homography(projective.h)
    case tangent: Warp.Tangent[?, ?] =>
      SpatialTransform.Tangent(tangent.perspective, tangent.sense)
    case composed: Warp.Then[?, ?, ?] =>
      SpatialTransform.Then(
        SpatialTransform.of(composed.f),
        SpatialTransform.of(composed.g)
      )

/** Why a reported fixation spread is scientifically available. */
enum SummaryEvidence derives CanEqual:
  case Declared
  case SourceSupported(source: RecordingRef, support: SampleRange)
  case Recomputed(
      source: RecordingRef,
      support: SampleRange,
      from: FrameId,
      to: FrameId,
      transform: SpatialTransform
  )

/** Why a fixation has no spatial spread after an otherwise valid operation. */
enum DispersionUnavailable derives CanEqual:
  case NotReported
  case SourceSupportUnavailable(from: FrameId, to: FrameId)

/** Availability and evidence for a fixation's derived spatial spread. */
enum DispersionStatus[U <: Unit2D] derives CanEqual:
  case Available(value: Dispersion[U], evidence: SummaryEvidence)
  case Unavailable(reason: DispersionUnavailable)

/** Why a saccade has no peak velocity estimate. */
enum PeakVelocityUnavailable derives CanEqual:
  case NotMeasured
  case RequiresReestimation(
      source: RecordingRef,
      support: SampleRange,
      from: FrameId,
      to: FrameId
  )

/** Availability of the detector-specific peak velocity summary. */
enum PeakVelocityStatus[U <: Unit2D] derives CanEqual:
  case Available(value: Velocity[U])
  case Unavailable(reason: PeakVelocityUnavailable)

/** Positive measured or derived sample support for an event summary. */
opaque type EventSampleCount = Int

object EventSampleCount:
  def of(sampleCount: Int): Either[EventError, EventSampleCount] =
    if sampleCount > 0 then Right(sampleCount)
    else Left(EventError.NonPositiveSampleCount(sampleCount))

  extension (sampleCount: EventSampleCount) def value: Int = sampleCount

/** A public event constructor rejected an incoherent summary. */
enum EventError derives CanEqual:
  case EmptySpan(eventType: String, span: Interval)
  case NonFinitePoint(eventType: String, role: String, span: Interval, x: Double, y: Double)
  case InvalidDispersion(value: Double, method: DispersionMethod)
  case NonPositiveSampleCount(sampleCount: Int)
  case EmptyPursuit(span: Interval)
  case NonFinitePursuitPoint(span: Interval, index: Int, x: Double, y: Double)

  def message: String = this match
    case EmptySpan(eventType, span) =>
      s"A $eventType event needs positive temporal extent, got span=${span.render}."
    case NonFinitePoint(eventType, role, span, x, y) =>
      s"A $eventType event needs a finite $role, got ($x, $y) for span=${span.render}."
    case InvalidDispersion(value, method) =>
      s"Fixation dispersion must be finite and non-negative, got value=$value for method=$method."
    case NonPositiveSampleCount(sampleCount) =>
      s"Fixation support must contain at least one sample, got sampleCount=$sampleCount."
    case EmptyPursuit(span) =>
      s"A pursuit event needs at least one path position, got an empty path for span=${span.render}."
    case NonFinitePursuitPoint(span, index, x, y) =>
      s"A pursuit event needs finite path positions, got path[$index]=($x, $y) for span=${span.render}."

end EventError

object Event:

  /** The eye held still. */
  sealed abstract case class Fixation[U <: Unit2D] private (
      span: Interval,
      centre: Pt[U],
      dispersionStatus: DispersionStatus[U],
      support: EventSampleCount
  ) extends Event[U]:
    def sampleCount: Int = support.value

    def dispersion: Option[Dispersion[U]] = dispersionStatus match
      case DispersionStatus.Available(value, _) => Some(value)
      case DispersionStatus.Unavailable(_)      => None

  object Fixation:
    def of[U <: Unit2D](
        span: Interval,
        centre: Pt[U],
        dispersion: Double,
        method: DispersionMethod,
        sampleCount: Int
    ): Either[CoreError, Fixation[U]] =
      for
        _       <- validateSpan("fixation", span)
        _       <- validatePoint("fixation", "centre", span, centre)
        spread  <- Dispersion.of[U](dispersion, method).left.map(CoreError.OfEvent.apply)
        support <- EventSampleCount.of(sampleCount).left.map(CoreError.OfEvent.apply)
      yield new Fixation(
        span,
        centre,
        DispersionStatus.Available(spread, SummaryEvidence.Declared),
        support
      ) {}

    /** Build a coherent fixation when spatial spread was not measured or is
      * invalidated by a transformation.
      */
    def withoutDispersion[U <: Unit2D](
        span: Interval,
        centre: Pt[U],
        sampleCount: Int
    ): Either[CoreError, Fixation[U]] =
      for
        _       <- validateSpan("fixation", span)
        _       <- validatePoint("fixation", "centre", span, centre)
        support <- EventSampleCount.of(sampleCount).left.map(CoreError.OfEvent.apply)
      yield new Fixation(
        span,
        centre,
        DispersionStatus.Unavailable(DispersionUnavailable.NotReported),
        support
      ) {}

    private[core] def withSourceSupport[U <: Unit2D](
        fixation: Fixation[U],
        source: RecordingRef,
        range: SampleRange
    ): Fixation[U] =
      val status = fixation.dispersionStatus match
        case DispersionStatus.Available(value, SummaryEvidence.Declared) =>
          DispersionStatus.Available(value, SummaryEvidence.SourceSupported(source, range))
        case other => other
      new Fixation(fixation.span, fixation.centre, status, fixation.support) {}

    private[core] def withRecomputedDispersion[U <: Unit2D](
        fixation: Fixation[U],
        spread: Dispersion[U],
        evidence: SummaryEvidence.Recomputed
    ): Fixation[U] =
      new Fixation(
        fixation.span,
        fixation.centre,
        DispersionStatus.Available(spread, evidence),
        fixation.support
      ) {}

    private[core] def transformed[U <: Unit2D, V <: Unit2D](
        source: Fixation[U],
        centre: Pt[V],
        from: FrameId,
        to: FrameId
    ): Fixation[V] =
      val status: DispersionStatus[V] = source.dispersionStatus match
        case DispersionStatus.Available(_, _) =>
          DispersionStatus.Unavailable(DispersionUnavailable.SourceSupportUnavailable(from, to))
        case DispersionStatus.Unavailable(reason) => DispersionStatus.Unavailable(reason)
      new Fixation(source.span, centre, status, source.support) {}

  /** The eye moved from one place to another.
    *
    * `peakVelocity` is optional because there are two ways to come by a
    * saccade, and they know different things. A detector working from samples
    * measures the velocity profile. A saccade *inferred* from a pair of
    * consecutive fixations knows only its endpoints -- and reporting a peak
    * velocity that was never measured is exactly the sort of invented number
    * this library exists to prevent.
    */
  sealed abstract case class Saccade[U <: Unit2D] private (
      span: Interval,
      from: Pt[U],
      to: Pt[U],
      peakVelocityStatus: PeakVelocityStatus[U]
  ) extends Event[U]:
    def peakVelocity: Option[Velocity[U]] = peakVelocityStatus match
      case PeakVelocityStatus.Available(value) => Some(value)
      case PeakVelocityStatus.Unavailable(_)   => None

  object Saccade:
    def of[U <: Unit2D](
        span: Interval,
        from: Pt[U],
        to: Pt[U],
        peakVelocity: Option[Velocity[U]]
    ): Either[CoreError, Saccade[U]] =
      val status: PeakVelocityStatus[U] = peakVelocity match
        case Some(value) => PeakVelocityStatus.Available(value)
        case None        => PeakVelocityStatus.Unavailable(PeakVelocityUnavailable.NotMeasured)
      for
        _ <- validateSpan("saccade", span)
        _ <- validatePoint("saccade", "origin", span, from)
        _ <- validatePoint("saccade", "destination", span, to)
      yield new Saccade(span, from, to, status) {}

    private[core] def transformed[U <: Unit2D, V <: Unit2D](
        saccade: Saccade[U],
        from: Pt[V],
        to: Pt[V],
        source: RecordingRef,
        support: SampleRange,
        fromFrame: FrameId,
        toFrame: FrameId
    ): Saccade[V] =
      val status: PeakVelocityStatus[V] = saccade.peakVelocityStatus match
        case PeakVelocityStatus.Available(_) =>
          PeakVelocityStatus.Unavailable(
            PeakVelocityUnavailable.RequiresReestimation(
              source,
              support,
              fromFrame,
              toFrame
            )
          )
        case PeakVelocityStatus.Unavailable(reason) => PeakVelocityStatus.Unavailable(reason)
      new Saccade(saccade.span, from, to, status) {}

  /** The eye was closed. */
  sealed abstract case class Blink[U <: Unit2D] private (span: Interval) extends Event[U]

  object Blink:
    def of[U <: Unit2D](span: Interval): Either[CoreError, Blink[U]] =
      validateSpan("blink", span).map(_ => new Blink[U](span) {})

  /** The eye tracked a moving target. */
  sealed abstract case class Pursuit[U <: Unit2D] private (
      span: Interval,
      path: IArray[Pt[U]]
  ) extends Event[U]

  object Pursuit:
    def of[U <: Unit2D](
        span: Interval,
        path: IArray[Pt[U]]
    ): Either[CoreError, Pursuit[U]] =
      for
        _ <- validateSpan("pursuit", span)
        _ <- Either.cond(path.nonEmpty, (), CoreError.OfEvent(EventError.EmptyPursuit(span)))
        _ <- path.indices
          .find(i => !finite(path(i)))
          .fold[Either[CoreError, Unit]](Right(())) { i =>
            val point = path(i)
            Left(CoreError.OfEvent(EventError.NonFinitePursuitPoint(span, i, point.x, point.y)))
          }
      yield new Pursuit(span, path) {}

  private def finite[U <: Unit2D](point: Pt[U]): Boolean =
    point.x.isFinite && point.y.isFinite

  private def validateSpan(eventType: String, span: Interval): Either[CoreError, Unit] =
    Either.cond(!span.isEmpty, (), CoreError.OfEvent(EventError.EmptySpan(eventType, span)))

  private def validatePoint[U <: Unit2D](
      eventType: String,
      role: String,
      span: Interval,
      point: Pt[U]
  ): Either[CoreError, Unit] =
    Either.cond(
      finite(point),
      (),
      CoreError.OfEvent(EventError.NonFinitePoint(eventType, role, span, point.x, point.y))
    )

  extension [U <: Unit2D](s: Event.Saccade[U])

    /** Straight-line displacement between the endpoints. */
    def displacement: Vec2[U] = s.from.vectorTo(s.to)

    /** Magnitude of that displacement. In an angular frame, the amplitude in
      * degrees; in a display frame, a number of pixels and not comparable
      * across setups.
      */
    def amplitude: Double = displacement.norm

    def direction: Angle = displacement.angle

    /** Average speed over the extent, which is not the peak.
      *
      * Named separately from [[Event.Saccade.peakVelocity]] because the two
      * differ by roughly a factor of two for a typical saccade, and a main
      * sequence fitted to the wrong one is simply a different relationship.
      */
    def meanVelocity: Option[Velocity[U]] = Velocity.over(displacement, s.duration)

  extension [U <: Unit2D](f: Event.Fixation[U])
    /** Whether the fixation's centre lies in a region. */
    def isIn(r: Region[U]): Boolean = r.contains(f.centre)

  /** Select events under an explicit straddling policy.
    *
    * The policy is required rather than defaulted. `eyesim`'s implicit choice
    * is onset-inside, applied at four separate call sites and named at none of
    * them; making it an argument means a reader of the call can see which
    * convention an analysis used.
    */
  def select[U <: Unit2D](
      events: Vector[Event[U]],
      window: Interval,
      policy: Overlap
  ): Either[TimeError, Vector[Event[U]]] =
    events.foldLeft[Either[TimeError, Vector[Event[U]]]](Right(Vector.empty)) { (acc, e) =>
      for
        kept <- acc
        hit  <- policy.selects(e.span, window)
      yield if hit then kept :+ e else kept
    }
