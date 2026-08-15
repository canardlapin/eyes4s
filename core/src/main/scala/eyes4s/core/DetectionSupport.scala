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

/** Nominal identity of a source recording. */
final case class RecordingRef(value: String) derives CanEqual:
  override def toString: String = value

/** Non-empty half-open source-sample interval `[from, until)`. */
final class SampleRange private (val from: Int, val until: Int) derives CanEqual:
  def length: Int                   = until - from
  def contains(index: Int): Boolean = index >= from && index < until

  override def equals(other: Any): Boolean = other match
    case that: SampleRange => from == that.from && until == that.until
    case _                 => false

  override def hashCode: Int    = 31 * from + until
  override def toString: String = s"[$from,$until)"

object SampleRange:
  def of(from: Int, until: Int): Either[DetectionSupportError, SampleRange] =
    if from < 0 || until <= from then
      Left(DetectionSupportError.InvalidSampleRange(from, until))
    else Right(new SampleRange(from, until))

/** Immutable identity and derivation ledger for the samples underlying events.
  *
  * The ledger preserves the complete ordered lineage of every source sample.
  * Transformations append their derivation step, so a later consumer can still
  * distinguish measured, interpolated, smoothed, and projected support without
  * retaining an untyped external side table.
  */
final case class EventSourceLineage(
    frame: FrameId,
    contentHash: ContentHash,
    samples: Vector[SampleLineage]
)

/** Event construction support tied to one source, frame, and clock. */
final class EventSeries[U <: Unit2D] private (
    val recording: Recording[U],
    val source: RecordingRef,
    val events: Vector[Event[U]],
    val support: Vector[SampleRange],
    val lineage: EventSourceLineage
):
  def frame: Frame[U] = recording.frame
  def clock: ClockId  = recording.clock
  def size: Int       = events.length

  def supported(index: Int): Option[(Event[U], SampleRange)] =
    for
      event <- events.lift(index)
      range <- support.lift(index)
    yield event -> range

  /** Rebuild spatial event summaries from their original sample support.
    *
    * No scalar summary is copied across units. Fixation centres and dispersion
    * are recomputed from the source samples used by the detector; saccade and
    * pursuit geometry is transformed pointwise. An undefined image is a named
    * failure and never shortens an event or its support.
    */
  def warp[V <: Unit2D](transform: Warp[U, V]): Either[CoreError, EventSeries[V]] =
    for
      _     <- CoreError.widenGeometry(Agreement.frames(frame, transform.from))
      moved <- events.indices.foldLeft[Either[CoreError, Vector[Event[V]]]](
        Right(Vector.empty)
      ) { (acc, eventIndex) =>
        for
          built <- acc
          event <- EventSeries.transformEvent(
            recording,
            source,
            events(eventIndex),
            support(eventIndex),
            eventIndex,
            transform
          )
        yield built :+ event
      }
      projected <- recording.warp(transform)
      series    <- CoreError.widenDetectionSupport(
        EventSeries.of(projected, source, moved, support)
      )
    yield new EventSeries(
      series.recording,
      series.source,
      series.events,
      series.support,
      EventSourceLineage(lineage.frame, lineage.contentHash, series.lineage.samples)
    )

object EventSeries:
  def of[U <: Unit2D](
      recording: Recording[U],
      source: RecordingRef,
      events: Vector[Event[U]],
      support: Vector[SampleRange]
  ): Either[DetectionSupportError, EventSeries[U]] =
    for
      _ <- Either.cond(
        events.length == support.length,
        (),
        DetectionSupportError.EventSupportCountMismatch(source, events.length, support.length)
      )
      _ <- events.indices
        .find(index => events(index).span.clock != recording.clock)
        .fold[Either[DetectionSupportError, Unit]](Right(())) { index =>
          Left(
            DetectionSupportError.EventClockMismatch(
              source,
              index,
              recording.clock,
              events(index).span.clock
            )
          )
        }
      _ <- support.indices
        .find(index => support(index).until > recording.size)
        .fold[Either[DetectionSupportError, Unit]](Right(())) { index =>
          Left(
            DetectionSupportError.SampleRangeOutsideRecording(
              source,
              index,
              support(index),
              recording.size
            )
          )
        }
      _ <- support.indices
        .drop(1)
        .find(index => support(index).from < support(index - 1).until)
        .fold[Either[DetectionSupportError, Unit]](Right(())) { index =>
          Left(
            DetectionSupportError.OverlappingSampleRanges(
              source,
              index,
              support(index - 1),
              support(index)
            )
          )
        }
      supportedEvents <- events.indices.foldLeft[
        Either[DetectionSupportError, Vector[Event[U]]]
      ](Right(Vector.empty)) { (acc, index) =>
        for
          built <- acc
          event <- sourceSupportedEvent(recording, source, events(index), support(index), index)
        yield built :+ event
      }
    yield new EventSeries(
      recording,
      source,
      supportedEvents,
      support,
      EventSourceLineage(
        recording.frame.id,
        recording.contentHash,
        recording.samples.map(_.lineage).toVector
      )
    )

  private def sourceSupportedEvent[U <: Unit2D](
      recording: Recording[U],
      source: RecordingRef,
      event: Event[U],
      declared: SampleRange,
      eventIndex: Int
  ): Either[DetectionSupportError, Event[U]] =
    for
      _ <- Either.cond(
        event.span.onset.toMicros >= recording.extent.onset.toMicros &&
          event.span.offset.toMicros <= recording.extent.offset.toMicros,
        (),
        DetectionSupportError.EventSpanOutsideRecording(
          source,
          eventIndex,
          event.span,
          recording.extent
        )
      )
      derived <- rangeForSpan(recording, source, eventIndex, event.span)
      _       <- Either.cond(
        declared == derived,
        (),
        DetectionSupportError.EventSampleRangeMismatch(
          source,
          eventIndex,
          event.span,
          declared,
          derived
        )
      )
      supported <- event match
        case fixation: Event.Fixation[U] =>
          deriveFixation(recording, source, fixation, declared, eventIndex).map(identity)
        case other => Right(other)
    yield supported

  private def rangeForSpan[U <: Unit2D](
      recording: Recording[U],
      source: RecordingRef,
      eventIndex: Int,
      span: Interval
  ): Either[DetectionSupportError, SampleRange] =
    val indices =
      recording.samples.indices.filter(index => span.contains(recording.samples(index).t))
    indices.headOption match
      case None => Left(DetectionSupportError.EventSpanHasNoSamples(source, eventIndex, span))
      case Some(first) =>
        val until = indices.lastOption.fold(first + 1)(_ + 1)
        SampleRange
          .of(first, until)
          .left
          .map(_ =>
            DetectionSupportError.InvalidDerivedSampleRange(
              source,
              eventIndex,
              span,
              first,
              until
            )
          )

  private def deriveFixation[U <: Unit2D](
      recording: Recording[U],
      source: RecordingRef,
      fixation: Event.Fixation[U],
      range: SampleRange,
      eventIndex: Int
  ): Either[DetectionSupportError, Event[U]] =
    val points = (range.from until range.until).flatMap { sampleIndex =>
      val sample = recording.samples(sampleIndex)
      if sample.isUsable then sample.position else None
    }.toVector
    if points.isEmpty then
      Left(DetectionSupportError.NoUsableSourceSamples(source, eventIndex, range))
    else
      val centre  = centroid(points)
      val rebuilt = fixation.dispersion match
        case Some(spread) =>
          Event.Fixation.of(
            fixation.span,
            centre,
            dispersion(points, centre, spread.method),
            spread.method,
            points.length
          )
        case None => Event.Fixation.withoutDispersion(fixation.span, centre, points.length)
      rebuilt.left
        .map(DetectionSupportError.InvalidDerivedFixation(source, eventIndex, range, _))
        .map { value =>
          val evidenced = (value.dispersionStatus, fixation.dispersionStatus) match
            case (
                  DispersionStatus.Available(rebuiltSpread, _),
                  DispersionStatus.Available(_, evidence: SummaryEvidence.Recomputed)
                ) =>
              Event.Fixation.withRecomputedDispersion(value, rebuiltSpread, evidence)
            case _ => Event.Fixation.withSourceSupport(value, source, range)
          evidenced: Event[U]
        }

  private def transformEvent[U <: Unit2D, V <: Unit2D](
      recording: Recording[U],
      source: RecordingRef,
      event: Event[U],
      range: SampleRange,
      eventIndex: Int,
      transform: Warp[U, V]
  ): Either[CoreError, Event[V]] = event match
    case fixation: Event.Fixation[U] =>
      reconstructFixation(recording, source, fixation, range, eventIndex, transform)
    case saccade: Event.Saccade[U] =>
      for
        from <- mapEventPoint(source, eventIndex, "saccade-origin", 0, saccade.from, transform)
        to <- mapEventPoint(source, eventIndex, "saccade-destination", 1, saccade.to, transform)
      yield Event.Saccade.transformed(
        saccade,
        from,
        to,
        source,
        range,
        transform.from.id,
        transform.to.id
      )
    case blink: Event.Blink[U]     => Event.Blink.of[V](blink.span)
    case pursuit: Event.Pursuit[U] =>
      for
        path <- pursuit.path.indices.foldLeft[Either[CoreError, Vector[Pt[V]]]](
          Right(Vector.empty)
        ) { (acc, pointIndex) =>
          for
            built <- acc
            point <- mapEventPoint(
              source,
              eventIndex,
              "pursuit-path",
              pointIndex,
              pursuit.path(pointIndex),
              transform
            )
          yield built :+ point
        }
        moved <- Event.Pursuit.of(pursuit.span, IArray.from(path))
      yield moved

  private def reconstructFixation[U <: Unit2D, V <: Unit2D](
      recording: Recording[U],
      source: RecordingRef,
      fixation: Event.Fixation[U],
      range: SampleRange,
      eventIndex: Int,
      transform: Warp[U, V]
  ): Either[CoreError, Event.Fixation[V]] =
    val sourcePoints = (range.from until range.until).flatMap { sampleIndex =>
      val sample = recording.samples(sampleIndex)
      if sample.isUsable then sample.position.map(sampleIndex -> _) else None
    }
    for
      mapped <- sourcePoints.foldLeft[Either[CoreError, Vector[Pt[V]]]](Right(Vector.empty)) {
        case (acc, (sampleIndex, point)) =>
          for
            built <- acc
            moved <- transform(point).toRight(
              CoreError.OfDetectionSupport(
                DetectionSupportError.UnmappableSourceSample(
                  source,
                  eventIndex,
                  sampleIndex,
                  transform.from.id,
                  transform.to.id,
                  point.x,
                  point.y
                )
              )
            )
          yield built :+ moved
      }
      _ <- Either.cond(
        mapped.nonEmpty,
        (),
        CoreError.OfDetectionSupport(
          DetectionSupportError.NoUsableSourceSamples(source, eventIndex, range)
        )
      )
      centre = centroid(mapped)
      base <- fixation.dispersion match
        case Some(spread) =>
          val value = dispersion(mapped, centre, spread.method)
          Event.Fixation.of(
            fixation.span,
            centre,
            value,
            spread.method,
            mapped.length
          )
        case None =>
          Event.Fixation.withoutDispersion(fixation.span, centre, mapped.length)
      result <- fixation.dispersion match
        case Some(spread) =>
          Dispersion
            .of[V](dispersion(mapped, centre, spread.method), spread.method)
            .left
            .map(CoreError.OfEvent.apply)
            .map { rebuilt =>
              Event.Fixation.withRecomputedDispersion(
                base,
                rebuilt,
                SummaryEvidence.Recomputed(
                  source,
                  range,
                  transform.from.id,
                  transform.to.id,
                  SpatialTransform.of(transform)
                )
              )
            }
        case None => Right(base)
    yield result

  private def mapEventPoint[U <: Unit2D, V <: Unit2D](
      source: RecordingRef,
      eventIndex: Int,
      role: String,
      pointIndex: Int,
      point: Pt[U],
      transform: Warp[U, V]
  ): Either[CoreError, Pt[V]] =
    transform(point).toRight(
      CoreError.OfDetectionSupport(
        DetectionSupportError.UnmappableEventPoint(
          source,
          eventIndex,
          role,
          pointIndex,
          transform.from.id,
          transform.to.id,
          point.x,
          point.y
        )
      )
    )

  private def centroid[U <: Unit2D](points: Vector[Pt[U]]): Pt[U] =
    Pt(points.map(_.x).sum / points.length, points.map(_.y).sum / points.length)

  private def dispersion[U <: Unit2D](
      points: Vector[Pt[U]],
      centre: Pt[U],
      method: DispersionMethod
  ): Double = method match
    case DispersionMethod.RmsRadius =>
      math.sqrt(
        points.map(point => math.pow(centre.distanceTo(point), 2.0)).sum / points.length
      )
    case DispersionMethod.BoundingBoxWidth =>
      points.map(_.x).max - points.map(_.x).min
    case DispersionMethod.BoundingBoxDiagonal =>
      math.hypot(
        points.map(_.x).max - points.map(_.x).min,
        points.map(_.y).max - points.map(_.y).min
      )
    case DispersionMethod.MedianAbsoluteDeviation =>
      val medianPoint = Pt[U](median(points.map(_.x)), median(points.map(_.y)))
      median(points.map(_.distanceTo(medianPoint)))

  private def median(values: Vector[Double]): Double =
    val sorted = values.sorted
    val middle = sorted.length / 2
    if sorted.length % 2 == 1 then sorted(middle)
    else (sorted(middle - 1) + sorted(middle)) / 2.0

/** A source-support invariant was not satisfied. */
enum DetectionSupportError derives CanEqual:
  case InvalidSampleRange(from: Int, until: Int)
  case EventSupportCountMismatch(source: RecordingRef, events: Int, ranges: Int)
  case EventClockMismatch(
      source: RecordingRef,
      eventIndex: Int,
      expected: ClockId,
      actual: ClockId
  )
  case SampleRangeOutsideRecording(
      source: RecordingRef,
      rangeIndex: Int,
      range: SampleRange,
      recordingSamples: Int
  )
  case OverlappingSampleRanges(
      source: RecordingRef,
      rangeIndex: Int,
      previous: SampleRange,
      current: SampleRange
  )
  case EventSpanOutsideRecording(
      source: RecordingRef,
      eventIndex: Int,
      eventSpan: Interval,
      recordingExtent: Interval
  )
  case EventSpanHasNoSamples(source: RecordingRef, eventIndex: Int, eventSpan: Interval)
  case EventSampleRangeMismatch(
      source: RecordingRef,
      eventIndex: Int,
      eventSpan: Interval,
      declared: SampleRange,
      derived: SampleRange
  )
  case InvalidDerivedSampleRange(
      source: RecordingRef,
      eventIndex: Int,
      eventSpan: Interval,
      from: Int,
      until: Int
  )
  case InvalidDerivedFixation(
      source: RecordingRef,
      eventIndex: Int,
      range: SampleRange,
      underlying: CoreError
  )
  case NoUsableSourceSamples(source: RecordingRef, eventIndex: Int, range: SampleRange)
  case UnmappableSourceSample(
      source: RecordingRef,
      eventIndex: Int,
      sampleIndex: Int,
      from: FrameId,
      to: FrameId,
      x: Double,
      y: Double
  )
  case UnmappableEventPoint(
      source: RecordingRef,
      eventIndex: Int,
      role: String,
      pointIndex: Int,
      from: FrameId,
      to: FrameId,
      x: Double,
      y: Double
  )

  def message: String = this match
    case InvalidSampleRange(from, until) =>
      s"A source sample range must be non-empty and half-open with from >= 0, got [$from,$until)."
    case EventSupportCountMismatch(source, events, ranges) =>
      s"Recording '$source' has $events detected events but $ranges source-support ranges."
    case EventClockMismatch(source, index, expected, actual) =>
      s"Recording '$source' event[$index] is on clock '$actual', expected '$expected'."
    case SampleRangeOutsideRecording(source, index, range, sampleCount) =>
      s"Recording '$source' support[$index]=$range exceeds source sample count=$sampleCount."
    case OverlappingSampleRanges(source, index, previous, current) =>
      s"Recording '$source' support[$index]=$current overlaps previous=$previous; " +
        "a segmentation must be ordered and non-overlapping."
    case EventSpanOutsideRecording(source, index, eventSpan, recordingExtent) =>
      s"Recording '$source' event[$index] span=${eventSpan.render} lies outside " +
        s"recording extent=${recordingExtent.render}."
    case EventSpanHasNoSamples(source, index, eventSpan) =>
      s"Recording '$source' event[$index] span=${eventSpan.render} contains no source samples."
    case EventSampleRangeMismatch(source, index, eventSpan, declared, derived) =>
      s"Recording '$source' event[$index] span=${eventSpan.render} declares support=$declared, " +
        s"but the exact source timestamps derive support=$derived."
    case InvalidDerivedSampleRange(source, index, eventSpan, from, until) =>
      s"Recording '$source' event[$index] span=${eventSpan.render} produced an invalid " +
        s"derived source range=[$from,$until)."
    case InvalidDerivedFixation(source, index, range, underlying) =>
      s"Recording '$source' fixation[$index] support=$range could not be reconstructed: " +
        underlying.message
    case NoUsableSourceSamples(source, eventIndex, range) =>
      s"Recording '$source' event[$eventIndex] support=$range has no usable source samples " +
        "from which to reconstruct its spatial summary."
    case UnmappableSourceSample(source, eventIndex, sampleIndex, from, to, x, y) =>
      s"Recording '$source' event[$eventIndex] source sample[$sampleIndex]=($x, $y) in " +
        s"frame '$from' has no finite image in frame '$to'; the event was not shortened."
    case UnmappableEventPoint(source, eventIndex, role, pointIndex, from, to, x, y) =>
      s"Recording '$source' event[$eventIndex] $role[$pointIndex]=($x, $y) in frame '$from' " +
        s"has no finite image in frame '$to'; the event was not shortened."

end DetectionSupportError
