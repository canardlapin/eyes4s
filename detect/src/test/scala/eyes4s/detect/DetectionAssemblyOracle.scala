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

import eyes4s.core.*
import eyes4s.kernel.*

/** Pre-resumable assembly retained as an independent oracle for labels, gaps, reports,
  * event summaries and lineage. Nothing here calls the production `EventSeries`: the
  * source-support series is rebuilt with the direct linear-scan and sort-based formulas of
  * 131970d (`core/.../DetectionSupport.scala`), so a defect in the resumable assembly
  * cannot also appear in its expected value.
  */
private object DetectionAssemblyOracle:

  /** An event as the comparison sees it. Fixation evidence constructors are private to
    * core, so a fixation is compared through its public fields.
    */
  enum EventView[U <: Unit2D] derives CanEqual:
    case Fixation(
        span: Interval,
        centre: Pt[U],
        status: DispersionStatus[U],
        sampleCount: Int
    )
    case Other(event: Event[U])

  def viewOf[U <: Unit2D](event: Event[U]): EventView[U] = event match
    case fixation: Event.Fixation[U] =>
      EventView.Fixation(
        fixation.span,
        fixation.centre,
        fixation.dispersionStatus,
        fixation.sampleCount
      )
    case other => EventView.Other(other)

  /** Everything the suites compare about one `DetectionResult`. */
  final case class ResultView[U <: Unit2D](
      labels: Vector[SampleClass],
      events: Vector[EventView[U]],
      support: Vector[SampleRange],
      lineage: EventSourceLineage,
      report: DetectionReport,
      provenance: String
  )

  def viewOf[U <: Unit2D](result: DetectionResult[U]): ResultView[U] = ResultView(
    result.labels.toVector,
    result.eventSeries.events.map(viewOf),
    result.eventSeries.support,
    result.eventSeries.lineage,
    result.report,
    result.provenance.render
  )

  /** The 131970d `EventSeries.of`, with the same validation order and error values. */
  def referenceSeries[U <: Unit2D](
      recording: Recording[U],
      source: RecordingRef,
      events: Vector[Event[U]],
      support: Vector[SampleRange]
  ): Either[DetectionSupportError, (Vector[EventView[U]], EventSourceLineage)] =
    def first(indices: Seq[Int])(bad: Int => Boolean)(
        error: Int => DetectionSupportError
    ): Either[DetectionSupportError, Unit] =
      indices.find(bad).fold(Right(()))(index => Left(error(index)))
    for
      _ <- Either.cond(
        events.length == support.length,
        (),
        DetectionSupportError.EventSupportCountMismatch(source, events.length, support.length)
      )
      _ <- first(events.indices)(i => events(i).span.clock != recording.clock) { i =>
        DetectionSupportError.EventClockMismatch(
          source,
          i,
          recording.clock,
          events(i).span.clock
        )
      }
      _ <- first(support.indices)(i => support(i).until > recording.size) { i =>
        DetectionSupportError.SampleRangeOutsideRecording(source, i, support(i), recording.size)
      }
      _ <- first(support.indices.drop(1))(i => support(i).from < support(i - 1).until) { i =>
        DetectionSupportError.OverlappingSampleRanges(source, i, support(i - 1), support(i))
      }
      views <- events.indices.foldLeft[Either[DetectionSupportError, Vector[EventView[U]]]](
        Right(Vector.empty)
      ) { (acc, i) =>
        for
          built <- acc
          view  <- referenceEvent(recording, source, events(i), support(i), i)
        yield built :+ view
      }
    yield (
      views,
      EventSourceLineage(
        recording.frame.id,
        contentHash(recording),
        recording.samples.map(_.lineage).toVector
      )
    )

  private def referenceEvent[U <: Unit2D](
      recording: Recording[U],
      source: RecordingRef,
      event: Event[U],
      declared: SampleRange,
      eventIndex: Int
  ): Either[DetectionSupportError, EventView[U]] =
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
      indices = recording.samples.indices.filter(i =>
        event.span.contains(recording.samples(i).t)
      )
      derived <- indices.headOption match
        case None =>
          Left(DetectionSupportError.EventSpanHasNoSamples(source, eventIndex, event.span))
        case Some(from) =>
          val until = indices.last + 1
          SampleRange
            .of(from, until)
            .left
            .map(_ =>
              DetectionSupportError
                .InvalidDerivedSampleRange(source, eventIndex, event.span, from, until)
            )
      _ <- Either.cond(
        declared == derived,
        (),
        DetectionSupportError
          .EventSampleRangeMismatch(source, eventIndex, event.span, declared, derived)
      )
      view <- event match
        case fixation: Event.Fixation[U] =>
          referenceFixation(recording, source, fixation, declared, eventIndex)
        case other => Right(EventView.Other(other))
    yield view

  private def referenceFixation[U <: Unit2D](
      recording: Recording[U],
      source: RecordingRef,
      fixation: Event.Fixation[U],
      range: SampleRange,
      eventIndex: Int
  ): Either[DetectionSupportError, EventView[U]] =
    val points = (range.from until range.until).flatMap { i =>
      val sample = recording.samples(i)
      if sample.isUsable then sample.position else None
    }.toVector
    if points.isEmpty then
      Left(DetectionSupportError.NoUsableSourceSamples(source, eventIndex, range))
    else
      val centre =
        Pt[U](points.map(_.x).sum / points.length, points.map(_.y).sum / points.length)
      val rebuilt = fixation.dispersion match
        case Some(spread) =>
          Event.Fixation.of(
            fixation.span,
            centre,
            referenceDispersion(points, centre, spread.method),
            spread.method,
            points.length
          )
        case None => Event.Fixation.withoutDispersion(fixation.span, centre, points.length)
      rebuilt.left
        .map(DetectionSupportError.InvalidDerivedFixation(source, eventIndex, range, _))
        .map { value =>
          val status: DispersionStatus[U] =
            (value.dispersionStatus, fixation.dispersionStatus) match
              case (
                    DispersionStatus.Available(spread, _),
                    DispersionStatus.Available(_, evidence: SummaryEvidence.Recomputed)
                  ) =>
                DispersionStatus.Available(spread, evidence)
              case (DispersionStatus.Available(spread, SummaryEvidence.Declared), _) =>
                DispersionStatus.Available(
                  spread,
                  SummaryEvidence.SourceSupported(source, range)
                )
              case (other, _) => other
          EventView.Fixation(fixation.span, centre, status, points.length)
        }

  def referenceDispersion[U <: Unit2D](
      points: Vector[Pt[U]],
      centre: Pt[U],
      method: DispersionMethod
  ): Double = method match
    case DispersionMethod.RmsRadius =>
      math.sqrt(points.map(p => math.pow(centre.distanceTo(p), 2.0)).sum / points.length)
    case DispersionMethod.BoundingBoxWidth =>
      points.map(_.x).max - points.map(_.x).min
    case DispersionMethod.BoundingBoxDiagonal =>
      math.hypot(
        points.map(_.x).max - points.map(_.x).min,
        points.map(_.y).max - points.map(_.y).min
      )
    case DispersionMethod.MedianAbsoluteDeviation =>
      val middle = Pt[U](referenceMedian(points.map(_.x)), referenceMedian(points.map(_.y)))
      referenceMedian(points.map(_.distanceTo(middle)))

  def referenceMedian(values: Vector[Double]): Double =
    val sorted = values.sorted
    val middle = sorted.length / 2
    if sorted.length % 2 == 1 then sorted(middle)
    else (sorted(middle - 1) + sorted(middle)) / 2.0

  private[detect] def assembleEmissions[U <: Unit2D](
      source: RecordingRef,
      recording: Recording[U],
      identity: DetectorIdentity,
      gapPolicy: GapPolicy,
      temporalSupport: SampleSupportLedger,
      emissions: Vector[DetectionEmission[U]],
      parameters: Vector[(String, Provenance.Param)]
  ): Either[DetectionResultError, ResultView[U]] =
    val detector = identity.detectorRef
    locally {
      val events = emissions.collect { case Right(event) => event }
      for
        support <- supportFor(source, recording, detector, events)
        series  <- referenceSeries(recording, source, events, support).left
          .map(DetectionResultError.SourceSupport(source, detector, _))
        bridged <- validateGaps(
          source,
          recording,
          detector,
          gapPolicy,
          temporalSupport,
          support
        )
        result <- assemble(
          source,
          recording,
          identity,
          gapPolicy,
          temporalSupport,
          series,
          support,
          bridged,
          parameters
        )
      yield result
    }

  private def supportFor[U <: Unit2D](
      source: RecordingRef,
      recording: Recording[U],
      detector: DetectorRef,
      events: Vector[Event[U]]
  ): Either[DetectionResultError, Vector[SampleRange]] =
    events.zipWithIndex.foldLeft[Either[DetectionResultError, Vector[SampleRange]]](
      Right(Vector.empty)
    ) { case (acc, (event, eventIndex)) =>
      for
        ranges <- acc
        _      <- Either.cond(
          event.span.onset.toMicros >= recording.extent.onset.toMicros &&
            event.span.offset.toMicros <= recording.extent.offset.toMicros,
          (),
          DetectionResultError.EventOutsideRecording(
            source,
            detector,
            eventIndex,
            event.span,
            recording.extent
          )
        )
        indices = (0 until recording.size).filter(index =>
          event.span.contains(recording.samples(index).t)
        )
        range <- (indices.headOption, indices.lastOption) match
          case (Some(first), Some(last)) =>
            SampleRange
              .of(first, last + 1)
              .left
              .map(
                DetectionResultError.InvalidDerivedRange(
                  source,
                  detector,
                  s"event[$eventIndex]-support",
                  first,
                  last + 1,
                  _
                )
              )
          case _ =>
            Left(
              DetectionResultError.EventOutsideRecording(
                source,
                detector,
                eventIndex,
                event.span,
                recording.extent
              )
            )
      yield ranges :+ range
    }

  private def validateGaps[U <: Unit2D](
      source: RecordingRef,
      recording: Recording[U],
      detector: DetectorRef,
      policy: GapPolicy,
      temporalSupport: SampleSupportLedger,
      support: Vector[SampleRange]
  ): Either[DetectionResultError, Vector[SampleRange]] =
    support.zipWithIndex.foldLeft[Either[DetectionResultError, Vector[SampleRange]]](
      Right(Vector.empty)
    ) { case (acc, (eventRange, eventIndex)) =>
      for
        collected <- acc
        gaps      <- contiguousRanges(
          source,
          detector,
          s"event[$eventIndex]-invalid-support",
          (eventRange.from until eventRange.until).filter(index =>
            !recording.samples(index).isUsable
          )
        )
        accepted <- gaps.foldLeft[Either[DetectionResultError, Vector[SampleRange]]](
          Right(collected)
        ) { (gapAcc, gap) =>
          val duration = representedDuration(temporalSupport, gap)
          policy match
            case GapPolicy.Bridge(maximum) if duration.toMicros <= maximum.span.toMicros =>
              gapAcc.map(_ :+ gap)
            case _ =>
              Left(
                DetectionResultError.GapPolicyViolation(
                  source,
                  detector,
                  eventIndex,
                  gap,
                  duration,
                  policy
                )
              )
        }
      yield accepted
    }

  private def assemble[U <: Unit2D](
      source: RecordingRef,
      recording: Recording[U],
      identity: DetectorIdentity,
      gapPolicy: GapPolicy,
      temporalSupport: SampleSupportLedger,
      series: (Vector[EventView[U]], EventSourceLineage),
      support: Vector[SampleRange],
      bridged: Vector[SampleRange],
      parameters: Vector[(String, Provenance.Param)]
  ): Either[DetectionResultError, ResultView[U]] =
    val detector = identity.detectorRef
    val classes  = Array.tabulate(recording.size) { index =>
      recording.samples(index).gaze match
        case Gaze.Tracked(_, _) => SampleClass.Unclassified
        case Gaze.Blink()       => SampleClass.Blink
        case Gaze.Lost()        => SampleClass.Missing
        case Gaze.OffScreen(_)  => SampleClass.OffSurface
    }

    val (events, lineage) = series
    events.indices.foreach { eventIndex =>
      val eventClass = events(eventIndex) match
        case _: EventView.Fixation[U]              => SampleClass.Fixation
        case EventView.Other(_: Event.Fixation[U]) => SampleClass.Fixation
        case EventView.Other(_: Event.Saccade[U])  => SampleClass.Saccade
        case EventView.Other(_: Event.Pursuit[U])  => SampleClass.Pursuit
        case EventView.Other(_: Event.Blink[U])    => SampleClass.Blink
      val range = support(eventIndex)
      (range.from until range.until).foreach { sampleIndex =>
        if recording.samples(sampleIndex).isUsable then classes(sampleIndex) = eventClass
      }
    }

    for unclassified <- contiguousRanges(
        source,
        detector,
        "unclassified-support",
        classes.indices.filter(i => classes(i) == SampleClass.Unclassified)
      )
    yield
      val labels   = classes.toVector
      val warnings = unclassified.map { range =>
        DetectionWarning.UnclassifiedSupport(
          source,
          detector,
          range,
          representedDuration(temporalSupport, range)
        )
      }
      val durations = SampleClass.values.toVector.map { sampleClass =>
        val micros = classes.indices
          .filter(index => classes(index) == sampleClass)
          .map(index => temporalSupport.durationAtKnownIndex(index).toMicros)
          .sum
        sampleClass -> Span.micros(micros)
      }
      val report = DetectionReport(
        source,
        detector,
        gapPolicy,
        temporalSupport.policy,
        temporalSupport.censoredTime,
        recording.size,
        durations,
        unclassified,
        bridged,
        warnings
      )
      val step = Provenance.Step(
        "detect",
        Vector(
          "detector"  -> Provenance.Param.Text(detector.name),
          "version"   -> Provenance.Param.Text(detector.version),
          "source"    -> Provenance.Param.Text(source.value),
          "gapPolicy" -> Provenance.Param.Text(gapPolicy.render)
        ) ++ parameters
      )
      ResultView(
        labels,
        events,
        support,
        lineage,
        report,
        Provenance.raw(contentHash(recording)).andThen(step).render
      )

  private def contiguousRanges(
      source: RecordingRef,
      detector: DetectorRef,
      role: String,
      indices: Seq[Int]
  ): Either[DetectionResultError, Vector[SampleRange]] =
    indices.headOption match
      case None        => Right(Vector.empty)
      case Some(first) =>
        val boundaries = indices.tail.foldLeft(Vector.empty[(Int, Int)] -> (first, first)) {
          case ((completed, (from, last)), index) =>
            if index == last + 1 then completed      -> (from, index)
            else (completed :+ (from -> (last + 1))) -> (index, index)
        }
        val (completed, (from, last)) = boundaries
        (completed :+ (from -> (last + 1))).foldLeft[
          Either[DetectionResultError, Vector[SampleRange]]
        ](Right(Vector.empty)) { case (acc, (rangeFrom, rangeUntil)) =>
          for
            built <- acc
            range <- SampleRange
              .of(rangeFrom, rangeUntil)
              .left
              .map(
                DetectionResultError.InvalidDerivedRange(
                  source,
                  detector,
                  role,
                  rangeFrom,
                  rangeUntil,
                  _
                )
              )
          yield built :+ range
        }

  private def representedDuration(
      temporalSupport: SampleSupportLedger,
      range: SampleRange
  ): Span =
    Span.micros(
      (range.from until range.until).foldLeft(0L) { (total, index) =>
        total + temporalSupport.durationAtKnownIndex(index).toMicros
      }
    )

  def contentHash[U <: Unit2D](input: Recording[U]): ContentHash =
    import input.*
    val samplingHash = samplingEvidence match
      case fixed: SamplingEvidence.Fixed =>
        ContentHash.combineAll(
          Seq(
            ContentHash.ofString("sampling:fixed"),
            ContentHash.of(IArray(fixed.rate.value)),
            ContentHash.ofString(fixed.nominalPeriod.toMicros.toString),
            ContentHash.ofString(fixed.tolerance.toSpan.toMicros.toString),
            ContentHash.ofString(fixed.maximumDeviation.toMicros.toString)
          )
        )
      case SamplingEvidence.Irregular => ContentHash.ofString("sampling:irregular")
    val frameSpec = frame.spec
    val yAxisHash = frameSpec.yAxis match
      case YAxis.Down => ContentHash.ofString("y-axis:down")
      case YAxis.Up   => ContentHash.ofString("y-axis:up")
    val eyeHash = eye match
      case Eye.Left      => ContentHash.ofString("eye:left")
      case Eye.Right     => ContentHash.ofString("eye:right")
      case Eye.Cyclopean => ContentHash.ofString("eye:cyclopean")
    val pupilUnitHash = pupilUnit match
      case None                      => ContentHash.ofString("pupil-unit:none")
      case Some(PupilUnit.Area)      => ContentHash.ofString("pupil-unit:area")
      case Some(PupilUnit.Diameter)  => ContentHash.ofString("pupil-unit:diameter")
      case Some(PupilUnit.Arbitrary) =>
        ContentHash.ofString("pupil-unit:arbitrary")
    val metadata = ContentHash.combineAll(
      Seq(
        ContentHash.ofString("recording:v2"),
        ContentHash.ofString(frame.id.name),
        ContentHash.of(IArray(frameSpec.xMin, frameSpec.yMin, frameSpec.xMax, frameSpec.yMax)),
        yAxisHash,
        ContentHash.ofString(clock.name),
        eyeHash,
        pupilUnitHash,
        samplingHash
      )
    )
    val sampleHashes = (0 until size).map { index =>
      val sample = samples(index)
      val state  = sample.gaze match
        case Gaze.Tracked(point, pupil) =>
          val pupilHash = pupil match
            case None        => ContentHash.ofString("pupil:none")
            case Some(value) =>
              ContentHash.combine(
                ContentHash.ofString("pupil:some"),
                ContentHash.of(IArray(value))
              )
          ContentHash.combineAll(
            Seq(
              ContentHash.ofString("tracked"),
              ContentHash.of(IArray(point.x, point.y)),
              pupilHash
            )
          )
        case Gaze.OffScreen(point) =>
          ContentHash.combine(
            ContentHash.ofString("off-screen"),
            ContentHash.of(IArray(point.x, point.y))
          )
        case Gaze.Blink() => ContentHash.ofString("blink")
        case Gaze.Lost()  => ContentHash.ofString("lost")
      ContentHash.combineAll(
        Seq(
          ContentHash.ofString(sample.t.toMicros.toString),
          state,
          ContentHash.ofString(sample.lineage.render)
        )
      )
    }
    ContentHash.combineAll(metadata +: sampleHashes)
