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

/** Pre-resumable assembly retained as an independent report, gap and label oracle.
  * EventSeries reconstruction has its own direct numerical oracle in the suite.
  */
private object DetectionAssemblyOracle:
  private[detect] def assembleEmissions[U <: Unit2D](
      source: RecordingRef,
      recording: Recording[U],
      identity: DetectorIdentity,
      gapPolicy: GapPolicy,
      temporalSupport: SampleSupportLedger,
      emissions: Vector[DetectionEmission[U]],
      parameters: Vector[(String, Provenance.Param)]
  ): Either[DetectionResultError, DetectionResult[U]] =
    val detector = identity.detectorRef
    locally {
      val events = emissions.collect { case Right(event) => event }
      for
        support <- supportFor(source, recording, detector, events)
        series  <- EventSeries
          .of(recording, source, events, support)
          .left
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
      series: EventSeries[U],
      bridged: Vector[SampleRange],
      parameters: Vector[(String, Provenance.Param)]
  ): Either[DetectionResultError, DetectionResult[U]] =
    val detector = identity.detectorRef
    val classes  = Array.tabulate(recording.size) { index =>
      recording.samples(index).gaze match
        case Gaze.Tracked(_, _) => SampleClass.Unclassified
        case Gaze.Blink()       => SampleClass.Blink
        case Gaze.Lost()        => SampleClass.Missing
        case Gaze.OffScreen(_)  => SampleClass.OffSurface
    }

    series.events.indices.foreach { eventIndex =>
      val eventClass = series.events(eventIndex) match
        case _: Event.Fixation[U] => SampleClass.Fixation
        case _: Event.Saccade[U]  => SampleClass.Saccade
        case _: Event.Pursuit[U]  => SampleClass.Pursuit
        case _: Event.Blink[U]    => SampleClass.Blink
      val range = series.support(eventIndex)
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
      val labels   = SampleLabels.from(classes)
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
      new DetectionResult(
        identity,
        labels,
        series,
        report,
        Provenance.raw(recording.contentHash).andThen(step)
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
