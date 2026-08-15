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

/** Versioned identity of the detector that produced an artifact. */
final case class DetectorRef(name: String, version: String) derives CanEqual:
  def render: String = s"$name@$version"

/** Scientific identity retained by a detection artifact. */
enum DetectorIdentity derives CanEqual:
  case Algorithm(card: AlgorithmCard)
  case Custom(reference: DetectorRef)

  def detectorRef: DetectorRef = this match
    case Algorithm(card)   => card.detectorRef
    case Custom(reference) => reference

  def algorithmCard: Option[AlgorithmCard] = this match
    case Algorithm(card) => Some(card)
    case Custom(_)       => None

/** How a detector may treat invalid observations within an event candidate. */
enum GapPolicy derives CanEqual:
  case Break
  case Bridge(maxDuration: InterpolationGap)
  case UseInterpolatedOnly

  def render: String = this match
    case Break               => "break"
    case Bridge(maximum)     => s"bridge(maxDuration=${maximum.span.render})"
    case UseInterpolatedOnly => "use-interpolated-only"

/** Exhaustive sample-level classification retained by a detection artifact. */
enum SampleClass derives CanEqual:
  case Fixation
  case Saccade
  case Pursuit
  case Blink
  case Missing
  case OffSurface
  case Unclassified

/** One class for every source sample, in source order. */
final class SampleLabels private (private val values: IArray[SampleClass]):
  def size: Int                            = values.length
  def get(index: Int): Option[SampleClass] = values.lift(index)
  def toVector: Vector[SampleClass]        = values.toVector

object SampleLabels:
  private[detect] def from(values: Array[SampleClass]): SampleLabels =
    new SampleLabels(IArray.from(values))

/** Non-fatal fact that remains visible in a detection report. */
enum DetectionWarning derives CanEqual:
  case UnclassifiedSupport(
      recording: RecordingRef,
      detector: DetectorRef,
      range: SampleRange,
      duration: Span
  )

  def message: String = this match
    case UnclassifiedSupport(recording, detector, range, duration) =>
      s"Recording '$recording' detector '${detector.render}' left source range $range " +
        s"unclassified for ${duration.render}."

/** Accounting attached to a complete detection result. */
final case class DetectionReport(
    recording: RecordingRef,
    detector: DetectorRef,
    gapPolicy: GapPolicy,
    temporalSupport: TemporalSupport,
    policyCensoredTime: Span,
    totalSamples: Int,
    classDurations: Vector[(SampleClass, Span)],
    unclassifiedRanges: Vector[SampleRange],
    bridgedGaps: Vector[SampleRange],
    warnings: Vector[DetectionWarning]
) derives CanEqual:
  def unclassifiedSamples: Int = unclassifiedRanges.map(_.length).sum

/** Events, exhaustive labels, accounting, and derivation identity. */
final class DetectionResult[U <: Unit2D] private[detect] (
    val identity: DetectorIdentity,
    val labels: SampleLabels,
    val eventSeries: EventSeries[U],
    val report: DetectionReport,
    val provenance: Provenance
)

/** A complete detection artifact could not be assembled. */
enum DetectionResultError derives CanEqual:
  case DetectorEmissionFailed(
      recording: RecordingRef,
      detector: DetectorRef,
      emissionIndex: Int,
      underlying: DetectionFailure
  )
  case EventOutsideRecording(
      recording: RecordingRef,
      detector: DetectorRef,
      eventIndex: Int,
      eventSpan: Interval,
      recordingExtent: Interval
  )
  case SourceSupport(
      recording: RecordingRef,
      detector: DetectorRef,
      underlying: DetectionSupportError
  )
  case GapPolicyViolation(
      recording: RecordingRef,
      detector: DetectorRef,
      eventIndex: Int,
      gap: SampleRange,
      duration: Span,
      policy: GapPolicy
  )
  case InvalidDerivedRange(
      recording: RecordingRef,
      detector: DetectorRef,
      role: String,
      from: Int,
      until: Int,
      underlying: DetectionSupportError
  )

  def message: String = this match
    case DetectorEmissionFailed(recording, detector, index, underlying) =>
      s"Recording '$recording' detector '${detector.render}' emission[$index] failed: " +
        underlying.message
    case EventOutsideRecording(recording, detector, index, span, extent) =>
      s"Recording '$recording' detector '${detector.render}' event[$index] at ${span.render} " +
        s"has no source support inside recording extent ${extent.render}."
    case SourceSupport(recording, detector, underlying) =>
      s"Recording '$recording' detector '${detector.render}' has invalid source support: " +
        underlying.message
    case GapPolicyViolation(recording, detector, index, gap, duration, policy) =>
      s"Recording '$recording' detector '${detector.render}' event[$index] spans invalid " +
        s"source range $gap for ${duration.render}, forbidden by gapPolicy=${policy.render}."
    case InvalidDerivedRange(recording, detector, role, from, until, underlying) =>
      s"Recording '$recording' detector '${detector.render}' could not construct $role " +
        s"range=[$from,$until): ${underlying.message}"

/** Execute an event machine and assemble the auditable artifact around it. */
object Detection:

  /** Execute a shipped named detector without allowing its scientific
    * identity to diverge from the machine that is actually run.
    */
  def run[U <: Unit2D](
      source: RecordingRef,
      recording: Recording[U],
      detector: EventDetector[U],
      gapPolicy: GapPolicy,
      parameters: Vector[(String, Provenance.Param)]
  ): Either[DetectionResultError, DetectionResult[U]] =
    execute(
      source,
      recording,
      DetectorIdentity.Algorithm(detector.card),
      gapPolicy,
      recording.representedSupport,
      detector.machine,
      detector.configuration ++ parameters
    )

  def run[U <: Unit2D](
      source: RecordingRef,
      recording: Recording[U],
      detector: EventDetector[U],
      gapPolicy: GapPolicy
  ): Either[DetectionResultError, DetectionResult[U]] =
    run(source, recording, detector, gapPolicy, Vector.empty)

  def run[U <: Unit2D](
      source: RecordingRef,
      recording: Recording[U],
      detector: EventDetector[U],
      gapPolicy: GapPolicy,
      temporalSupport: TemporalSupport,
      parameters: Vector[(String, Provenance.Param)]
  ): Either[DetectionResultError, DetectionResult[U]] =
    execute(
      source,
      recording,
      DetectorIdentity.Algorithm(detector.card),
      gapPolicy,
      recording.representedSupport(temporalSupport),
      detector.machine,
      detector.configuration ++ parameters
    )

  def run[U <: Unit2D](
      source: RecordingRef,
      recording: Recording[U],
      detector: EventDetector[U],
      gapPolicy: GapPolicy,
      temporalSupport: TemporalSupport
  ): Either[DetectionResultError, DetectionResult[U]] =
    run(source, recording, detector, gapPolicy, temporalSupport, Vector.empty)

  /** Execute an explicitly custom machine without granting it a paper-named
    * algorithm card. Scientific export can distinguish and reject this weaker
    * identity rather than accepting substituted citations or assumptions.
    */
  def runCustom[U <: Unit2D](
      source: RecordingRef,
      recording: Recording[U],
      detector: DetectorRef,
      gapPolicy: GapPolicy,
      machine: Machine[Sample[U], DetectionEmission[U]],
      parameters: Vector[(String, Provenance.Param)] = Vector.empty
  ): Either[DetectionResultError, DetectionResult[U]] =
    execute(
      source,
      recording,
      DetectorIdentity.Custom(detector),
      gapPolicy,
      recording.representedSupport,
      machine,
      parameters
    )

  def runCustom[U <: Unit2D](
      source: RecordingRef,
      recording: Recording[U],
      detector: DetectorRef,
      gapPolicy: GapPolicy,
      temporalSupport: TemporalSupport,
      machine: Machine[Sample[U], DetectionEmission[U]],
      parameters: Vector[(String, Provenance.Param)]
  ): Either[DetectionResultError, DetectionResult[U]] =
    execute(
      source,
      recording,
      DetectorIdentity.Custom(detector),
      gapPolicy,
      recording.representedSupport(temporalSupport),
      machine,
      parameters
    )

  private def execute[U <: Unit2D](
      source: RecordingRef,
      recording: Recording[U],
      identity: DetectorIdentity,
      gapPolicy: GapPolicy,
      temporalSupport: SampleSupportLedger,
      machine: Machine[Sample[U], DetectionEmission[U]],
      parameters: Vector[(String, Provenance.Param)]
  ): Either[DetectionResultError, DetectionResult[U]] =
    val detector  = identity.detectorRef
    val emissions = machine.runAll(recording.samples)
    emissions.zipWithIndex.collectFirst { case (Left(error), index) => (error, index) } match
      case Some((error, index)) =>
        Left(DetectionResultError.DetectorEmissionFailed(source, detector, index, error))
      case None =>
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

end Detection
