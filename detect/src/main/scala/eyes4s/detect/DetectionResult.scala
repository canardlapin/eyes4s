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
final class SampleLabels private (private val values: Vector[SampleClass]):
  def size: Int                            = values.length
  def get(index: Int): Option[SampleClass] = values.lift(index)
  def toVector: Vector[SampleClass]        = values.toVector

object SampleLabels:
  private[detect] def from(values: Array[SampleClass]): SampleLabels =
    new SampleLabels(values.toVector)

  private[detect] def fromVector(values: Vector[SampleClass]): SampleLabels =
    new SampleLabels(values)

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

  /** Bounded execution of a shipped or independently documented detector: the
    * same machine that `run` drives, stepped over sample chunks. Driving the
    * cursor to completion is `run`; see [[DetectionCursor]].
    */
  def stepped[U <: Unit2D](
      source: RecordingRef,
      recording: Recording[U],
      detector: EventDetector[U],
      gapPolicy: GapPolicy,
      temporalSupport: TemporalSupport,
      parameters: Vector[(String, Provenance.Param)] = Vector.empty
  ): DetectionCursor[U] =
    DetectionCursor.begin(
      source,
      recording,
      DetectorIdentity.Algorithm(detector.card),
      gapPolicy,
      recording.representedSupport(temporalSupport),
      detector.machine,
      detector.configuration ++ parameters
    )

  /** Bounded execution of an explicitly custom machine, the stepped form of
    * [[runCustom]] with the same weaker identity.
    */
  def steppedCustom[U <: Unit2D](
      source: RecordingRef,
      recording: Recording[U],
      detector: DetectorRef,
      gapPolicy: GapPolicy,
      machine: Machine[Sample[U], DetectionEmission[U]],
      parameters: Vector[(String, Provenance.Param)] = Vector.empty
  ): DetectionCursor[U] =
    DetectionCursor.begin(
      source,
      recording,
      DetectorIdentity.Custom(detector),
      gapPolicy,
      recording.representedSupport,
      machine,
      parameters
    )

  /** Checked reconstruction of an archived detection from its events and
    * their declared sample support, without re-running any detector.
    *
    * The assembly `run` applies to a detector's emissions is applied to the
    * archived events: each event must lie inside the recording, its declared
    * support must be the sample range its span covers
    * (`EventSeries.of`, which also re-derives fixation centres, sample counts
    * and dispersions from the samples), every invalid observation inside an
    * event must be permitted by the gap policy, and the exhaustive labels,
    * the report with its class durations, unclassified ranges, bridged gaps
    * and warnings, and the provenance are derived exactly as `run` derives
    * them. `parameters` are the detection's parameters after the four the
    * provenance step always records (`detector`, `version`, `source`,
    * `gapPolicy`): for a shipped detector, its configuration.
    */
  def reconstruct[U <: Unit2D](
      source: RecordingRef,
      recording: Recording[U],
      identity: DetectorIdentity,
      gapPolicy: GapPolicy,
      temporalSupport: TemporalSupport,
      events: Vector[Event[U]],
      support: Vector[SampleRange],
      parameters: Vector[(String, Provenance.Param)]
  ): Either[DetectionResultError, DetectionResult[U]] =
    val detector = identity.detectorRef
    val ledger   = recording.representedSupport(temporalSupport)
    for
      // Every event inside the recording, as `run` requires, before the
      // declared ranges are compared with the ranges their spans cover.
      _      <- supportFor(source, recording, detector, events)
      series <- EventSeries
        .of(recording, source, events, support)
        .left
        .map(DetectionResultError.SourceSupport(source, detector, _))
      bridged <- validateGaps(source, recording, detector, gapPolicy, ledger, support)
      result  <- assemble(
        source,
        recording,
        identity,
        gapPolicy,
        ledger,
        series,
        bridged,
        parameters
      )
    yield result

  private def execute[U <: Unit2D](
      source: RecordingRef,
      recording: Recording[U],
      identity: DetectorIdentity,
      gapPolicy: GapPolicy,
      temporalSupport: SampleSupportLedger,
      machine: Machine[Sample[U], DetectionEmission[U]],
      parameters: Vector[(String, Provenance.Param)]
  ): Either[DetectionResultError, DetectionResult[U]] =
    DetectionCursor.complete(
      DetectionCursor
        .begin(source, recording, identity, gapPolicy, temporalSupport, machine, parameters),
      Int.MaxValue
    )

  /** The first failed emission at or after `from`, by its global index. */
  private[detect] def firstFailure[U <: Unit2D](
      source: RecordingRef,
      detector: DetectorRef,
      emissions: Vector[DetectionEmission[U]],
      from: Int
  ): Either[DetectionResultError, Unit] =
    val failed = emissions.indexWhere(_.isLeft, from)
    if failed < 0 then Right(())
    else
      emissions(failed) match
        case Left(error) =>
          Left(DetectionResultError.DetectorEmissionFailed(source, detector, failed, error))
        case Right(_) => Right(())

  private[detect] def assemblyWork[U <: Unit2D](
      source: RecordingRef,
      recording: Recording[U],
      identity: DetectorIdentity,
      gapPolicy: GapPolicy,
      temporalSupport: SampleSupportLedger,
      emissions: Vector[DetectionEmission[U]],
      parameters: Vector[(String, Provenance.Param)]
  ): AssemblyWork[Either[DetectionResultError, DetectionResult[U]]] =
    import AssemblyWork.*
    fold(AssemblyPhase.Emissions, 0, emissions.size, Vector.empty[Event[U]]) { (events, i) =>
      emissions(i).fold(_ => events, events :+ _)
    }.flatMap { events =>
      continue(supportWork(source, recording, identity.detectorRef, events)) { support =>
        continue(
          EventSeries
            .assembly(recording, source, events, support)
            .map(
              _.left.map(DetectionResultError.SourceSupport(source, identity.detectorRef, _))
            )
        ) { series =>
          continue(
            gapsWork(
              source,
              recording,
              identity.detectorRef,
              gapPolicy,
              temporalSupport,
              support
            )
          ) { bridged =>
            assembleWork(
              source,
              recording,
              identity,
              gapPolicy,
              temporalSupport,
              series,
              bridged,
              parameters
            )
          }
        }
      }
    }

  private def continue[A, B](work: AssemblyWork[Either[DetectionResultError, A]])(
      next: A => AssemblyWork[Either[DetectionResultError, B]]
  ): AssemblyWork[Either[DetectionResultError, B]] = work.flatMap {
    case Left(error)  => AssemblyWork.Done(Left(error))
    case Right(value) => next(value)
  }

  private[detect] def supportFor[U <: Unit2D](
      source: RecordingRef,
      recording: Recording[U],
      detector: DetectorRef,
      events: Vector[Event[U]]
  ): Either[DetectionResultError, Vector[SampleRange]] =
    supportWork(source, recording, detector, events).complete

  private def supportWork[U <: Unit2D](
      source: RecordingRef,
      recording: Recording[U],
      detector: DetectorRef,
      events: Vector[Event[U]]
  ): AssemblyWork[Either[DetectionResultError, Vector[SampleRange]]] =
    AssemblyWork.foldEither(AssemblyPhase.Support, 0, events.size, Vector.empty[SampleRange]) {
      (built, eventIndex) =>
        val event = events(eventIndex)
        AssemblyWork.Done {
          for
            ranges <- Right(built)
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
            bounds = recording.sampleBounds(event.span)
            range <- bounds match
              case (first, until) if first < until =>
                SampleRange
                  .of(first, until)
                  .left
                  .map(
                    DetectionResultError.InvalidDerivedRange(
                      source,
                      detector,
                      s"event[$eventIndex]-support",
                      first,
                      until,
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
    }

  private def validateGaps[U <: Unit2D](
      source: RecordingRef,
      recording: Recording[U],
      detector: DetectorRef,
      policy: GapPolicy,
      temporalSupport: SampleSupportLedger,
      support: Vector[SampleRange]
  ): Either[DetectionResultError, Vector[SampleRange]] =
    gapsWork(source, recording, detector, policy, temporalSupport, support).complete

  private def gapsWork[U <: Unit2D](
      source: RecordingRef,
      recording: Recording[U],
      detector: DetectorRef,
      policy: GapPolicy,
      temporalSupport: SampleSupportLedger,
      support: Vector[SampleRange]
  ): AssemblyWork[Either[DetectionResultError, Vector[SampleRange]]] =
    import AssemblyWork.*
    foldEither(AssemblyPhase.Gaps, 0, support.size, Vector.empty[SampleRange]) {
      (collected, eventIndex) =>
        val range = support(eventIndex)
        def close(
            result: Either[DetectionResultError, Vector[SampleRange]],
            start: Option[Int],
            until: Int,
            duration: Long
        ): Either[DetectionResultError, Vector[SampleRange]] = start match
          case None       => result
          case Some(from) =>
            SampleRange
              .of(from, until)
              .left
              .map(
                DetectionResultError.InvalidDerivedRange(
                  source,
                  detector,
                  s"event[$eventIndex]-invalid-support",
                  from,
                  until,
                  _
                )
              )
              .flatMap { gap =>
                policy match
                  case GapPolicy.Bridge(maximum) if duration <= maximum.span.toMicros =>
                    result.map(_ :+ gap)
                  case _ =>
                    Left(
                      DetectionResultError.GapPolicyViolation(
                        source,
                        detector,
                        eventIndex,
                        gap,
                        Span.micros(duration),
                        policy
                      )
                    )
              }
        val initial: (Either[DetectionResultError, Vector[SampleRange]], Option[Int], Long) =
          (Right(collected), None, 0L)
        fold(AssemblyPhase.Gaps, range.from, range.until, initial) {
          case ((result, start, duration), i) =>
            if !recording.samples(i).isUsable then
              (
                result,
                start.orElse(Some(i)),
                duration + temporalSupport.durationAtKnownIndex(i).toMicros
              )
            else (close(result, start, i, duration), None, 0L)
        }.map { case (result, start, duration) => close(result, start, range.until, duration) }
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
    assembleWork(
      source,
      recording,
      identity,
      gapPolicy,
      temporalSupport,
      series,
      bridged,
      parameters
    ).complete

  private def assembleWork[U <: Unit2D](
      source: RecordingRef,
      recording: Recording[U],
      identity: DetectorIdentity,
      gapPolicy: GapPolicy,
      temporalSupport: SampleSupportLedger,
      series: EventSeries[U],
      bridged: Vector[SampleRange],
      parameters: Vector[(String, Provenance.Param)]
  ): AssemblyWork[Either[DetectionResultError, DetectionResult[U]]] =
    val detector = identity.detectorRef
    final case class Accounting(
        labels: Vector[SampleClass],
        event: Int,
        durations: Vector[Long],
        open: Option[Int],
        openDuration: Long,
        ranges: Vector[SampleRange],
        warnings: Vector[DetectionWarning]
    )
    def close(state: Accounting, until: Int): Either[DetectionResultError, Accounting] =
      state.open match
        case None       => Right(state)
        case Some(from) =>
          SampleRange
            .of(from, until)
            .left
            .map(
              DetectionResultError
                .InvalidDerivedRange(source, detector, "unclassified-support", from, until, _)
            )
            .map { range =>
              state.copy(
                open = None,
                openDuration = 0L,
                ranges = state.ranges :+ range,
                warnings = state.warnings :+ DetectionWarning
                  .UnclassifiedSupport(source, detector, range, Span.micros(state.openDuration))
              )
            }
    val initial = Accounting(
      Vector.empty,
      0,
      Vector.fill(SampleClass.values.length)(0L),
      None,
      0L,
      Vector.empty,
      Vector.empty
    )
    AssemblyWork
      .foldEither(AssemblyPhase.LabelsAndReport, 0, recording.size, initial) { (state, i) =>
        var event = state.event
        while event < series.size && series.support(event).until <= i do event += 1
        val sample = recording.samples(i)
        val label  = sample.gaze match
          case Gaze.Blink()       => SampleClass.Blink
          case Gaze.Lost()        => SampleClass.Missing
          case Gaze.OffScreen(_)  => SampleClass.OffSurface
          case Gaze.Tracked(_, _) =>
            if event < series.size && series.support(event).contains(i) then
              series.events(event) match
                case _: Event.Fixation[U] => SampleClass.Fixation
                case _: Event.Saccade[U]  => SampleClass.Saccade
                case _: Event.Pursuit[U]  => SampleClass.Pursuit
                case _: Event.Blink[U]    => SampleClass.Blink
            else SampleClass.Unclassified
        val duration = temporalSupport.durationAtKnownIndex(i).toMicros
        val updated  = state.copy(
          labels = state.labels :+ label,
          event = event,
          durations =
            state.durations.updated(label.ordinal, state.durations(label.ordinal) + duration)
        )
        AssemblyWork.Done {
          if label == SampleClass.Unclassified then
            Right(
              updated.copy(
                open = state.open.orElse(Some(i)),
                openDuration = state.openDuration + duration
              )
            )
          else close(updated, i)
        }
      }
      .map(_.flatMap(close(_, recording.size)).map { state =>
        val report = DetectionReport(
          source,
          detector,
          gapPolicy,
          temporalSupport.policy,
          temporalSupport.censoredTime,
          recording.size,
          SampleClass.values.toVector.map(c => c -> Span.micros(state.durations(c.ordinal))),
          state.ranges,
          bridged,
          state.warnings
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
          SampleLabels.fromVector(state.labels),
          series,
          report,
          Provenance.raw(series.lineage.contentHash).andThen(step)
        )
      })

end Detection

/** One bounded step: charged samples while feeding, operations during assembly.
  *
  * `workUnits` replaced the former `samples` field because assembly pages charge
  * operations, not samples. The page that feeds the last sample returns `More` with a
  * cursor in assembly (`assemblyPhase` is defined), and from then on `consumed` equals
  * `total`; only an assembly page or a detector failure returns `Done`. Loop until
  * `Done`: `consumed == total` does not mean the result is ready.
  */
enum DetectionPage[U <: Unit2D]:
  case More(workUnits: Int, next: DetectionCursor[U])
  case Done(workUnits: Int, result: Either[DetectionResultError, DetectionResult[U]])

/** Immutable detection cursor. Feeding consumes at most `maximum` samples;
  * after the last feed and flush, assembly advances at most `maximum` operations
  * within one [[eyes4s.core.AssemblyPhase AssemblyPhase]]. Pages charge samples while feeding and assembly
  * operations thereafter. `consumed` always counts only source samples.
  * Cancellation during assembly exposes no partial artifact. A custom machine's
  * per-sample step and flush remain indivisible operations.
  */
final class DetectionCursor[U <: Unit2D] private (
    source: RecordingRef,
    recording: Recording[U],
    identity: DetectorIdentity,
    gapPolicy: GapPolicy,
    temporalSupport: SampleSupportLedger,
    parameters: Vector[(String, Provenance.Param)],
    machine: MachineCursor[Sample[U], DetectionEmission[U]],
    checked: Int,
    assembly: Option[AssemblyWork[Either[DetectionResultError, DetectionResult[U]]]] = None
):
  /** Samples fed so far and the recording's sample count. */
  def consumed: Int = if assembly.nonEmpty then total else machine.consumed
  def assemblyPhase: Option[AssemblyPhase] = assembly.flatMap(_.phase)
  def total: Int                           = machine.total

  /** Each page checks only the emissions it added: `checked` counts those
    * already known to be `Right`, so a run scans every emission once.
    */
  def advance(maximum: Int): DetectionPage[U] = advance(maximum, maximum)

  /** Independently control feeding and assembly operation quanta. */
  def advance(maximum: Int, assemblyMaximum: Int): DetectionPage[U] = assembly match
    case Some(work) =>
      val (units, next) = work.advance(assemblyMaximum)
      next match
        case AssemblyWork.Done(result) => DetectionPage.Done(units, result)
        case _                         =>
          DetectionPage.More(
            units,
            new DetectionCursor(
              source,
              recording,
              identity,
              gapPolicy,
              temporalSupport,
              parameters,
              machine,
              checked,
              Some(next)
            )
          )
    case None => feed(maximum)

  private def feed(maximum: Int): DetectionPage[U] =
    machine.advance(maximum) match
      case MachinePage.More(units, next) =>
        Detection.firstFailure(source, identity.detectorRef, next.emitted, checked) match
          case Left(error) => DetectionPage.Done(units, Left(error))
          case Right(())   =>
            DetectionPage.More(
              units,
              new DetectionCursor(
                source,
                recording,
                identity,
                gapPolicy,
                temporalSupport,
                parameters,
                next,
                next.emitted.size
              )
            )
      case MachinePage.Done(units, emissions) =>
        Detection.firstFailure(source, identity.detectorRef, emissions, checked) match
          case Left(error) => DetectionPage.Done(units, Left(error))
          case Right(())   =>
            val work = AssemblyWork.defer(AssemblyPhase.Emissions) {
              Detection.assemblyWork(
                source,
                recording,
                identity,
                gapPolicy,
                temporalSupport,
                emissions,
                parameters
              )
            }
            DetectionPage.More(
              units,
              new DetectionCursor(
                source,
                recording,
                identity,
                gapPolicy,
                temporalSupport,
                parameters,
                machine,
                emissions.size,
                Some(work)
              )
            )

object DetectionCursor:
  private[detect] def begin[U <: Unit2D](
      source: RecordingRef,
      recording: Recording[U],
      identity: DetectorIdentity,
      gapPolicy: GapPolicy,
      temporalSupport: SampleSupportLedger,
      machine: Machine[Sample[U], DetectionEmission[U]],
      parameters: Vector[(String, Provenance.Param)]
  ): DetectionCursor[U] =
    new DetectionCursor(
      source,
      recording,
      identity,
      gapPolicy,
      temporalSupport,
      parameters,
      MachineCursor.of(machine, recording.samples),
      0
    )

  /** Drain feeding and assembly with at most `maximum` work units per step. */
  def complete[U <: Unit2D](
      cursor: DetectionCursor[U],
      maximum: Int
  ): Either[DetectionResultError, DetectionResult[U]] =
    @annotation.tailrec
    def loop(cursor: DetectionCursor[U]): Either[DetectionResultError, DetectionResult[U]] =
      cursor.advance(maximum) match
        case DetectionPage.More(_, next)   => loop(next)
        case DetectionPage.Done(_, result) => result
    loop(cursor)
