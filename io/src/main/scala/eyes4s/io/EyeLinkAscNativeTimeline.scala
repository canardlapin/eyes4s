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

package eyes4s.io

import cats.data.NonEmptyVector
import eyes4s.core.Gaze
import eyes4s.core.Sample
import eyes4s.kernel.ClockId
import eyes4s.kernel.Instant
import eyes4s.kernel.Mark
import eyes4s.kernel.ObservedTimeline
import eyes4s.kernel.TimelineError
import eyes4s.kernel.Unit2D

/** Exact conversion from EyeLink's millisecond time representation into the
  * kernel's integer-microsecond timeline.
  */
object AscTrackerTime:
  def instant(
      source: AscSourceLine,
      field: String,
      milliseconds: BigDecimal
  ): Either[AscNativeTimelineError, Instant] =
    val micros = milliseconds * BigDecimal(1000)
    micros.toBigIntExact match
      case None =>
        Left(
          AscNativeTimelineError.FractionalMicrosecond(
            source.source,
            source.number,
            field,
            milliseconds.toString
          )
        )
      case Some(value) if !value.isValidLong =>
        Left(
          AscNativeTimelineError.InstantOutsideLongRange(
            source.source,
            source.number,
            field,
            milliseconds.toString
          )
        )
      case Some(value) => Right(Instant.micros(value.longValue))

enum AscBlinkReconciliationPolicy derives CanEqual:
  /** Keep sample validity exactly as reported by the sample stream. */
  case PreserveSampleValidity

  /** Classify samples in a confirmed native SBLINK/EBLINK interval as Blink. */
  case ConfirmedNativeIntervals

enum AscNativePairingDiagnostic:
  case DuplicateStart(
      source: String,
      line: Long,
      block: Long,
      eventType: AscNativeEventType,
      eye: AscRecordedEye,
      previousLine: Long
  )
  case OrphanEnd(
      source: String,
      line: Long,
      block: Long,
      eventType: AscNativeEventType,
      eye: AscRecordedEye
  )
  case StartMismatch(
      source: String,
      line: Long,
      block: Long,
      eventType: AscNativeEventType,
      eye: AscRecordedEye,
      startRecord: String,
      endSummary: String
  )
  case UnclosedStart(
      source: String,
      line: Long,
      block: Long,
      eventType: AscNativeEventType,
      eye: AscRecordedEye,
      onset: String
  )

  def message: String = this match
    case DuplicateStart(source, line, block, eventType, eye, previousLine) =>
      s"ASC source='$source' line=$line block=$block native event=$eventType eye=$eye repeats an open start from line=$previousLine."
    case OrphanEnd(source, line, block, eventType, eye) =>
      s"ASC source='$source' line=$line block=$block native event=$eventType eye=$eye has an end with no open start."
    case StartMismatch(source, line, block, eventType, eye, start, end) =>
      s"ASC source='$source' line=$line block=$block native event=$eventType eye=$eye start record=$start disagrees with end-summary start=$end."
    case UnclosedStart(source, line, block, eventType, eye, onset) =>
      s"ASC source='$source' line=$line block=$block native event=$eventType eye=$eye start=$onset has no matching end."

end AscNativePairingDiagnostic

/** Native events with the subset whose start/end records agree exactly. */
final class AscNativeEventLedger private[io] (
    val events: Vector[AscParsedNativeEvent],
    val confirmedEnds: Vector[AscParsedNativeEvent],
    val diagnostics: Vector[AscNativePairingDiagnostic]
)

object AscNativeEventLedger:
  private final case class Key(
      source: String,
      block: Long,
      eventType: AscNativeEventType,
      eye: AscRecordedEye
  )

  def reconcile(events: Vector[AscParsedNativeEvent]): AscNativeEventLedger =
    var open        = Map.empty[Key, AscParsedNativeEvent]
    val confirmed   = Vector.newBuilder[AscParsedNativeEvent]
    val diagnostics = Vector.newBuilder[AscNativePairingDiagnostic]

    events.foreach { event =>
      val key = Key(event.source.source, event.blockNumber, event.eventType, event.eye)
      event.phase match
        case AscNativeEventPhase.Start =>
          open.get(key).foreach { previous =>
            diagnostics += AscNativePairingDiagnostic.DuplicateStart(
              event.source.source,
              event.source.number,
              event.blockNumber,
              event.eventType,
              event.eye,
              previous.source.number
            )
          }
          open = open.updated(key, event)
        case AscNativeEventPhase.End =>
          open.get(key) match
            case None =>
              diagnostics += AscNativePairingDiagnostic.OrphanEnd(
                event.source.source,
                event.source.number,
                event.blockNumber,
                event.eventType,
                event.eye
              )
            case Some(start) =>
              event.interval match
                case Some(interval) if interval.start == start.onset => confirmed += event
                case Some(interval)                                  =>
                  diagnostics += AscNativePairingDiagnostic.StartMismatch(
                    event.source.source,
                    event.source.number,
                    event.blockNumber,
                    event.eventType,
                    event.eye,
                    AscDiagnosticText.bounded(start.onset.toString),
                    AscDiagnosticText.bounded(interval.start.toString)
                  )
                case None =>
                  diagnostics += AscNativePairingDiagnostic.OrphanEnd(
                    event.source.source,
                    event.source.number,
                    event.blockNumber,
                    event.eventType,
                    event.eye
                  )
              open = open - key
        case AscNativeEventPhase.Update => ()
    }

    open.values.toVector.sortBy(_.source.number).foreach { event =>
      diagnostics += AscNativePairingDiagnostic.UnclosedStart(
        event.source.source,
        event.source.number,
        event.blockNumber,
        event.eventType,
        event.eye,
        AscDiagnosticText.bounded(event.onset.toString)
      )
    }
    new AscNativeEventLedger(events, confirmed.result(), diagnostics.result())

object EyeLinkAscNativeTimeline:

  /** Materialize tracker messages only as observed marks on their source clock. */
  def observedMessages(
      clock: ClockId,
      messages: Vector[AscParsedMessage]
  ): Either[NonEmptyVector[AscNativeTimelineError], ObservedTimeline[AscParsedMessage]] =
    val converted = messages.map { message =>
      AscTrackerTime
        .instant(message.source, "message-effective-time", message.effectiveTime)
        .map(time => Mark(time, message))
    }
    val errors = converted.collect { case Left(error) => error }
    NonEmptyVector.fromVector(errors) match
      case Some(values) => Left(values)
      case None         =>
        ObservedTimeline
          .of(clock, converted.collect { case Right(mark) => mark })
          .left
          .map(error =>
            NonEmptyVector.one(AscNativeTimelineError.InvalidTimeline(clock, error))
          )

  /** Apply one named blink policy to one block's samples. Missing samples alone
    * never become Blink; only a confirmed native start/end pair can do so.
    */
  def reconcileBlinkSamples[U <: Unit2D](
      blockNumber: Long,
      eye: AscRecordedEye,
      samples: IArray[Sample[U]],
      ledger: AscNativeEventLedger,
      policy: AscBlinkReconciliationPolicy
  ): Either[NonEmptyVector[AscNativeTimelineError], IArray[Sample[U]]] =
    policy match
      case AscBlinkReconciliationPolicy.PreserveSampleValidity   => Right(samples)
      case AscBlinkReconciliationPolicy.ConfirmedNativeIntervals =>
        val converted = ledger.confirmedEnds.collect {
          case event
              if event.blockNumber == blockNumber &&
                event.eye == eye &&
                event.eventType == AscNativeEventType.Blink =>
            event.interval.map { interval =>
              for
                start <- AscTrackerTime.instant(event.source, "blink-start", interval.start)
                end   <- AscTrackerTime.instant(event.source, "blink-end", interval.end)
              yield start -> end
            }
        }.flatten
        val errors = converted.collect { case Left(error) => error }
        NonEmptyVector.fromVector(errors) match
          case Some(values) => Left(values)
          case None         =>
            val intervals = converted.collect { case Right(value) => value }
            Right(
              IArray.tabulate(samples.length) { index =>
                val sample = samples(index)
                val within = intervals.exists { case (start, end) =>
                  sample.t.toMicros >= start.toMicros && sample.t.toMicros < end.toMicros
                }
                if within then Sample(sample.t, Gaze.Blink[U](), sample.lineage)
                else sample
              }
            )

enum AscNativeTimelineError derives CanEqual:
  case FractionalMicrosecond(
      source: String,
      line: Long,
      field: String,
      milliseconds: String
  )
  case InstantOutsideLongRange(
      source: String,
      line: Long,
      field: String,
      milliseconds: String
  )
  case InvalidTimeline(clock: ClockId, underlying: TimelineError)

  def message: String = this match
    case FractionalMicrosecond(source, line, field, milliseconds) =>
      s"ASC source='$source' line=$line field=$field time=${milliseconds}ms is not an exact integer number of microseconds."
    case InstantOutsideLongRange(source, line, field, milliseconds) =>
      s"ASC source='$source' line=$line field=$field time=${milliseconds}ms exceeds the Instant Long-microsecond range."
    case InvalidTimeline(clock, underlying) =>
      s"ASC observed-message timeline clock='$clock' is invalid: ${underlying.message}"

end AscNativeTimelineError
