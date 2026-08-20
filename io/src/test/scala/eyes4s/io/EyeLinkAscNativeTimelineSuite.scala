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

import eyes4s.core.Gaze
import eyes4s.core.Sample
import eyes4s.kernel.ClockId
import eyes4s.kernel.Instant
import eyes4s.kernel.Pt
import eyes4s.kernel.Unit2D

class EyeLinkAscNativeTimelineSuite extends munit.FunSuite:

  private def ascii(value: String): IArray[Byte] =
    IArray.tabulate(value.length)(index => value.charAt(index).toByte)

  private def parsed(values: String*): Vector[AscNativeParseResult] =
    val lines = values.zipWithIndex.map { case (value, index) =>
      val source = AscSourceLine
        .of("timeline.asc", index + 1L, index * 100L, ascii(value))
        .fold(error => fail(error.message), identity)
      EyeLinkAscLexer.parse(source)
    }
    EyeLinkAscBlocks.machine
      .andThen(EyeLinkAscNative.machine)
      .runAll(lines)
      .collect { case AscNativeEmission.Parsed(value) => value }

  private def messages(values: String*): Vector[AscParsedMessage] =
    parsed(values*).flatMap(_.record).collect { case AscParsedNativeRecord.Message(value) =>
      value
    }

  private def events(values: String*): Vector[AscParsedNativeEvent] =
    parsed(values*).flatMap(_.record).collect { case AscParsedNativeRecord.Event(value) =>
      value
    }

  test("messages become observed marks at effective time with stable simultaneous order") {
    val clock  = ClockId("tracker")
    val source = messages(
      "MSG 10 first",
      "MSG 15 -5 second",
      "MSG 10 third"
    )
    val timeline = EyeLinkAscNativeTimeline
      .observedMessages(clock, source)
      .fold(errors => fail(errors.toVector.map(_.message).mkString("\n")), identity)

    assertEquals(timeline.clock, clock)
    assertEquals(timeline.marks.map(_.at.toMicros), Vector(10000L, 10000L, 10000L))
    assertEquals(
      timeline.marks.map(_.value.payload.ascii),
      Vector(Some("first"), Some("second"), Some("third"))
    )
  }

  test("sub-microsecond and out-of-range tracker times are accumulated failures") {
    val source = AscSourceLine
      .of("precision.asc", 4L, 10L, ascii("MSG 0 payload"))
      .fold(error => fail(error.message), identity)
    val fractional = AscTrackerTime.instant(source, "time", BigDecimal("1.0001"))
    val outside    = AscTrackerTime.instant(
      source,
      "time",
      BigDecimal(Long.MaxValue) / BigDecimal(1000) + BigDecimal(1)
    )

    assert(fractional.left.exists(_.isInstanceOf[AscNativeTimelineError.FractionalMicrosecond]))
    assert(outside.left.exists(_.isInstanceOf[AscNativeTimelineError.InstantOutsideLongRange]))
    assert(fractional.left.exists(_.message.contains("source='precision.asc' line=4")))
  }

  test("native starts and ends reconcile only when block, type, eye, and onset agree") {
    val source = events(
      "START 1 LEFT EVENTS",
      "EVENTS GAZE LEFT RATE 1000",
      "SFIX L 10",
      "EFIX L 10 20 10 1 2 3",
      "SSACC L 21",
      "ESACC L 22 30 8 1 2 3 4 5 6",
      "SBLINK L 31",
      "END 40"
    )
    val ledger = AscNativeEventLedger.reconcile(source)

    assertEquals(ledger.events.length, 5)
    assertEquals(ledger.confirmedEnds.map(_.eventType), Vector(AscNativeEventType.Fixation))
    assert(ledger.diagnostics.exists(_.isInstanceOf[AscNativePairingDiagnostic.StartMismatch]))
    assert(ledger.diagnostics.exists(_.isInstanceOf[AscNativePairingDiagnostic.UnclosedStart]))
  }

  test("duplicate starts and orphan ends remain named evidence") {
    val source = events(
      "START 1 LEFT EVENTS",
      "EVENTS GAZE LEFT RATE 1000",
      "SFIX L 1",
      "SFIX L 2",
      "EFIX L 2 3 1 1 2 3",
      "EBLINK L 4 5 1",
      "END 6"
    )
    val ledger = AscNativeEventLedger.reconcile(source)

    assertEquals(ledger.confirmedEnds.length, 1)
    assert(ledger.diagnostics.exists(_.isInstanceOf[AscNativePairingDiagnostic.DuplicateStart]))
    assert(ledger.diagnostics.exists(_.isInstanceOf[AscNativePairingDiagnostic.OrphanEnd]))
    assert(ledger.diagnostics.forall(_.message.contains("source='timeline.asc'")))
  }

  test("only the explicit confirmed-native policy turns samples into Blink") {
    val native = events(
      "START 1 LEFT EVENTS",
      "EVENTS GAZE LEFT RATE 1000",
      "SBLINK L 10",
      "EBLINK L 10 12 2",
      "END 13"
    )
    val ledger  = AscNativeEventLedger.reconcile(native)
    val samples = IArray(
      Sample(Instant.millis(9), Gaze.Lost[Unit2D.Px]()),
      Sample(Instant.millis(10), Gaze.Lost[Unit2D.Px]()),
      Sample(
        Instant.millis(11),
        Gaze.Tracked(Pt[Unit2D.Px](1.0, 2.0), None)
      ),
      Sample(Instant.millis(12), Gaze.Lost[Unit2D.Px]())
    )
    val preserved = EyeLinkAscNativeTimeline
      .reconcileBlinkSamples(
        1L,
        AscRecordedEye.Left,
        samples,
        ledger,
        AscBlinkReconciliationPolicy.PreserveSampleValidity
      )
      .fold(errors => fail(errors.toVector.map(_.message).mkString("\n")), identity)
    val reconciled = EyeLinkAscNativeTimeline
      .reconcileBlinkSamples(
        1L,
        AscRecordedEye.Left,
        samples,
        ledger,
        AscBlinkReconciliationPolicy.ConfirmedNativeIntervals
      )
      .fold(errors => fail(errors.toVector.map(_.message).mkString("\n")), identity)

    assert(preserved(1).gaze.isInstanceOf[Gaze.Lost[?]])
    assert(preserved(2).gaze.isInstanceOf[Gaze.Tracked[?]])
    assert(reconciled(0).gaze.isInstanceOf[Gaze.Lost[?]])
    assert(reconciled(1).gaze.isInstanceOf[Gaze.Blink[?]])
    assert(reconciled(2).gaze.isInstanceOf[Gaze.Blink[?]])
    assert(reconciled(3).gaze.isInstanceOf[Gaze.Lost[?]])
  }

  test("missing samples without confirmed blink events remain signal loss") {
    val samples = IArray(Sample(Instant.millis(1), Gaze.Lost[Unit2D.Px]()))
    val ledger  = AscNativeEventLedger.reconcile(Vector.empty)
    val result  = EyeLinkAscNativeTimeline
      .reconcileBlinkSamples(
        1L,
        AscRecordedEye.Left,
        samples,
        ledger,
        AscBlinkReconciliationPolicy.ConfirmedNativeIntervals
      )
      .fold(errors => fail(errors.toVector.map(_.message).mkString("\n")), identity)

    assert(result(0).gaze.isInstanceOf[Gaze.Lost[?]])
  }

end EyeLinkAscNativeTimelineSuite
