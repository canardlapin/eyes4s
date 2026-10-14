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

import eyes4s.core.*
import eyes4s.detect.*
import eyes4s.kernel.*

/** Complete unchanged public EDF-derived ASC; physical viewing geometry is unknown. */
class PublicAscEndToEndSuite extends munit.FunSuite:
  override val munitTimeout = scala.concurrent.duration.Duration(5, "minutes")

  test("public ASC reconciles every source row and reaches source-supported pixel Scanpaths") {
    val bytes = PublicAscFixture.bytes
    assertEquals(bytes.length, 8794652)
    assertEquals(
      Sha256.ofBytes(bytes).hex,
      "c23a7fb86d2fa4d7dcd8578a06f82911214073f1b6d36536bc9bc39316d70b19"
    )
    val frame = Frame
      .screen("zenodo1156863:C023:display", 1600, 1200)
      .fold(error => fail(error.message), identity)
    val clock    = ClockId("zenodo1156863:C023:tracker")
    val settings = AscStreamSettings
      .of("zenodo1156863:C023.asc", 16384, 65536)
      .fold(error => fail(error.message), identity)
    val config = EyeLinkAscSessionConfig
      .of(
        frame,
        clock,
        ConversionEvidencePolicy.Exploratory,
        AscBlinkReconciliationPolicy.ConfirmedNativeIntervals,
        AscUnspecifiedPupilPolicy.RejectMeasuredValues,
        nativePairing = AscNativePairingPolicy.PreserveUnconfirmedEvents
      )
      .fold(error => fail(error.message), identity)
    val imported = EyeLinkAscImport.fromBytes(bytes, settings, config)
    println(
      s"PUBLIC_ASC_REPORT physical=${imported.report.physicalLines} samples=${imported.report.parsedSamples} materialized=${imported.report.materializedSamples} native=${imported.report.parsedNativeRecords} warnings=${imported.report.warnings} errors=${imported.report.errors}"
    )
    assert(imported.report.isReconciled)
    assertEquals(imported.report.physicalLines, 247992)
    assertEquals(imported.report.sampleRecords, 237664)
    assertEquals(imported.report.parsedSamples, 237664)
    assertEquals(imported.report.materializedSamples, 237664)
    val session = imported.trusted.fold(
      errors => fail(errors.toVector.take(20).map(_.message).mkString("\n")),
      identity
    )
    assertEquals(imported.report.errors, 0)
    assertEquals(
      imported.warnings.count {
        case EyeLinkAscSessionDiagnostic.UnconfirmedPairing(
              _: AscNativePairingDiagnostic.DuplicateStart
            ) =>
          true
        case _ => false
      },
      1
    )
    assertEquals(session.nativeEvents.events.length, 10253)
    assertEquals(session.nativeEvents.diagnostics.length, 1)
    assertEquals(session.recordingBlocks.length, 3)
    assertEquals(session.observedMessages.marks.length, 42)
    assertEquals(session.observedMessages.clock, clock)
    assertEquals(session.raw.origin.ascDigest, Sha256.ofBytes(bytes))
    session.raw.sampleRows.foreach { row =>
      val sample = row.sample.getOrElse(fail(s"rejected public sample=$row"))
      sample.crWarnings match
        case AscCrWarningEvidence.Observed(flags) =>
          assertEquals(flags.raw, "...")
          assert(!flags.interpolated)
        case other => fail(s"missing native CR warning evidence=$other")
    }
    val extent  = Extent.square[Unit2D.Px](50.0).fold(error => fail(error.message), identity)
    val minimum = MinimumEventDuration
      .of(Span.millis(100))
      .fold(error => fail(error.message), identity)
    val expected = Vector(
      (
        1L,
        77610,
        504,
        133016000L,
        "15ce6d411b5cc6e0f85734ea945679375948a72dbae23909572c8b05ff90d152"
      ),
      (
        2L,
        50334,
        477,
        129772000L,
        "ec450cd1d615a239fca3261f21bf6ba582bc23c0ab0404715dbc151d086ff098"
      ),
      (
        3L,
        109720,
        1235,
        313812000L,
        "5dd8c55e38c6f3e5bf693bb143d77e090e3eedad8d74330951bdbcc45a997627"
      )
    )
    assertEquals(session.nativeEvents.confirmedEnds.length, 5126)
    assertEquals(imported.report.warnings, 1662)
    session.nativeEvents.events.flatMap(_.interval).foreach { interval =>
      assertEquals(
        interval.convention,
        AscNativeIntervalConvention.InclusiveFinalSample(BigDecimal(250))
      )
      assertEquals(interval.halfOpenOffset, interval.end + BigDecimal(4))
    }
    session.recordingBlocks.zip(expected).foreach { (block, witness) =>
      val recording = block.recording match
        case EyeLinkAscRecordingArtifact.Monocular(value) => value
        case other                                        =>
          fail(s"expected declared monocular block=${block.blockNumber}, found=$other")
      assertEquals(recording.frame, frame)
      assertEquals(recording.clock, clock)
      assertEquals(recording.size, block.sampleSources.length)
      val reference = RecordingRef(s"zenodo1156863:C023:block-${block.blockNumber}")
      val detected  = Detection
        .run(reference, recording, Detectors.idt(extent, minimum, clock), GapPolicy.Break)
        .fold(error => fail(error.message), identity)
      val path = Scanpath
        .fromEvents(detected.eventSeries)
        .fold(error => fail(error.message), identity)
      assertEquals(block.blockNumber, witness._1)
      assertEquals(recording.size, witness._2)
      assertEquals(path.n, witness._3)
      assertEquals(path.dwellTotal.toMicros, witness._4)
      assertEquals(path.source, Some(reference))
      assertEquals(path.sourceRecording, Some(recording))
      assertEquals(path.sampleSupport.map(_.length), Some(path.n))
      val signature = path.fixations.iterator
        .map { fixation =>
          s"${fixation.span.onset.toMicros}:${fixation.span.offset.toMicros}:${java.lang.Double.doubleToLongBits(fixation.centre.x)}:${java.lang.Double.doubleToLongBits(fixation.centre.y)}"
        }
        .mkString("\n")
      assertEquals(Sha256.ofUtf8(signature).hex, witness._5)
      println(
        s"PUBLIC_ASC_SCANPATH block=${block.blockNumber} samples=${recording.size} fixations=${path.n} dwell_us=${path.dwellTotal.toMicros} sha256=${Sha256.ofUtf8(signature).hex}"
      )
    }
  }
