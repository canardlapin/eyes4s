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

class EyeLinkAscImportSuite extends munit.FunSuite:

  private val frame =
    Frame.screen("eyelink-display", 1920, 1080).fold(error => fail(error.message), identity)
  private val clock    = ClockId("eyelink-tracker")
  private val settings = AscStreamSettings
    .of("guide-fixture.asc", 16384, 17)
    .fold(error => fail(error.message), identity)
  private val config = EyeLinkAscSessionConfig
    .of(
      frame,
      clock,
      ConversionEvidencePolicy.Exploratory,
      AscBlinkReconciliationPolicy.ConfirmedNativeIntervals,
      AscUnspecifiedPupilPolicy.RejectMeasuredValues
    )
    .fold(error => fail(error.message), identity)

  private val sourceText =
    val samples = (0 until 40).map(index => s"$index 960 540 1000").mkString("\n")
    s"""START 0 LEFT SAMPLES EVENTS
       |SAMPLES GAZE LEFT RATE 1000
       |EVENTS GAZE LEFT RATE 1000
       |PUPIL AREA
       |MSG 0 TRIALID guide
       |$samples
       |END 40
       |""".stripMargin

  private val sourceBytes: IArray[Byte] =
    IArray.from(sourceText.iterator.map(_.toByte).toArray)

  test("byte import derives exact exploratory origin and reconciles every source row") {
    val result = EyeLinkAscImport.fromBytes(sourceBytes, settings, config)

    assertEquals(result.raw.origin.ascDigest, Sha256.ofBytes(sourceBytes))
    assertEquals(result.raw.computedDigest, Some(Sha256.ofBytes(sourceBytes)))
    assert(result.report.isReconciled)
    assertEquals(result.report.errors, 0)
    assertEquals(result.report.materializedRecordingBlocks, 1)
    assert(result.trusted.isRight)
  }

  test("explicit origin mismatch is retained as a named error") {
    val result = EyeLinkAscImport.fromBytes(
      sourceBytes,
      EyeLinkAscOrigin.Unidentified(Sha256.ofUtf8("different ASC")),
      settings,
      config
    )

    assert(result.errors.exists {
      case EyeLinkAscSessionDiagnostic.SourceDigestMismatch(_, _, _) => true
      case _                                                         => false
    })
    assert(result.trusted.isLeft)
  }

  test("task-level path reaches a source-supported scanpath without losing provenance") {
    val session = EyeLinkAscImport
      .fromBytes(sourceBytes, settings, config)
      .trusted
      .fold(errors => fail(errors.head.message), identity)
    val pixels = session.recordingBlocks.headOption
      .getOrElse(fail("expected one materialized EyeLink recording block"))
      .recording match
      case EyeLinkAscRecordingArtifact.Monocular(recording) => recording
      case EyeLinkAscRecordingArtifact.Binocular(_)         =>
        fail("guide fixture declares a monocular left recording")

    val viewing = Viewing
      .millimetres(distance = 600.0, screenWidth = 530.0, screenHeight = 300.0)
      .fold(error => fail(error.message), identity)
    val angularFrame = Frame
      .angular(
        "eyelink-visual-field",
        viewing.horizontalExtent.toDegrees,
        viewing.verticalExtent.toDegrees
      )
      .fold(error => fail(error.message), identity)
    val degrees = pixels
      .warp(Viewing.angularWarp(viewing, frame, angularFrame))
      .fold(error => fail(error.message), identity)
    val threshold = IvtThreshold
      .of(Velocity.degPerSecond(30.0).fold(error => fail(error.message), identity))
      .fold(error => fail(error.message), identity)
    val minimum = MinimumEventDuration
      .of(Span.millis(10))
      .fold(error => fail(error.message), identity)
    val detection = Detection
      .run(
        RecordingRef("guide-fixture:block-1"),
        degrees,
        Detectors.ivt(threshold, minimum, clock),
        GapPolicy.Break
      )
      .fold(error => fail(error.message), identity)
    val scanpath = Scanpath
      .fromEvents(detection.eventSeries)
      .fold(error => fail(error.message), identity)

    assertEquals(session.observedMessages.marks.length, 1)
    assertEquals(scanpath.n, 1)
    assertEquals(scanpath.source, Some(RecordingRef("guide-fixture:block-1")))
    assertEquals(scanpath.sampleSupport.map(_.length), Some(1))
  }

end EyeLinkAscImportSuite
