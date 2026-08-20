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
import eyes4s.core.PupilUnit
import eyes4s.kernel.ClockId
import eyes4s.kernel.Frame

class EyeLinkAscSessionSuite extends munit.FunSuite:

  private val frame =
    Frame.screen("stimulus", 100, 100).fold(error => fail(error.message), identity)
  private val clock = ClockId("eyelink-tracker")

  private def bytes(value: String): IArray[Byte] =
    IArray.tabulate(value.length)(index => value.charAt(index).toByte)

  private def config(
      pupil: AscUnspecifiedPupilPolicy = AscUnspecifiedPupilPolicy.RejectMeasuredValues,
      blink: AscBlinkReconciliationPolicy =
        AscBlinkReconciliationPolicy.ConfirmedNativeIntervals
  ): EyeLinkAscSessionConfig =
    EyeLinkAscSessionConfig
      .of(
        frame,
        clock,
        ConversionEvidencePolicy.Exploratory,
        blink,
        pupil
      )
      .fold(error => fail(error.message), identity)

  private def materialize(
      contents: String,
      sessionConfig: EyeLinkAscSessionConfig = config()
  ): EyeLinkAscSessionMaterialization =
    val settings = AscStreamSettings
      .of("study.asc", 4096, 64)
      .fold(error => fail(error.message), identity)
    val normalized  = contents.stripMargin.trim + "\n"
    val sourceBytes = bytes(normalized)
    EyeLinkAscSessions.materialize(
      EyeLinkAscOrigin.Unidentified(Sha256.ofBytes(sourceBytes)),
      sessionConfig,
      EyeLinkAscFraming.whole(settings, sourceBytes)
    )

  test("multiple mono and binocular blocks retain native evidence and exact source support") {
    val result = materialize(
      """
        |START 0 LEFT SAMPLES EVENTS
        |PUPIL AREA
        |SAMPLES GAZE LEFT RATE 1000
        |EVENTS GAZE LEFT RATE 1000
        |BUTTON 0 1 0
        |INPUT 0 255
        |LOST_DATA_EVENT EVENT
        |MSG 0 trial-start
        |0 10 20 30
        |1 11 21 31
        |SFIX L 0
        |EFIX L 0 1 1 10 20 30
        |END 2
        |START 2 LEFT RIGHT SAMPLES EVENTS
        |PUPIL DIAMETER
        |SAMPLES GAZE LEFT RIGHT RATE 1000
        |EVENTS GAZE LEFT RIGHT RATE 1000
        |MSG 3 first
        |MSG 4 -1 second
        |3 30 40 4 50 60 5
        |SBLINK L 3
        |4 31 41 4.1 51 61 5.1
        |EBLINK L 3 5 2
        |5 32 42 4.2 52 62 5.2
        |END 6
        |"""
    )
    val session = result.trusted.fold(
      errors => fail(errors.toVector.map(_.message).mkString("\n")),
      identity
    )

    assertEquals(session.recordingBlocks.length, 2)
    assertEquals(session.recordingBlocks.map(_.sampleSources.length), Vector(2, 3))
    assertEquals(
      session.recordingBlocks(0).sampleSources.toVector.map(_.number),
      Vector(9L, 10L)
    )
    assertEquals(session.nativeEvents.confirmedEnds.length, 2)
    assertEquals(
      session.metadata.map(_.kind),
      Vector(
        AscNativeMetadataKind.Button,
        AscNativeMetadataKind.Input,
        AscNativeMetadataKind.LostData
      )
    )
    assertEquals(session.conversion.origin, session.raw.origin)
    assertEquals(session.raw.computedDigest, Some(session.raw.origin.ascDigest))
    assertEquals(session.frame, frame)
    assertEquals(session.clock, clock)

    session.recordingBlocks(0).recording match
      case EyeLinkAscRecordingArtifact.Monocular(recording) =>
        assertEquals(recording.size, 2)
        assertEquals(recording.pupilUnit, Some(PupilUnit.Area))
      case other => fail(s"expected monocular first block, found=$other")

    session.recordingBlocks(1).recording match
      case EyeLinkAscRecordingArtifact.Binocular(recording) =>
        assertEquals(recording.size, 3)
        assert(recording.leftGaze(0).isInstanceOf[Gaze.Blink[?]])
        assert(recording.leftGaze(1).isInstanceOf[Gaze.Blink[?]])
        assert(recording.leftGaze(2).isInstanceOf[Gaze.Tracked[?]])
        assert(recording.rightGaze.toVector.forall(_.isInstanceOf[Gaze.Tracked[?]]))
        assertEquals(recording.pupilUnit, Some(PupilUnit.Diameter))
      case other => fail(s"expected binocular second block, found=$other")

    assertEquals(
      session.observedMessages.marks.map(_.at.toMicros),
      Vector(0L, 3000L, 3000L)
    )
    assertEquals(
      session.observedMessages.marks.map(_.value.payload.ascii),
      Vector(Some("trial-start"), Some("first"), Some("second"))
    )
    assertEquals(result.report.sampleRecords, 5)
    assertEquals(result.report.parsedSamples, 5)
    assertEquals(result.report.materializedSamples, 5)
    assertEquals(result.report.excludedSamples, 0)
    assertEquals(result.report.materializedRecordingBlocks, 2)
    assertEquals(result.report.metadata, 3)
    assertEquals(result.report.errors, 0)
    assertEquals(result.report.warnings, 1)
    assert(result.report.isReconciled)
    assertEquals(result.raw.framing.length, result.report.physicalLines)
  }

  test("events-only conversion remains a trusted native-evidence session") {
    val result = materialize(
      """
        |START 0 LEFT EVENTS
        |EVENTS GAZE LEFT RATE 1000
        |MSG 0 TRIALID events-only
        |SFIX L 0
        |EFIX L 0 10 10 10 20 30
        |END 11
        |"""
    )
    val session = result.trusted.fold(
      errors => fail(errors.toVector.map(_.message).mkString("\n")),
      identity
    )

    assertEquals(session.recordingBlocks, Vector.empty)
    assertEquals(session.nativeEvents.confirmedEnds.length, 1)
    assertEquals(session.observedMessages.marks.length, 1)
    assertEquals(result.report.recordingBlocks, 1)
    assertEquals(result.report.materializedRecordingBlocks, 0)
    assertEquals(result.report.sampleRecords, 0)
    assertEquals(result.report.nativeEvents, 2)
    assert(result.report.isReconciled)
  }

  test(
    "mixed valid and invalid blocks retain a lossless partition and refuse trusted assembly"
  ) {
    val result = materialize(
      """
        |START 0 LEFT SAMPLES
        |PUPIL AREA
        |SAMPLES GAZE LEFT RATE 1000
        |0 10 20 30
        |END 1
        |START 1 LEFT SAMPLES
        |PUPIL AREA
        |SAMPLES GAZE LEFT RATE 1000
        |1 . 20 30
        |END 2
        |"""
    )

    assert(result.trusted.isLeft)
    assertEquals(result.report.sampleRecords, 2)
    assertEquals(result.report.parsedSamples, 1)
    assertEquals(result.report.parserRejectedSamples, 1)
    assertEquals(result.report.materializedSamples, 1)
    assertEquals(result.report.excludedSamples, 1)
    assertEquals(result.report.materializedRecordingBlocks, 1)
    assert(result.report.isReconciled)
    assert(
      result.errors.exists(
        _.isInstanceOf[EyeLinkAscSessionDiagnostic.Sample]
      )
    )
    assert(
      result.errors.forall(error =>
        error.message.contains("ASC") || error.message.contains("recording")
      )
    )
    assertEquals(result.raw.sampleRows.length, 2)
    assertEquals(result.raw.lines.length, result.report.parsedLines)
  }

  test("HREF and RAW blocks fail with their named coordinate mode and frame") {
    Vector("HREF", "RAW").foreach { mode =>
      val expectedMode =
        if mode == "HREF" then AscCoordinateMode.HeadReference
        else AscCoordinateMode.RawPupil
      val result = materialize(
        s"""
           |START 0 LEFT SAMPLES
           |PUPIL AREA
           |SAMPLES $mode LEFT RATE 1000
           |0 10 20 30
           |END 1
           |"""
      )

      assert(result.trusted.isLeft)
      assert(
        result.errors.exists {
          case EyeLinkAscSessionDiagnostic.UnsupportedCoordinateMode(
                source,
                _,
                block,
                coordinateMode,
                materializationFrame
              ) =>
            source == "study.asc" && block == 1L && coordinateMode == expectedMode &&
            materializationFrame == "stimulus"
          case _ => false
        },
        result.errors.map(_.message).mkString("\n")
      )
      assertEquals(result.report.materializedSamples, 0)
      assertEquals(result.report.excludedSamples, 1)
    }
  }

  test("undeclared measured pupil values require an explicit caller policy") {
    val source =
      """
        |START 0 LEFT SAMPLES
        |SAMPLES GAZE LEFT RATE 1000
        |0 10 20 30
        |END 1
        |"""
    val rejected = materialize(source)
    val accepted = materialize(
      source,
      config(AscUnspecifiedPupilPolicy.TreatMeasuredValuesAsArbitrary)
    )

    assert(
      rejected.errors.exists(
        _.isInstanceOf[EyeLinkAscSessionDiagnostic.UnspecifiedMeasuredPupil]
      )
    )
    val session = accepted.trusted.fold(
      errors => fail(errors.toVector.map(_.message).mkString("\n")),
      identity
    )
    session.recordingBlocks.head.recording match
      case EyeLinkAscRecordingArtifact.Monocular(recording) =>
        assertEquals(recording.pupilUnit, Some(PupilUnit.Arbitrary))
      case other => fail(s"expected monocular recording, found=$other")
    assert(
      accepted.warnings.exists(
        _.isInstanceOf[EyeLinkAscSessionDiagnostic.PupilTreatedAsArbitrary]
      )
    )
  }

  test("off-surface pupil loss is explicit while its native value remains recoverable") {
    val result = materialize(
      """
        |START 0 LEFT SAMPLES
        |PUPIL AREA
        |SAMPLES GAZE LEFT RATE 1000
        |0 110 20 30
        |END 1
        |"""
    )
    val session = result.trusted.fold(
      errors => fail(errors.toVector.map(_.message).mkString("\n")),
      identity
    )

    session.recordingBlocks.head.recording match
      case EyeLinkAscRecordingArtifact.Monocular(recording) =>
        assert(recording.samples(0).gaze.isInstanceOf[Gaze.OffScreen[?]])
      case other => fail(s"expected monocular recording, found=$other")
    assert(
      result.warnings.exists(
        _.isInstanceOf[EyeLinkAscSessionDiagnostic.OffSurfacePupilExcluded]
      )
    )
    assertEquals(
      result.raw.sampleRows.head.sample.flatMap(_.left).map(_.pupil.observation),
      Some(AscPupilObservation.Measured(BigDecimal(30)))
    )
  }

  test("a sample-declared empty block cannot disappear from a trusted session") {
    val result = materialize(
      """
        |START 0 LEFT SAMPLES
        |PUPIL AREA
        |SAMPLES GAZE LEFT RATE 1000
        |END 1
        |START 1 LEFT SAMPLES
        |PUPIL AREA
        |SAMPLES GAZE LEFT RATE 1000
        |1 10 20 30
        |END 2
        |"""
    )

    assert(result.trusted.isLeft)
    assertEquals(result.report.recordingBlocks, 2)
    assertEquals(result.report.materializedRecordingBlocks, 1)
    assert(
      result.errors.exists(
        _.isInstanceOf[EyeLinkAscSessionDiagnostic.EmptyDeclaredSampleBlock]
      )
    )
  }

  test("fractional-microsecond sample times are excluded with source-located errors") {
    val result = materialize(
      """
        |START 0 LEFT SAMPLES
        |PUPIL AREA
        |SAMPLES GAZE LEFT RATE 1000
        |0.0001 10 20 30
        |END 1
        |"""
    )

    assert(result.trusted.isLeft)
    assertEquals(result.report.parsedSamples, 1)
    assertEquals(result.report.materializedSamples, 0)
    assertEquals(result.report.excludedSamples, 1)
    assert(
      result.errors.exists {
        case EyeLinkAscSessionDiagnostic.Timeline(
              AscNativeTimelineError.FractionalMicrosecond(source, line, field, _)
            ) =>
          source == "study.asc" && line == 4L && field == "sample-time"
        case _ => false
      }
    )
  }

  test("declared ASC provenance must match the exact framed source bytes") {
    val source =
      "START 0 LEFT SAMPLES\nPUPIL AREA\nSAMPLES GAZE LEFT RATE 1000\n0 1 2 3\nEND 1\n"
    val settings = AscStreamSettings
      .of("study.asc", 4096, 64)
      .fold(error => fail(error.message), identity)
    val result = EyeLinkAscSessions.materialize(
      EyeLinkAscOrigin.Unidentified(Sha256.ofUtf8("different-source")),
      config(),
      EyeLinkAscFraming.whole(settings, bytes(source))
    )

    assert(result.trusted.isLeft)
    assertEquals(result.raw.computedDigest, Some(Sha256.ofBytes(bytes(source))))
    assert(
      result.errors.exists(
        _.isInstanceOf[EyeLinkAscSessionDiagnostic.SourceDigestMismatch]
      )
    )
  }

  test("blank nominal frame and clock identities are rejected before materialization") {
    val blankFrame = Frame.screen(" ", 10, 10).fold(error => fail(error.message), identity)

    assert(
      EyeLinkAscSessionConfig
        .of(
          blankFrame,
          clock,
          ConversionEvidencePolicy.Exploratory,
          AscBlinkReconciliationPolicy.PreserveSampleValidity,
          AscUnspecifiedPupilPolicy.RejectMeasuredValues
        )
        .left
        .exists(_.isInstanceOf[EyeLinkAscSessionConfigError.BlankFrame])
    )
    assert(
      EyeLinkAscSessionConfig
        .of(
          frame,
          ClockId(" "),
          ConversionEvidencePolicy.Exploratory,
          AscBlinkReconciliationPolicy.PreserveSampleValidity,
          AscUnspecifiedPupilPolicy.RejectMeasuredValues
        )
        .left
        .exists(_.isInstanceOf[EyeLinkAscSessionConfigError.BlankClock])
    )
  }

end EyeLinkAscSessionSuite
