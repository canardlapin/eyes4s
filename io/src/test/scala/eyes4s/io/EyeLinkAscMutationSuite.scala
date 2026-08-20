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
import eyes4s.kernel.ClockId
import eyes4s.kernel.Frame

/** Named, non-vacuous assertions for the required EyeLink semantic mutants.
  * Mutation execution receipts bind these test names to the changed production
  * expression and observed JVM/Scala.js failure.
  */
class EyeLinkAscMutationSuite extends munit.FunSuite:

  private val frame =
    Frame.screen("mutation-surface", 200, 200).fold(error => fail(error.message), identity)
  private val clock  = ClockId("mutation-tracker")
  private val config = EyeLinkAscSessionConfig
    .of(
      frame,
      clock,
      ConversionEvidencePolicy.Exploratory,
      AscBlinkReconciliationPolicy.ConfirmedNativeIntervals,
      AscUnspecifiedPupilPolicy.RejectMeasuredValues
    )
    .fold(error => fail(error.message), identity)

  private def bytes(value: String): IArray[Byte] =
    IArray.tabulate(value.length)(index => value.charAt(index).toByte)

  private def normalized(contents: String): IArray[Byte] =
    bytes(contents.stripMargin.trim + "\n")

  private def framing(contents: String): (IArray[Byte], Vector[AscFramingEmission]) =
    val input    = normalized(contents)
    val settings = AscStreamSettings
      .of("mutation.asc", 4096, 64)
      .fold(error => fail(error.message), identity)
    input -> EyeLinkAscFraming.whole(settings, input)

  private def materialize(
      contents: String,
      originFor: IArray[Byte] => EyeLinkAscOrigin = input =>
        EyeLinkAscOrigin.Unidentified(Sha256.ofBytes(input))
  ): EyeLinkAscSessionMaterialization =
    val (input, framed) = framing(contents)
    EyeLinkAscSessions.materialize(originFor(input), config, framed)

  private def lex(values: String*): Vector[AscLineResult] =
    values.zipWithIndex.map { case (value, index) =>
      val source = AscSourceLine
        .of(
          "mutation.asc",
          index + 1L,
          values.take(index).map(_.length + 1L).sum,
          bytes(value),
          terminator = AscLineTerminator.LineFeed
        )
        .fold(error => fail(error.message), identity)
      EyeLinkAscLexer.parse(source)
    }.toVector

  private def trusted(result: EyeLinkAscSessionMaterialization): EyeLinkAscSession =
    result.trusted.fold(
      errors => fail(errors.toVector.map(_.message).mkString("\n")),
      identity
    )

  test("mutant swap-left-and-right-eyes: binocular source columns retain eye identity") {
    val result = materialize(
      """
        |START 0 LEFT RIGHT SAMPLES
        |PUPIL AREA
        |SAMPLES GAZE LEFT RIGHT RATE 1000
        |0 10 20 30 70 80 90
        |END 1
        |"""
    )
    assertEquals(result.report.sampleRecords, 1)
    val native = result.raw.sampleRows.headOption
      .flatMap(_.sample)
      .getOrElse(fail("expected exactly one parsed native sample"))
    assertEquals(
      native.left.map(_.coordinates.position),
      Some(AscNativePair.Values(BigDecimal(10), BigDecimal(20)))
    )
    assertEquals(
      native.right.map(_.coordinates.position),
      Some(AscNativePair.Values(BigDecimal(70), BigDecimal(80)))
    )
    trusted(result).recordingBlocks.head.recording match
      case EyeLinkAscRecordingArtifact.Binocular(recording) =>
        assertEquals(recording.size, 1)
        recording.leftGaze(0) match
          case Gaze.Tracked(position, _) => assertEquals((position.x, position.y), (10.0, 20.0))
          case other                     => fail(s"expected tracked left eye, found=$other")
        recording.rightGaze(0) match
          case Gaze.Tracked(position, _) => assertEquals((position.x, position.y), (70.0, 80.0))
          case other                     => fail(s"expected tracked right eye, found=$other")
      case other => fail(s"expected one binocular recording, found=$other")
  }

  test("mutant ignore-START-configuration: declarations and contradictions remain explicit") {
    val output = EyeLinkAscBlocks.machine.runAll(
      lex(
        "START 0 LEFT EVENTS",
        "EVENTS GAZE RIGHT RATE 1000",
        "END 1"
      )
    )
    val rows = output.collect { case AscBlockEmission.Line(value) => value }
    assertEquals(rows.length, 3)
    val start = rows.head.block.getOrElse(fail("START must open a block")).start
    assertEquals(start.declaredEyeLayout, Some(AscEyeLayout.Left))
    assertEquals(start.declaresSamples, false)
    assertEquals(start.declaresEvents, true)
    assert(
      rows(1).outcomes.exists {
        case AscBlockOutcome.StartAndConfigurationEyeMismatch(_, _, 1L, "events", _, _) =>
          true
        case _ => false
      },
      rows(1).outcomes.map(_.message).mkString("\n")
    )
  }

  test("mutant reuse-prior-block-layout: each START resets configuration state") {
    val blocks = EyeLinkAscBlocks.machine.runAll(
      lex(
        "START 0 LEFT SAMPLES",
        "PUPIL AREA",
        "SAMPLES GAZE LEFT RATE 1000",
        "0 10 20 30",
        "END 1",
        "START 10 RIGHT SAMPLES",
        "10 70 80 90",
        "SAMPLES HREF RIGHT RATE 500",
        "12 71 81 91",
        "END 14"
      )
    )
    val rows = blocks.collect { case AscBlockEmission.Line(value) => value }
    assertEquals(rows.length, 10)
    val beforeConfiguration = rows(6)
    assertEquals(beforeConfiguration.block.map(_.start.blockNumber), Some(2L))
    assert(
      beforeConfiguration.requirements.exists {
        case AscBlockRequirement.SampleConfigurationMissing(_, _, 2L) => true
        case _                                                        => false
      }
    )
    val parsedSamples = EyeLinkAscSamples.machine
      .runAll(blocks)
      .collect { case AscSampleEmission.Parsed(value) => value }
    assertEquals(parsedSamples.length, 3)
    assertEquals(parsedSamples.map(_.sample.nonEmpty), Vector(true, false, true))
    val finalSample =
      parsedSamples.last.sample.getOrElse(fail("expected configured second-block row"))
    assertEquals(finalSample.blockNumber, 2L)
    assertEquals(
      finalSample.right.map(_.coordinates.mode),
      Some(AscCoordinateMode.HeadReference)
    )
  }

  test("mutant drop-MSG: every message is emitted exactly once with its payload") {
    val output = EyeLinkAscBlocks.machine
      .andThen(EyeLinkAscNative.machine)
      .runAll(lex("MSG 10 alpha", "MSG 11 beta"))
    val messages = output.collect { case AscNativeEmission.Parsed(value) =>
      value.record.collect { case AscParsedNativeRecord.Message(message) => message }
    }.flatten
    assertEquals(messages.length, 2)
    assertEquals(messages.map(_.payload.ascii), Vector(Some("alpha"), Some("beta")))
    assertEquals(messages.map(_.source.number), Vector(1L, 2L))
  }

  test("mutant ignore-message-offset: effective tracker time applies the signed offset") {
    val output = EyeLinkAscBlocks.machine
      .andThen(EyeLinkAscNative.machine)
      .runAll(lex("MSG 100 -5 DISPLAY_ON"))
    val messages = output.collect { case AscNativeEmission.Parsed(value) =>
      value.record.collect { case AscParsedNativeRecord.Message(message) => message }
    }.flatten
    assertEquals(messages.length, 1)
    assertEquals(messages.head.loggedTime, BigDecimal(100))
    assertEquals(messages.head.integrationOffset.map(_.value), Some(BigDecimal(-5)))
    assertEquals(messages.head.effectiveTime, BigDecimal(95))
    assertEquals(messages.head.payload.ascii, Some("DISPLAY_ON"))
  }

  test("mutant map-dot-gaze-to-zero: native dot coordinates remain MissingDot") {
    val result = materialize(
      """
        |START 0 LEFT SAMPLES
        |PUPIL AREA
        |SAMPLES GAZE LEFT RATE 1000
        |0 . . 0
        |END 1
        |"""
    )
    assertEquals(result.report.parsedSamples, 1)
    val position = result.raw.sampleRows.headOption
      .flatMap(_.sample)
      .flatMap(_.left)
      .map(_.coordinates.position)
    assertEquals(position, Some(AscNativePair.MissingDot))
  }

  test("mutant classify-all-missing-as-Blink: unconfirmed dot gaze materializes as Lost") {
    val result = materialize(
      """
        |START 0 LEFT SAMPLES
        |PUPIL AREA
        |SAMPLES GAZE LEFT RATE 1000
        |0 . . 0
        |END 1
        |"""
    )
    assertEquals(result.report.nativeEvents, 0)
    trusted(result).recordingBlocks.head.recording match
      case EyeLinkAscRecordingArtifact.Monocular(recording) =>
        assertEquals(recording.size, 1)
        recording.samples(0).gaze match
          case _: Gaze.Lost[?] => ()
          case other           => fail(s"expected Lost without blink evidence, found=$other")
      case other => fail(s"expected one monocular recording, found=$other")
  }

  test("mutant accept-HREF-as-pixels: HREF stays native and cannot become a pixel recording") {
    val result = materialize(
      """
        |START 0 LEFT SAMPLES
        |PUPIL AREA
        |SAMPLES HREF LEFT RATE 1000
        |0 10 20 30
        |END 1
        |"""
    )
    assertEquals(result.report.sampleRecords, 1)
    assertEquals(
      result.raw.sampleRows.headOption
        .flatMap(_.sample)
        .flatMap(_.left)
        .map(_.coordinates.mode),
      Some(AscCoordinateMode.HeadReference)
    )
    assert(result.trusted.isLeft)
    assert(
      result.errors.exists {
        case EyeLinkAscSessionDiagnostic.UnsupportedCoordinateMode(
              "mutation.asc",
              _,
              1L,
              AscCoordinateMode.HeadReference,
              "mutation-surface"
            ) =>
          true
        case _ => false
      },
      result.errors.map(_.message).mkString("\n")
    )
  }

  test(
    "mutant discard-unknown-record: unknown source rows and diagnostics remain inspectable"
  ) {
    val result = materialize(
      """
        |START 0 LEFT SAMPLES
        |PUPIL AREA
        |SAMPLES GAZE LEFT RATE 1000
        |VENDOR_EXTENSION exact-tail
        |0 10 20 30
        |END 1
        |"""
    )
    assertEquals(result.report.physicalLines, 6)
    assertEquals(result.report.unknownRecords, 1)
    assertEquals(
      result.raw.lines.count {
        case AscLineResult(_, _, AscRecord.Unknown(_, _), _) => true
        case _                                               => false
      },
      1
    )
    assert(
      result.warnings.exists {
        case EyeLinkAscSessionDiagnostic.UnknownRecord("mutation.asc", 4L, token) =>
          token == "VENDOR_EXTENSION"
        case _ => false
      }
    )
    assertEquals(trusted(result).recordingBlocks.length, 1)
  }

  test("mutant omit-final-block: EOF flush emits and diagnoses the open block") {
    val lines = lex(
      "START 0 LEFT SAMPLES",
      "PUPIL AREA",
      "SAMPLES GAZE LEFT RATE 1000",
      "0 10 20 30"
    )
    val emissions = EyeLinkAscBlocks.machine.runAll(lines)
    val closures  = emissions.collect { case AscBlockEmission.Closed(value) => value }
    assertEquals(closures.length, 1)
    assertEquals(closures.head.block.start.blockNumber, 1L)
    assertEquals(closures.head.reason, AscBlockClosureReason.EndOfInput)

    val result = materialize(
      """
        |START 0 LEFT SAMPLES
        |PUPIL AREA
        |SAMPLES GAZE LEFT RATE 1000
        |0 10 20 30
        |"""
    )
    assertEquals(result.report.recordingBlocks, 1)
    assert(result.trusted.isLeft)
    assert(
      result.errors.exists {
        case EyeLinkAscSessionDiagnostic.BlockUnclosedAtEndOfInput(
              "mutation.asc",
              1L,
              1L
            ) =>
          true
        case _ => false
      }
    )
  }

  test("mutant lose-converter-provenance: trusted and raw artifacts retain the full receipt") {
    var expectedReceipt: Option[Edf2AscReceipt] = None
    val result                                  = materialize(
      """
        |START 0 LEFT SAMPLES
        |PUPIL AREA
        |SAMPLES GAZE LEFT RATE 1000
        |0 10 20 30
        |END 1
        |""",
      input =>
        val converter = Edf2AscConverter
          .of("edf2asc", Sha256.ofUtf8("mutation-converter"), Some("4.2.1"))
          .fold(error => fail(error.message), identity)
        val arguments = Edf2AscArguments
          .of(Vector("-s", "-e", "-failsafe"))
          .fold(error => fail(error.message), identity)
        val receipt = Edf2AscReceipt
          .declared(
            None,
            Sha256.ofBytes(input),
            converter,
            arguments,
            "mutation-platform",
            Edf2AscRecovery.Failsafe
          )
          .fold(error => fail(error.message), identity)
        expectedReceipt = Some(receipt)
        EyeLinkAscOrigin.Converted(receipt)
    )
    val receipt = expectedReceipt.getOrElse(fail("origin factory did not create a receipt"))
    assertEquals(result.raw.origin, EyeLinkAscOrigin.Converted(receipt))
    val session = trusted(result)
    assertEquals(session.raw.origin, EyeLinkAscOrigin.Converted(receipt))
    assertEquals(session.conversion.origin, EyeLinkAscOrigin.Converted(receipt))
    assertEquals(session.conversion.canonicalDigest, receipt.canonicalDigest)
    assertEquals(
      session.conversion.warnings,
      Vector(
        ConversionEvidenceWarning.UserDeclaredConversion(receipt.ascDigest),
        ConversionEvidenceWarning.MissingEdfDigest(receipt.ascDigest),
        ConversionEvidenceWarning.FailsafeRecovery(receipt.ascDigest)
      )
    )
  }

end EyeLinkAscMutationSuite
