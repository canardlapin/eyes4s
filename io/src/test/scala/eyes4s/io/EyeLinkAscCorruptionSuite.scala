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

import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.Prop.forAll

import eyes4s.kernel.ClockId
import eyes4s.kernel.Frame

class EyeLinkAscCorruptionSuite extends ScalaCheckSuite:

  private val courtSeed = 0x4559454c494e4bL

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters
      .withMinSuccessfulTests(400)
      .withMaxSize(80)
      .withInitialSeed(courtSeed)

  private def bytes(value: String): IArray[Byte] =
    IArray.tabulate(value.length)(index => value.charAt(index).toByte)

  private def settings(
      source: String = "corruption.asc",
      maximumLineBytes: Int = 4096
  ): AscStreamSettings =
    AscStreamSettings
      .of(source, maximumLineBytes, 31)
      .fold(error => fail(error.message), identity)

  private val frame = Frame
    .screen("corruption-display", 1920, 1080)
    .fold(error => fail(error.message), identity)

  private val sessionConfig = EyeLinkAscSessionConfig
    .of(
      frame,
      ClockId("corruption-tracker"),
      ConversionEvidencePolicy.Exploratory,
      AscBlinkReconciliationPolicy.ConfirmedNativeIntervals,
      AscUnspecifiedPupilPolicy.TreatMeasuredValuesAsArbitrary
    )
    .fold(error => fail(error.message), identity)

  private val numericToken = Gen.oneOf(
    ".",
    "0",
    "-0",
    "1",
    "-1",
    "1.0001",
    "NaN",
    "Infinity",
    "-Infinity",
    "9223372036854775808",
    "1e10000",
    "not-a-number"
  )

  private val eyeToken = Gen.oneOf("L", "R", "LEFT", "RIGHT", "LEFT RIGHT", "BOTH", "")

  private val knownToken = Gen.oneOf(
    "START",
    "END",
    "SAMPLES",
    "EVENTS",
    "PUPIL",
    "MSG",
    "SFIX",
    "EFIX",
    "SSACC",
    "ESACC",
    "SBLINK",
    "EBLINK",
    "BUTTON",
    "INPUT",
    "LOST_DATA_EVENT"
  )

  private val unknownToken = Gen.oneOf("FUTURE", "VENDOR_EXT", "!UNKNOWN", "CUSTOM_RECORD")

  private val payloadCharacter = Gen.frequency(
    8 -> Gen.choose(0x21, 0x7e).map(_.toChar),
    1 -> Gen.const(' '),
    1 -> Gen.const('\t')
  )

  private val payload = Gen.listOf(payloadCharacter).map(_.mkString.take(120))

  private val generatedLine = for
    tag <- Gen.frequency(
      7 -> knownToken,
      2 -> unknownToken,
      1 -> Gen.const(""),
      1 -> Gen.const("**")
    )
    separator <- Gen.oneOf(" ", "  ", "\t", " \t ")
    count     <- Gen.choose(0, 14)
    fields <- Gen.listOfN(count, Gen.frequency(5 -> numericToken, 2 -> eyeToken, 3 -> payload))
    truncate <- Gen.choose(0, 4)
  yield
    val complete =
      if tag.isEmpty then fields.mkString(separator)
      else if fields.isEmpty then tag
      else tag + separator + fields.mkString(separator)
    if truncate == 0 && complete.nonEmpty then complete.take(complete.length / 2)
    else complete

  private val document = for
    count       <- Gen.choose(1, 35)
    lines       <- Gen.listOfN(count, generatedLine)
    terminators <- Gen.listOfN(count, Gen.oneOf("\n", "\r\n"))
  yield lines.zip(terminators).map { case (line, ending) => line + ending }.mkString -> lines

  private val chunkPlan = Gen.nonEmptyListOf(Gen.choose(1, 37)).map(_.toVector)

  property("seeded line-accounting law partitions every generated physical line exactly once") {
    forAll(document) { case (contents, sourceLines) =>
      val framed           = EyeLinkAscFraming.whole(settings(), bytes(contents))
      val accounting       = EyeLinkAscAccounting.audit(framed)
      val expectedNonblank = sourceLines.count(line =>
        line.exists(character => character != ' ' && character != '\t')
      )

      Prop.all(
        accounting.lawHolds,
        accounting.physicalLines == sourceLines.length,
        accounting.nonblankLines == expectedNonblank,
        accounting.entries.map(_.line) == (1L to sourceLines.length.toLong).toVector
      )
    }
  }

  property("seeded arbitrary chunk plans equal whole-input records and diagnostics") {
    forAll(document, chunkPlan) { case ((contents, _), plan) =>
      val input    = bytes(contents)
      val expected = canonical(EyeLinkAscFraming.whole(settings(), input))
      val chunks   = partition(input, plan)
      val actual   = canonical(EyeLinkAscFraming.machine(settings()).runAll(chunks))

      actual == expected
    }
  }

  property("seeded hostile token streams remain total through semantic materialization") {
    forAll(document) { case (contents, _) =>
      val input  = bytes(contents)
      val framed = EyeLinkAscFraming.whole(settings(), input)
      val result = EyeLinkAscSessions.materialize(
        EyeLinkAscOrigin.Unidentified(Sha256.ofBytes(input)),
        sessionConfig,
        framed
      )
      val messagesBounded = result.diagnostics.forall(_.message.length <= 8192)

      result.report.isReconciled && EyeLinkAscAccounting.audit(framed).lawHolds &&
      messagesBounded
    }
  }

  test("the published partition distinguishes comments, unknowns, blanks, and rejections") {
    val contents   = "** comment\nFUTURE tail\n   \n" + ("Z" * 80) + "\nMSG 4 retained\n"
    val accounting = EyeLinkAscAccounting.audit(
      EyeLinkAscFraming.whole(settings(maximumLineBytes = 32), bytes(contents))
    )

    assertEquals(
      accounting.entries.map(_.disposition),
      Vector(
        AscLineDisposition.TypedRecord,
        AscLineDisposition.PreservedUnknown,
        AscLineDisposition.Blank,
        AscLineDisposition.Rejected,
        AscLineDisposition.TypedRecord
      )
    )
    assertEquals(accounting.nonblankLines, 4)
    assert(accounting.lawHolds)
  }

  test("oversized and lone-CR lines are one bounded rejection and parsing resumes") {
    val huge     = "X" * 500
    val contents = huge + "\nMSG 2 good\rbad\nMSG 3 resumed\n"
    val framed   = EyeLinkAscFraming.whole(settings(maximumLineBytes = 32), bytes(contents))
    val audit    = EyeLinkAscAccounting.audit(framed)
    val rejected = framed.collect { case AscFramingEmission.Rejected(value) => value }

    assertEquals(framed.length, 4)
    assertEquals(rejected.length, 2)
    assert(audit.lawHolds)
    rejected.foreach {
      case AscFramingDiagnostic.LineTooLong(_, _, _, limit, actual, excerpt) =>
        assertEquals(limit, 32)
        assertEquals(actual, 500L)
        assert(excerpt.length <= 160)
      case AscFramingDiagnostic.LoneCarriageReturn(_, _, _, _, excerpt) =>
        assert(excerpt.length <= 160)
      case other => fail(s"unexpected rejection=$other")
    }
    assert(
      framed.lastOption.exists {
        case AscFramingEmission.Parsed(value) =>
          value.source.number == 4L && value.source.diagnosticExcerpt == "MSG 3 resumed"
        case AscFramingEmission.Rejected(_) => false
      }
    )
  }

  test("near-limit hostile operands are bounded in diagnostics but exact in raw lines") {
    val hostile  = "x" * 1000
    val contents =
      s"""START 0 LEFT SAMPLES EVENTS
         |PUPIL AREA
         |SAMPLES GAZE LEFT RATE 1000
         |EVENTS GAZE LEFT RATE 1000
         |PRESCALER $hostile
         |0 $hostile 2 3
         |SFIX L $hostile
         |END 1
         |""".stripMargin
    val input  = bytes(contents)
    val result = EyeLinkAscSessions.materialize(
      EyeLinkAscOrigin.Unidentified(Sha256.ofBytes(input)),
      sessionConfig,
      EyeLinkAscFraming.whole(settings(), input)
    )
    val boundedOperands = result.errors.flatMap {
      case EyeLinkAscSessionDiagnostic.BlockOutcome(
            AscBlockOutcome.InvalidConfigurationValue(_, _, _, _, value)
          ) =>
        Vector(value)
      case EyeLinkAscSessionDiagnostic.Sample(
            AscSampleDiagnostic.InvalidDecimal(_, _, _, _, _, value)
          ) =>
        Vector(value)
      case EyeLinkAscSessionDiagnostic.Native(
            AscNativeDiagnostic.InvalidDecimal(_, _, _, _, _, value)
          ) =>
        Vector(value)
      case _ => Vector.empty
    }

    assertEquals(boundedOperands.length, 3)
    assert(boundedOperands.forall(_.length <= AscDiagnosticText.MaximumCharacters))
    assert(boundedOperands.forall(_.endsWith("...")))
    assert(result.errors.forall(_.message.length <= 512))
    assert(result.raw.lines.exists(_.source.bytes.length > AscDiagnosticText.MaximumCharacters))
  }

  test("numeric, order, block, layout, and event-pair corruption remain named values") {
    val contents =
      """START 0 LEFT SAMPLES EVENTS
        |PUPIL AREA
        |SAMPLES GAZE LEFT RATE 1000
        |EVENTS GAZE LEFT RATE 1000
        |0 NaN 2 3
        |2 1 2 3
        |1 1 2 3
        |SAMPLES GAZE RIGHT RATE 1000
        |SBLINK L 2
        |EBLINK L 3 4 1
        |""".stripMargin
    val input  = bytes(contents)
    val result = EyeLinkAscSessions.materialize(
      EyeLinkAscOrigin.Unidentified(Sha256.ofBytes(input)),
      sessionConfig,
      EyeLinkAscFraming.whole(settings(), input)
    )

    assert(result.trusted.isLeft)
    assert(result.report.isReconciled)
    assert(
      result.errors.exists {
        case EyeLinkAscSessionDiagnostic.BlockOutcome(
              AscBlockOutcome.StartAndConfigurationEyeMismatch(_, _, _, _, _, _)
            ) =>
          true
        case _ => false
      }
    )
    assert(
      result.errors.exists {
        case EyeLinkAscSessionDiagnostic.Sample(
              AscSampleDiagnostic.InvalidDecimal(_, _, _, _, _, _)
            ) =>
          true
        case _ => false
      }
    )
    assert(
      result.errors.exists {
        case EyeLinkAscSessionDiagnostic.Sample(
              AscSampleDiagnostic.NonIncreasingTimestamp(_, _, _, _, _)
            ) =>
          true
        case _ => false
      }
    )
    assert(
      result.errors.exists(
        _.isInstanceOf[EyeLinkAscSessionDiagnostic.BlockUnclosedAtEndOfInput]
      )
    )
    assert(
      result.errors.exists {
        case EyeLinkAscSessionDiagnostic.Pairing(
              AscNativePairingDiagnostic.StartMismatch(_, _, _, _, _, _, _)
            ) =>
          true
        case _ => false
      }
    )
  }

  test("failsafe converter evidence remains a warning on damaged but accounted output") {
    val contents =
      "START 0 LEFT SAMPLES\nPUPIL AREA\nSAMPLES GAZE LEFT RATE 1000\n0 1 2 3\nEND 1\n"
    val input      = bytes(contents)
    val ascDigest  = Sha256.ofBytes(input)
    val executable = Sha256.ofUtf8("edf2asc-executable")
    val converter  = Edf2AscConverter
      .of("edf2asc", executable, Some("test-version"))
      .fold(error => fail(error.message), identity)
    val arguments = Edf2AscArguments
      .of(Vector("-s", "-e"))
      .fold(error => fail(error.message), identity)
    val receipt = Edf2AscReceipt
      .declared(
        None,
        ascDigest,
        converter,
        arguments,
        "test-platform",
        Edf2AscRecovery.Failsafe
      )
      .fold(error => fail(error.message), identity)
    val result = EyeLinkAscSessions.materialize(
      EyeLinkAscOrigin.Converted(receipt),
      sessionConfig,
      EyeLinkAscFraming.whole(settings(), input)
    )

    assert(result.trusted.isRight)
    assert(result.report.isReconciled)
    assert(
      result.warnings.exists {
        case EyeLinkAscSessionDiagnostic.ConversionWarning(
              ConversionEvidenceWarning.FailsafeRecovery(digest)
            ) =>
          digest == ascDigest
        case _ => false
      }
    )
  }

  private def partition(input: IArray[Byte], plan: Vector[Int]): Vector[IArray[Byte]] =
    val output = Vector.newBuilder[IArray[Byte]]
    var offset = 0
    var index  = 0
    while offset < input.length do
      val width = plan(index % plan.length)
      val end   = math.min(input.length, offset + width)
      output += input.slice(offset, end)
      offset = end
      index += 1
    output.result()

  private def canonical(values: Vector[AscFramingEmission]): Vector[String] =
    values.map {
      case AscFramingEmission.Parsed(value) =>
        val source = value.source
        s"parsed:${source.source}:${source.number}:${source.byteOffset}:${source.terminator}:${source.bytes.toVector}:${recordName(value.record)}:${value.diagnostics.map(_.message)}"
      case AscFramingEmission.Rejected(value) => s"rejected:${value.message}"
    }

  private def recordName(value: AscRecord): String = value match
    case AscRecord.Blank                   => "blank"
    case AscRecord.Comment(_)              => "comment"
    case AscRecord.Sample(_)               => "sample"
    case AscRecord.Message(_)              => "message"
    case AscRecord.Boundary(kind, _)       => s"boundary:$kind"
    case AscRecord.Configuration(kind, _)  => s"configuration:$kind"
    case AscRecord.NativeEvent(kind, _)    => s"native:$kind"
    case AscRecord.Button(_)               => "button"
    case AscRecord.Input(_)                => "input"
    case AscRecord.LostData(_)             => "lost-data"
    case AscRecord.Unknown(token, _)       => s"unknown:${token.ascii}"
    case AscRecord.MalformedKnown(kind, _) => s"malformed:$kind"

end EyeLinkAscCorruptionSuite
