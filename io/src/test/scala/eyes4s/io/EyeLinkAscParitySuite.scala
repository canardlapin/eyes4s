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

import eyes4s.kernel.ClockId
import eyes4s.kernel.Frame

private[io] final case class EyeLinkAscParityFixture(id: String, input: IArray[Byte])

private[io] final case class EyeLinkAscParityExpectedRow(
    fixture: String,
    inputDigest: String,
    canonicalDigest: String,
    facts: Int
)

private[io] object EyeLinkAscParityExpected:
  val schemaVersion: String = "eyes4s-eyelink-parity-v1"

  val rows: Vector[EyeLinkAscParityExpectedRow] = Vector(
    EyeLinkAscParityExpectedRow(
      "text-decoding",
      "cdfe402f72c80dd10fbf01277e7b234a9a6ec852db170aa197ddff501c41918d",
      "0549c768532eebbfa0c5cc37972b8262468e1f20b0f3687cd562136fdc817bbe",
      195
    ),
    EyeLinkAscParityExpectedRow(
      "decimal-negative-zero",
      "0410e7597ec09927154ce984f67a99a6003585499fafb603c4ba82a4a830cc31",
      "37de2cb9dffbfd6a8ad9c32c1c22c23d8001ad7bcf58bf81a3ba5e11581675d0",
      344
    ),
    EyeLinkAscParityExpectedRow(
      "newline-handling",
      "eb9cf30ee73c6b421205ecf040f281cb0c3b86bc2fa956b1b5a9ca82c108e21b",
      "f972b96f9c978adc57e764916c2a2b98471fdb1dbdf12322eb83b6b5f5275f59",
      202
    ),
    EyeLinkAscParityExpectedRow(
      "long-integer-time",
      "4a0a214afde6fffaa353f1f23d6703d058bb4f948fc6c3438e4abeae96dce6f0",
      "ae623a9dcbf9404c01578d13b722d3cc00075f9a57070bdc144c7740736042f4",
      202
    ),
    EyeLinkAscParityExpectedRow(
      "simultaneous-ordering",
      "1332921e058bf2402a37e104dc82fde9e3457f3d5ccb3b7cf2a971015c13c52c",
      "144cc357aff21b80a07cc3d7dd22e247226e50bbe709cff6416dbea98cd1261a",
      228
    ),
    EyeLinkAscParityExpectedRow(
      "synthetic-binocular-remote-2000",
      "284d4ad51b6d42a0c51cca867a88f786c8b6ff0140a0de0268fb3e2e559421d8",
      "fee9a3132d2a0dedd0e3323fa090f138f15fb8ad792a450f420ce93a8e219d19",
      213
    ),
    EyeLinkAscParityExpectedRow(
      "synthetic-events-messages",
      "6d79ac7455ccaaeaf414059253d9dc252dc0368c6211af5bed11fdfb248f0d77",
      "6a4bffed45a48916cd338a1f5ecf181f0b7ceb5970623c6c1ec0c1020526be4a",
      705
    ),
    EyeLinkAscParityExpectedRow(
      "synthetic-failsafe-corrupt",
      "8672021b7b82dfd5839d171a29251e8fcf0af69d7cc108d2688bd81e3681d414",
      "cfd40a9062725fafcd17363789cc34c87cc8071ba361b540340b6f1bc43cdd22",
      403
    ),
    EyeLinkAscParityExpectedRow(
      "synthetic-left-headfixed-1000",
      "94f2d62d176d8d26559b54cd61d3df605c01764485055636f4ddff76548282c6",
      "4a6e454a91ca77244b1774ff9d80cdf27f6482cf65c9ccb140aad17f6c2b80e6",
      277
    ),
    EyeLinkAscParityExpectedRow(
      "synthetic-multiblock",
      "37888b1eac961fd1634ac623e5a1d6a37c4c3b96dd93fdf42b6f5c56418e0ad7",
      "44c7835f7acd7b4b18a6a8633799af81ce6e51023b0ff38bbee5a2f19e7226c3",
      546
    ),
    EyeLinkAscParityExpectedRow(
      "synthetic-right-headfixed-500",
      "9975faafede13328c50d815d9f92d66dc6e42afb6b2a862e3add5c58b66d956b",
      "d7e162c2ca08d753df23d53662348d520d12b5286c5df3f22d581553fd16942d",
      208
    )
  )

  val byFixture: Map[String, EyeLinkAscParityExpectedRow] =
    rows.map(row => row.fixture -> row).toMap

  def renderTsv: String =
    val header = "schema\tfixture\tinput_sha256\tcanonical_sha256\tfacts"
    val values = rows.map(row =>
      Vector(
        schemaVersion,
        row.fixture,
        row.inputDigest,
        row.canonicalDigest,
        row.facts.toString
      ).mkString("\t")
    )
    (header +: values).mkString("\n") + "\n"

private[io] object EyeLinkAscParityFixtures:
  private def ascii(value: String): Vector[Byte]            = value.toVector.map(_.toByte)
  private def immutable(values: Vector[Byte]): IArray[Byte] = IArray.from(values)
  private def lines(id: String, values: String*): EyeLinkAscParityFixture =
    EyeLinkAscParityFixture(id, immutable(ascii(values.mkString("\n") + "\n")))

  val textDecoding: EyeLinkAscParityFixture =
    val bytes = Vector(0xef.toByte, 0xbb.toByte, 0xbf.toByte) ++
      ascii("** decoder fixture\r\nMSG 5 payload-") ++
      Vector(0xff.toByte) ++
      ascii(
        "-tail  \r\nSTART 0 LEFT SAMPLES\r\nPUPIL AREA\r\nSAMPLES GAZE LEFT RATE 1000\r\n0 10 20 30\r\nEND 1\r\n"
      )
    EyeLinkAscParityFixture("text-decoding", immutable(bytes))

  val decimalNegativeZero: EyeLinkAscParityFixture = EyeLinkAscParityFixture(
    "decimal-negative-zero",
    immutable(
      ascii(
        """START -0.0 LEFT SAMPLES EVENTS
          |PUPIL AREA
          |SAMPLES GAZE LEFT RATE 1000
          |EVENTS GAZE LEFT RATE 1000
          |-0.0 -0.0 0.000 0
          |1.000 1.2300 2.5000 3.000
          |SFIX L -0.0
          |EFIX L -0.0 1.000 1.000 -0.0 0.000 3.0
          |END 2.000
          |""".stripMargin
      )
    )
  )

  val newlineHandling: EyeLinkAscParityFixture = EyeLinkAscParityFixture(
    "newline-handling",
    immutable(
      ascii("START 0 LEFT SAMPLES\r\nPUPIL AREA\nSAMPLES GAZE LEFT RATE 1000\r\n") ++
        ascii("0 10 20 30\n1 11 21 31\r\nEND 2")
    )
  )

  val longIntegerTime: EyeLinkAscParityFixture = EyeLinkAscParityFixture(
    "long-integer-time",
    immutable(
      ascii(
        """START 9007199254740 LEFT SAMPLES
          |PUPIL AREA
          |SAMPLES GAZE LEFT RATE 1000
          |9007199254740.993 10 20 30
          |9007199254741.993 11 21 31
          |END 9007199254742
          |""".stripMargin
      )
    )
  )

  val simultaneousOrdering: EyeLinkAscParityFixture = EyeLinkAscParityFixture(
    "simultaneous-ordering",
    immutable(
      ascii(
        """MSG 100 +2 third
          |MSG 105 -5 first
          |MSG 101 -1 second
          |START 0 LEFT SAMPLES
          |PUPIL AREA
          |SAMPLES GAZE LEFT RATE 1000
          |0 10 20 30
          |END 1
          |""".stripMargin
      )
    )
  )

  val syntheticBinocularRemote2000: EyeLinkAscParityFixture = lines(
    "synthetic-binocular-remote-2000",
    "** eyes4s synthetic fixture; generated data; no human participant data",
    "START\t300\tLEFT\tRIGHT\tSAMPLES",
    "PUPIL\tARBITRARY_VENDOR_UNIT",
    "SAMPLES\tGAZE\tLEFT\tRIGHT\tRATE\t2000.00\tTRACKING\tCR\tFILTER\t2\tVEL\tRES\tINPUT\tBUTTONS\tSTATUS\tHTARGET",
    "300.000\t1\t2\t3\t4\t5\t6\t0.1\t0.2\t0.3\t0.4\t30\t31\t7\t8\t9\t40\t41\t42\tMANC",
    "300.500\t.\t.\t0\t4.5\t5.5\t6.5\t.\t.\t0.3\t0.4\t30\t31\t7\t8\t9\t40\t41\t42\tMANC",
    "END\t301"
  )

  val syntheticEventsMessages: EyeLinkAscParityFixture = lines(
    "synthetic-events-messages",
    "** eyes4s synthetic fixture; generated data; no human participant data",
    "START\t100\tLEFT\tEVENTS",
    "EVENTS\tGAZE\tLEFT\tRATE\t1000.00",
    "MSG\t100 TRIALID synthetic-1",
    "MSG\t101 -5 DISPLAY_ON",
    "MSG\t102 12345",
    "SFIX\tL\t103",
    "EFIX\tL\t103\t120\t17\t100.0\t200.0\t500",
    "SSACC\tL\t121",
    "ESACC\tL\t121\t130\t9\t100.0\t200.0\t120.0\t220.0\t30.0\t400",
    "SBLINK\tL\t131",
    "EBLINK\tL\t131\t140\t9",
    "END\t150",
    "START\t200\tLEFT\tSAMPLES\tEVENTS",
    "PUPIL\tAREA",
    "SAMPLES\tGAZE\tLEFT\tRATE\t1000.00",
    "EVENTS\tGAZE\tLEFT\tRATE\t1000.00",
    "200\t100\t200\t500",
    "MSG\t200 !V TRIAL_VAR condition synthetic",
    "EFIX\tL\t200\t201\t1\t100.0\t200.0\t500",
    "END\t202"
  )

  val syntheticFailsafeCorrupt: EyeLinkAscParityFixture = lines(
    "synthetic-failsafe-corrupt",
    "** eyes4s synthetic corruption fixture; generated data; no human data; not actual EDF2ASC failsafe evidence",
    "START\t1\tLEFT\tSAMPLES",
    "PUPIL\tAREA",
    "SAMPLES\tGAZE\tLEFT\tRATE\t1000.00",
    "1\t10\t20\t30",
    "2\t10\t.\t30",
    "START\t3\tRIGHT\tSAMPLES",
    "SAMPLES\tGAZE\tRIGHT\tRATE\t500.00",
    "3\t10\t20",
    "UNRECOGNIZED_RECOVERY_RECORD\tretained",
    "END\t4",
    "START\t5\tLEFT\tSAMPLES",
    "SAMPLES\tRAW\tLEFT\tRATE\t1000.00",
    "5\t1\t2\t3"
  )

  val syntheticLeftHeadfixed1000: EyeLinkAscParityFixture = lines(
    "synthetic-left-headfixed-1000",
    "** eyes4s synthetic fixture; generated data; no human participant data",
    "START\t100\tLEFT\tSAMPLES",
    "PRESCALER\t1",
    "VPRESCALER\t1",
    "PUPIL\tAREA",
    "SAMPLES\tGAZE\tLEFT\tRATE\t1000.00\tTRACKING\tCR\tFILTER\t2\tRES\tVEL\tINPUT\tBUTTONS\tSTATUS",
    "100.000\t100\t200\t500\t0.1\t0.2\t30\t31\t1\t2\t0",
    "101.000\t.\t.\t0\t.\t.\t30\t31\t1\t2\t1",
    "VENDOR_EXTENSION\tretained synthetic payload",
    "END\t102"
  )

  val syntheticMultiblock: EyeLinkAscParityFixture = lines(
    "synthetic-multiblock",
    "** eyes4s synthetic fixture; generated data; no human participant data",
    "START\t10\tLEFT\tSAMPLES",
    "PUPIL\tAREA",
    "SAMPLES\tGAZE\tLEFT\tRATE\t250.00",
    "10\t100\t200\t300",
    "14\t101\t201\t301",
    "END\t18",
    "START\t100\tRIGHT\tSAMPLES",
    "PUPIL\tDIAMETER",
    "SAMPLES\tHREF\tRIGHT\tRATE\t1000.00",
    "100\t1\t2\t3",
    "101\t2\t3\t4",
    "END\t102",
    "START\t200\tLEFT\tSAMPLES",
    "PUPIL\tARBITRARY_CAMERA_UNIT",
    "SAMPLES\tRAW\tLEFT\tRATE\t1000.00",
    "200\t11\t12\t13",
    "201\t12\t13\t14",
    "END\t202"
  )

  val syntheticRightHeadfixed500: EyeLinkAscParityFixture = lines(
    "synthetic-right-headfixed-500",
    "** eyes4s synthetic fixture; generated data; no human participant data",
    "START\t200\tRIGHT\tSAMPLES",
    "PUPIL\tDIAMETER",
    "SAMPLES\tHREF\tRIGHT\tRATE\t500.00\tTRACKING\tPUPIL-CR\tFILTER\t1",
    "200.000\t10.5\t20.25\t40",
    "202.000\t.\t.\t0",
    "END\t204"
  )

  val all: Vector[EyeLinkAscParityFixture] = Vector(
    textDecoding,
    decimalNegativeZero,
    newlineHandling,
    longIntegerTime,
    simultaneousOrdering,
    syntheticBinocularRemote2000,
    syntheticEventsMessages,
    syntheticFailsafeCorrupt,
    syntheticLeftHeadfixed1000,
    syntheticMultiblock,
    syntheticRightHeadfixed500
  )

class EyeLinkAscParitySuite extends munit.FunSuite:
  private val frame =
    Frame.screen("parity-surface", 1000, 1000).fold(error => fail(error.message), identity)
  private val config = EyeLinkAscSessionConfig
    .of(
      frame,
      ClockId("parity-tracker"),
      ConversionEvidencePolicy.Exploratory,
      AscBlinkReconciliationPolicy.ConfirmedNativeIntervals,
      AscUnspecifiedPupilPolicy.RejectMeasuredValues
    )
    .fold(error => fail(error.message), identity)

  private def materialize(fixture: EyeLinkAscParityFixture): EyeLinkAscSessionMaterialization =
    val settings = AscStreamSettings
      .of(s"${fixture.id}.asc", 16384, 7)
      .fold(error => fail(error.message), identity)
    EyeLinkAscSessions.materialize(
      EyeLinkAscOrigin.Unidentified(Sha256.ofBytes(fixture.input)),
      config,
      EyeLinkAscFraming.whole(settings, fixture.input)
    )

  EyeLinkAscParityFixtures.all.foreach { fixture =>
    test(s"${fixture.id}: canonical manifest matches the checked-in portable digest") {
      val manifest = EyeLinkAscCanonicalManifest.from(materialize(fixture))
      val input    = Sha256.ofBytes(fixture.input).hex
      val expected = EyeLinkAscParityExpected.byFixture.getOrElse(
        fixture.id,
        fail(
          s"missing parity baseline fixture=${fixture.id} input=$input canonical=${manifest.digest.hex} facts=${manifest.factCount}"
        )
      )

      assertEquals(input, expected.inputDigest)
      assertEquals(manifest.factCount, expected.facts)
      assertEquals(EyeLinkParityPlatform.observed(manifest).hex, expected.canonicalDigest)
    }
  }

  test("text-decoding fixture preserves BOM handling and opaque non-ASCII payload bytes") {
    val result = materialize(EyeLinkAscParityFixtures.textDecoding)
    assertEquals(result.report.physicalLines, 7)
    assertEquals(result.report.messages, 1)
    val message = result.raw.nativeRows
      .flatMap(_.record)
      .collectFirst { case AscParsedNativeRecord.Message(value) => value }
      .getOrElse(fail("expected the non-ASCII message payload"))
    assertEquals(message.payload.ascii, None)
    assert(message.payload.bytes.toVector.contains(0xff.toByte))
    assertEquals(result.raw.lines.head.source.number, 1L)
    assertEquals(result.raw.lines.head.record.isInstanceOf[AscRecord.Comment], true)
  }

  test("decimal fixture retains raw negative zero while canonical parsed zero is exact") {
    val result = materialize(EyeLinkAscParityFixtures.decimalNegativeZero)
    assertEquals(result.report.sampleRecords, 2)
    val samples = result.raw.sampleRows.flatMap(_.sample)
    assertEquals(samples.length, 2)
    assertEquals(samples.head.timestamp, BigDecimal(0))
    assertEquals(
      samples.head.left.map(_.coordinates.position),
      Some(AscNativePair.Values(BigDecimal(0), BigDecimal(0)))
    )
    val rawTimestamp = result.raw.lines.collectFirst {
      case AscLineResult(_, _, AscRecord.Sample(fields), _) => fields.token(0).flatMap(_.ascii)
    }.flatten
    assertEquals(rawTimestamp, Some("-0.0"))
  }

  test("newline fixture retains LF, CRLF, and terminal EOF distinctions") {
    val result = materialize(EyeLinkAscParityFixtures.newlineHandling)
    assertEquals(result.report.physicalLines, 6)
    assertEquals(
      result.raw.lines.map(_.source.terminator),
      Vector(
        AscLineTerminator.CarriageReturnLineFeed,
        AscLineTerminator.LineFeed,
        AscLineTerminator.CarriageReturnLineFeed,
        AscLineTerminator.LineFeed,
        AscLineTerminator.CarriageReturnLineFeed,
        AscLineTerminator.EndOfFile
      )
    )
  }

  test("long-time fixture remains exact beyond the JavaScript safe-integer boundary") {
    val result  = materialize(EyeLinkAscParityFixtures.longIntegerTime)
    val session = result.trusted.fold(
      errors => fail(errors.toVector.map(_.message).mkString("\n")),
      identity
    )
    val expected = BigDecimal("9007199254740.993")
    assertEquals(result.raw.sampleRows.head.sample.map(_.timestamp), Some(expected))
    session.recordingBlocks.head.recording match
      case EyeLinkAscRecordingArtifact.Monocular(recording) =>
        assertEquals(recording.size, 2)
        assertEquals(recording.samples(0).t.toMicros, 9007199254740993L)
        assertEquals(recording.samples(1).t.toMicros, 9007199254741993L)
      case other => fail(s"expected a monocular long-time recording, found=$other")
  }

  test("simultaneous messages use stable effective-time ordering") {
    val result  = materialize(EyeLinkAscParityFixtures.simultaneousOrdering)
    val session = result.trusted.fold(
      errors => fail(errors.toVector.map(_.message).mkString("\n")),
      identity
    )
    assertEquals(result.report.messages, 3)
    assertEquals(
      session.observedMessages.marks.map(_.at.toMicros),
      Vector(100000L, 100000L, 102000L)
    )
    assertEquals(
      session.observedMessages.marks.map(_.value.source.number),
      Vector(2L, 3L, 1L)
    )
  }

end EyeLinkAscParitySuite
