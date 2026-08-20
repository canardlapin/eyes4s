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

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths

class EyeLinkOracleJvmSuite extends munit.FunSuite:

  private val CorpusRoot = "/eyes4s/io/eyelink/corpus/"
  private val OracleRoot = "/eyes4s/io/eyelink/oracles/"

  private def resourceBytes(root: String, name: String): Array[Byte] =
    val stream = Option(getClass.getResourceAsStream(root + name))
      .getOrElse(fail(s"missing resource=$root$name"))
    try stream.readAllBytes()
    finally stream.close()

  private def resourceText(root: String, name: String): String =
    new String(resourceBytes(root, name), StandardCharsets.UTF_8)

  private lazy val corpus: EyeLinkCorpusManifest =
    EyeLinkCorpusManifest
      .parseTsv("classpath:corpus/manifest.tsv", resourceText(CorpusRoot, "manifest.tsv"))
      .fold(errors => fail(errors.toVector.map(_.message).mkString("\n")), identity)

  private lazy val oracleText: String =
    resourceText(OracleRoot, "synthetic-events-messages-eyelinker.tsv")

  private lazy val oracle: EyeLinkOracleManifest =
    EyeLinkOracleManifest
      .parseTsv("classpath:oracles/synthetic-events-messages-eyelinker.tsv", oracleText)
      .fold(errors => fail(errors.toVector.map(_.message).mkString("\n")), identity)

  private lazy val repositoryRoot: Path =
    Iterator
      .iterate(Paths.get("").toAbsolutePath.normalize())(_.getParent)
      .takeWhile(_ != null)
      .find(path => Files.isRegularFile(path.resolve("build.sbt")))
      .getOrElse(fail("could not locate repository root from user.dir"))

  private def repositoryBytes(path: String): Array[Byte] =
    Files.readAllBytes(repositoryRoot.resolve(path))

  private def repositoryText(path: String): String =
    new String(repositoryBytes(path), StandardCharsets.UTF_8)

  private lazy val nativeFixture: Vector[AscNativeParseResult] =
    val settings = AscStreamSettings
      .of("synthetic-events-messages.asc", 1024 * 1024, 64 * 1024)
      .fold(error => fail(error.message), identity)
    val framed = EyeLinkAscFraming
      .whole(
        settings,
        IArray.from(resourceBytes(CorpusRoot, "synthetic-events-messages.asc"))
      )
      .map {
        case AscFramingEmission.Parsed(value)   => value
        case AscFramingEmission.Rejected(value) => fail(value.message)
      }
    EyeLinkAscBlocks.machine
      .andThen(EyeLinkAscNative.machine)
      .runAll(framed)
      .collect { case AscNativeEmission.Parsed(value) => value }

  private lazy val sampleFixture: Vector[AscNativeSample] =
    val settings = AscStreamSettings
      .of("synthetic-events-messages.asc", 1024 * 1024, 64 * 1024)
      .fold(error => fail(error.message), identity)
    val framed = EyeLinkAscFraming
      .whole(
        settings,
        IArray.from(resourceBytes(CorpusRoot, "synthetic-events-messages.asc"))
      )
      .map {
        case AscFramingEmission.Parsed(value)   => value
        case AscFramingEmission.Rejected(value) => fail(value.message)
      }
    EyeLinkAscBlocks.machine
      .andThen(EyeLinkAscSamples.machine)
      .runAll(framed)
      .collect { case AscSampleEmission.Parsed(value) => value.sample }
      .flatten

  private def oracleValue(record: Vector[EyeLinkOracleFact], field: String): String =
    record
      .find(fact => fact.fieldPath == field && fact.presence == EyeLinkOraclePresence.Value)
      .map(_.value)
      .getOrElse(fail(s"oracle record=${record.head.recordOrdinal} missing field=$field"))

  private def oracleDecimal(record: Vector[EyeLinkOracleFact], field: String): BigDecimal =
    BigDecimal(oracleValue(record, field))

  test("committed eyelinker oracle is canonical and bound to exact corpus bytes") {
    val fixture = corpus.fixtures
      .find(_.id == oracle.descriptor.fixtureId)
      .getOrElse(fail(s"missing fixture=${oracle.descriptor.fixtureId}"))
    val resourceDigest =
      Sha256.ofBytes(IArray.from(oracleText.getBytes(StandardCharsets.UTF_8)))

    assertEquals(oracle.renderTsv, oracleText)
    assertEquals(oracle.scientificDigest, resourceDigest)
    assertEquals(oracle.descriptor.kind, EyeLinkOracleKind.Eyelinker)
    assertEquals(oracle.descriptor.toolVersion, "0.2.2")
    assertEquals(
      oracle.descriptor.toolDigest.hex,
      "baa08d892a8019da90d0ea5d11f9002a0c340aa3be4b365128ab7cb804c61973"
    )
    assertEquals(oracle.descriptor.inputDigest, fixture.ascDigest.getOrElse(fail("digest")))
    assertEquals(oracle.records.length, 10)
    assertEquals(oracle.facts.length, 95)
  }

  test("independent reader records native events, messages, samples, and metadata") {
    val byKind = oracle.records.groupBy(_.head.recordKind).view.mapValues(_.length).toMap
    assertEquals(byKind("sample"), 1)
    assertEquals(byKind("native-saccade"), 1)
    assertEquals(byKind("native-fixation"), 2)
    assertEquals(byKind("native-blink"), 1)
    assertEquals(byKind("message"), 4)
    assertEquals(byKind("recording-metadata"), 1)

    val values = oracle.facts.collect {
      case fact if fact.presence == EyeLinkOraclePresence.Value => fact.fieldPath -> fact.value
    }
    assert(values.contains("reader.sacc.eye" -> "L"))
    assert(values.contains("reader.fix.eye" -> "L"))
    assert(values.contains("reader.info.sample.dtype" -> "GAZE"))
    assert(values.contains("reader.info.event.dtype" -> "GAZE"))
    assert(values.contains("reader.info.pupil.dtype" -> "AREA"))
    assert(values.contains("reader.info.sample.rate" -> "1000"))
    assert(values.contains("message.raw-time" -> "101"))
    assert(values.contains("message.payload" -> "-5 DISPLAY_ON"))
  }

  test("native parser agrees field-by-field with the digest-pinned eyelinker oracle") {
    val events = nativeFixture.flatMap(_.record).collect {
      case AscParsedNativeRecord.Event(value) if value.phase == AscNativeEventPhase.End => value
    }
    val messages = nativeFixture.collect {
      case result if result.record.exists(_.isInstanceOf[AscParsedNativeRecord.Message]) =>
        val value = result.record
          .collect { case AscParsedNativeRecord.Message(found) => found }
          .getOrElse(fail("message guard lost message"))
        result -> value
    }
    val oracleEvents   = oracle.records.filter(_.head.recordKind.startsWith("native-"))
    val oracleMessages = oracle.records.filter(_.head.recordKind == "message")

    assertEquals(events.length, oracleEvents.length)
    assertEquals(messages.length, oracleMessages.length)
    events.foreach { actual =>
      val kind = actual.eventType match
        case AscNativeEventType.Fixation => "native-fixation"
        case AscNativeEventType.Saccade  => "native-saccade"
        case AscNativeEventType.Blink    => "native-blink"
      val prefix = actual.eventType match
        case AscNativeEventType.Fixation => "reader.fix"
        case AscNativeEventType.Saccade  => "reader.sacc"
        case AscNativeEventType.Blink    => "reader.blinks"
      val expected = oracleEvents
        .find(record =>
          record.head.recordKind == kind &&
            oracleValue(record, s"$prefix.stime") == actual.onset.toString
        )
        .getOrElse(fail(s"oracle has no kind=$kind onset=${actual.onset}"))
      val interval = actual.interval.getOrElse(fail(s"end event kind=$kind has no interval"))

      assertEquals(oracleValue(expected, s"$prefix.block"), actual.blockNumber.toString)
      assertEquals(oracleDecimal(expected, s"$prefix.stime"), interval.start)
      assertEquals(oracleDecimal(expected, s"$prefix.etime"), interval.end)
      assertEquals(oracleDecimal(expected, s"$prefix.dur"), interval.duration)
      assertEquals(oracleValue(expected, s"$prefix.eye"), actual.eye.toString.take(1))

      actual.body match
        case AscNativeEventBody.FixationEnd(primary, None, None) =>
          val position = primary.position match
            case AscNativePair.Values(x, y) => x -> y
            case AscNativePair.MissingDot   => fail("fixture fixation unexpectedly missing")
          val pupil = primary.pupil match
            case AscNativeScalar.Value(value) => value
            case AscNativeScalar.MissingDot   => fail("fixture fixation pupil missing")
          assertEquals(oracleDecimal(expected, "reader.fix.axp"), position._1)
          assertEquals(oracleDecimal(expected, "reader.fix.ayp"), position._2)
          assertEquals(oracleDecimal(expected, "reader.fix.aps"), pupil)
        case AscNativeEventBody.SaccadeEnd(primary, None, None) =>
          val start = primary.startPosition match
            case AscNativePair.Values(x, y) => x -> y
            case AscNativePair.MissingDot   => fail("fixture saccade start missing")
          val end = primary.endPosition match
            case AscNativePair.Values(x, y) => x -> y
            case AscNativePair.MissingDot   => fail("fixture saccade end missing")
          val amplitude = primary.amplitude match
            case AscNativeScalar.Value(value) => value
            case AscNativeScalar.MissingDot   => fail("fixture saccade amplitude missing")
          val velocity = primary.peakVelocity match
            case AscNativeScalar.Value(value) => value
            case AscNativeScalar.MissingDot   => fail("fixture saccade velocity missing")
          assertEquals(oracleDecimal(expected, "reader.sacc.sxp"), start._1)
          assertEquals(oracleDecimal(expected, "reader.sacc.syp"), start._2)
          assertEquals(oracleDecimal(expected, "reader.sacc.exp"), end._1)
          assertEquals(oracleDecimal(expected, "reader.sacc.eyp"), end._2)
          assertEquals(oracleDecimal(expected, "reader.sacc.ampl"), amplitude)
          assertEquals(oracleDecimal(expected, "reader.sacc.pv"), velocity)
        case AscNativeEventBody.BlinkEnd => ()
        case other                       => fail(s"unexpected external-parity body=$other")
    }

    messages.foreach { case (result, actual) =>
      val expected = oracleMessages
        .find(record => oracleValue(record, "reader.msg.time") == actual.loggedTime.toString)
        .getOrElse(fail(s"oracle has no message time=${actual.loggedTime}"))
      val rawPayload = result.line.line.record match
        case AscRecord.Message(fields) =>
          fields.remainderAfter(0).flatMap(_.ascii).getOrElse(fail("message payload"))
        case other => fail(s"parsed message retained unexpected lexical record=$other")
      assertEquals(oracleValue(expected, "reader.msg.block"), actual.blockNumber.mkString)
      assertEquals(oracleDecimal(expected, "reader.msg.time"), actual.loggedTime)
      assertEquals(oracleValue(expected, "reader.msg.text"), rawPayload)
    }
  }

  test("typed conformance comparator certifies every common sample, event, and message field") {
    def decimal(value: BigDecimal): String =
      if value == 0 then "0" else value.bigDecimal.stripTrailingZeros.toPlainString

    def pair(value: AscNativePair): (BigDecimal, BigDecimal) = value match
      case AscNativePair.Values(x, y) => x -> y
      case AscNativePair.MissingDot => fail("external parity fixture unexpectedly missing pair")

    def scalar(value: AscNativeScalar): BigDecimal = value match
      case AscNativeScalar.Value(found) => found
      case AscNativeScalar.MissingDot   =>
        fail("external parity fixture unexpectedly missing scalar")

    val actualEvents = nativeFixture.flatMap(_.record).collect {
      case AscParsedNativeRecord.Event(value) if value.phase == AscNativeEventPhase.End => value
    }
    val actualMessages = nativeFixture.collect {
      case result if result.record.exists(_.isInstanceOf[AscParsedNativeRecord.Message]) =>
        val message = result.record.collect { case AscParsedNativeRecord.Message(value) =>
          value
        }.get
        result -> message
    }

    def actualFor(record: Vector[EyeLinkOracleFact], path: String): String =
      record.head.recordKind match
        case "sample" =>
          val sample = sampleFixture
            .find(_.blockNumber == record.head.block.getOrElse(-1))
            .getOrElse(
              fail(s"eyes4s sample missing oracle record=${record.head.recordOrdinal}")
            )
          val eye      = sample.left.getOrElse(fail("expected left sample"))
          val position = pair(eye.coordinates.position)
          path match
            case "reader.raw.block" => sample.blockNumber.toString
            case "reader.raw.time"  => decimal(sample.timestamp)
            case "reader.raw.xp"    => decimal(position._1)
            case "reader.raw.yp"    => decimal(position._2)
            case "reader.raw.ps"    =>
              eye.pupil.observation match
                case AscPupilObservation.Measured(value) => decimal(value)
                case other => fail(s"expected measured sample pupil, got $other")
            case other => fail(s"unmapped sample parity field=$other")
        case kind if kind.startsWith("native-") =>
          val prefix = kind match
            case "native-fixation" => "reader.fix"
            case "native-saccade"  => "reader.sacc"
            case "native-blink"    => "reader.blinks"
            case other             => fail(s"unmapped oracle event kind=$other")
          val onset = BigDecimal(oracleValue(record, s"$prefix.stime"))
          val event = actualEvents
            .find(value => value.onset == onset)
            .getOrElse(
              fail(s"eyes4s event missing oracle kind=$kind onset=$onset")
            )
          val interval = event.interval.getOrElse(fail(s"event kind=$kind missing interval"))
          path match
            case value if value == s"$prefix.block" => event.blockNumber.toString
            case value if value == s"$prefix.stime" => decimal(interval.start)
            case value if value == s"$prefix.etime" => decimal(interval.end)
            case value if value == s"$prefix.dur"   => decimal(interval.duration)
            case value if value == s"$prefix.eye"   => event.eye.toString.take(1)
            case "reader.fix.axp"                   =>
              event.body match
                case AscNativeEventBody.FixationEnd(primary, None, None) =>
                  decimal(pair(primary.position)._1)
                case other => fail(s"unexpected fixation body=$other")
            case "reader.fix.ayp" =>
              event.body match
                case AscNativeEventBody.FixationEnd(primary, None, None) =>
                  decimal(pair(primary.position)._2)
                case other => fail(s"unexpected fixation body=$other")
            case "reader.fix.aps" =>
              event.body match
                case AscNativeEventBody.FixationEnd(primary, None, None) =>
                  decimal(scalar(primary.pupil))
                case other => fail(s"unexpected fixation body=$other")
            case saccadeField if saccadeField.startsWith("reader.sacc.") =>
              event.body match
                case AscNativeEventBody.SaccadeEnd(primary, None, None) =>
                  val start = pair(primary.startPosition)
                  val end   = pair(primary.endPosition)
                  saccadeField match
                    case "reader.sacc.sxp"  => decimal(start._1)
                    case "reader.sacc.syp"  => decimal(start._2)
                    case "reader.sacc.exp"  => decimal(end._1)
                    case "reader.sacc.eyp"  => decimal(end._2)
                    case "reader.sacc.ampl" => decimal(scalar(primary.amplitude))
                    case "reader.sacc.pv"   => decimal(scalar(primary.peakVelocity))
                    case other              => fail(s"unmapped saccade parity field=$other")
                case other => fail(s"unexpected saccade body=$other")
            case other => fail(s"unmapped event parity field=$other")
        case "message" =>
          val rawTime           = BigDecimal(oracleValue(record, "reader.msg.time"))
          val (result, message) = actualMessages
            .find(_._2.loggedTime == rawTime)
            .getOrElse(
              fail(s"eyes4s message missing oracle time=$rawTime")
            )
          val rawPayload = result.line.line.record match
            case AscRecord.Message(fields) =>
              fields.remainderAfter(0).flatMap(_.ascii).getOrElse(fail("message payload"))
            case other => fail(s"message retained unexpected record=$other")
          path match
            case "reader.msg.block" | "message.block"   => message.blockNumber.mkString
            case "reader.msg.time" | "message.raw-time" => decimal(message.loggedTime)
            case "reader.msg.text" | "message.payload"  => rawPayload
            case other => fail(s"unmapped message parity field=$other")
        case other => fail(s"unmapped oracle record kind=$other")

    val commonRecords =
      oracle.records.filter(record => record.head.recordKind != "recording-metadata")
    val expectedFacts = commonRecords.flatMap { record =>
      record.collect {
        case value if value.presence == EyeLinkOraclePresence.Value =>
          EyeLinkConformanceFact
            .create(
              s"record.${value.recordOrdinal}.${value.fieldPath}",
              EyeLinkConformanceValue.value(value.value)
            )
            .fold(error => fail(error.message), identity)
      }
    }
    val actualFacts = commonRecords.flatMap { record =>
      record.collect {
        case value if value.presence == EyeLinkOraclePresence.Value =>
          EyeLinkConformanceFact
            .create(
              s"record.${value.recordOrdinal}.${value.fieldPath}",
              EyeLinkConformanceValue.value(actualFor(record, value.fieldPath))
            )
            .fold(error => fail(error.message), identity)
      }
    }
    val expectedOperand = EyeLinkConformanceOperand
      .create(oracle.descriptor.fixtureId, oracle.descriptor.oracleId, oracle.scientificDigest)
      .fold(error => fail(error.message), identity)
    val actualOperand = EyeLinkConformanceOperand
      .create(
        oracle.descriptor.fixtureId,
        "eyes4s-asc",
        Sha256.ofUtf8(actualFacts.map(_.fieldPath).mkString("\n"))
      )
      .fold(error => fail(error.message), identity)
    val expected = EyeLinkConformanceManifest
      .create(expectedOperand, expectedFacts)
      .fold(errors => fail(errors.toVector.map(_.message).mkString("\n")), identity)
    val actual = EyeLinkConformanceManifest
      .create(actualOperand, actualFacts)
      .fold(errors => fail(errors.toVector.map(_.message).mkString("\n")), identity)
    val comparison = EyeLinkConformance
      .compare(actual, expected)
      .fold(errors => fail(errors.toVector.map(_.message).mkString("\n")), identity)

    assertEquals(comparison.comparedFields, 57)
  }

  test("reader limitations are explicit and cannot masquerade as observed semantics") {
    assertEquals(oracle.descriptor.ordering, EyeLinkOracleOrdering.Unavailable)
    assert(
      oracle.records.forall(record =>
        record.exists(fact =>
          fact.fieldPath == "oracle.source-order" &&
            fact.presence == EyeLinkOraclePresence.Omitted
        )
      )
    )
    val messages = oracle.records.filter(_.head.recordKind == "message")
    assert(messages.nonEmpty)
    assert(
      messages.forall(record =>
        record.exists(fact =>
          fact.fieldPath == "message.effective-time" &&
            fact.presence == EyeLinkOraclePresence.Omitted
        ) && record.exists(fact =>
          fact.fieldPath == "message.offset" &&
            fact.presence == EyeLinkOraclePresence.Omitted
        )
      )
    )
  }

  test("oracle registry detects stale adapters, inputs, and generated manifests") {
    val rows = resourceText(OracleRoot, "registry.tsv")
      .split("\n", -1)
      .toVector
      .filter(line => line.nonEmpty && !line.startsWith("#"))
    val header    = rows.head.split("\t", -1).toVector
    val parsed    = rows.tail.map(row => header.zip(row.split("\t", -1).toVector).toMap)
    val eyelinker = parsed.find(_("oracle_kind") == "eyelinker").getOrElse(fail("registry"))
    val edfApi = parsed.find(_("oracle_kind") == "edf-access-api").getOrElse(fail("registry"))

    assertEquals(
      Sha256.ofBytes(IArray.from(resourceBytes(OracleRoot, eyelinker("manifest_path")))).hex,
      eyelinker("manifest_digest")
    )
    assertEquals(
      Sha256
        .ofBytes(
          IArray.from(resourceBytes(CorpusRoot, "synthetic-events-messages.asc"))
        )
        .hex,
      eyelinker("input_digest")
    )
    parsed.foreach { row =>
      assertEquals(
        Sha256.ofBytes(IArray.from(repositoryBytes(row("adapter_path")))).hex,
        row("adapter_digest"),
        row("oracle_kind")
      )
    }
    assertEquals(edfApi("status"), "awaiting-licensed-real-input")
    assertEquals(edfApi("manifest_path"), "")
    assertEquals(edfApi("manifest_digest"), "")
    assertEquals(edfApi("tool_digest"), "")
  }

  test("reference extractors call independent APIs and cannot import eyes4s parser code") {
    val rAdapter     = repositoryText("tools/eyelink-oracles/eyelinker_reference.R")
    val cAdapter     = repositoryText("tools/eyelink-oracles/edfapi_reference.c")
    val regeneration = repositoryText("tools/eyelink-oracles/regenerate.sh")
    val forbidden    = Vector(
      "EyeLinkAscLexer",
      "EyeLinkAscBlocks",
      "EyeLinkAscSamples",
      "eyes4s.io.",
      "io/src/main/scala"
    )

    assert(rAdapter.contains("eyelinker::read_asc"))
    assert(cAdapter.contains("edf_open_file"))
    assert(cAdapter.contains("edf_get_next_data"))
    assert(cAdapter.contains("edf_get_float_data"))
    assert(cAdapter.contains("message->len"))
    assert(cAdapter.contains("message->c"))
    assert(regeneration.contains("EYELINKER_SOURCE_SHA256"))
    forbidden.foreach { value =>
      assert(!rAdapter.contains(value), s"R adapter contains forbidden dependency=$value")
      assert(!cAdapter.contains(value), s"C adapter contains forbidden dependency=$value")
    }
  }

end EyeLinkOracleJvmSuite
