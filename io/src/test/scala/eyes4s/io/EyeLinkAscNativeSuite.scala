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

class EyeLinkAscNativeSuite extends munit.FunSuite:

  private def ascii(value: String): IArray[Byte] =
    IArray.tabulate(value.length)(index => value.charAt(index).toByte)

  private def source(value: String, index: Int): AscLineResult =
    val validated = AscSourceLine.of(
      "native.asc",
      index + 1L,
      index * 100L,
      ascii(value)
    )
    EyeLinkAscLexer.parse(validated.fold(error => fail(error.message), identity))

  private def run(values: String*): Vector[AscNativeEmission] =
    val lines = values.zipWithIndex.map { case (value, index) => source(value, index) }
    EyeLinkAscBlocks.machine.andThen(EyeLinkAscNative.machine).runAll(lines)

  private def parsed(values: String*): Vector[AscNativeParseResult] =
    run(values*).collect { case AscNativeEmission.Parsed(value) => value }

  private def event(value: AscNativeParseResult): AscParsedNativeEvent =
    value.record match
      case Some(AscParsedNativeRecord.Event(found)) => found
      case other => fail(s"expected native event, found=$other")

  private def message(value: AscNativeParseResult): AscParsedMessage =
    value.record match
      case Some(AscParsedNativeRecord.Message(found)) => found
      case other                                      => fail(s"expected message, found=$other")

  test("GAZE start and end records preserve EyeLink identity and exact summaries") {
    val results = parsed(
      "START 1 LEFT EVENTS",
      "EVENTS GAZE LEFT RATE 1000",
      "SFIX L 103",
      "EFIX L 103 120 17 100.0 200.0 500",
      "SSACC L 121",
      "ESACC L 121 130 9 100.0 200.0 120.0 220.0 30.0 400",
      "SBLINK L 131",
      "EBLINK L 131 140 9",
      "END 141"
    ).map(event)

    assertEquals(results.length, 6)
    assert(results.forall(_.producer == AscNativeEventProducer.EyeLinkOnlineParser))
    assert(results.forall(_.blockNumber == 1L))
    assert(results.forall(_.coordinateMode == AscCoordinateMode.Gaze))
    assertEquals(
      results.map(_.phase),
      Vector(
        AscNativeEventPhase.Start,
        AscNativeEventPhase.End,
        AscNativeEventPhase.Start,
        AscNativeEventPhase.End,
        AscNativeEventPhase.Start,
        AscNativeEventPhase.End
      )
    )
    assertEquals(results.map(_.source.number), Vector(3L, 4L, 5L, 6L, 7L, 8L))

    results(1).body match
      case AscNativeEventBody.FixationEnd(primary, None, None) =>
        assertEquals(
          primary.position,
          AscNativePair.Values(BigDecimal("100.0"), BigDecimal("200.0"))
        )
        assertEquals(primary.pupil, AscNativeScalar.Value(BigDecimal(500)))
      case other => fail(s"expected GAZE fixation summary, found=$other")

    results(3).body match
      case AscNativeEventBody.SaccadeEnd(primary, None, None) =>
        assertEquals(
          primary.startPosition,
          AscNativePair.Values(BigDecimal("100.0"), BigDecimal("200.0"))
        )
        assertEquals(
          primary.endPosition,
          AscNativePair.Values(BigDecimal("120.0"), BigDecimal("220.0"))
        )
        assertEquals(primary.amplitude, AscNativeScalar.Value(BigDecimal("30.0")))
        assertEquals(primary.peakVelocity, AscNativeScalar.Value(BigDecimal(400)))
      case other => fail(s"expected GAZE saccade summary, found=$other")

    assertEquals(results(5).interval.map(_.duration), Some(BigDecimal(9)))
    assertEquals(results(5).body, AscNativeEventBody.BlinkEnd)
  }

  test("HREF end records preserve both HREF and GAZE summaries plus resolution") {
    val results = parsed(
      "START 1 LEFT EVENTS",
      "EVENTS HREF LEFT RATE 500 RES",
      "EFIX L 10 20 10 1 2 3 101 102 3 40 41",
      "ESACC L 21 30 9 1 2 3 4 5 6 101 102 103 104 7 6 40 41",
      "END 31"
    ).map(event)

    results(0).body match
      case AscNativeEventBody.FixationEnd(primary, Some(gaze), Some(resolution)) =>
        assertEquals(primary.position, AscNativePair.Values(BigDecimal(1), BigDecimal(2)))
        assertEquals(primary.pupil, AscNativeScalar.Value(BigDecimal(3)))
        assertEquals(gaze.position, AscNativePair.Values(BigDecimal(101), BigDecimal(102)))
        assertEquals(resolution, AscNativePair.Values(BigDecimal(40), BigDecimal(41)))
      case other => fail(s"expected HREF fixation summary, found=$other")

    results(1).body match
      case AscNativeEventBody.SaccadeEnd(primary, Some(gaze), Some(resolution)) =>
        assertEquals(primary.startPosition, AscNativePair.Values(BigDecimal(1), BigDecimal(2)))
        assertEquals(primary.amplitude, AscNativeScalar.Value(BigDecimal(5)))
        assertEquals(gaze.endPosition, AscNativePair.Values(BigDecimal(103), BigDecimal(104)))
        assertEquals(gaze.peakVelocity, AscNativeScalar.Value(BigDecimal(6)))
        assertEquals(resolution, AscNativePair.Values(BigDecimal(40), BigDecimal(41)))
      case other => fail(s"expected HREF saccade summary, found=$other")
  }

  test(
    "MSG signed integration offsets are explicit and numeric-leading payloads stay payload"
  ) {
    val results = parsed(
      "MSG\t100 TRIALID synthetic-1",
      "MSG\t101 -5 DISPLAY_ON",
      "MSG\t102 12345",
      "MSG\t103 +2 !V TRIAL_VAR condition synthetic"
    ).map(message)

    assertEquals(results.map(_.loggedTime), Vector(100, 101, 102, 103).map(BigDecimal(_)))
    assertEquals(results.map(_.effectiveTime), Vector(100, 96, 102, 105).map(BigDecimal(_)))
    assertEquals(results(0).category, AscMessageCategory.Trial)
    assertEquals(results(0).payload.ascii, Some("TRIALID synthetic-1"))
    assertEquals(results(1).integrationOffset.map(_.value), Some(BigDecimal(-5)))
    assertEquals(results(1).payload.ascii, Some("DISPLAY_ON"))
    assertEquals(results(2).integrationOffset, None)
    assertEquals(results(2).payload.ascii, Some("12345"))
    assertEquals(results(3).category, AscMessageCategory.DataViewerIntegration)
    assertEquals(results(3).payload.ascii, Some("!V TRIAL_VAR condition synthetic"))
    assert(results.forall(_.blockNumber.isEmpty))
  }

  test("message payload bytes and trailing whitespace remain byte exact") {
    val prefix  = ascii("MSG\t10 ").toVector
    val payload = Vector[Byte]('A'.toByte, 0xff.toByte, 'B'.toByte, ' '.toByte, '\t'.toByte)
    val line    = AscSourceLine
      .of("bytes.asc", 1L, 0L, IArray.from(prefix ++ payload))
      .fold(error => fail(error.message), identity)
    val blockLine = EyeLinkAscBlocks.machine
      .runAll(Vector(EyeLinkAscLexer.parse(line)))
      .collectFirst { case AscBlockEmission.Line(value) => value }
      .getOrElse(fail("expected block line"))
    val result = EyeLinkAscNative.parse(blockLine)
    val parsed = message(result)

    assertEquals(parsed.payload.bytes.toVector, payload)
    assertEquals(parsed.payload.ascii, None)
    assertEquals(parsed.category, AscMessageCategory.Native)
  }

  test("message categories are typed without changing their payload") {
    val inputs = Vector(
      "MSG 1 !CAL calibration"      -> AscMessageCategory.Calibration,
      "MSG 2 !VAL validation"       -> AscMessageCategory.Validation,
      "MSG 3 DRIFTCORRECT 10 20"    -> AscMessageCategory.DriftCorrection,
      "MSG 4 RECCFG CR 1000"        -> AscMessageCategory.RecordingMetadata,
      "MSG 5 EYELINK CL 5.15"       -> AscMessageCategory.System,
      "MSG 6 vendor-specific value" -> AscMessageCategory.Native
    )
    val results = parsed(inputs.map(_._1)*).map(message)

    assertEquals(results.map(_.category), inputs.map(_._2))
    assertEquals(
      results.map(_.payload.ascii),
      inputs.map { case (line, _) => Some(line.split(" ", 3)(2)) }
    )
  }

  test(
    "updates and event metadata preserve uninterpreted fields without detector relabelling"
  ) {
    val results = parsed(
      "START 1 LEFT EVENTS",
      "EVENTS GAZE LEFT RATE 1000",
      "FIXUPDATE L 10 1.25 2.5 . vendor-tail",
      "BUTTON 11 1 0",
      "INPUT 12 255",
      "LOST_DATA_EVENT EVENT",
      "END 13"
    )

    val update = event(results(0))
    assertEquals(update.phase, AscNativeEventPhase.Update)
    assertEquals(
      update.body,
      AscNativeEventBody.PreservedUpdate(Vector("1.25", "2.5", ".", "vendor-tail"))
    )
    assertEquals(update.producer, AscNativeEventProducer.EyeLinkOnlineParser)

    val metadata = results.drop(1).map(_.record).map {
      case Some(AscParsedNativeRecord.Metadata(value)) => value
      case other => fail(s"expected metadata, found=$other")
    }
    assertEquals(
      metadata.map(_.kind),
      Vector(
        AscNativeMetadataKind.Button,
        AscNativeMetadataKind.Input,
        AscNativeMetadataKind.LostData
      )
    )
    assertEquals(metadata(0).fields, Vector("11", "1", "0"))
    assertEquals(metadata(1).fields, Vector("12", "255"))
    assertEquals(metadata(2).fields, Vector("EVENT"))
  }

  test("invalid event widths, eyes, intervals, pairs, and offsets are named and not dropped") {
    val results = parsed(
      "START 1 LEFT EVENTS",
      "EVENTS GAZE LEFT RATE 1000",
      "EFIX L 1 2 1 10 20",
      "SFIX R 3",
      "EBLINK L 5 4 1",
      "EBLINK L 5 6 9",
      "EFIX L 7 8 1 . 20 3",
      "MSG 10 +. payload",
      "MSG 1 -5 payload"
    )

    assert(
      results(0).diagnostics.exists(_.isInstanceOf[AscNativeDiagnostic.UnexpectedFieldCount])
    )
    assert(results(1).diagnostics.exists(_.isInstanceOf[AscNativeDiagnostic.InvalidEye]))
    assert(results(2).diagnostics.exists(_.isInstanceOf[AscNativeDiagnostic.EndBeforeStart]))
    assert(results(3).diagnostics.exists(_.isInstanceOf[AscNativeDiagnostic.DurationMismatch]))
    assert(
      results(4).diagnostics.exists(_.isInstanceOf[AscNativeDiagnostic.PartialMissingPair])
    )
    assert(
      results(5).diagnostics.exists(
        _.isInstanceOf[AscNativeDiagnostic.InvalidIntegrationOffset]
      )
    )
    assert(results(6).diagnostics.exists(_.isInstanceOf[AscNativeDiagnostic.NegativeTime]))
    assert(results.forall(_.record.isEmpty))
    assert(results.flatMap(_.diagnostics).forall(_.message.contains("source='native.asc'")))
  }

  test("native records outside their required block remain explicit failures") {
    val result = parsed("SFIX L 1").head

    assert(result.record.isEmpty)
    assert(result.hasErrors)
    assert(result.diagnostics.exists(_.isInstanceOf[AscNativeDiagnostic.BlockRequirement]))
    assert(result.diagnostics.exists(_.isInstanceOf[AscNativeDiagnostic.EventBlockMissing]))
  }

  test(
    "the machine emits once for each parsed native line and preserves every other emission"
  ) {
    val output = run(
      "# comment",
      "START 1 LEFT EVENTS",
      "EVENTS GAZE LEFT RATE 1000",
      "MSG 1 ready",
      "SFIX L 2",
      "END 3"
    )

    assertEquals(output.count(_.isInstanceOf[AscNativeEmission.Parsed]), 2)
    assertEquals(output.count(_.isInstanceOf[AscNativeEmission.Preserved]), 5)
    assert(
      output.exists {
        case AscNativeEmission.Preserved(AscBlockEmission.Closed(_)) => true
        case _                                                       => false
      }
    )
  }

end EyeLinkAscNativeSuite
