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
import eyes4s.kernel.*

class AscCrWarningsSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private def parsed(eyes: String, tracking: String, row: String): AscSampleParseResult =
    val lines = Vector(
      s"START 1 $eyes SAMPLES",
      "PUPIL AREA",
      s"SAMPLES GAZE $eyes RATE 250 $tracking",
      row,
      "END 20"
    )
    val input = lines.zipWithIndex.map { (line, index) =>
      EyeLinkAscLexer.parse(
        get(
          AscSourceLine.of(
            "cr.asc",
            index + 1L,
            index * 100L,
            IArray.from(line.getBytes(java.nio.charset.StandardCharsets.US_ASCII))
          )
        )
      )
    }
    EyeLinkAscBlocks.machine
      .andThen(EyeLinkAscSamples.machine)
      .runAll(input)
      .collectFirst { case AscSampleEmission.Parsed(value) => value }
      .getOrElse(fail("missing sample"))
  private def flags(result: AscSampleParseResult): AscCrWarningFlags =
    result.sample
      .getOrElse(fail(result.diagnostics.map(_.message).mkString("; ")))
      .crWarnings match
      case AscCrWarningEvidence.Observed(value) => value
      case other => fail(s"expected observed flags, found=$other")

  test("monocular left and right warning tokens retain every warning combination") {
    for
      eye <- Vector("LEFT", "RIGHT"); i <- Vector('.', 'I'); c <- Vector('.', 'C');
      r   <- Vector('.', 'R')
    do
      val raw    = s"$i$c$r"
      val result = parsed(eye, "TRACKING CR", s"2 10 20 30 $raw")
      val value  = flags(result)
      assertEquals(value.raw, raw)
      assertEquals(value.interpolated, i == 'I')
      val selected = if eye == "LEFT" then value.left else value.right
      val absent   = if eye == "LEFT" then value.right else value.left
      assertEquals(selected.map(_.missing), Some(c == 'C'))
      assertEquals(selected.map(_.recovering), Some(r == 'R'))
      assertEquals(absent, None)
      assertEquals(
        result.sample.map(_.convention),
        Some(AscSampleLayoutConvention.Edf2AscCanonicalCrWarningsV1)
      )
  }

  test("binocular warnings keep each eye separate and follow all optional numeric columns") {
    val result = parsed(
      "LEFT RIGHT",
      "TRACKING CR VEL RES INPUT BUTTONS STATUS HTARGET",
      "2 10 20 30 40 50 60 1 2 3 4 5 6 7 8 9 10 11 12 target ICR.R"
    )
    val value = flags(result)
    assert(value.interpolated)
    assertEquals(value.left.map(v => v.missing -> v.recovering), Some(true -> true))
    assertEquals(value.right.map(v => v.missing -> v.recovering), Some(false -> true))
    assertEquals(result.sample.flatMap(_.headTarget).map(_.flags), Some("target"))
  }

  test("absent exports and undeclared tracking stay distinct; dots do not infer blink") {
    assertEquals(
      parsed("LEFT", "TRACKING CR", "2 10 20 30").sample.map(_.crWarnings),
      Some(AscCrWarningEvidence.NotExported)
    )
    assertEquals(
      parsed("LEFT", "", "2 10 20 30").sample.map(_.crWarnings),
      Some(AscCrWarningEvidence.NotDeclared)
    )
    val missing = parsed("LEFT", "TRACKING CR", "2 . . 0 ...")
    assertEquals(flags(missing).raw, "...")
    assertEquals(
      missing.sample.flatMap(_.left).map(_.coordinates.position),
      Some(AscNativePair.MissingDot)
    )
    assert(parsed("LEFT", "", "2 10 20 30 ...").sample.isEmpty)
  }

  test("malformed warning lengths and positions retain source operands as failures") {
    for raw <- Vector("..", "....", "C..", ".R.", "..C", "icr") do
      val result = parsed("LEFT", "TRACKING CR", s"2 10 20 30 $raw")
      assert(result.sample.isEmpty)
      assert(
        result.diagnostics.exists {
          case AscSampleDiagnostic
                .InvalidCrWarningFlags("cr.asc", 4L, 1L, AscEyeLayout.Left, value) =>
            value == raw
          case _ => false
        },
        clue(raw)
      )
    assert(parsed("LEFT", "TRACKING CR", "2 10 20 30 ... surplus").sample.isEmpty)
  }

  test("native interpolation reaches recording lineage without manufacturing a blink") {
    val text =
      "START 0 LEFT SAMPLES\nPUPIL AREA\nSAMPLES GAZE LEFT RATE 250 TRACKING CR\n0 10 20 30 I..\n4 11 21 31 ...\nEND 8\n"
    val frame  = get(Frame.screen("cr-display", 100, 100))
    val config = get(
      EyeLinkAscSessionConfig.of(
        frame,
        ClockId("cr-clock"),
        ConversionEvidencePolicy.Exploratory,
        AscBlinkReconciliationPolicy.ConfirmedNativeIntervals,
        AscUnspecifiedPupilPolicy.RejectMeasuredValues
      )
    )
    val result = EyeLinkAscImport.fromBytes(
      IArray.from(text.getBytes(java.nio.charset.StandardCharsets.US_ASCII)),
      get(AscStreamSettings.of("cr.asc", 4096, 17)),
      config
    )
    val session = get(result.trusted)
    session.recordingBlocks.head.recording match
      case EyeLinkAscRecordingArtifact.Monocular(recording) =>
        assertEquals(recording.size, 2)
        assertEquals(recording.samples(0).lineage, SampleLineage.interpolated)
        assertEquals(recording.samples(1).lineage, SampleLineage.measured)
        assert(recording.samples(0).gaze.isUsable)
        assert(!recording.samples(0).gaze.isMissing)
      case other => fail(s"expected monocular, found=$other")
  }
