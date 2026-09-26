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

package consumer

import eyes4s.io.*
import eyes4s.compare.MeasureDistance

class PublicIoSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)

  test("IO failures identify the original source row field and rejected operands") {
    val examples = Vector(
      AscFramingDiagnostic
        .LineTooLong("source-X", 7, 13, 17, 19, "excerpt-X")
        .message -> Vector("source-X", "7", "13", "17", "19"),
      AscPerformanceValidationError.NonPositive("chunk-X", -17).message -> Vector(
        "chunk-X",
        "-17"
      ),
      AscSampleMaterializationError
        .PixelValueOutsideDoubleRange(AscRecordedEye.Left, "1e999", "2e999", "frame-X")
        .message -> Vector("1e999", "2e999", "frame-X"),
      AscSourceLineError.NonPositiveLineNumber("source-X", -17).message -> Vector(
        "source-X",
        "-17"
      ),
      AscStreamConfigurationError.NonPositiveReadChunk(-17).message -> Vector("-17"),
      ContrastExportError
        .Components(Vector("expected-X"), Vector("actual-X"))
        .message -> Vector("expected-X", "actual-X"),
      DelimitedSchemaError.DuplicateLogicalColumn("column-X", 3, 7).message -> Vector(
        "column-X",
        "3",
        "7"
      ),
      EyeLinkAscSessionConfigError.BlankClock(" ").message -> Vector("clock=' '"),
      EyeLinkConformanceError
        .InvalidOperand("artifact-X", "field-X", "actual-X", "expected-X")
        .message -> Vector("artifact-X", "field-X", "actual-X", "expected-X"),
      EyeLinkOracleError.InvalidPreamble("source-X", "preamble-X").message -> Vector(
        "source-X",
        "preamble-X"
      ),
      FixationImportError.Columns(Vector("column-X")).message -> Vector("column-X"),
      FixationRowError.Number("column-X", "invalid-X", "numeric-X").message -> Vector(
        "column-X",
        "invalid-X",
        "numeric-X"
      ),
      PsychologyWorkflowError
        .MillisecondsOutsideRange("duration-X", Long.MaxValue)
        .message -> Vector("duration-X", Long.MaxValue.toString),
      ResultExportError.Width(7, 13, 19).message                     -> Vector("7", "13", "19"),
      TemplateCsvError.Row(7, Vector("field-X"), "reason-X").message -> Vector(
        "7",
        "field-X",
        "reason-X"
      ),
      TidyCsvError.WrongColumnCount(7, 13, 19).message         -> Vector("7", "13", "19"),
      TidyResultError.BlankTrial("participant-X", " ").message -> Vector("participant-X", "' '")
    )
    examples.foreach { (message, operands) =>
      operands.foreach(o => assert(message.contains(o), message))
    }
  }

  test("IO convenience views preserve identity and independent hash and score values") {
    val digest = Sha256.ofUtf8("")
    assertEquals(
      digest.toString,
      "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    )
    assertEquals(digest.hashCode, Sha256.ofBytes(IArray.empty[Byte]).hashCode)
    assertEquals(Set(digest, Sha256.ofBytes(IArray.empty[Byte])).size, 1)
    assertEquals(get(StudyTrial.of("p", "t")).hashCode, get(StudyTrial.of("p", "t")).hashCode)
    assertEquals(Set(get(StudyTrial.of("p", "t")), get(StudyTrial.of("p", "t"))).size, 1)
    assertEquals(ScoreColumns.distance.names, Vector("value"))
    assertEquals(
      get(ScoreColumns.distance.scores(get(MeasureDistance.of(2.5)), "score-X")),
      Vector(2.5)
    )
    assert(AscReadChunkSize.Default.bytes > 0)
    assert(EyeLinkSupport.preservedOnly.nonEmpty)
    assert(EyeLinkSupport.explicitlyRejected.nonEmpty)
    EyeLinkSupport.capabilities.foreach(c => assert(c.toString.contains(c.id)))
    assertEquals(
      AscNativeDiagnostic.EventBlockMissing("source-X", 7).severity,
      AscDiagnosticSeverity.Error
    )
  }
