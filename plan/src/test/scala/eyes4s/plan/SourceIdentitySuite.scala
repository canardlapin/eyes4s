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

package eyes4s.plan

import eyes4s.kernel.*
import scala.compiletime.testing.typeCheckErrors

class SourceIdentitySuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
  private val frame                             = get(Frame.screen("source-frame", 100, 100))
  private val columns                           = get(
    SourceFixationColumns.of(
      "n",
      "x",
      "y",
      "onset",
      "duration",
      SampleCountRule.PositiveColumn("samples")
    )
  )
  private def spec(
      unit: SourceTimeUnit = SourceTimeUnit.Milliseconds,
      decision: AdmissionDecision = AdmissionDecision.ReviewExclusions
  ) =
    get(
      ImportSpec.of(
        SourceKeyColumns.Study("participant", "item", "phase"),
        columns,
        frame,
        unit,
        AdmissionPolicy.default[StudyKey],
        decision
      )
    )
  private val header =
    Vector("participant", "item", "phase", "n", "x", "y", "onset", "duration", "samples")
  private val rows = Vector(Vector("p", "i", "encode", "0", "10", "20", "0", "100", "10"))

  test("stable identity tag and display names outside semantic identity") {
    assertEquals(SourceIdentity.versionTag, "eyes4s.source-identity/1")
    val a = spec().source("old/path.csv", header, rows)
    val b = spec().source("new/path.csv", header, rows)
    assertEquals(a.identity, b.identity)
    assertEquals(a.records, SourceRef.of("legacy", header, rows).records)
    assertEquals(SourceRef.of("legacy", header, rows).identity, None)
  }
  test("unit, admission decision and rejected source records affect identity") {
    val base = spec().source("a", header, rows).identity
    assertNotEquals(base, spec(SourceTimeUnit.Seconds).source("a", header, rows).identity)
    assertNotEquals(
      base,
      spec(decision = AdmissionDecision.RequireComplete).source("a", header, rows).identity
    )
    assertNotEquals(base, spec().source("a", header, rows :+ Vector("bad")).identity)
  }
  test("identical bytes under changed options are stale") {
    val a = spec().source("a", header, rows)
    val b = spec(SourceTimeUnit.Seconds).source("a", header, rows)
    assertEquals(
      SourceComparison.of(true, a, b),
      SourceComparison.ChangedIdentity(IdentityChanges.of(IdentityChange.Options))
    )
    assertEquals(SourceComparison.of(true, a, a), SourceComparison.SameBytes)
    assertEquals(SourceComparison.of(false, a, a), SourceComparison.SameIdentity)
  }
  test("blank and overlapping columns are rejected by public constructors") {
    assert(
      SourceFixationColumns
        .of("n", "x", "x", "t", "d", SampleCountRule.PositiveColumn("s"))
        .isLeft
    )
    assert(
      ImportSpec
        .of(
          SourceKeyColumns.Study("x", "item", "phase"),
          columns,
          frame,
          SourceTimeUnit.Seconds,
          AdmissionPolicy.default[StudyKey],
          AdmissionDecision.RequireComplete
        )
        .isLeft
    )
  }
  test("key columns and correction policy have the same key type") {
    assert(typeCheckErrors("""
      import eyes4s.plan.*
      import eyes4s.kernel.*
      val c = SourceFixationColumns.of("n", "x", "y", "t", "d", SampleCountRule.PositiveColumn("s")).toOption.get
      val f = Frame.screen("f", 100, 100).toOption.get
      ImportSpec.of(SourceKeyColumns.Study("p", "i", "phase"), c, f,
        SourceTimeUnit.Seconds, AdmissionPolicy.default[TrialKey], AdmissionDecision.RequireComplete)
    """).nonEmpty)
  }

  test("construction failures name their operands") {
    assert(get(SourceIdentity.parse("bad").swap).message.contains("bad"))
    assert(
      get(
        ImportSpec
          .of(
            SourceKeyColumns.Study("p", "i", "phase"),
            columns,
            frame.withId(FrameId("")),
            SourceTimeUnit.Seconds,
            AdmissionPolicy.default[StudyKey],
            AdmissionDecision.RequireComplete
          )
          .swap
      ).message.contains("frame")
    )
    assert(
      get(
        ImportSpec
          .of(
            SourceKeyColumns.Trial("p", "phase", "trial", None, None),
            columns,
            frame,
            SourceTimeUnit.Seconds,
            AdmissionPolicy.default[TrialKey],
            AdmissionDecision.RequireComplete
          )
          .swap
      ).message.contains("item")
    )
  }

  test("declared interpretations check supported parsers and options schemas together") {
    val options = spec().digest
    assertEquals(
      SourceInterpretation.declared(
        SourceFormat.FixationCsv,
        SourceImportDefinitions.inventoryParser,
        SourceOptionsSchema.FixationCsvV1,
        options
      ),
      Left(
        SourceIdentityError.Parser(
          SourceFormat.FixationCsv,
          SourceImportDefinitions.fixationParser,
          SourceImportDefinitions.inventoryParser
        )
      )
    )
    assertEquals(
      SourceInterpretation.declared(
        SourceFormat.FixationCsv,
        SourceImportDefinitions.fixationParser,
        SourceOptionsSchema.TrialInventoryCsvV1,
        options
      ),
      Left(
        SourceIdentityError.OptionsSchema(
          SourceFormat.FixationCsv,
          SourceOptionsSchema.FixationCsvV1,
          SourceOptionsSchema.TrialInventoryCsvV1
        )
      )
    )
    val declared = SourceInterpretation.fixation(spec())
    val read     = get(
      SourceInterpretation.declared(
        declared.format,
        declared.parser,
        declared.optionsSchema,
        declared.options
      )
    )
    assertEquals(read, declared)
    assertEquals(read.hashCode, declared.hashCode)
    assert(read.toString.contains("FixationCsvV1"))
    assert(!declared.equals("not an interpretation"))
    val unknown = get(DefinitionId.of("custom.unsupported-parser", 1))
    assert(
      SourceInterpretation
        .declared(declared.format, unknown, declared.optionsSchema, declared.options)
        .isLeft
    )
  }

  test("declared interpretation cannot bypass checking with a constructor or copy") {
    val construction = typeCheckErrors("""
      import eyes4s.plan.*
      import eyes4s.kernel.ContentHash
      new SourceInterpretation.Declared(SourceFormat.FixationCsv,
        SourceImportDefinitions.inventoryParser, ContentHash.ofString("options"))
    """)
    assert(construction.exists(_.message.contains("constructor")), construction.toString)
    val copying = typeCheckErrors("""
      import eyes4s.plan.*
      def bypass(value: SourceInterpretation.Declared) =
        value.copy(parser = SourceImportDefinitions.inventoryParser)
    """)
    assert(copying.exists(_.message.contains("copy")), copying.toString)
  }

  test("legacy and checked interpretations retain their explicit diagnostic structure") {
    val project = summon[DiagnosticOperand[SourceInterpretation, Nothing]]
    assertEquals(
      project(SourceInterpretation.LegacyUnspecified),
      Operand.Token("LegacyUnspecified")
    )
    val declared = SourceInterpretation.fixation(spec())
    assertEquals(
      project(declared),
      Operand.Fields(
        Vector(
          "kind"          -> Operand.Token("Declared"),
          "format"        -> Operand.Token("FixationCsv"),
          "parser"        -> Operand.Definition(SourceImportDefinitions.fixationParser),
          "optionsSchema" -> Operand.Token("FixationCsvV1"),
          "options"       -> Operand.Artifact(spec().digest.render)
        )
      )
    )
  }
