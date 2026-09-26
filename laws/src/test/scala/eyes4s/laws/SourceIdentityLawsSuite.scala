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

package eyes4s.laws

import eyes4s.codec.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import org.scalacheck.Gen

class SourceIdentityLawsSuite extends munit.DisciplineSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
  private val declared                          = Gen.alphaNumStr.map(text =>
    SourceRef(
      "file.csv",
      ArtifactRef.of(SourceRef.digest(Vector("x"), Vector(Vector(text)))),
      SourceInterpretation.Declared(
        SourceFormat.FixationCsv,
        SourceImportDefinitions.fixationParser,
        ContentHash.ofString("options")
      )
    )
  )
  private val changes = for
    before <- declared
    chosen <- Gen
      .someOf(
        IdentityChange.Format,
        IdentityChange.Parser,
        IdentityChange.Options,
        IdentityChange.Records
      )
      .suchThat(_.nonEmpty)
  yield
    val set    = chosen.toSet
    val format = if set(IdentityChange.Format) then SourceFormat.TrialInventoryCsv
    else SourceFormat.FixationCsv
    val parser = if set(IdentityChange.Parser) then
      get(DefinitionId.of(SourceImportDefinitions.fixationParser.name, 2))
    else SourceImportDefinitions.fixationParser
    val options =
      ContentHash.ofString(if set(IdentityChange.Options) then "changed" else "options")
    val records = if set(IdentityChange.Records) then
      ArtifactRef.of[Vector[Vector[String]]](SourceRef.digest(Vector("y"), Vector.empty))
    else before.records
    (
      before,
      SourceRef(
        "different-label",
        records,
        SourceInterpretation.Declared(format, parser, options)
      ),
      set
    )

  checkAll("source identity", SourceIdentityLaws.identity(declared, changes))
  checkAll(
    "source reference",
    CodecLaws.roundTrip(
      SourceIdentityCodec.source,
      declared,
      (a: SourceRef, b: SourceRef) => a == b
    )
  )

  private val frame   = get(Frame.screen("source-frame", 100, 100))
  private val columns = get(
    SourceFixationColumns.of(
      "n",
      "x",
      "y",
      "onset",
      "duration",
      SampleCountRule.PositiveColumn("samples")
    )
  )
  private val specs = Gen
    .oneOf(SourceTimeUnit.values.toSeq)
    .map(unit =>
      get(
        ImportSpec.of(
          SourceKeyColumns.Study("participant", "item", "phase"),
          columns,
          frame,
          unit,
          AdmissionPolicy.default[StudyKey],
          AdmissionDecision.ReviewExclusions
        )
      )
    )
  checkAll(
    "import spec",
    CodecLaws.roundTrip(
      ImportSpecCodec.study[Px],
      specs,
      (a: ImportSpec[StudyKey, Px], b: ImportSpec[StudyKey, Px]) => a.digest == b.digest
    )
  )
  private val inventories = Gen.alphaNumStr
    .suchThat(_.nonEmpty)
    .map(name =>
      get(
        InventoryImportSpec.of(
          "participant",
          "phase",
          "trial",
          attributes = Vector(AttributeColumn("attr-" + name, AttributeKind.Text))
        )
      )
    )
  checkAll(
    "inventory import spec",
    CodecLaws.roundTrip(
      ImportSpecCodec.inventory,
      inventories,
      (a: InventoryImportSpec, b: InventoryImportSpec) => a == b
    )
  )
  private val ledgers = specs.map(spec =>
    get(
      AdmissionLedger.of(
        spec.source("empty.csv", Vector("x"), Vector.empty),
        Vector("x"),
        Vector.empty,
        AdmissionOutcome.Complete,
        spec.policy,
        Vector.empty
      )
    )
  )
  checkAll(
    "source ledger",
    CodecLaws.roundTrip(
      StudyInputCodecs.study[Px].ledger,
      ledgers,
      (a: AdmissionLedger[StudyKey], b: AdmissionLedger[StudyKey]) => a == b
    )
  )

  private val ordinary = get(
    ImportSpec.of(
      SourceKeyColumns.Study("participant", "item", "phase"),
      columns,
      frame,
      SourceTimeUnit.Milliseconds,
      AdmissionPolicy.default[StudyKey],
      AdmissionDecision.ReviewExclusions
    )
  )
  private def variant(
      keys: SourceKeyColumns[StudyKey] = ordinary.keys,
      c: SourceFixationColumns = columns,
      f: Frame[Px] = frame,
      time: SourceTimeUnit = ordinary.timeUnit,
      policy: AdmissionPolicy[StudyKey] = ordinary.policy,
      decision: AdmissionDecision = ordinary.decision
  ) = get(ImportSpec.of(keys, c, f, time, policy, decision))
  private def cols(
      n: String = "n",
      x: String = "x",
      y: String = "y",
      t: String = "onset",
      d: String = "duration",
      samples: SampleCountRule = columns.samples,
      attrs: Vector[AttributeColumn] = Vector.empty
  ) =
    get(SourceFixationColumns.of(n, x, y, t, d, samples, attrs))
  private val optionVariants = Vector(
    variant(keys = SourceKeyColumns.Study("participant2", "item", "phase")),
    variant(keys = SourceKeyColumns.Study("participant", "item2", "phase")),
    variant(keys = SourceKeyColumns.Study("participant", "item", "phase2")),
    variant(c = cols(n = "n2")),
    variant(c = cols(x = "x2")),
    variant(c = cols(y = "y2")),
    variant(c = cols(t = "time2")),
    variant(c = cols(d = "duration2")),
    variant(c = cols(samples = SampleCountRule.PositiveColumn("samples2"))),
    variant(c = cols(samples = SampleCountRule.DerivedFromDuration(get(Hz(100))))),
    variant(c = cols(attrs = Vector(AttributeColumn("response", AttributeKind.Text)))),
    variant(f = frame.withId(FrameId("frame2"))),
    variant(f = Frame.of(frame.id, get(Bounds.of[Px](1, 2, 101, 102)), frame.yAxis)),
    variant(f = Frame.of(frame.id, frame.bounds, YAxis.Up)),
    variant(time = SourceTimeUnit.values.find(_ != ordinary.timeUnit).get),
    variant(policy = AdmissionPolicy.version1[StudyKey]),
    variant(policy =
      AdmissionPolicy(
        OffScreenPolicy.ExcludeRecord,
        Vector(AppliedCorrection(CorrectionScope.AllTrials[StudyKey](), Correction.FlipX))
      )
    ),
    variant(policy =
      AdmissionPolicy(
        OffScreenPolicy.ExcludeRecord,
        Vector(AppliedCorrection(CorrectionScope.Participant[StudyKey]("p"), Correction.FlipY))
      )
    ),
    variant(policy =
      AdmissionPolicy(
        OffScreenPolicy.ExcludeRecord,
        Vector(
          AppliedCorrection(
            CorrectionScope.Trial(StudyKey("p", "i", "phase")),
            get(Correction.translate(1, 2))
          )
        )
      )
    ),
    variant(decision = AdmissionDecision.RequireComplete)
  )
  checkAll(
    "admission options",
    SourceIdentityLaws.options(
      Gen
        .oneOf(optionVariants)
        .map(v =>
          (
            ordinary.source("a", Vector("header"), Vector(Vector("row"))),
            v.source("b", Vector("header"), Vector(Vector("row")))
          )
        )
    )
  )

  test("every option witness changes identity with the records held fixed") {
    val original = ordinary.source("a", Vector("header"), Vector(Vector("row")))
    optionVariants.zipWithIndex.foreach { case (spec, index) =>
      val changed = spec.source("b", Vector("header"), Vector(Vector("row")))
      assertNotEquals(original.identity, changed.identity, s"option witness $index")
      assertEquals(
        SourceComparison.of(true, original, changed),
        SourceComparison.ChangedIdentity(IdentityChanges.of(IdentityChange.Options))
      )
    }
  }
