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

import eyes4s.design.KeyDigest
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.*
import eyes4s.plan.*

class LedgerExecutionEvidenceSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
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
  private val study = SourceKeyColumns.Study("participant", "item", "phase")
  private val trial =
    SourceKeyColumns.Trial("participant", "phase", "trial", Some("item"), None)

  private def spec[K: KeyDigest, U <: Unit2D: UnitLabel](keys: SourceKeyColumns[K]) =
    val frame = Frame.of(FrameId("evidence"), get(Bounds.of[U](0, 0, 100, 100)), YAxis.Down)
    get(
      ImportSpec.of(
        keys,
        columns,
        frame,
        SourceTimeUnit.Milliseconds,
        AdmissionPolicy.default[K],
        AdmissionDecision.ReviewExclusions
      )
    )

  test("both built-in key digests and every canonical unit instance are admitted") {
    assertEquals(LedgerExecutionEvidence.qualify(spec[StudyKey, Px](study)), Right(()))
    assertEquals(LedgerExecutionEvidence.qualify(spec[StudyKey, Deg](study)), Right(()))
    assertEquals(LedgerExecutionEvidence.qualify(spec[StudyKey, Norm](study)), Right(()))
    assertEquals(LedgerExecutionEvidence.qualify(spec[StudyKey, Mm](study)), Right(()))
    assertEquals(LedgerExecutionEvidence.qualify(spec[TrialKey, Px](trial)), Right(()))
  }

  test("custom digest is refused without calling digest or equality") {
    var calls  = 0
    val custom = new KeyDigest[StudyKey]:
      def digest(key: StudyKey): ContentHash =
        calls += 1
        throw new AssertionError("digest must not be called")
      override def equals(other: Any): Boolean =
        calls += 1
        throw new AssertionError("equality must not be called")
    val input = spec[StudyKey, Px](study)(using custom, summon[UnitLabel[Px]])
    assertEquals(
      LedgerExecutionEvidence.qualify(input),
      Left(LedgerExecutionEvidence.Unsupported.KeyDigest)
    )
    assertEquals(calls, 0)
    assert(input.keyDigest eq custom)
  }

  test("custom unit is refused without reading its symbol, name or equality") {
    var calls  = 0
    val custom = new UnitLabel[Px]:
      def symbol: String =
        calls += 1
        throw new AssertionError("symbol must not be read")
      def name: String =
        calls += 1
        throw new AssertionError("name must not be read")
      def planar: PlanarUnit =
        calls += 1
        throw new AssertionError("planar must not be read")
      override def equals(other: Any): Boolean =
        calls += 1
        throw new AssertionError("equality must not be called")
    val input = spec[StudyKey, Px](study)(using summon[KeyDigest[StudyKey]], custom)
    assertEquals(
      LedgerExecutionEvidence.qualify(input),
      Left(LedgerExecutionEvidence.Unsupported.UnitLabel)
    )
    assertEquals(calls, 0)
    assert(input.unit eq custom)
  }

  test("custom key columns remain unsupported even with canonical evidence") {
    val custom = SourceKeyColumns.Custom[StudyKey](
      SourceImportDefinitions.fixationParser,
      SourceImportDefinitions.inventoryParser,
      Vector("participant", "item", "phase")
    )
    assertEquals(
      LedgerExecutionEvidence.qualify(spec[StudyKey, Px](custom)),
      Left(LedgerExecutionEvidence.Unsupported.KeyColumns)
    )
  }
