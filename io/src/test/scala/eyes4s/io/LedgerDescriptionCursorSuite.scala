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

import eyes4s.design.{KeyDigest, SampleQuantum}
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

class LedgerDescriptionCursorSuite extends munit.FunSuite:
  import LedgerExecutionLimitFixtures.*
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
  private val frame                             = get(Frame.screen("frame", 100, 100))
  private def spec(
      rules: Vector[AppliedCorrection[StudyKey]] = Vector.empty,
      attributes: Vector[AttributeColumn] = Vector.empty,
      x: String = "x"
  )(using KeyDigest[StudyKey], UnitLabel[Px]): ImportSpec[StudyKey, Px] =
    val columns = get(
      SourceFixationColumns.of(
        "n",
        x,
        "y",
        "onset",
        "duration",
        SampleCountRule.PositiveColumn("samples"),
        attributes
      )
    )
    get(
      ImportSpec.of(
        SourceKeyColumns.Study("participant", "item", "phase"),
        columns,
        frame,
        SourceTimeUnit.Milliseconds,
        AdmissionPolicy(OffScreenPolicy.ExcludeRecord, rules),
        AdmissionDecision.ReviewExclusions
      )
    )

  private def drain[K](
      description: ImportSpec[K, Px],
      envelope: LedgerExecutionLimits,
      budget: Int
  ): Either[LedgerDescriptionError, (Long, Long)] =
    val quantum = get(SampleQuantum.of(budget))
    LedgerDescriptionCursor.start(description, envelope).flatMap { initial =>
      @annotation.tailrec
      def loop(
          cursor: LedgerDescriptionCursor,
          total: Long
      ): Either[LedgerDescriptionError, (Long, Long)] =
        cursor.advance(quantum) match
          case Left(error) => Left(LedgerDescriptionError.Resource(error))
          case Right(step) =>
            assert(step.workUnits > 0 && step.workUnits <= budget)
            step.next match
              case Some(next) => loop(next, total + step.workUnits)
              case None       => Right((step.retainedUnits, total + step.workUnits))
      loop(initial, 0)
    }

  private def trialSpec(
      rules: Vector[AppliedCorrection[TrialKey]] = Vector.empty,
      inventory: Option[InventoryImportSpec] = None
  ): ImportSpec[TrialKey, Px] =
    get(
      ImportSpec.of(
        SourceKeyColumns.Trial("p", "phase", "trial", Some("item"), Some("occurrence")),
        spec().columns,
        frame,
        SourceTimeUnit.Milliseconds,
        AdmissionPolicy(OffScreenPolicy.ExcludeRecord, rules),
        AdmissionDecision.ReviewExclusions,
        inventory = inventory.map(value =>
          SourceInventory(
            value,
            get(SourceIdentity.parse(ContentHash.ofString("inventory").render))
          )
        )
      )
    )

  test("metadata visitation is immutable and independent of the work quantum") {
    val rules = Vector.tabulate(500)(i =>
      AppliedCorrection[StudyKey](CorrectionScope.Participant(s"p$i"), Correction.FlipX)
    )
    val description =
      spec(rules, Vector.tabulate(30)(i => AttributeColumn(s"a$i", AttributeKind.Text)))
    val expected = drain(description, limits(), 1)
    assert(expected.isRight)
    assert(expected.toOption.get._2 > 1000)
    Vector(2, 17, 1024).foreach(b => assertEquals(drain(description, limits(), b), expected))
    val cursor  = get(LedgerDescriptionCursor.start(description, limits()))
    val quantum = get(SampleQuantum.of(1))
    assertEquals(cursor.advance(quantum), cursor.advance(quantum))
    assertEquals(cursor.retained, 0L)
  }

  test("declaration widths, attribute counts and correction counts have exact boundaries") {
    val attrs = Vector(
      AttributeColumn("a", AttributeKind.Text),
      AttributeColumn("b", AttributeKind.Number)
    )
    val rules =
      Vector.fill(2)(AppliedCorrection[StudyKey](CorrectionScope.AllTrials(), Correction.FlipY))
    val examples = Vector(
      (spec(), LedgerResource.Columns, 9L),
      (spec(attributes = attrs), LedgerResource.DeclaredAttributes, 2L),
      (spec(rules), LedgerResource.CorrectionRules, 2L)
    )
    examples.foreach { (description, dimension, extent) =>
      assert(drain(description, limits(dimension -> extent), 1).isRight)
      assert(drain(description, limits(dimension -> (extent + 1)), 2).isRight)
      assertEquals(
        drain(description, limits(dimension -> (extent - 1)), 1),
        Left(
          LedgerDescriptionError.Resource(
            LedgerResourceError.Exceeded(
              dimension,
              extent - 1,
              BigInt(extent),
              LedgerResourceLocation(LedgerResourceSource.ImportDescription)
            )
          )
        )
      )
    }
  }

  test("names and correction operands are bounded before callback or digest work") {
    val long        = "x" * 10000
    val description = spec(x = long)
    assertEquals(
      drain(description, limits(LedgerResource.FieldCodeUnits -> 100L), 1),
      Left(
        LedgerDescriptionError.Resource(
          LedgerResourceError.Exceeded(
            LedgerResource.FieldCodeUnits,
            100,
            BigInt(10000),
            LedgerResourceLocation(LedgerResourceSource.ImportDescription)
          )
        )
      )
    )
    val rules = Vector(
      AppliedCorrection[StudyKey](CorrectionScope.Participant("owner"), Correction.FlipX)
    )
    assert(
      drain(spec(rules), limits(LedgerResource.CorrectionOperandCodeUnits -> 5L), 1).isRight
    )
    assertEquals(
      drain(spec(rules), limits(LedgerResource.CorrectionOperandCodeUnits -> 4L), 1),
      Left(
        LedgerDescriptionError.Resource(
          LedgerResourceError.Exceeded(
            LedgerResource.CorrectionOperandCodeUnits,
            4,
            BigInt(5),
            LedgerResourceLocation(LedgerResourceSource.ImportDescription)
          )
        )
      )
    )
  }

  test("unqualified evidence is refused at start without calling it") {
    var calls  = 0
    val custom = new KeyDigest[StudyKey]:
      def digest(key: StudyKey): ContentHash =
        calls += 1
        throw new AssertionError("digest called")
    val description = spec()(using custom, summon[UnitLabel[Px]])
    assertEquals(
      LedgerDescriptionCursor.start(description, limits()),
      Left(LedgerDescriptionError.Unsupported(LedgerExecutionEvidence.Unsupported.KeyDigest))
    )
    assertEquals(calls, 0)
  }

  test("every trial-key correction operand is bounded across work quanta") {
    for index <- 0 until 4 do
      val fields = Vector("p", "f", "t", "i").updated(index, "long-name")
      val key    =
        get(TrialKey.of(fields(0), fields(1), fields(2), TrialOccurrence.first, fields(3)))
      val description =
        trialSpec(Vector(AppliedCorrection(CorrectionScope.Trial(key), Correction.FlipX)))
      val admitted =
        drain(description, limits(LedgerResource.CorrectionOperandCodeUnits -> 9L), 1)
      assert(admitted.isRight)
      for budget <- Vector(1, 3, 64) do
        assertEquals(
          drain(description, limits(LedgerResource.CorrectionOperandCodeUnits -> 9L), budget),
          admitted
        )
        assertEquals(
          drain(description, limits(LedgerResource.CorrectionOperandCodeUnits -> 8L), budget),
          Left(
            LedgerDescriptionError.Resource(
              LedgerResourceError.Exceeded(
                LedgerResource.CorrectionOperandCodeUnits,
                8,
                BigInt(9),
                LedgerResourceLocation(LedgerResourceSource.ImportDescription)
              )
            )
          )
        )
  }

  test("inventory columns, attributes and names have independent exact boundaries") {
    val inventory = get(
      InventoryImportSpec.of(
        "ip",
        "if",
        "it",
        Some("io"),
        Some("ii"),
        Vector.tabulate(10)(i => AttributeColumn(s"a$i", AttributeKind.Text))
      )
    )
    val description = trialSpec(inventory = Some(inventory))
    val expected    = drain(description, limits(), 1)
    assert(expected.isRight)
    Vector(2, 31).foreach(budget =>
      assertEquals(drain(description, limits(), budget), expected)
    )
    for (dimension, extent) <- Vector(
        LedgerResource.Columns            -> 15L,
        LedgerResource.DeclaredAttributes -> 10L
      )
    do
      assert(drain(description, limits(dimension -> extent), 1).isRight)
      assertEquals(
        drain(description, limits(dimension -> (extent - 1)), 2),
        Left(
          LedgerDescriptionError.Resource(
            LedgerResourceError.Exceeded(
              dimension,
              extent - 1,
              BigInt(extent),
              LedgerResourceLocation(LedgerResourceSource.ImportDescription)
            )
          )
        )
      )
    val longName = get(InventoryImportSpec.of("p" * 20, "f", "t", item = Some("i")))
    assertEquals(
      drain(
        trialSpec(inventory = Some(longName)),
        limits(LedgerResource.FieldCodeUnits -> 19L),
        1
      ),
      Left(
        LedgerDescriptionError.Resource(
          LedgerResourceError.Exceeded(
            LedgerResource.FieldCodeUnits,
            19,
            BigInt(20),
            LedgerResourceLocation(LedgerResourceSource.ImportDescription)
          )
        )
      )
    )
    val retained = expected.toOption.get._1
    assert(
      drain(description, limits(LedgerResource.RetainedEvidenceUnits -> retained), 1).isRight
    )
    assert(
      drain(
        description,
        limits(LedgerResource.RetainedEvidenceUnits -> (retained - 1)),
        1
      ).isLeft
    )
  }
