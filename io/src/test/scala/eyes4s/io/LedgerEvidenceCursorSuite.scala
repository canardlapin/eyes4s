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

class LedgerEvidenceCursorSuite extends munit.FunSuite:
  import LedgerExecutionLimitFixtures.*
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
  private val frame                             = get(Frame.screen("frame", 100, 100))
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
  private def spec(using KeyDigest[StudyKey]): ImportSpec[StudyKey, Px] = get(
    ImportSpec.of(
      SourceKeyColumns.Study("p", "item", "phase"),
      columns,
      frame,
      SourceTimeUnit.Milliseconds,
      AdmissionPolicy.default[StudyKey],
      AdmissionDecision.ReviewExclusions
    )
  )
  private val source =
    SourceRef("source", ArtifactRef.of[Vector[Vector[String]]](ContentHash.ofString("source")))
  private def rejected(
      reason: AdmissionReason,
      raw: Vector[String] = Vector("raw"),
      key: Option[StudyKey] = None
  ) =
    SourceRecord(2, Disposition.Rejected(raw, key, reason))
  private def ledger(
      rows: Vector[SourceRecord[StudyKey]],
      header: Vector[String] = Vector("h"),
      policy: AdmissionPolicy[StudyKey] = AdmissionPolicy.default[StudyKey],
      ref: SourceRef = source
  ): AdmissionLedger[StudyKey] = get(
    AdmissionLedger.of(
      ref,
      header,
      rows,
      if rows.forall(_.isAdmitted) then AdmissionOutcome.Complete
      else AdmissionOutcome.ReviewedExclusions,
      policy,
      Vector.empty
    )
  )

  private def drain[K](
      description: ImportSpec[K, Px],
      value: AdmissionLedger[K],
      envelope: LedgerExecutionLimits,
      budget: Int
  ): Either[LedgerResourceError, (Long, Long)] =
    val initial = get(LedgerEvidenceCursor.start(description, value, envelope))
    val quantum = get(SampleQuantum.of(budget))
    @annotation.tailrec
    def loop(
        cursor: LedgerEvidenceCursor[K],
        total: Long
    ): Either[LedgerResourceError, (Long, Long)] =
      cursor.advance(quantum) match
        case Left(error) => Left(error)
        case Right(step) =>
          assert(step.workUnits > 0 && step.workUnits <= budget)
          step.next match
            case Some(next) => loop(next, total + step.workUnits)
            case None       => Right(step.retainedUnits -> (total + step.workUnits))
    loop(initial, 0)

  test("large rejected evidence is resumable, immutable and quantum independent") {
    val rows = Vector.tabulate(4000)(i =>
      SourceRecord(
        i + 2,
        Disposition.Rejected(
          Vector("bad", "value"),
          Option.empty[StudyKey],
          AdmissionReason.Key("invalid")
        )
      )
    )
    val value    = ledger(rows)
    val expected = get(drain(spec, value, limits(), 1))
    assert(expected._2 > rows.size * 5L)
    Vector(2, 17, 1024).foreach(b =>
      assertEquals(drain(spec, value, limits(), b), Right(expected))
    )
    val initial = get(LedgerEvidenceCursor.start(spec, value, limits()))
    val q       = get(SampleQuantum.of(1))
    assertEquals(initial.advance(q), initial.advance(q))
    assertEquals(initial.retained, 0L)
    val retained = expected._1
    assert(
      drain(spec, value, limits(LedgerResource.RetainedEvidenceUnits -> retained), 7).isRight
    )
    assert(
      drain(
        spec,
        value,
        limits(LedgerResource.RetainedEvidenceUnits -> (retained - 1)),
        7
      ).isLeft
    )
  }

  test("header width, logical record count and correction count have exact boundaries") {
    val rules =
      Vector.fill(3)(AppliedCorrection[StudyKey](CorrectionScope.AllTrials(), Correction.FlipX))
    val value = ledger(
      Vector(rejected(AdmissionReason.Key("bad"))),
      Vector("a", "b"),
      AdmissionPolicy(OffScreenPolicy.ExcludeRecord, rules)
    )
    Vector(
      LedgerResource.Columns         -> 2L,
      LedgerResource.LogicalRecords  -> 2L,
      LedgerResource.CorrectionRules -> 3L
    ).foreach { (dimension, size) =>
      Vector(1, 7).foreach { budget =>
        assert(drain(spec, value, limits(dimension -> size), budget).isRight)
        assert(drain(spec, value, limits(dimension -> (size + 1)), budget).isRight)
        assert(drain(spec, value, limits(dimension -> (size - 1)), budget).left.exists {
          case LedgerResourceError.Exceeded(`dimension`, limit, observed, _) =>
            limit == size - 1 && observed == BigInt(size)
          case _ => false
        })
      }
    }
  }

  test("source labels and rejected raw fields are refused before rendering") {
    val huge     = "q" * 20000
    val value    = ledger(Vector(rejected(AdmissionReason.Key("bad"), Vector("ok", huge))))
    val expected = Left(
      LedgerResourceError.Exceeded(
        LedgerResource.FieldCodeUnits,
        100,
        BigInt(20000),
        LedgerResourceLocation(LedgerResourceSource.ExpectedLedger, Some(2), Some(2))
      )
    )
    Vector(1, 3, 1024).foreach(b =>
      assertEquals(
        drain(spec, value, limits(LedgerResource.FieldCodeUnits -> 100L), b),
        expected
      )
    )
    val labelled = ledger(Vector.empty, ref = source.copy(label = huge))
    val error    =
      drain(spec, labelled, limits(LedgerResource.FieldCodeUnits -> 100L), 1).swap.toOption.get
    assert(error.message.length < 250)
    assert(!error.message.contains(huge))
  }

  test("every variable-size rejection and quarantine operand is preflighted") {
    val huge   = "q" * 20000
    val causes = Vector(
      QuarantineCause.Overlap(0, huge, "end"),
      QuarantineCause.Overlap(0, "start", huge),
      QuarantineCause.WrongClock(0, huge, "clock"),
      QuarantineCause.WrongClock(0, "clock", huge),
      QuarantineCause.InvalidTransition(0, huge),
      QuarantineCause.InvalidExtent(huge),
      QuarantineCause.UnmappableFixation(0, FrameId(huge), FrameId("to"), 0, 0),
      QuarantineCause.UnmappableFixation(0, FrameId("from"), FrameId(huge), 0, 0),
      QuarantineCause.ItemConflict(Vector("a", huge))
    )
    val reasons = Vector(
      AdmissionReason.Key(huge),
      AdmissionReason.Event(huge),
      AdmissionReason.Number(huge, "1", "number"),
      AdmissionReason.Number("x", huge, "number"),
      AdmissionReason.Number("x", "1", huge),
      AdmissionReason.Time(huge, "1", "ms", "time"),
      AdmissionReason.Time("0", huge, "ms", "time"),
      AdmissionReason.Time("0", "1", huge, "time"),
      AdmissionReason.Time("0", "1", "ms", huge),
      AdmissionReason.Position(0, 0, FrameId(huge))
    ) ++
      causes.map(c => AdmissionReason.Quarantined(Vector(2), c))
    reasons.foreach { reason =>
      val value = ledger(Vector(rejected(reason)))
      Vector(1, 17).foreach(b =>
        assert(drain(spec, value, limits(LedgerResource.FieldCodeUnits -> 100L), b).isLeft)
      )
    }
  }

  test("admitted keys and every correction-scope key operand are bounded") {
    for index <- 0 until 3 do
      val fields   = Vector("p", "i", "f").updated(index, "long-name")
      val key      = StudyKey(fields(0), fields(1), fields(2))
      val admitted = ledger(Vector(SourceRecord(2, Disposition.Admitted(key, 0))))
      assert(drain(spec, admitted, limits(LedgerResource.FieldCodeUnits -> 8L), 1).isLeft)
      val corrected = ledger(
        Vector.empty,
        policy = AdmissionPolicy(
          OffScreenPolicy.ExcludeRecord,
          Vector(AppliedCorrection(CorrectionScope.Trial(key), Correction.FlipX))
        )
      )
      assert(
        drain(
          spec,
          corrected,
          limits(LedgerResource.CorrectionOperandCodeUnits -> 9L),
          1
        ).isRight
      )
      assert(
        drain(
          spec,
          corrected,
          limits(LedgerResource.CorrectionOperandCodeUnits -> 8L),
          1
        ).isLeft
      )
  }

  test("inventory attribute text is checked even when the primary records are short") {
    val huge          = "r" * 20000
    val inventoryText = s"p,phase,trial,item,response\np,f,t,i,$huge\n"
    val invSpec       = get(
      InventoryImportSpec.of(
        "p",
        "phase",
        "trial",
        item = Some("item"),
        attributes = Vector(AttributeColumn("response", AttributeKind.Text))
      )
    )
    val invSource   = get(SourceAdmission.inventorySource("inventory", inventoryText, invSpec))
    val description = get(
      ImportSpec.of(
        SourceKeyColumns.Trial("p", "phase", "trial", None, None),
        columns,
        frame,
        SourceTimeUnit.Milliseconds,
        AdmissionPolicy.default[TrialKey],
        AdmissionDecision.ReviewExclusions,
        inventory = Some(SourceInventory(invSpec, invSource.identity.get))
      )
    )
    val csv   = "p,phase,trial,n,x,y,onset,duration,samples\np,f,t,0,10,20,0,100,10\n"
    val value = get(
      SourceAdmission.read("fixations", csv, description, Some("inventory" -> inventoryText))
    ).ledger
    val expected = drain(description, value, limits(LedgerResource.FieldCodeUnits -> 20000L), 1)
    assert(expected.isRight)
    Vector(2, 101).foreach(b =>
      assertEquals(
        drain(description, value, limits(LedgerResource.FieldCodeUnits -> 20000L), b),
        expected
      )
    )
    assert(drain(description, value, limits(LedgerResource.FieldCodeUnits -> 19999L), 1).isLeft)
    assert(drain(description, value, limits(LedgerResource.DeclaredAttributes -> 0L), 1).isLeft)
  }

  test("unqualified digest evidence is refused without any callback") {
    var calls  = 0
    val custom = new KeyDigest[StudyKey]:
      def digest(key: StudyKey): ContentHash =
        calls += 1
        throw new AssertionError("digest called")
    val description = spec(using custom)
    val value       = ledger(Vector.empty)
    assertEquals(
      LedgerEvidenceCursor.start(description, value, limits()),
      Left(LedgerExecutionEvidence.Unsupported.KeyDigest)
    )
    assertEquals(calls, 0)
  }
