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
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

class FixationTrialDecisionSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
  private val frame                             = get(Frame.screen("frame", 100, 100))
  private val key                               = StudyKey("p", "i", "f")
  private val header                            = "p,item,phase,n,x,y,onset,duration,samples\n"
  private def spec(conflicting: Boolean)        = get(
    ImportSpec.of(
      SourceKeyColumns.Study("p", "item", "phase"),
      get(
        SourceFixationColumns
          .of("n", "x", "y", "onset", "duration", SampleCountRule.PositiveColumn("samples"))
      ),
      frame,
      SourceTimeUnit.Milliseconds,
      AdmissionPolicy(
        OffScreenPolicy.ExcludeRecord,
        if conflicting then
          Vector(
            AppliedCorrection[StudyKey](CorrectionScope.AllTrials(), Correction.FlipX),
            AppliedCorrection[StudyKey](CorrectionScope.AllTrials(), Correction.FlipY)
          )
        else Vector.empty
      ),
      AdmissionDecision.ReviewExclusions
    )
  )

  test(
    "synchronous admission keeps rejection before correction conflict before duplicate ordinal"
  ) {
    for
      rejected  <- Vector(false, true)
      conflict  <- Vector(false, true)
      duplicate <- Vector(false, true)
    do
      val ordinal = if duplicate then 0 else 1
      val csv     = header + s"p,i,f,0,10,20,0,100,2\np,i,f,$ordinal,10,20,100,100,2\n" +
        (if rejected then "p,i,f,2,bad,20,200,100,2\n" else "")
      val actual = get(SourceAdmission.read("source", csv, spec(conflict))).ledger
      val cause  = if rejected then Some(QuarantineCause.RejectedRecords)
      else if conflict then Some(QuarantineCause.CorrectionConflict(0, 1))
      else if duplicate then Some(QuarantineCause.DuplicateOrdinals)
      else None
      cause match
        case None           => assert(actual.records.forall(_.isAdmitted))
        case Some(expected) =>
          val affected = if rejected then Vector(2, 3, 4) else Vector(2, 3)
          assertEquals(
            actual.records.head.disposition,
            Disposition.Rejected(
              Vector("p", "i", "f", "0", "10", "20", "0", "100", "2"),
              Some(key),
              AdmissionReason.Quarantined(affected, expected)
            )
          )
  }

  test("inventory override retains its own rows and renamed key ahead of all other defects") {
    val clock    = ClockId("clock")
    val fixation = get(
      Event.Fixation.withoutDispersion(
        get(Interval.of(clock, Instant.micros(0), Instant.micros(100))),
        Pt[Px](10, 20),
        1
      )
    )
    val raw   = Vector("row")
    val valid = Vector(
      Parsed(2, raw, key, 0, fixation, None, Some(0 -> 1), Attributes.empty),
      Parsed(3, raw, key, 0, fixation, None, None, Attributes.empty)
    )
    val invalid = Vector(RejectedFixationRow(4, raw, Some(key), FixationRowError.Key("bad")))
    val renamed = StudyKey("p", "inventory-item", "f")
    val rows    = Vector(2, 3, 4, 5)
    val cause   = QuarantineCause.InventoryItemConflict("inventory-item", Vector("i"))
    val (result, attributes) = FixationCsv.assemble(
      Vector("field"),
      Vector.fill(3)(raw),
      valid,
      invalid,
      frame,
      (_: StudyKey) => clock,
      Map(key -> (renamed, rows, cause)),
      AdmissionPolicy.default[StudyKey]
    )
    assert(result.accepted.rows.isEmpty)
    assert(attributes.isEmpty)
    assertEquals(result.rejected.map(_.key), Vector.fill(3)(Some(renamed)))
    assertEquals(
      result.rejected.take(2).map(_.error),
      Vector.fill(2)(FixationRowError.Trial(rows, cause))
    )
    assertEquals(result.rejected.last.error, FixationRowError.Key("bad"))
  }

  test("duplicate work is not evaluated after a higher-priority refusal") {
    var calls              = 0
    def duplicate: Boolean = { calls += 1; true }
    val rows               = Vector(2, 3)
    val overrideCause      = Some(rows -> QuarantineCause.ItemConflict(Vector("a", "b")))
    assert(
      FixationCsv.trialRefusal(rows, overrideCause, true, Some(0 -> 1), duplicate).nonEmpty
    )
    assert(FixationCsv.trialRefusal(rows, None, true, Some(0 -> 1), duplicate).nonEmpty)
    assert(FixationCsv.trialRefusal(rows, None, false, Some(0 -> 1), duplicate).nonEmpty)
    assertEquals(calls, 0)
    assertEquals(
      FixationCsv.trialRefusal(rows, None, false, None, duplicate),
      Some(FixationRowError.Trial(rows, QuarantineCause.DuplicateOrdinals))
    )
    assertEquals(calls, 1)
  }

  test("an otherwise acceptable group still reaches the scanpath overlap check") {
    val csv    = header + "p,i,f,0,10,20,50,100,2\np,i,f,1,10,20,0,100,2\n"
    val actual = get(SourceAdmission.read("source", csv, spec(false))).ledger
    assert(actual.records.head.disposition match
      case Disposition.Rejected(
            _,
            _,
            AdmissionReason.Quarantined(Vector(2, 3), QuarantineCause.Overlap(1, _, _))
          ) =>
        true
      case _ => false)
  }
