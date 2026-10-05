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

import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px

/** Why a focal trial has no matched reference (owner decision on bead S0.7b,
  * S7.1 review): judged against the trial inventory's declared design, so a
  * recognition lure or novel probe is unmatched by design, while an old probe
  * whose study trial admission dropped is not.
  */
class UnmatchedReasonsSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(error => fail(s"$error"), identity)
  private val frame                         = get(Frame.screen("unmatched", 4, 4))
  private val grid                          = get(Grid.over(frame, 2, 2))
  private val source = SourceRef.of("trials.csv", Vector("h"), Vector.empty)

  private def trialId(phase: String, trial: String, occurrence: Int) =
    get(TrialIdentity.of("p1", phase, trial, get(TrialOccurrence.of(occurrence))))

  /** One inventory trial: its record numbers say where it was read. */
  private final case class Declared(
      phase: String,
      trial: String,
      item: String,
      disposition: TrialDisposition,
      records: Vector[Int],
      occurrence: Int = 1
  ):
    val id: TrialIdentity = trialId(phase, trial, occurrence)
    def key: TrialKey     = get(id.withItem(item))

  private def inventory(trials: Vector[Declared]): InventoryLedger =
    val built = trials.zipWithIndex.map { (t, i) =>
      get(
        InventoryTrial.of(
          t.id,
          Vector(i + 2),
          Some(t.item),
          Attributes.empty,
          Vector.empty,
          t.records,
          t.disposition
        )
      )
    }
    get(
      InventoryLedger.of(
        source,
        Vector("h"),
        Vector.empty,
        built,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        SampleCountRule.PositiveColumn("n")
      )
    )

  private def path(k: TrialKey): Scanpath[Px] =
    val clock = ClockId(s"${k.phase}/${k.trial}/${k.occurrence.value}")
    get(
      Scanpath.of(
        frame,
        clock,
        IArray(
          get(
            Event.Fixation.withoutDispersion(
              get(Interval.of(clock, Instant.micros(0), Instant.micros(100))),
              Pt[Px](0.5, 1.5),
              1
            )
          )
        )
      )
    )

  private def plan(input: StudyInput[TrialKey, Px], pairing: StudyPairing) = get(
    StudyPlan.configure(
      input.reference,
      TrialKey.layout(TrialKeyDefinitions.trialLayout),
      StudyGeometry.WholeFrame(grid),
      "recognition",
      "study",
      Weight.Duration,
      Vector(StudyScale.Native(StudyEstimate.Binned())),
      None,
      FailurePolicy.RequireAll,
      StudyMethod.cosine[Px](DefinitionId.cosine),
      (),
      pairing
    )
  )

  // Studied: beach (admitted), dog (absent), cat (no fixations), tree (admitted twice).
  private val beach = Declared("study", "s1", "beach", TrialDisposition.Admitted, Vector(2))
  private val dog   = Declared("study", "s2", "dog", TrialDisposition.Absent, Vector.empty)
  private val cat   = Declared("study", "s3", "cat", TrialDisposition.NoFixations, Vector(3))
  private val ball  = Declared("study", "s4", "ball", TrialDisposition.Admitted, Vector(4))
  // Probes: old beach, old dog, old cat, a lure resembling beach, a novel scene.
  private val oldBeach =
    Declared("recognition", "r1", "beach", TrialDisposition.Admitted, Vector(5))
  private val oldDog =
    Declared("recognition", "r2", "dog", TrialDisposition.Admitted, Vector(6))
  private val oldCat =
    Declared("recognition", "r3", "cat", TrialDisposition.Admitted, Vector(7))
  private val lure =
    Declared("recognition", "r4", "beach-lure", TrialDisposition.Admitted, Vector(8))
  private val novel =
    Declared("recognition", "r5", "market", TrialDisposition.Admitted, Vector(9))
  private val declared = Vector(beach, dog, cat, ball, oldBeach, oldDog, oldCat, lure, novel)
  private val ledger   = inventory(declared)
  private val input    = StudyInput(
    Trials(
      declared
        .filter(_.disposition == TrialDisposition.Admitted)
        .map(d => Trial(d.key, (), path(d.key)))
    )
  )

  test("lures and novel probes have no reference by design; dropped study trials are named") {
    val work    = get(plan(input, StudyPairing.default).prepare(input))
    val reasons = get(work.unmatchedReasons(ledger))
    assertEquals(
      reasons.reasons.toMap,
      Map(
        oldDog.key -> UnmatchedKind.ReferenceNotAdmitted(
          Vector(DeclaredReference(dog.id, TrialDisposition.Absent))
        ),
        oldCat.key -> UnmatchedKind.ReferenceNotAdmitted(
          Vector(DeclaredReference(cat.id, TrialDisposition.NoFixations))
        ),
        lure.key  -> UnmatchedKind.NoReferenceInDesign,
        novel.key -> UnmatchedKind.NoReferenceInDesign
      )
    )
    assertEquals((reasons.byDesign, reasons.notAdmitted, reasons.notPairable), (2, 2, 0))
    // The old beach probe is matched, so it has no reason at all.
    assertEquals(reasons.reason(oldBeach.key), None)
  }

  test(
    "preflight carries the reasons when given the inventory, and says undetermined without it"
  ) {
    val p                                            = plan(input, StudyPairing.default)
    def unmatched(report: StudyReport[TrialKey, Px]) =
      report.findings.collect { case StudyFinding.UnmatchedFocal(k, r) => k -> r }.toMap
    val judged = unmatched(p.preflight(Some(input), inventory = Some(ledger)))
    assertEquals(judged(lure.key), UnmatchedKind.NoReferenceInDesign)
    assertEquals(judged(oldDog.key).code, "reference-not-admitted")
    assert(unmatched(p.preflight(Some(input))).values.forall(_ == UnmatchedKind.Undetermined))
    // The diagnostic names the reason by its code, and the trials the inventory declares.
    val finding = StudyFinding.UnmatchedFocal[TrialKey, Px](oldDog.key, judged(oldDog.key))
    val d       = Diagnostic.of(finding)
    assertEquals(d.code.render, "study-finding.unmatched-focal")
    assert(d.message.contains("p1/study/s2#1 (absent)"), d.message)
    assert(d.operand("reason").exists(_.toString.contains("ReferenceNotAdmitted")), d.operands)
  }

  test("an admitted reference the pairing cannot use is not pairable, not by design") {
    // ball's study trial is admitted twice under one key: a repeated key pairs with nothing.
    val oldBall = Declared("recognition", "r6", "ball", TrialDisposition.Admitted, Vector(10))
    val ledger2 = inventory(declared :+ oldBall)
    val rows    = (declared :+ oldBall)
      .filter(_.disposition == TrialDisposition.Admitted)
      .map(d => Trial(d.key, (), path(d.key)))
    val input2  = StudyInput(Trials(rows :+ Trial(ball.key, (), path(ball.key))))
    val work    = get(plan(input2, StudyPairing.default).prepare(input2))
    val reasons = get(work.unmatchedReasons(ledger2))
    assertEquals(
      reasons.reason(oldBall.key),
      Some(UnmatchedKind.ReferenceNotPairable(Vector(ball.id)))
    )
    assertEquals(reasons.notPairable, 1)
  }

  test("under SameOccurrence the occurrence is part of the key the inventory is searched by") {
    // A second presentation of the beach probe: studied only once.
    val again = Declared("recognition", "r7", "beach", TrialDisposition.Admitted, Vector(11), 2)
    val ledger3 = inventory(declared :+ again)
    val input3  = StudyInput(Trials(input.trials.rows :+ Trial(again.key, (), path(again.key))))
    val same    = StudyPairing(
      MatchedReferences.SameOccurrence,
      ControlReferences.SameSelection,
      UnmatchedFocalPolicy.ReportNoMatch
    )
    val reasons = get(get(plan(input3, same).prepare(input3)).unmatchedReasons(ledger3))
    assertEquals(reasons.reason(again.key), Some(UnmatchedKind.NoReferenceInDesign))
    // Matched on item alone, the first presentation's reference is found: not unmatched.
    val select = StudyPairing(
      MatchedReferences.Select(OccurrenceChoice.First),
      ControlReferences.SameSelection,
      UnmatchedFocalPolicy.ReportNoMatch
    )
    val selected = get(get(plan(input3, select).prepare(input3)).unmatchedReasons(ledger3))
    assertEquals(selected.reason(again.key), None)
  }

  test("every unmatched focal trial has exactly one reason, and the counts partition them") {
    val work    = get(plan(input, StudyPairing.default).prepare(input))
    val reasons = get(work.unmatchedReasons(ledger))
    val keys    = get(work.matchedCardinality).unmatched
    assertEquals(reasons.reasons.map(_._1), keys)
    assertEquals(reasons.byDesign + reasons.notAdmitted + reasons.notPairable, keys.size)
  }

  test("every kind projects under the catalog's conventions, a quarantine cause as its cause") {
    val alignment = DiagnosticAlignment(DiagnosticSamples.all)
    val kinds     = Vector(
      UnmatchedKind.Undetermined,
      UnmatchedKind.NoReferenceInDesign,
      UnmatchedKind.ReferenceNotAdmitted(
        Vector(
          DeclaredReference(dog.id, TrialDisposition.Absent),
          DeclaredReference(
            cat.id,
            TrialDisposition.Quarantined(QuarantineCause.Overlap(1, "[0,10)", "[5,15)"))
          )
        )
      ),
      UnmatchedKind.ReferenceNotPairable(Vector(ball.id))
    )
    kinds.foreach { kind =>
      // The alignment knows StudyKey keys; the kind's projection is the same for any key.
      val finding =
        StudyFinding.UnmatchedFocal[StudyKey, Px](StudyKey("p1", "a", "recall"), kind)
      assertEquals(
        alignment.aligned(finding, Diagnostic.of(finding).asInstanceOf[Diagnostic[Any]]),
        Vector.empty,
        kind.toString
      )
    }
    assertEquals(
      kinds.map(_.code),
      Vector(
        "undetermined",
        "no-reference-in-design",
        "reference-not-admitted",
        "reference-not-pairable"
      )
    )
  }
