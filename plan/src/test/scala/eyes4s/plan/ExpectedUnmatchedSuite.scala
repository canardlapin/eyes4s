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

class ExpectedUnmatchedSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(e => fail(e.toString), identity)
  private val frame                         = get(Frame.screen("expected-unmatched", 4, 4))
  private val grid                          = get(Grid.over(frame, 2, 2))
  private val layout                        = TrialKey.layout(TrialKeyDefinitions.trialLayout)
  private def key(phase: String, trial: String, item: String) =
    get(TrialKey.of("p1", phase, trial, TrialOccurrence.first, item))
  private val studied                         = key("study", "s1", "beach")
  private val other                           = key("study", "s2", "tree")
  private val old                             = key("recognition", "r1", "beach")
  private val lure                            = key("recognition", "r2", "beach")
  private val novel                           = key("recognition", "r3", "tree")
  private val all                             = Vector(studied, other, old, lure, novel)
  private def path(k: TrialKey): Scanpath[Px] =
    val clock = ClockId(k.trial)
    get(
      Scanpath.of(
        frame,
        clock,
        IArray(
          get(
            Event.Fixation.withoutDispersion(
              get(Interval.of(clock, Instant.micros(0), Instant.micros(100))),
              Pt[Px](.5, 1.5),
              1
            )
          )
        )
      )
    )
  private def input(keys: Vector[TrialKey]) = StudyInput(
    Trials(keys.map(k => Trial(k, (), path(k))))
  )
  private def selection = get(
    ExpectedUnmatched.of(
      "old_new",
      Vector("new", "lure"),
      all.map(k =>
        TrialIdentity.of(k) -> (if k == lure then "lure"
                                else if k == novel then "new"
                                else "old")
      )
    )
  )
  private def plan(in: StudyInput[TrialKey, Px], selected: Option[ExpectedUnmatched]) = get(
    StudyPlan.configure(
      in.reference,
      layout,
      StudyGeometry.WholeFrame(grid),
      "recognition",
      "study",
      Weight.Duration,
      Vector(StudyScale.Native(StudyEstimate.Binned())),
      None,
      FailurePolicy.RequireAll,
      StudyMethod.cosine[Px](DefinitionId.cosine),
      (),
      expectedUnmatched = selected
    )
  )

  test(
    "declared lures with colliding items get neither matched nor control pairs and remain counted"
  ) {
    val in     = input(all)
    val p      = plan(in, Some(selection))
    val counts = get(get(p.prepare(in)).counts)
    assertEquals(counts.cardinality.unmatched.toSet, Set(lure, novel))
    assertEquals(counts.queryPairs(old).matchedPairs, 1)
    assertEquals(counts.queryPairs(old).controlPairs, 1)
    Vector(lure, novel).foreach { k =>
      assertEquals(counts.queryPairs(k).matchedPairs, 0)
      assertEquals(counts.queryPairs(k).controlPairs, 0)
    }
    val executed = get(get(p.prepare(in)).run)
    executed.scales.foreach { scale =>
      assertEquals(scale.analyses.control.entries.map(_.key), Vector(old))
      assertEquals(
        scale.analyses.matchedSource.diagnostics.unmatchedLeft.toSet,
        Set(lure, novel)
      )
      val contrasts = get(scale.contrast).rows.map(r => r.key -> r.difference.isRight).toMap
      assertEquals(contrasts, Map(old -> true, lure -> false, novel -> false))
    }
    assertEquals(counts.keysPerDesign, 3L)
    assertEquals(counts.totalContrastRows, 3L)
    assertEquals(
      get(get(plan(in, None).prepare(in)).counts).cardinality.unmatched,
      Vector.empty
    )
    val report = p.preflight(Some(in))
    assert(report.findings.exists {
      case StudyFinding.UnmatchedFocal(`lure`, UnmatchedKind.NoReferenceInDesign) => true
      case _                                                                      => false
    })
  }

  test("dropping a studied item cannot reclassify its colliding lure") {
    val in     = input(all.filterNot(_ == studied))
    val p      = plan(in, Some(selection))
    val counts = get(get(p.prepare(in)).counts)
    assert(counts.cardinality.unmatched.contains(lure))
    val reasons =
      UnmatchedReasons.declared(layout, counts.cardinality.unmatched, p.expectedUnmatched)
    assertEquals(reasons.reason(lure), Some(UnmatchedKind.NoReferenceInDesign))
    assertEquals(reasons.reason(old), Some(UnmatchedKind.Undetermined))
  }

  test(
    "missing and blank focal declarations refuse preparation with column and trial operands"
  ) {
    val in      = input(all)
    val missing = get(
      ExpectedUnmatched.of(
        "old_new",
        Vector("new"),
        selection.assignments.filterNot(_._1 == TrialIdentity.of(lure))
      )
    )
    val blank = get(
      ExpectedUnmatched.of(
        "old_new",
        Vector("new"),
        selection.assignments.map((k, v) =>
          k -> (if k == TrialIdentity.of(lure) then "" else v)
        )
      )
    )
    Vector(missing, blank).foreach { s =>
      val error = plan(in, Some(s)).prepare(in).swap.toOption.get
      assert(error.message.contains("old_new"))
      assert(error.message.contains(lure.trial))
    }
  }

  test("assignment ordering is canonical and duplicate identities are refused") {
    assertEquals(
      get(
        ExpectedUnmatched
          .of(selection.column, selection.values.reverse, selection.assignments.reverse)
      ),
      selection
    )
    assert(
      ExpectedUnmatched
        .of(selection.column, selection.values, selection.assignments ++ selection.assignments)
        .isLeft
    )
    assert(ExpectedUnmatched.of("", Vector("new"), Vector.empty).isLeft)
    assert(ExpectedUnmatched.of("old_new", Vector.empty, Vector.empty).isLeft)
  }

  test("selection participates in scientific identity and reversible typed plan diff") {
    val in     = input(all)
    val before = plan(in, None)
    val after  = plan(in, Some(selection))
    assertNotEquals(before.description, after.description)
    assert(StudyText.sentence(after).text.contains("old_new"))
    assert(StudyText.methods(after).text.contains("excluded from both pair schedules"))
    val changes = before.structuralDiff(after)
    assertEquals(changes.map(_.field), Vector(StudyField.ExpectedUnmatched))
    assertEquals(get(before.revise(changes)), after)
    assertEquals(get(after.revise(changes.map(_.inverse))), before)
    assert(get(after.inspect).fields.exists(_.info.id == "expectedUnmatched"))
  }

  test("form parsing preserves the explicit selection and all trial assignments") {
    val p      = plan(input(all), Some(selection))
    val form   = StudyForm[Px](StudyFormContext.of(p))
    val recipe = get(form.parse(form.values(p)))
    assertEquals(recipe.expectedUnmatched, Some(selection))
    assertEquals(get(recipe.plan(p.input, p.layout, p.method, p.parameters)), p)
  }
