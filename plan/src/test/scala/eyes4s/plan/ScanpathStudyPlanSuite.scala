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

package eyes4s.scanpathstudyconsumer

import eyes4s.compare.*
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import scala.compiletime.testing.typeCheckErrors

class ScanpathStudyPlanSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val frame                             = get(Frame.screen("scanpath-study", 100, 100))
  private val clock                             = ClockId("scanpath-study")
  private val layout                            = new StudyLayout[String](
    get(DefinitionId.of("example.scanpath-study", 1)),
    Projection.named("participant")(_ => "p"),
    Projection.named("stimulus")(_.split("/")(0)),
    Projection.named("phase")(_.split("/")(1))
  )
  private def path(x: Double, n: Int = 3, sourceFrame: Frame[Px] = frame) =
    get(
      Scanpath.of(
        sourceFrame,
        clock,
        IArray.from((0 until n).map { index =>
          get(
            Event.Fixation.withoutDispersion(
              get(
                Interval
                  .of(clock, Instant.micros(index * 1000L), Instant.micros(index * 1000L + 500))
              ),
              Pt[Px](x + index, 1 + index.toDouble),
              1
            )
          )
        })
      )
    )
  private val rows = Vector(
    Trial("a/focal", (), path(1)),
    Trial("b/focal", (), path(20)),
    Trial("a/reference", (), path(1)),
    Trial("b/reference", (), path(20)),
    Trial("c/reference", (), path(40)),
    Trial("a/unused", (), path(60))
  )
  private val overlap = ScanpathMethod.Overlap(
    get(
      FixationOverlapSpec.of(
        clock,
        Vector(Instant.micros(0), Instant.micros(1000), Instant.micros(1000)),
        get(OverlapThreshold.of[Px](0.5)),
        FixationGroundDistance.Euclidean,
        TrajectoryEndpoint.OnsetRange,
        MissingOverlapPolicy.CountAsNonOverlap
      )
    )
  )
  private def ledger(pairs: ScanpathPairs[String]) = pairs.rows.map { row =>
    (
      row.left,
      row.right,
      row.result.map {
        case ScanpathMethodOutput.Overlap(value) => (value.rows, value.policy, value.similarity)
        case other => fail(s"Expected overlap ledger, found=$other")
      }
    )
  }
  private def plan(
      source: Vector[Trial[String, Unit, Scanpath[Px]]] = rows,
      method: ScanpathMethod[Px] = overlap,
      pairing: StudyPairing = StudyPairing.default,
      selection: Selection = Selection.All,
      policy: FailurePolicy = FailurePolicy.RequireAll,
      budget: PairScheduleBudget = PairScheduleBudget.default,
      target: Frame[Px] = frame
  ) = get(
    ScanpathStudyPlan.of(
      layout,
      StudyInput(Trials(source)),
      target,
      "focal",
      "reference",
      method,
      pairing,
      selection,
      policy,
      budget
    )
  )

  test("keyed pair ledgers and component means agree with direct comparison") {
    val p      = plan()
    val result = get(p.run)
    assertEquals(result.matched.rows.size, 2)
    assertEquals(result.controls.rows.size, 4)
    assertEquals(result.excludedPhases, Vector("a/unused"))
    val byKey = rows.map(row => row.key -> row.value).toMap
    (result.matched.rows ++ result.controls.rows).foreach { row =>
      assertEquals(
        row.result.map(_.scores),
        overlap.compare(byKey(row.left), byKey(row.right)).map(_.scores)
      )
    }
    result.contrasts.foreach { row =>
      assertEquals(row.matched.requested, 1)
      assertEquals(row.control.requested, 2)
      assertEquals(row.matched.contributing, 1)
      assertEquals(row.control.contributing, 2)
      assertEquals(row.matched.result, Right(1.0))
      assertEquals(row.control.result, Right(0.0))
      assertEquals(row.difference, Right(1.0))
    }
    assertEquals(result.inputHash, p.inputHash)
    assertEquals(result.planHash, p.planHash)
    assertEquals(p.diff(p), Vector.empty)
  }

  test("baseline MultiMatch retains exactly the public baseline components") {
    val p      = plan(method = ScanpathMethods.baselineMultiMatch[Px])
    val result = get(p.run)
    val byKey  = rows.map(row => row.key -> row.value).toMap
    result.matched.rows.foreach { row =>
      val expected = get(
        ScanpathComparison.baseline(
          get(BaselineScanpath.of(byKey(row.left), CompareOperand.Left)),
          get(BaselineScanpath.of(byKey(row.right), CompareOperand.Right))
        )
      )
      assertEquals(row.result, Right(ScanpathMethodOutput.MultiMatch(expected)))
    }
    assertEquals(result.contrasts.map(_.component).distinct, p.method.componentNames)
  }

  test("whole-pair pages are immutable and quantum-independent") {
    val p   = plan()
    val one = WorkQuanta(get(PairQuantum.of(1)), ComparisonQuantum.default)
    def complete(quanta: WorkQuanta) =
      @annotation.tailrec
      def loop(
          cursor: ScanpathStudyCursor[String, Px],
          steps: Int
      ): (ScanpathStudyResult[String, Px], Int) =
        get(cursor.advance(quanta)) match
          case WorkStep.More(_, units, next) =>
            if cursor.stage == ScanpathStudyStage.ComparingMatchedPairs ||
              cursor.stage == ScanpathStudyStage.ComparingControlPairs
            then assert(units <= quanta.pairs.value)
            loop(next, steps + 1)
          case WorkStep.Done(_, result) => (result, steps + 1)
      loop(p.work, 0)
    val (fine, fineSteps)     = complete(one)
    val (coarse, coarseSteps) = complete(WorkQuanta.default)
    assert(fineSteps > coarseSteps)
    assertEquals(
      fine.matched.rows.map(_.result.map(_.scores)),
      coarse.matched.rows.map(_.result.map(_.scores))
    )
    assertEquals(
      fine.controls.rows.map(_.result.map(_.scores)),
      coarse.controls.rows.map(_.result.map(_.scores))
    )
    assertEquals(fine.planHash, coarse.planHash)
    assertEquals(p.work.stage, ScanpathStudyStage.PreparingWholeStudy)
    assertEquals(ledger(get(p.work.complete(one)).matched), ledger(fine.matched))
    assertEquals(ledger(fine.controls), ledger(coarse.controls))
  }

  test("method failures keep both keys and explicit reduction failure counts") {
    val bad    = rows.updated(0, rows.head.copy(value = path(1, 2)))
    val result = get(plan(bad, ScanpathMethods.baselineMultiMatch[Px]).run)
    val failed = result.matched.rows.find(_.left == "a/focal").get
    assertEquals(failed.right, "a/reference")
    assert(failed.result.isLeft)
    val reduced = result.contrasts.find(_.key == "a/focal").get
    assertEquals(reduced.matched.result, Left(ReductionError.FailedScores("a/focal", 0, 1)))
    assertEquals(reduced.control.result, Left(ReductionError.FailedScores("a/focal", 0, 2)))
    assertEquals(reduced.matched.contributing, 0)
    assert(
      result.diagnostics.forall(diagnostic => DiagnosticCatalog.codes.contains(diagnostic.code))
    )
    assert(result.diagnostics.flatMap(_.affectedTrials).contains("a/focal"))
    assert(result.diagnostics.flatMap(_.affectedTrials).contains("a/reference"))
    val successful = get(
      plan(
        bad,
        ScanpathMethods.baselineMultiMatch[Px],
        policy = get(FailurePolicy.successfulOnly(1))
      ).run
    )
    assertEquals(
      successful.contrasts.find(_.key == "a/focal").get.matched.result,
      Left(ReductionError.InsufficientSuccessful("a/focal", 1, 0, 1))
    )
  }

  test("duplicates, unmatched focal keys and reference ambiguity remain explicit") {
    val duplicated = get(plan(rows :+ rows.head).run)
    assert(duplicated.matched.diagnostics.ambiguous.nonEmpty)
    assertEquals(
      duplicated.contrasts.head.matched.result,
      Left(ReductionError.AmbiguousKey("a/focal", Vector(0, 6)))
    )
    val unmatched = rows :+ Trial("z/focal", (), path(80))
    val reported  = get(plan(unmatched).run)
    assert(reported.matched.diagnostics.unmatchedLeft.contains("z/focal"))
    assert(!reported.controls.rows.exists(_.left == "z/focal"))
    assert(
      plan(
        unmatched,
        pairing = StudyPairing.default.copy(unmatched = UnmatchedFocalPolicy.Refuse)
      ).run.isLeft
    )
    val ambiguous = rows :+ Trial("a/reference/second", (), path(2))
    assert(plan(ambiguous).run.isLeft)
    assert(plan(ambiguous, pairing = StudyPairing.version1).run.isRight)
  }

  test("control selection is reproducible and conservative budgets refuse before comparison") {
    val selection =
      Selection.BottomK(get(PairLimit.of(1)), Seed(Long.MinValue), SampleId("sample"))
    val p     = plan(selection = selection)
    val first = get(p.run)
    assertEquals(first.controls.rows.size, 2)
    assertEquals(ledger(first.controls), ledger(get(p.run).controls))
    assertNotEquals(p.planHash, plan().planHash)
    val bounded = plan(budget = get(PairScheduleBudget.of(100, 100, 5)))
    assert(bounded.run.isLeft)
    assertEquals(
      bounded.preflight(Some(bounded.inputRef)).availability,
      Availability.Unavailable
    )
    assertEquals(p.preflight(None).availability, Availability.Unavailable)
    assertEquals(p.preflight(Some(p.inputRef)).availability, Availability.Ready)
    assert(
      typeCheckErrors(
        "summon[scala.deriving.Mirror.ProductOf[eyes4s.plan.ScanpathStudyPlan[String, eyes4s.kernel.Unit2D.Px]]]"
      ).nonEmpty
    )
  }

  test("nominal frame mismatch stays keyed and expected frame geometry changes plan identity") {
    val changed = get(Frame.screen("scanpath-study", 200, 100))
    val p       = plan(target = changed)
    assertNotEquals(p.planHash, plan().planHash)
    val result = get(p.run)
    assert(result.matched.rows.forall(_.result.isLeft))
    assertEquals(
      result.matched.rows.map(row => row.left -> row.right),
      Vector("a/focal" -> "a/reference", "b/focal" -> "b/reference")
    )
  }

  test("family form restores every typed method and complete scientific description") {
    val transport = ScanpathMethod.Transport(
      get(
        FixationTransportConfig
          .of[Px](20, 20, Span.micros(1000), 0.25, 0.5, 10000, 1e-10, 100, 1000000L)
      )
    )
    Vector(ScanpathMethods.baselineMultiMatch[Px], overlap, transport).foreach { method =>
      val p = plan(method = method); val form = new ScanpathStudyForm[Px](clock)
      assertEquals(get(p.inspect).description, p.description)
      assertEquals(form.views, form.fields.map(_.view))
      form.fields.foreach { field =>
        val raw = form.values(p).get(field.view.id)
        assertEquals(field.restore(raw), Right(raw))
        assertEquals(form.validate(field.view.id, raw), Right(()))
      }
      val restored = get(get(form.parse(form.values(p))).plan(p))
      assertEquals(restored.description, p.description)
      assertEquals(restored.planHash, p.planHash)
      assertEquals(p.diff(restored), Vector.empty)
    }
  }

  test("form preserves exact64 queries, limits, transport time scale and control seed") {
    val times =
      Vector(Long.MaxValue, Long.MinValue, 9007199254740993L, Long.MaxValue).map(Instant.micros)
    val overlap = ScanpathMethod.Overlap(
      get(
        FixationOverlapSpec.of(
          clock,
          times,
          get(OverlapThreshold.of[Px](0.0)),
          FixationGroundDistance.Manhattan,
          TrajectoryEndpoint.HoldLastOnset,
          MissingOverlapPolicy.RequireComplete
        )
      )
    )
    val transport = ScanpathMethod.Transport(
      get(
        FixationTransportConfig.of[Px](
          1.25,
          2.5,
          Span.micros(Long.MaxValue),
          0.0,
          0.125,
          Int.MaxValue,
          1e-14,
          Int.MaxValue,
          Long.MaxValue
        )
      )
    )
    val multiMatch = ScanpathMethod.MultiMatch[Px](
      MultiMatchCardinality.NativeTwoFixations,
      get(AlignmentCellLimit.of(Long.MaxValue))
    )
    Vector(multiMatch, overlap, transport).foreach { method =>
      val p = plan(
        method = method,
        selection = Selection.BottomK(get(PairLimit.of(1)), Seed(Long.MinValue), SampleId("λ")),
        policy = get(FailurePolicy.successfulOnly(1)),
        budget = get(PairScheduleBudget.of(Int.MaxValue, Long.MaxValue, Int.MaxValue))
      )
      val form     = new ScanpathStudyForm[Px](clock)
      val restored = get(get(form.parse(form.values(p))).plan(p))
      assertEquals(restored.description, p.description)
      assertEquals(restored.method.parameterHash, p.method.parameterHash)
      assertEquals(restored.planHash, p.planHash)
    }
  }

  test("form accumulates independent refusals and rejects unknown fields") {
    val p      = plan(); val form = new ScanpathStudyForm[Px](clock)
    val values = form
      .values(p)
      .updated(form.method.view.id, RawValue.Variant("overlap", Vector.empty))
      .updated(form.budget.view.id, RawValue.Group(Vector.empty))
    assertEquals(form.parse(values).left.toOption.get.toVector.size, 2)
    assert(form.validate(form.method.view.id, RawValue.Variant("unknown", Vector.empty)).isLeft)
    val unknown = get(FieldId.of("unknown-scanpath-field"))
    assertEquals(
      form.validate(unknown, RawValue.Absent),
      Left(FieldError.UnknownField(unknown))
    )
  }
