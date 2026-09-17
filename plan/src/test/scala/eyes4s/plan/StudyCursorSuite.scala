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

import eyes4s.compare.*
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.surface.EdgePolicy

import scala.compiletime.testing.typeCheckErrors

/** Resumable study execution against the pure runner.
  *
  * `work.run` is itself `StudyWork.complete(cursor)`, so the equalities here
  * prove that cutting the work at any quantum changes nothing, not that the
  * cursor agrees with the pre-X3 runner; that equivalence rests on the pinned
  * oracle suites (`StudyGuideOracleSuite`, `StudyWorkSuite`).
  */
class StudyCursorSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(e => fail(s"$e"), identity)

  /** Hand-calculated targets are compared at this named tolerance; everything
    * that the pure runner also produces is compared exactly.
    */
  private val HandCalculated = 1e-12

  private val frame = get(Frame.screen("cursor", 2, 2))
  private val grid  = get(Grid.over(frame, 2, 2))
  private val a     = StudyKey("p1", "a", "recall")
  private val b     = StudyKey("p1", "b", "recall")
  private val ar    = StudyKey("p1", "a", "encode")
  private val br    = StudyKey("p1", "b", "encode")
  private val cr    = StudyKey("p1", "c", "encode")

  private def trial(key: StudyKey, f: Frame[Px], points: (Double, Double)*) =
    val clock = ClockId(key.participant + "/" + key.stimulus + "/" + key.phase)
    val fixes = points.zipWithIndex.map { case ((x, y), i) =>
      get(
        Event.Fixation.withoutDispersion(
          get(Interval.of(clock, Instant.micros(i * 2000L), Instant.micros(i * 2000L + 1000L))),
          Pt[Px](x, y),
          1
        )
      )
    }
    Trial(key, (), get(Scanpath.of(f, clock, IArray.from(fixes))))

  // Focal a occupies two cells equally; its matched reference occupies one of
  // them and its control reference straddles one of them and a third cell.
  private val input = StudyInput(
    Trials(
      Vector(
        trial(a, frame, (0.5, 0.5), (1.5, 0.5)),
        trial(b, frame, (0.5, 1.5)),
        trial(ar, frame, (0.5, 0.5)),
        trial(br, frame, (1.5, 0.5), (0.5, 1.5)),
        trial(StudyKey("p2", "c", "excluded"), frame, (0.5, 0.5))
      )
    )
  )

  private def plan(
      source: StudyInput[StudyKey, Px] = input,
      scales: Vector[StudyEstimate[Px]] = Vector(StudyEstimate.Binned()),
      policy: FailurePolicy = FailurePolicy.RequireAll,
      method: StudyMethod[Unit, Px, Similarity, SignedDifference] =
        StudyMethod.cosine[Px](DefinitionId.cosine)
  ) =
    get(
      StudyPlan.of(
        source.reference,
        StudyKey.layout(DefinitionId.studyLayout),
        grid,
        "recall",
        "encode",
        Weight.Duration,
        scales,
        policy,
        method,
        ()
      )
    )

  private val quanta = Vector(
    WorkQuanta(get(PairQuantum.of(1)), get(ComparisonQuantum.of(1))),
    WorkQuanta(get(PairQuantum.of(2)), get(ComparisonQuantum.of(3))),
    WorkQuanta.default
  )

  private def drive[S, D](
      cursor: StudyCursor[StudyKey, Px, S, D],
      quanta: WorkQuanta
  ): (Vector[StudyStage], StudyResult[StudyKey, Px, S, D]) =
    val stages = Vector.newBuilder[StudyStage]
    @annotation.tailrec
    def loop(cursor: StudyCursor[StudyKey, Px, S, D]): StudyResult[StudyKey, Px, S, D] =
      val expected = cursor.stage
      get(cursor.advance(quanta)) match
        case StudyStep.More(stage, units, next) =>
          assertEquals(stage, expected)
          assert(units >= 0)
          stages += stage
          loop(next)
        case StudyStep.Done(units, result) =>
          assert(units >= 0)
          stages += expected
          result
    val result = loop(cursor)
    stages.result() -> result

  private def assertSameResult[S, D](
      observed: StudyResult[StudyKey, Px, S, D],
      expected: StudyResult[StudyKey, Px, S, D],
      clue: Any
  ): Unit =
    assertEquals(observed.input, expected.input)
    assertEquals(observed.description, expected.description)
    assertEquals(observed.scales.size, expected.scales.size, clue)
    observed.scales.zip(expected.scales).foreach { case (o, e) =>
      assertEquals(o.estimate, e.estimate)
      assertEquals(o.excludedPhases, e.excludedPhases)
      assertEquals(o.estimation.map(_._1), e.estimation.map(_._1))
      o.estimation.zip(e.estimation).foreach {
        case ((_, Right(x)), (_, Right(y))) =>
          assertEquals(x.values.toVector, y.values.toVector)
          assertEquals(x.provenance, y.provenance)
        case ((_, Left(x)), (_, Left(y))) => assertEquals(x, y)
        case (x, y)                       => fail(s"estimation outcome differs: $x versus $y")
      }
      (o.contrast, e.contrast) match
        case (Right(x), Right(y)) =>
          assertEquals(x.rows.map(_.key), y.rows.map(_.key))
          assertEquals(x.rows.map(_.difference), y.rows.map(_.difference))
          assertEquals(x.rows.map(_.matched), y.rows.map(_.matched))
          assertEquals(x.rows.map(_.control), y.rows.map(_.control))
          Vector(x.matched -> y.matched, x.control -> y.control).foreach { case (m, n) =>
            assertEquals(m.entries, n.entries)
            assertEquals(m.diagnostics, n.diagnostics)
            assertEquals(m.provenance, n.provenance)
            assertEquals(m.evaluation.name, n.evaluation.name)
            assertEquals(m.evaluation.scale, n.evaluation.scale)
            assertEquals(
              m.evaluation.specification
                .map(s => (s.method, s.revision, s.parameters, s.components)),
              n.evaluation.specification
                .map(s => (s.method, s.revision, s.parameters, s.components))
            )
            assertEquals(m.source.rows: Any, n.source.rows: Any)
            assertEquals(m.source.diagnostics: Any, n.source.diagnostics: Any)
            assertEquals(m.source.provenance, n.source.provenance)
          }
        case (Left(x), Left(y)) => assertEquals(x, y)
        case (x, y)             => fail(s"contrast outcome differs: $x versus $y")
    }

  test("hand-calculated matched-minus-control agrees at every quantum with the pure runner") {
    val p        = plan()
    val work     = get(p.prepare(input))
    val pure     = get(work.run)
    val contrast = get(pure.scales.head.contrast)
    val rows     = contrast.rows.map(r => r.key -> get(r.difference).value).toMap
    // a: cos((.5,.5,0,0),(1,0,0,0)) - cos((.5,.5,0,0),(0,.5,.5,0)) = 1/sqrt(2) - 1/2.
    assertEqualsDouble(rows(a), 1.0 / math.sqrt(2.0) - 0.5, HandCalculated)
    // b: cos((0,0,1,0),(0,.5,.5,0)) - cos((0,0,1,0),(1,0,0,0)) = 1/sqrt(2) - 0.
    assertEqualsDouble(rows(b), 1.0 / math.sqrt(2.0), HandCalculated)
    assertEquals(
      contrast.rows.map(r => (r.matched.map(_.contributing), r.control.map(_.contributing))),
      Vector((Some(1), Some(1)), (Some(1), Some(1)))
    )
    assertEquals(work.capability, ExecutionCapability.BoundedComparison)
    assertEquals(get(p.inspect).execution, ExecutionCapability.BoundedComparison)

    quanta.foreach { q =>
      val cursor            = get(work.work())
      val (stages, resumed) = drive(cursor, q)
      assertSameResult(resumed, pure, q)
      assertEquals(get(work.boundedWork()).stage, StudyStage.Estimating(0, 0))
      assert(stages.count(_ == StudyStage.Comparing(0, StudyDesign.Matched)) >= 1)
    }
    // The finest cut yields inside each comparison: one step per cell, per pair.
    val finest = drive(get(work.work()), quanta.head)._1
    val coarse = drive(get(work.work()), quanta.last)._1
    assert(finest.size > coarse.size, clue((finest.size, coarse.size)))
    assert(finest.count(_ == StudyStage.Comparing(0, StudyDesign.Matched)) > 2 * grid.size)
  }

  test("stages advance in scientific order across scales") {
    val p = plan(scales =
      Vector(
        StudyEstimate.Binned(),
        StudyEstimate.Gaussian(get(Sigma.px(0.5)), EdgePolicy.Truncate)
      )
    )
    val work              = get(p.prepare(input))
    val (stages, resumed) = drive(get(work.work()), quanta.last)
    assertSameResult(resumed, get(work.run), "two scales")
    val order    = stages.distinct
    val perScale = (scale: Int) =>
      input.trials.rows.indices.map(StudyStage.Estimating(scale, _)).toVector ++ Vector(
        StudyStage.Comparing(scale, StudyDesign.Matched),
        StudyStage.Reducing(scale, StudyDesign.Matched),
        StudyStage.Comparing(scale, StudyDesign.Control),
        StudyStage.Reducing(scale, StudyDesign.Control),
        StudyStage.Contrasting(scale)
      )
    assertEquals(order, perScale(0) ++ perScale(1))
    assertEquals(stages.count(_ == StudyStage.Estimating(1, 3)), 1)
  }

  test("RequireAll and SuccessfulOnly through the cursor equal the pure runner with failures") {
    val other  = get(Frame.screen("elsewhere", 2, 2))
    val source = StudyInput(
      Trials(
        input.trials.rows.take(4) :+ trial(cr, other, (0.5, 0.5))
      )
    )
    Vector(FailurePolicy.RequireAll, get(FailurePolicy.successfulOnly(1))).foreach { policy =>
      val p    = plan(source, policy = policy)
      val work = get(p.prepare(source))
      val pure = get(work.run)
      val rowA = get(pure.scales.head.contrast).rows.find(_.key == a).get
      // a's controls: b succeeds, c fails its frame check.
      assertEquals(rowA.control.map(r => (r.successful, r.failed)), Some((1, 1)))
      policy match
        case FailurePolicy.RequireAll =>
          assertEquals(
            rowA.control.map(_.result),
            Some(Left(ReductionError.FailedScores(a, 1, 1)))
          )
          assert(rowA.difference.isLeft)
        case FailurePolicy.SuccessfulOnly(_) =>
          assertEquals(rowA.control.map(_.contributing), Some(1))
          assertEqualsDouble(
            get(rowA.difference).value,
            1.0 / math.sqrt(2.0) - 0.5,
            HandCalculated
          )
      quanta.foreach(q => assertSameResult(drive(get(work.work()), q)._2, pure, (policy, q)))
    }
  }

  test("reversed input changes traversal order but no per-key value") {
    val reversed = StudyInput(Trials(input.trials.rows.reverse))
    val forward  = drive(get(get(plan().prepare(input)).work()), quanta.head)._2
    val backward = drive(get(get(plan(reversed).prepare(reversed)).work()), quanta.head)._2
    val f        = get(forward.scales.head.contrast)
    val r        = get(backward.scales.head.contrast)
    assertEquals(f.matched.entries.map(_.key), Vector(a, b))
    assertEquals(r.matched.entries.map(_.key), Vector(b, a))
    assertEquals(f.rows.map(_.key), r.rows.map(_.key))
    assertEquals(f.rows.map(_.difference), r.rows.map(_.difference))
    assertEquals(f.rows.map(_.matched.map(_.result)), r.rows.map(_.matched.map(_.result)))
  }

  test("an unsupported synchronous extension is diagnosed before any bounded work") {
    var factory     = 0
    var comparisons = 0
    val synchronous = new StudyMethod[Unit, Px, Similarity, SignedDifference](
      DefinitionId.cosine,
      "synchronous cosine",
      _ => Vector.empty,
      _ =>
        factory += 1
        val inner = Distribution.cosine[Px]
        new Compare[Mass[Px], Mass[Px], Similarity]:
          def info: MeasureInfo                                                   = inner.info
          def compare(x: Mass[Px], y: Mass[Px]): Either[CompareError, Similarity] =
            comparisons += 1
            inner.compare(x, y)
      ,
      Some(MethodDescriptor.cosine[Px](DefinitionId.cosine))
    )
    val p    = plan(method = synchronous)
    val work = get(p.prepare(input))
    assertEquals(work.capability, ExecutionCapability.SynchronousWholeOperation)
    assertEquals(
      work.boundedWork().left.toOption,
      Some(
        PlanError.UnsupportedExecution(
          DefinitionId.cosine,
          ExecutionCapability.SynchronousWholeOperation
        )
      )
    )
    assertEquals((factory, comparisons), (0, 0))
    // The descriptor's bounded claim is false of this method; inspection says so.
    assertEquals(
      p.inspect.left.toOption,
      Some(
        DescriptorError.ExecutionMismatch(
          ExecutionCapability.BoundedComparison,
          ExecutionCapability.SynchronousWholeOperation
        )
      )
    )
    val report = p.preflight(Some(input))
    assert(report.findings.exists {
      case StudyFinding.InconsistentDescriptor(_, DescriptorError.ExecutionMismatch(_, _)) =>
        true
      case _ => false
    })
    assertEquals(report.blockers, Vector.empty)
    // Whole comparisons still run, one pair per step, and match the pure runner.
    val pure = get(work.run)
    assertEquals(factory, 1)
    val before            = comparisons
    val (stages, resumed) = drive(get(work.work()), quanta.last)
    assertEquals(get(work.work()).capability, ExecutionCapability.SynchronousWholeOperation)
    assertSameResult(resumed, pure, "synchronous")
    assertEquals(comparisons - before, 4)
    // One schedule page, one whole comparison per matched pair, one completion step.
    assertEquals(stages.count(_ == StudyStage.Comparing(0, StudyDesign.Matched)), 4)
    quanta.foreach(q => assertSameResult(drive(get(work.work()), q)._2, pure, q))
  }

  test("a comparison budget below the grid is refused with its operands before estimation") {
    var estimated                                   = 0
    val work                                        = get(plan().prepare(input))
    def counting(key: StudyKey, path: Scanpath[Px]) =
      estimated += 1
      path.occupancy(Weight.Duration).left.map(StudyFailure.Occupancy(key, _))
    def cursor(cells: Long) = work.work(get(ComparisonBudget.of(cells)), counting, Vector.empty)
    assertEquals(
      cursor(3).left.toOption,
      Some(PlanError.ComparisonWork(ComparisonWorkError.WorkBudget("cosine", 4, 3)))
    )
    assert(cursor(4).isRight)
    // Both the refusal and the accepted start precede any estimation.
    assertEquals(estimated, 0)
    val (_, resumed) = drive(get(cursor(4)), quanta.head)
    assertEquals(estimated, input.trials.rows.size)
    assertSameResult(resumed, get(work.run), "budget of exactly the grid")
  }

  test("wrong execution evidence, score and parameter types are rejected at compile time") {
    assert(
      typeCheckErrors("""
        import eyes4s.compare.*
        import eyes4s.plan.*
        import eyes4s.kernel.Unit2D.Px
        MethodExecution.Bounded[Unit, Px, MeasureDistance](_ => Distribution.totalVariation[Px])
      """).exists(_.message.contains("BoundedCompare")),
      "a whole synchronous metric was accepted as bounded execution"
    )
    assert(
      typeCheckErrors("""
        import eyes4s.compare.*
        import eyes4s.plan.*
        import eyes4s.kernel.Unit2D.Px
        MethodExecution.Bounded[Unit, Px, MeasureDistance](_ => Distribution.cosine[Px])
      """).exists(_.message.contains("MeasureDistance")),
      "a Similarity method was accepted with a MeasureDistance score type"
    )
    assert(
      typeCheckErrors("""
        import eyes4s.compare.*
        import eyes4s.plan.*
        import eyes4s.kernel.Unit2D.Px
        val execution: MethodExecution[Double, Px, Similarity] =
          MethodExecution.Bounded((p: Unit) => Distribution.cosine[Px])
      """).exists(_.message.contains("Double")),
      "a Unit-parameter method was accepted for Double parameters"
    )
    assert(
      typeCheckErrors("""
        import eyes4s.compare.*
        import eyes4s.design.*
        import eyes4s.plan.*
        import eyes4s.kernel.Unit2D.Px
        new StudyMethod[Unit, Px, MeasureDistance, SignedDifference](
          DefinitionId.cosine,
          "wrong",
          _ => Vector.empty,
          MethodExecution.Bounded(_ => Distribution.cosine[Px]),
          None
        )
      """).exists(_.message.contains("MeasureDistance")),
      "a StudyMethod accepted execution evidence of the wrong score type"
    )
  }
