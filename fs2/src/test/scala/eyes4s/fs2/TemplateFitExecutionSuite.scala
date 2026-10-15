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

package eyes4s.fs2

import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all.*
import eyes4s.compare.ComparisonQuantum
import eyes4s.design.*
import eyes4s.plan.*

class TemplateFitExecutionSuite extends munit.CatsEffectSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private def plan(rankDeficient: Boolean = false): TemplateFitPlan[String, Vector[Double]] =
    val basis = get(TemplateBasis.of("execution-template", Vector("x", "z"), "score"))
    val train = Vector(
      get(
        TemplateObservation
          .of("x", "train", if rankDeficient then Vector(1.0, 1.0) else Vector(1.0, 0.0), 2.0)
      ),
      get(
        TemplateObservation.of(
          "z",
          "train",
          if rankDeficient then Vector(2.0, 2.0) else Vector(0.0, 1.0),
          3.0
        )
      )
    )
    val held = Vector(0, 1, 2).map(i =>
      get(TemplateObservation.of(s"held-$i", "held", Vector(i.toDouble, 1.0), i.toDouble))
    )
    get(
      TemplateFitPlan.of(
        get(TemplateSplit.of(TemplateDesign.fixed(basis), train ++ held, Set("held")))
      )
    )
  private def quanta(n: Int) = WorkQuanta(get(PairQuantum.of(n)), ComparisonQuantum.default)
  private val runner         = TemplateFitExecution[IO]
  private def pure(p: TemplateFitPlan[String, Vector[Double]], q: WorkQuanta) =
    @annotation.tailrec
    def loop(
        cursor: TemplateFitCursor[String, Vector[Double]],
        seen: Vector[(TemplateValidationStage, Int)]
    ): Vector[(TemplateValidationStage, Int)] =
      get(cursor.advance(q)) match
        case WorkStep.More(stage, units, next) => loop(next, seen :+ (stage -> units))
        case WorkStep.Done(units, _)           => seen :+ (cursor.stage -> units)
    loop(p.work, Vector.empty)

  test("events replay pure stages with exact fitting and held-out totals") {
    Vector(1, 2, Int.MaxValue).traverse_ { amount =>
      val p = plan(); val q = quanta(amount)
      runner.events(p, q).compile.toVector.map { events =>
        val progress = events.collect { case RunEvent.Advanced(value) => value }
        assertEquals(progress.map(pr => pr.stage -> pr.stepUnits), pure(p, q))
        assertEquals(progress.head.stage, TemplateValidationStage.FittingWholeTraining)
        assertEquals(progress.head.segmentUnits, 1L)
        assertEquals(progress.head.segmentTotal, SegmentTotal.Exact(1L))
        progress.tail.foreach { pr =>
          assertEquals(pr.stage, TemplateValidationStage.EvaluatingWholeRows)
          assertEquals(pr.segmentTotal, SegmentTotal.Exact(3L))
          assert(pr.stepUnits > 0 && pr.stepUnits <= amount)
        }
        assertEquals(progress.last.segmentUnits, 3L)
        assertEquals(progress.last.totalUnits, 4L)
        assertEquals(progress.map(_.step), (1L to progress.size.toLong).toVector)
        events.last match
          case RunEvent.Finished(RunOutcome.Completed(_, last, result)) =>
            assertEquals(last, progress.last)
            assertEquals(result.inputHash, p.inputHash)
            assertEquals(result.planHash, p.planHash)
            assertEquals(
              result.validation.evaluation.rows,
              get(p.run).validation.evaluation.rows
            )
          case other => fail(s"$other")
      }
    }
  }

  test("native rank failure settles Failed before publishing a completed fitting unit") {
    val p        = plan(rankDeficient = true)
    val expected = p.run.left.toOption.get
    runner.events(p, quanta(1)).compile.toVector.map { events =>
      assertEquals(events.size, 1)
      events.head match
        case RunEvent.Finished(RunOutcome.Failed(id, error, last)) =>
          assertEquals(id, TemplateFitRunId.of(p, quanta(1)))
          assertEquals(error, expected)
          assertEquals(last, None)
        case other => fail(s"$other")
    }
  }

  test("run identity binds the source and recipe hashes and row quantum") {
    val p = plan()
    assertEquals(TemplateFitRunId.of(p, quanta(1)).input, p.inputHash.render)
    assertEquals(TemplateFitRunId.of(p, quanta(1)).plan, p.planHash.render)
    assertNotEquals(TemplateFitRunId.of(p, quanta(1)), TemplateFitRunId.of(p, quanta(2)))
    assertNotEquals(
      TemplateFitRunId.of(p, quanta(1)),
      TemplateFitRunId.of(plan(rankDeficient = true), quanta(1))
    )
  }

  test(
    "cancellation before work and at each fitting or row-page boundary keeps the last committed progress"
  ) {
    val p = plan(); val q = quanta(1)
    for
      before <- runner.start(p, q, IO.never).use(run => run.cancel >> run.outcome)
      _      <- (1 to 3).toVector.traverse_ { target =>
        for
          count   <- Ref.of[IO, Int](0)
          reached <- Deferred[IO, Unit]
          between = count
            .getAndUpdate(_ + 1)
            .flatMap(n =>
              if n == target then reached.complete(()).void >> IO.never else IO.cede
            )
          _ <- runner.start(p, q, between).use { run =>
            reached.get >> run.cancel >> run.outcome.flatMap { outcome =>
              run.progress.compile.toVector.map { terminal =>
                outcome match
                  case RunOutcome.Cancelled(_, Some(last)) =>
                    assertEquals(last.step, target.toLong)
                    assertEquals(last.totalUnits, target.toLong)
                    assertEquals(terminal.lastOption, Some(last))
                    assertEquals(
                      last.stage,
                      if target == 1 then TemplateValidationStage.FittingWholeTraining
                      else TemplateValidationStage.EvaluatingWholeRows
                    )
                  case other => fail(s"$other")
              }
            }
          }
        yield ()
      }
      completed <- runner.start(p, q).use(_.outcome)
    yield
      assertEquals(before, RunOutcome.Cancelled(TemplateFitRunId.of(p, q), None))
      completed match
        case RunOutcome.Completed(_, _, result) =>
          assertEquals(result.validation.evaluation.rows, get(p.run).validation.evaluation.rows)
        case other => fail(s"$other")
  }

  test("an imported fit reports receipt checking before held-out evaluation") {
    val native = plan()
    val design = TemplateDesign.importedLm(
      get(TemplateBasis.of("execution-template", Vector("x", "z"), "score"))
    )
    val split   = get(TemplateSplit.of(design, native.split.rows, native.split.heldOutGroups))
    val receipt = get(
      TemplateImportedFit.of(
        split.training,
        split.training.hash.render,
        design.basis.columns,
        Vector(2.0, 3.0),
        2,
        2,
        "external execution fixture"
      )
    )
    val p = get(TemplateFitPlan.imported(split, receipt))
    runner.events(p, quanta(1)).compile.toVector.map { events =>
      val progress = events.collect { case RunEvent.Advanced(value) => value }
      assertEquals(progress.head.stage, TemplateValidationStage.CheckingWholeImportedReceipt)
      assertEquals(progress.head.segmentTotal, SegmentTotal.Exact(1L))
      assertEquals(progress.map(_.totalUnits), Vector(1L, 2L, 3L, 4L))
      events.last match
        case RunEvent.Finished(RunOutcome.Completed(_, _, result)) =>
          assertEquals(result.validation.fitted.backend, "external execution fixture")
          assertEquals(result.validation.evaluation.rows, get(p.run).validation.evaluation.rows)
        case other => fail(s"$other")
    }
  }
