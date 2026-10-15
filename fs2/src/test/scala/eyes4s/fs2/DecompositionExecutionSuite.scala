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

import cats.effect.IO
import cats.syntax.all.*
import eyes4s.compare.ComparisonQuantum
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

class DecompositionExecutionSuite extends munit.CatsEffectSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val frame = get(Frame.screen("decomposition-execution", 3, 1))
  private val grid  = get(Grid.over(frame, 3, 1))
  private def mass(a: Double, b: Double, c: Double): Mass[Px] =
    val values = IArray(a, b, c)
    get(
      Surface
        .intensity(grid, values, Provenance.raw(ContentHash.of(values)))
        .flatMap(_.normalised)
    )
  private val response                                                = mass(1, 3, 0)
  private def plan(deficient: Boolean = false): DecompositionPlan[Px] =
    val a          = mass(1, 0, 0); val b = if deficient then a else mass(0, 1, 0)
    val predictors = get(
      PredictorSet.of(Vector(get(PredictorId.of("a")) -> a, get(PredictorId.of("b")) -> b))
    )
    get(
      DecompositionPlan.of(
        response,
        predictors,
        DecompositionSpec.Ols(Intercept.Exclude, RelativeRankTolerance.default)
      )
    )
  private val runner         = DecompositionExecution[IO]
  private def quanta(n: Int) = WorkQuanta(get(PairQuantum.of(n)), ComparisonQuantum.default)

  test("one whole solve reports one exact unit and returns a bound result for every quantum") {
    Vector(1, Int.MaxValue).traverse_ { amount =>
      val p = plan(); val q = quanta(amount)
      runner.events(p, q).compile.toVector.map { events =>
        val progress = events.collect { case RunEvent.Advanced(value) => value }
        assertEquals(progress.size, 1)
        assertEquals(progress.head.stage, DecompositionStage.SolvingWholeSurface)
        assertEquals(progress.head.stepUnits, 1)
        assertEquals(progress.head.segmentUnits, 1L)
        assertEquals(progress.head.segmentTotal, SegmentTotal.Exact(1L))
        events.last match
          case RunEvent.Finished(RunOutcome.Completed(id, last, result)) =>
            assertEquals(id, DecompositionRunId.of(p))
            assertEquals(last, progress.head)
            assertEquals(result.inputHash, p.inputHash)
            assertEquals(result.planHash, p.planHash)
          case other => fail(s"$other")
      }
    }
  }

  test("rank refusal is a typed failure without a fictitious completed solving unit") {
    val p = plan(deficient = true); val expected = p.run.left.toOption.get
    runner.events(p, quanta(1)).compile.toVector.map { events =>
      assertEquals(events.size, 1)
      events.head match
        case RunEvent.Finished(RunOutcome.Failed(_, error, last)) =>
          assertEquals(error, expected)
          assertEquals(last, None)
        case other => fail(s"$other")
    }
  }

  test("resource cancellation before the indivisible solve has no committed progress") {
    val p = plan()
    for
      cancelled <- runner.start(p, quanta(1), IO.never).use(run => run.cancel >> run.outcome)
      completed <- runner.start(p, quanta(1)).use(_.outcome)
    yield
      assertEquals(cancelled, RunOutcome.Cancelled(DecompositionRunId.of(p), None))
      completed match
        case RunOutcome.Completed(_, _, result) => assertEquals(result.inputHash, p.inputHash)
        case other                              => fail(s"$other")
  }

  test("submission identity excludes quanta because quanta do not subdivide this solve") {
    val p = plan()
    assertEquals(
      DecompositionExecution.submission(p, quanta(1)).id,
      DecompositionExecution.submission(p, quanta(Int.MaxValue)).id
    )
    assertNotEquals(DecompositionRunId.of(p), DecompositionRunId.of(plan(deficient = true)))
  }
