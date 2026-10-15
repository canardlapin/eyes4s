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
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

class ScanpathStudyExecutionSuite extends munit.CatsEffectSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val frame  = get(Frame.screen("scanpath-execution", 10, 10))
  private val clock  = ClockId("scanpath-execution")
  private val layout = new StudyLayout[String](
    get(DefinitionId.of("example.scanpath-execution", 1)),
    Projection.named("participant")(_ => "p"),
    Projection.named("stimulus")(_.split("/")(0)),
    Projection.named("phase")(_.split("/")(1))
  )
  private val path = get(
    Scanpath.of(
      frame,
      clock,
      IArray.from((0 until 3).map { index =>
        get(
          Event.Fixation.withoutDispersion(
            get(
              Interval
                .of(clock, Instant.micros(index * 1000L), Instant.micros(index * 1000L + 500))
            ),
            Pt[Px](index + 1.0, if index == 1 then 2.0 else 1.0),
            1
          )
        )
      })
    )
  )
  private val input = StudyInput(Trials(Vector("a", "b").flatMap { item =>
    Vector("focal", "reference").map(phase => Trial(s"$item/$phase", (), path))
  }))
  private def plan(budget: PairScheduleBudget = PairScheduleBudget.default) =
    get(
      ScanpathStudyPlan.of(
        layout,
        input,
        frame,
        "focal",
        "reference",
        ScanpathMethods.baselineMultiMatch[Px],
        budget = budget
      )
    )
  private def quanta(n: Int) = WorkQuanta(get(PairQuantum.of(n)), ComparisonQuantum.default)
  private val runner         = ScanpathStudyExecution[IO]

  test(
    "whole-operation and whole-pair segments report exact totals and bound result identity"
  ) {
    Vector(1, Int.MaxValue).traverse_ { n =>
      val p = plan()
      runner.events(p, quanta(n)).compile.toVector.map { events =>
        val progress = events.collect { case RunEvent.Advanced(value) => value }
        val totals   = progress.groupBy(_.stage).view.mapValues(_.map(_.stepUnits).sum).toMap
        assertEquals(
          totals,
          Map(
            ScanpathStudyStage.PreparingWholeStudy   -> 1,
            ScanpathStudyStage.ComparingMatchedPairs -> 2,
            ScanpathStudyStage.ComparingControlPairs -> 2,
            ScanpathStudyStage.ReducingWholeStudy    -> 1
          )
        )
        progress.foreach { value =>
          val expected = value.stage match
            case ScanpathStudyStage.PreparingWholeStudy |
                ScanpathStudyStage.ReducingWholeStudy =>
              1L
            case _ => 2L
          assertEquals(value.segmentTotal, SegmentTotal.Exact(expected))
          assert(value.segmentUnits <= expected)
        }
        events.last match
          case RunEvent.Finished(RunOutcome.Completed(id, _, result)) =>
            assertEquals(id, ScanpathStudyRunId.of(p, quanta(n)))
            assertEquals(result.inputHash, p.inputHash)
            assertEquals(result.planHash, p.planHash)
            assertEquals(result.matched.rows.size, 2)
            assertEquals(result.controls.rows.size, 2)
          case other => fail(s"$other")
      }
    }
  }

  test("preparation budget refusal returns a typed failure and no committed work") {
    val p        = plan(get(PairScheduleBudget.of(100, 0, 100)))
    val expected = p.run.left.toOption.get
    runner.events(p, quanta(1)).compile.toVector.map { events =>
      events.last match
        case RunEvent.Finished(RunOutcome.Failed(_, error, last)) =>
          assertEquals(error, expected)
          assertEquals(last, None)
        case other => fail(s"$other")
    }
  }

  test("resource cancellation before preparation returns no partial result") {
    val p = plan(); val q = quanta(1)
    runner.start(p, q, IO.never).use(run => run.cancel >> run.outcome).map { result =>
      assertEquals(result, RunOutcome.Cancelled(ScanpathStudyRunId.of(p, q), None))
    }
  }

  test("cancellation between preparation and comparisons retains only completed progress") {
    val p = plan(); val q = quanta(1)
    for
      calls   <- Ref.of[IO, Int](0)
      reached <- Deferred[IO, Unit]
      between = calls
        .updateAndGet(_ + 1)
        .flatMap(n => if n == 2 then reached.complete(()).void >> IO.never else IO.unit)
      outcome <- runner
        .start(p, q, between)
        .use(run => reached.get >> run.cancel >> run.outcome)
    yield outcome match
      case RunOutcome.Cancelled(id, Some(last)) =>
        assertEquals(id, ScanpathStudyRunId.of(p, q))
        assertEquals(last.stage, ScanpathStudyStage.PreparingWholeStudy)
        assertEquals(last.segmentUnits, 1L)
      case other => fail(s"$other")
  }

  test(
    "submission identity includes page size but comparison and sample quanta do not split pairs"
  ) {
    val p = plan()
    assertNotEquals(
      ScanpathStudyExecution.submission(p, quanta(1)).id,
      ScanpathStudyExecution.submission(p, quanta(2)).id
    )
    assertEquals(
      ScanpathStudyExecution.submission(p, quanta(1)).id,
      ScanpathStudyRunId.of(p, quanta(1))
    )
  }
