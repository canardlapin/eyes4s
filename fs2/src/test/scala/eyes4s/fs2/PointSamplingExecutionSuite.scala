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
import eyes4s.surface.*

class PointSamplingExecutionSuite extends munit.CatsEffectSuite:
  private def get[E, A](v: Either[E, A]): A = v.fold(e => fail(e.toString), identity)
  private val clock                         = ClockId("point-execution")
  private val frame                         = get(Frame.screen("point-execution", 1, 1))
  private val grid                          = get(Grid.over(frame, 1, 1))
  private val layout                        =
    StudyKey.layout(get(DefinitionId.of("example.point-execution-layout", 1)))
  private val path = get(
    Scanpath.of(
      frame,
      clock,
      IArray(
        get(
          Event.Fixation.withoutDispersion(
            get(Interval.of(clock, Instant.micros(0), Instant.micros(10))),
            Pt[Px](0.5, 0.5),
            1
          )
        )
      )
    )
  )
  private val sources = Vector("a", "b").flatMap(stimulus =>
    Vector("one", "two").map(phase => Trial(StudyKey("person", stimulus, phase), (), path))
  )
  private val templates = Trials(Vector("a" -> 2.0, "b" -> 5.0).map { (stimulus, value) =>
    val values = IArray(value)
    Trial(
      StudyKey("person", stimulus, "template"),
      (),
      get(Surface.signed(grid, values, Provenance.raw(ContentHash.of(values))))
    )
  })
  private val spec = get(
    PointSamplingSpec.of(
      clock,
      Vector(0L, 5L, 10L).map(Instant.micros),
      None,
      DensityNormalization.None,
      DensityLookupPolicy.NearestClampedRIndex,
      TrajectoryEndpoint.HoldLastOnset,
      PointControlSelection.Candidates(Selection.All),
      FailurePolicy.RequireAll,
      FailurePolicy.RequireAll
    )
  )
  private def plan(rows: Vector[Trial[StudyKey, Unit, Scanpath[Px]]] = sources) =
    PointSamplingPlan.of(layout, StudyInput(Trials(rows)), templates, spec)
  private def quanta(n: Int) = WorkQuanta(get(PairQuantum.of(n)), ComparisonQuantum.default)
  private val runner         = PointSamplingExecution[IO]
  private def evidence(result: PointSamplingResult[StudyKey, Px]) =
    result.rows.map(row =>
      (
        row.key,
        row.points.map(_.value),
        row.points.map(_.control.map(_.result)),
        row.controls.map(_.template)
      )
    )
  private def pure(p: PointSamplingPlan[StudyKey, Px], q: WorkQuanta) =
    @annotation.tailrec
    def loop(
        c: PointSamplingCursor[StudyKey, Px],
        seen: Vector[(PointSamplingStage, Int)]
    ): Vector[(PointSamplingStage, Int)] =
      c.advance(q) match
        case WorkStep.More(stage, units, next) => loop(next, seen :+ (stage -> units))
        case WorkStep.Done(units, _)           => seen :+ (c.stage -> units)
    loop(p.work, Vector.empty)

  test("events preserve pure stages, exact totals and the independent constant-map result") {
    Vector(1, 3, Int.MaxValue).traverse_ { size =>
      val p = plan(); val q = quanta(size)
      runner.events(p, q).compile.toVector.map { events =>
        val progress = events.collect { case RunEvent.Advanced(value) => value }
        assertEquals(progress.map(pr => pr.stage -> pr.stepUnits), pure(p, q))
        assertEquals(progress.map(_.step), (1L to progress.size.toLong).toVector)
        assertEquals(progress.head.stage, PointSamplingStage.PreparingWholeStudy)
        assertEquals(progress.head.segmentTotal, SegmentTotal.Exact(1L))
        progress.tail.foreach { pr =>
          assertEquals(pr.stage, PointSamplingStage.SamplingWholeTrials)
          assertEquals(pr.segmentTotal, SegmentTotal.Exact(4L))
          assert(pr.stepUnits > 0 && pr.stepUnits <= size)
        }
        assertEquals(progress.last.segmentUnits, 4L)
        assertEquals(progress.last.totalUnits, 5L)
        assert(
          progress.forall(pr => pr.run == PointSamplingRunId.of(p, q) && pr.segment == pr.stage)
        )
        events.last match
          case RunEvent.Finished(RunOutcome.Completed(_, last, result)) =>
            assertEquals(last, progress.last)
            assertEquals(evidence(result), evidence(p.run))
            result.rows.foreach { row =>
              val observed = if row.key.stimulus == "a" then 2.0 else 5.0
              val control  = if row.key.stimulus == "a" then 5.0 else 2.0
              assertEquals(row.points.map(_.value), Vector.fill(3)(Right(observed)))
              assertEquals(
                row.points.map(_.control.map(_.result)),
                Vector.fill(3)(Some(Right(control)))
              )
            }
          case other => fail(s"$other")
      }
    }
  }

  test("empty input completes in the single preparation segment") {
    val p = plan(Vector.empty)
    runner.events(p, quanta(1)).compile.toVector.map { events =>
      val progress = events.collect { case RunEvent.Advanced(pr) => pr }
      assertEquals(progress.size, 1)
      assertEquals(progress.head.stage, PointSamplingStage.PreparingWholeStudy)
      assertEquals(progress.head.segmentUnits, 1L)
      assertEquals(progress.head.segmentTotal, SegmentTotal.Exact(1L))
      events.last match
        case RunEvent.Finished(RunOutcome.Completed(_, _, result)) =>
          assertEquals(result.rows, Vector.empty)
        case other => fail(s"$other")
    }
  }

  test("run identity binds input, recipe and actual trial quantum") {
    val p = plan()
    assertEquals(PointSamplingRunId.of(p, quanta(1)), PointSamplingRunId.of(p, quanta(1)))
    assertNotEquals(PointSamplingRunId.of(p, quanta(1)), PointSamplingRunId.of(p, quanta(2)))
    assertNotEquals(
      PointSamplingRunId.of(p, quanta(1)),
      PointSamplingRunId.of(plan(sources.take(1)), quanta(1))
    )
    assertEquals(PointSamplingRunId.of(p, quanta(1)).input, p.inputHash.render)
    assertEquals(PointSamplingRunId.of(p, quanta(1)).plan, p.planHash.render)
  }

  test(
    "start completes, cancels before work, and cancels after each explicit operation boundary"
  ) {
    val p = plan(); val q = quanta(1)
    for
      completed <- runner.start(p, q).use(_.outcome)
      before    <- runner.start(p, q, IO.never).use(r => r.cancel >> r.outcome)
      _         <- (1 to 4).toVector.traverse_ { target =>
        for
          calls   <- Ref.of[IO, Int](0)
          reached <- Deferred[IO, Unit]
          between = calls
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
                      if target == 1 then PointSamplingStage.PreparingWholeStudy
                      else PointSamplingStage.SamplingWholeTrials
                    )
                  case other => fail(s"$other")
              }
            }
          }
        yield ()
      }
    yield
      completed match
        case RunOutcome.Completed(_, _, result) =>
          assertEquals(evidence(result), evidence(p.run))
        case other => fail(s"$other")
      assertEquals(before, RunOutcome.Cancelled(PointSamplingRunId.of(p, q), None))
  }
