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
import cats.effect.testkit.TestControl
import eyes4s.compare.*
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

class CountExecutionSuite extends munit.CatsEffectSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(e => fail(s"$e"), identity)

  private val frame = get(Frame.screen("execution", 2, 2))
  private val grid  = get(Grid.over(frame, 2, 2))
  private val a     = StudyKey("p1", "a", "recall")
  private val b     = StudyKey("p1", "b", "recall")
  private val ar    = StudyKey("p1", "a", "encode")
  private val br    = StudyKey("p1", "b", "encode")

  private def trial(key: StudyKey, points: (Double, Double)*) =
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
    Trial(key, (), get(Scanpath.of(frame, clock, IArray.from(fixes))))

  private val input = StudyInput(
    Trials(
      Vector(
        trial(a, (0.5, 0.5), (1.5, 0.5)),
        trial(b, (0.5, 1.5)),
        trial(ar, (0.5, 0.5)),
        trial(br, (1.5, 0.5), (0.5, 1.5)),
        trial(StudyKey("p2", "c", "excluded"), (0.5, 0.5))
      )
    )
  )

  private val cosine = StudyMethod.cosine[Px](DefinitionId.cosine)

  private def plan(
      source: StudyInput[StudyKey, Px] = input,
      scales: Vector[StudyEstimate[Px]] = Vector(StudyEstimate.Binned()),
      method: StudyMethod[Unit, Px, Similarity, SignedDifference] = cosine
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
        FailurePolicy.RequireAll,
        method,
        ()
      )
    )

  private val work = get(plan().prepare(input))

  private val finest = WorkQuanta(get(PairQuantum.of(1)), get(ComparisonQuantum.of(1)))

  test("counting runs through the common runner and ends with exact counts") {
    Execution[IO].events(CountExecution.submission(work, finest)).compile.toVector.map {
      events =>
        val advanced = events.collect { case RunEvent.Advanced(p) => p }
        assert(advanced.nonEmpty)
        assert(advanced.forall(_.segmentTotal == SegmentTotal.Counting))
        assert(advanced.forall(_.stepUnits <= finest.pairs.value))
        events.last match
          case RunEvent.Finished(RunOutcome.Completed(_, _, counts)) =>
            assertEquals(counts.matched.eligiblePairs, 2L)
            assertEquals(counts.controls.eligiblePairs, 2L)
            assertEquals(counts.totalPairs, 4L)
            assertEquals(get(work.preview(counts)).counts.map(_.totalPairs), Some(4L))
          case other => fail(s"unexpected final event: $other")
    }
  }

  test("cancelling between count pages returns no counts and exact last progress") {
    TestControl.executeEmbed {
      for
        calls   <- Ref.of[IO, Int](0)
        reached <- Deferred[IO, Unit]
        release <- Deferred[IO, Unit]
        between = calls.getAndUpdate(_ + 1).flatMap { n =>
          if n == 1 then reached.complete(()).void >> release.get else IO.unit
        }
        _ <- Execution[IO].start(CountExecution.submission(work, finest), between).use { run =>
          reached.get >> run.cancel >> run.outcome.map {
            case RunOutcome.Cancelled(_, Some(last)) =>
              assertEquals(last.step, 1L)
              assertEquals(last.segment, StudyDesign.Matched)
              assertEquals(last.stepUnits, 1)
            case other => fail(s"expected cancellation after one page, got $other")
          }
        }
      yield ()
    }
  }
