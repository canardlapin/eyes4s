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

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import eyes4s.codec.*
import eyes4s.compare.ComparisonQuantum
import eyes4s.design.{PairQuantum, SampleQuantum, WorkQuanta}
import eyes4s.fs2.{Execution, RunEvent, RunOutcome}
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

class StampedStudyExecutionSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
  private val plans                             = StudyCodecs.cosine[Px]
  private val inputs                            = StudyInputCodecs.study[Px]
  private val results                           = StudyResultCodecs.cosine[Px]
  private val bound                             = get(
    StampedStudy.prepare(SavedGraphFixture.plan, SavedGraphFixture.input, plans, inputs)
  )
  private val finest = WorkQuanta(get(PairQuantum.of(1)), get(ComparisonQuantum.of(1)))

  test("canonical stamp and execution quanta define portable submission identity") {
    val reloaded = get(
      StampedStudy.prepare(
        get(plans.codec.decode(get(plans.codec.encode(bound.plan)))),
        get(inputs.input.decode(get(inputs.input.encode(bound.prepared.input)))),
        plans,
        inputs
      )
    )
    val first  = StampedStudyExecution.submission(bound, quanta = finest).id
    val second = StampedStudyExecution
      .submission(
        reloaded,
        quanta = WorkQuanta(get(PairQuantum.of(1)), get(ComparisonQuantum.of(1)))
      )
      .id
    assertEquals(first, second)
    assert(first.stamp.sameAs(second.stamp))
    assert(first.sameAs(second))
    assert(!first.equals("not a run identity"))
    assertEquals(first.hashCode, second.hashCode)
    assertNotEquals(first, StampedStudyExecution.submission(bound).id)
    assertEquals(
      first,
      StampedStudyExecution
        .submission(bound, quanta = finest.copy(samples = get(SampleQuantum.of(1))))
        .id
    )
    assert(!first.stamp.equals("not a stamp"))
  }

  test("one effectful composite completion retains canonical binding and terminal telemetry") {
    Execution[IO]
      .events(StampedStudyExecution.submission(bound, quanta = finest))
      .compile
      .toVector
      .map { events =>
        val progress = events.collect { case RunEvent.Advanced(p) => p }
        assert(progress.exists(_.stage.isInstanceOf[StudyRunStage.Counting]))
        assert(progress.exists(_.stage.isInstanceOf[StudyRunStage.Running]))
        assertEquals(events.count { case RunEvent.Finished(_) => true; case _ => false }, 1)
        val wire = StudyProgressCodec.codec[StudyPlan[
          StudyKey,
          Px,
          Unit,
          eyes4s.compare.Similarity,
          eyes4s.design.SignedDifference
        ], StudyInput[StudyKey, Px]]
        progress.foreach { p =>
          val snapshot = get(StampedStudyExecution.snapshot(p))
          val document = get(wire.encode(snapshot))
          assertEquals(get(wire.encode(get(wire.parse(document.noSpaces)))), document)
          assertEquals(snapshot.step, p.step)
          assertEquals(snapshot.totalUnits, p.totalUnits)
        }
        val wrong =
          progress.head.copy(segment = StudyRunSegment.Running(StudySegment.Contrasting(0)))
        assert(StampedStudyExecution.snapshot(wrong).isLeft)
        events.last match
          case RunEvent.Finished(RunOutcome.Completed(id, last, completed)) =>
            assertEquals(last, progress.last)
            assertEquals(completed.stamp, id.stamp)
            assertEquals(
              get(results.codec.encode(get(completed.checkAgainst(bound)))),
              get(results.codec.encode(get(bound.run).result))
            )
            val archive = new DensityArchiveCodec(results)
            val bundle  = get(archive.encodeStamped(completed))
            val saved   = get(archive.codec.decode(get(archive.codec.encode(bundle.archive))))
            assert(saved.stampClaim.exists(_.sameAs(bound.stamp)))
            assert(saved.checkStamp(bound).isRight)
          case other => fail(s"unexpected composite outcome $other")
      }
      .unsafeToFuture()
  }
