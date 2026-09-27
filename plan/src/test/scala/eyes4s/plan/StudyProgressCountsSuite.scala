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

class StudyProgressCountsSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(e => fail(s"$e"), identity)

  private val frame = get(Frame.screen("cursor", 2, 2))
  private val grid  = get(Grid.over(frame, 2, 2))
  private val a     = StudyKey("p1", "a", "recall")
  private val b     = StudyKey("p1", "b", "recall")
  private val ar    = StudyKey("p1", "a", "encode")
  private val br    = StudyKey("p1", "b", "encode")

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

  test(
    "run-cumulative completed objects are monotone across scales and bounded by exact counts"
  ) {
    val p = plan(scales =
      Vector(
        StudyEstimate.Binned(),
        StudyEstimate.Gaussian(get(Sigma.px(0.5)), EdgePolicy.Truncate)
      )
    )
    val work    = get(p.prepare(input))
    val counts  = get(work.counts)
    val quantum = WorkQuanta(get(PairQuantum.of(1)), get(ComparisonQuantum.of(1)))
    @annotation.tailrec
    def loop(cursor: StudyCursor[StudyKey, Px, Similarity, SignedDifference]): Unit =
      assert(cursor.completedMaps <= counts.totalMaps)
      assert(cursor.completedPairs <= counts.totalPairs)
      get(cursor.advance(quantum)) match
        case StudyStep.More(_, _, next) =>
          assert(next.completedMaps >= cursor.completedMaps)
          assert(next.completedPairs >= cursor.completedPairs)
          loop(next)
        case StudyStep.Done(_, result) =>
          assertEquals(cursor.completedMaps, 10L)
          assertEquals(cursor.completedPairs, 8L)
          assertEquals(result.scales.map(_.estimation.size).sum.toLong, counts.totalMaps)
    loop(get(work.work()))
  }

  test("preview starts without exact counts, then accepts only counts for its choices") {
    val work = get(plan().prepare(input))
    assertEquals(get(work.preview).counts, None)
    assertEquals(get(work.preview(get(work.counts))).counts.map(_.totalPairs), Some(4L))
    val changed = get(
      plan(scales = Vector(StudyEstimate.Gaussian(get(Sigma.px(0.5)), EdgePolicy.Truncate)))
        .prepare(input)
    )
    assert(changed.preview(get(work.counts)).isLeft)
    val reversed = StudyInput(Trials(input.trials.rows.reverse))
    assert(get(plan(reversed).prepare(reversed)).preview(get(work.counts)).isLeft)
  }
