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

class StudyCountsSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A            = e.fold(e => fail(s"$e"), identity)
  private val frame                                    = get(Frame.screen("counting", 2, 2))
  private val grid                                     = get(Grid.over(frame, 2, 2))
  private def trial(key: StudyKey, offscreen: Boolean) =
    val clock    = ClockId(key.toString)
    val fixation = get(
      Event.Fixation.withoutDispersion(
        get(Interval.of(clock, Instant.micros(0L), Instant.micros(1000L))),
        Pt[Px](if offscreen then 3.5 else 0.5, 0.5),
        1
      )
    )
    Trial(key, (), get(Scanpath.of(frame, clock, IArray(fixation))))

  private def prepare(keys: Vector[StudyKey], scales: Int = 2, offscreen: Boolean = false) =
    val input = StudyInput(Trials(keys.map(k => trial(k, offscreen))))
    val plan  = get(
      StudyPlan.of(
        input.reference,
        StudyKey.layout(DefinitionId.studyLayout),
        grid,
        "recall",
        "encode",
        Weight.Duration,
        Vector.tabulate(scales)(i =>
          StudyEstimate.Gaussian(get(Sigma.px(i + 1.0)), eyes4s.surface.EdgePolicy.Truncate)
        ),
        FailurePolicy.RequireAll,
        StudyMethod.cosine[Px](DefinitionId.cosine),
        ()
      )
    )
    (plan, get(plan.prepare(input)))

  private val keys = Vector(
    StudyKey("p", "a", "recall"),
    StudyKey("p", "b", "recall"),
    StudyKey("q", "z", "recall"),
    StudyKey("p", "a", "encode"),
    StudyKey("p", "b", "encode"),
    StudyKey("p", "c", "encode"),
    StudyKey("z", "unused", "encode")
  )

  private def counts(keys: Vector[StudyKey], quantum: Int): StudyCounts[StudyKey] =
    val (plan, work) = prepare(keys)
    val cursor       = get(CountCursor.of(plan, work))
    get(
      Stepwise.complete(
        cursor,
        WorkQuanta(get(PairQuantum.of(quantum)), ComparisonQuantum.default)
      )
    )

  test("exact oracle includes matched, controls, unmatched, and totals across scales") {
    val c = counts(keys, 1)
    assertEquals(c.matched.eligiblePairs, 2L)
    assertEquals(c.controls.eligiblePairs, 4L)
    assertEquals(c.matched.focalWithPairs, 2L)
    assertEquals(c.matched.unmatchedFocal, 1L)
    assertEquals(c.matched.unmatchedReferences, 2L)
    assertEquals(c.controls.unmatchedReferences, 1L)
    assertEquals(c.eligibleQueries, 3L)
    assertEquals(c.pairRowsPerScale, 6L)
    assertEquals(c.totalPairs, 12L)
    assertEquals(c.totalMaps, 14L)
    assertEquals(c.cardinality.unmatched, Vector(keys(2)))
  }

  test("quantum changes neither counts nor cardinality; steps obey their bound") {
    val (plan, work) = prepare(keys)
    val quantum      = WorkQuanta(get(PairQuantum.of(1)), ComparisonQuantum.default)
    @annotation.tailrec
    def loop(cursor: CountCursor[StudyKey]): StudyCounts[StudyKey] =
      get(cursor.advance(quantum)) match
        case WorkStep.More(_, units, next) =>
          assert(units >= 0 && units <= 1)
          assertEquals(next.visited, cursor.visited + units)
          loop(next)
        case WorkStep.Done(units, result) =>
          assert(units >= 0 && units <= 1)
          result
    val small = loop(get(CountCursor.of(plan, work)))
    val large = counts(keys, 1000)
    assertEquals(small.totalPairs, large.totalPairs)
    assertEquals(small.cardinality.multiple, get(work.matchedCardinality).multiple)
    assertEquals(small.cardinality.unmatched, get(work.matchedCardinality).unmatched)
  }

  test("duplicate keys are ambiguous, not unmatched or eligible") {
    val c = counts(keys :+ keys.head, 2)
    assertEquals(c.matched.ambiguousKeys, 1L)
    assertEquals(c.controls.ambiguousKeys, 1L)
    assertEquals(c.matched.eligiblePairs, 1L)
    assertEquals(c.controls.eligiblePairs, 2L)
    assertEquals(c.eligibleQueries, 2L)
  }

  test("empty schedules finish with exact zero") {
    val c = counts(Vector.empty, 1)
    assertEquals(c.totalPairs, 0L)
    assertEquals(c.totalMaps, 0L)
    assertEquals(c.eligibleQueries, 0L)
  }

  test("window failures do not silently drop eligible queries or pairs") {
    val (plan, work) = prepare(keys, offscreen = true)
    assert(work.windowChecks.exists(_.isLeft))
    val c = get(Stepwise.complete(get(CountCursor.of(plan, work)), WorkQuanta.default))
    assertEquals(c.eligibleQueries, 3L)
    assertEquals(c.matched.eligiblePairs, 2L)
    assertEquals(c.controls.eligiblePairs, 4L)
  }
