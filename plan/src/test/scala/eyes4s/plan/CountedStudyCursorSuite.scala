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

class CountedStudyCursorSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
  private val frame                             = get(Frame.screen("counted-run", 2, 2))
  private val grid                              = get(Grid.over(frame, 2, 2))
  private val clock                             = ClockId("counted-run")
  private val fixation                          = get(
    Event.Fixation.withoutDispersion(
      get(Interval.of(clock, Instant.micros(0), Instant.micros(1000))),
      Pt[Px](0.5, 0.5),
      1
    )
  )
  private val path  = get(Scanpath.of(frame, clock, IArray(fixation)))
  private val input = StudyInput(
    Trials(
      (0 until 20).toVector.flatMap(i =>
        Vector("recall", "encode").map(phase =>
          Trial(StudyKey("p", s"item-$i", phase), (), path)
        )
      )
    )
  )

  private def prepare(observe: () => Unit) =
    val layout = new StudyLayout[StudyKey](
      DefinitionId.studyLayout,
      Projection.named("participant")(key => { observe(); key.participant }),
      Projection.named("stimulus")(key => { observe(); key.stimulus }),
      Projection.named("phase")(_.phase)
    )
    val plan = get(
      StudyPlan.configure(
        input.reference,
        layout,
        StudyGeometry.WholeFrame(grid),
        "recall",
        "encode",
        Weight.Duration,
        Vector(StudyScale.Native(StudyEstimate.Binned[Px]())),
        None,
        FailurePolicy.RequireAll,
        StudyMethod.cosine[Px](DefinitionId.cosine),
        (),
        StudyPairing.default
      )
    )
    (plan, get(plan.prepare(input)))

  test("count pages do not regroup all source keys at matched completion") {
    var projections   = 0
    val (_, prepared) = prepare(() => projections += 1)
    val beforeBegin   = projections
    val first         = get(prepared.countWork)
    assertEquals(projections, beforeBegin)
    val quantum = WorkQuanta(get(PairQuantum.of(1)), ComparisonQuantum.default)
    @annotation.tailrec
    def loop(cursor: CountCursor[StudyKey]): StudyCounts[StudyKey] =
      val before = projections
      val step   = get(cursor.advance(quantum))
      assert(
        projections - before <= 8,
        s"one candidate page performed ${projections - before} source projections"
      )
      step match
        case WorkStep.More(_, _, next) => loop(next)
        case WorkStep.Done(_, counts)  => counts
    val counts = loop(first)
    assertEquals(counts.matched.eligiblePairs, 20L)
    assertEquals(counts.controls.eligiblePairs, 380L)
    assertEquals(counts.totalReductionKeys, 40L)
    assertEquals(counts.totalContrastRows, 20L)
    assertEquals(counts.cardinality.multiple, get(prepared.matchedCardinality).multiple)
  }

  test("completed-count startup performs no pair traversal and rejects another preparation") {
    var projections      = 0
    val (plan, prepared) = prepare(() => projections += 1)
    val counts           = get(prepared.counts)
    val before           = projections
    val started          = get(prepared.countedWork(counts))
    assertEquals(projections, before)
    assertEquals(started.stage, StudyStage.Estimating(0, 0))
    val other = get(plan.prepare(input))
    assert(other.countedWork(counts).left.exists {
      case PlanError.ChangedPreparedPlan(method, layout) =>
        method == plan.method.id && layout == plan.layout.id
      case _ => false
    })
    // The compatibility preview may still use equivalent completed counts.
    assert(other.preview(counts).isRight)
    assert(prepared.countedWork(get(other.counts)).isLeft)
  }
