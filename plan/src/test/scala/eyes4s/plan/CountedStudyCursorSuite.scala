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

  test("refusal assembly is paged with no ordering or digest callbacks after preparation") {
    def key(phase: String, label: String, item: String) =
      get(TrialKey.of("p", phase, label, TrialOccurrence.first, item))
    val unmatched = (0 until 40).reverse.toVector.map(i => key("recall", s"q-$i", s"item-$i"))
    val ambiguous = (0 until 12).reverse.toVector.flatMap { i =>
      Vector(
        key("recall", s"q-$i", s"item-$i"),
        key("encode", s"a-$i", s"item-$i"),
        key("encode", s"b-$i", s"item-$i")
      )
    }
    val conflicts = (0 until 12).reverse.toVector.flatMap { i =>
      Vector(key("recall", s"q-$i", s"item-$i"), key("recall", s"q-$i", s"other-$i"))
    }
    Vector(unmatched, ambiguous, conflicts).foreach { keys =>
      val base        = TrialKey.layout(TrialKeyDefinitions.trialLayout)
      var projections = 0
      var ordering    = 0
      var digests     = 0
      def observed[A](p: Projection[TrialKey, A]): Projection[TrialKey, A] =
        Projection.named("observed")(k => { projections += 1; p(k) })
      val digest = new KeyDigest[TrialKey]:
        def digest(k: TrialKey): ContentHash = { digests += 1; base.digest.digest(k) }
      val order = new Ordering[TrialKey]:
        def compare(a: TrialKey, b: TrialKey): Int = {
          ordering += 1; base.ordering.compare(a, b)
        }
      val layout = new StudyLayout(
        base.id,
        observed(base.participant),
        observed(base.stimulus),
        observed(base.phase),
        base.occurrence.map(observed),
        base.trial.map(observed)
      )(using digest, order)
      val source     = StudyInput(Trials(keys.map(k => Trial(k, (), path))))
      val configured = get(
        StudyPlan.configure(
          source.reference,
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
          StudyPairing.default.copy(unmatched = UnmatchedFocalPolicy.Refuse)
        )
      )
      val prepared = get(configured.prepare(source))
      val expected = get(prepared.matchedCardinality).refusal(layout)
      assert(expected.nonEmpty)
      if keys == unmatched then
        assertEquals(
          expected,
          Some(
            PlanError.UnmatchedFocalRefused(
              unmatched.sorted(using base.ordering).map(k => base.digest.digest(k).render)
            )
          )
        )
      Vector(1, 3, 64).foreach { size =>
        val quanta = WorkQuanta(get(PairQuantum.of(size)), ComparisonQuantum.default)
        projections = 0
        ordering = 0
        digests = 0
        var diagnosticSteps = 0
        @annotation.tailrec
        def loop(cursor: CountCursor[TrialKey]): StudyCounts[TrialKey] =
          val before = projections
          val step   = get(cursor.advance(quanta))
          assert(projections - before <= 8 * size)
          assertEquals(ordering, 0)
          assertEquals(digests, 0)
          step match
            case WorkStep.More(_, units, next) =>
              assert(units <= size)
              if next.visited == cursor.visited && units > 0 then diagnosticSteps += 1
              loop(next)
            case WorkStep.Done(units, counts) =>
              assert(units <= size)
              counts
        val counts = loop(get(prepared.countWork))
        assertEquals(counts.pairingRefusal, expected)
        if keys == unmatched then assert(diagnosticSteps > 0)
        val beforeStartup = (projections, ordering, digests)
        assertEquals(prepared.countedWork(counts).left.toOption, expected)
        assertEquals((projections, ordering, digests), beforeStartup)
        @annotation.tailrec
        def composite(
            cursor: CountedStudyCursor[TrialKey, Px, Unit, Similarity, SignedDifference]
        ): StudyRunError =
          cursor.advance(quanta) match
            case Left(error)                          => error
            case Right(WorkStep.More(stage, _, next)) =>
              assert(stage.isInstanceOf[StudyRunStage.Counting])
              composite(next)
            case Right(WorkStep.Done(_, _)) => fail("invalid pairing produced a result")
        assertEquals(
          composite(get(CountedStudyCursor.of(prepared))),
          StudyRunError.Plan(expected.get)
        )
        assertEquals(ordering, 0)
        assertEquals(digests, 0)
      }
    }
  }

  test("composite refusal precedes a control selected-pair budget failure") {
    def key(phase: String, label: String, item: String) =
      get(TrialKey.of("p", phase, label, TrialOccurrence.first, item))
    val focal = key("recall", "q", "a")
    val keys  = Vector(
      focal,
      key("encode", "a1", "a"),
      key("encode", "a2", "a"),
      key("encode", "b", "b"),
      key("encode", "c", "c"),
      key("encode", "d", "d")
    )
    val source     = StudyInput(Trials(keys.map(k => Trial(k, (), path))))
    val configured = get(
      StudyPlan.configure(
        source.reference,
        TrialKey.layout(TrialKeyDefinitions.trialLayout),
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
    val prepared = get(configured.prepare(source, get(PairScheduleBudget.of(6, 100L, 2))))
    Vector(1, 3, 64).foreach { size =>
      val quanta   = WorkQuanta(get(PairQuantum.of(size)), ComparisonQuantum.default)
      val expected = prepared.work().left.toOption.map(StudyRunError.Plan(_))
      val result   = Stepwise.complete(get(CountedStudyCursor.of(prepared)), quanta)
      assertEquals(result.left.toOption, expected)
      assert(expected.exists {
        case StudyRunError.Plan(
              PlanError.MatchedCardinality(MatchedReferences.RequireOne, ks, _)
            ) =>
          ks == Vector(KeyDigest[TrialKey].digest(focal).render)
        case _ => false
      })
    }
  }
