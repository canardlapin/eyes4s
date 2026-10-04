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
  private def get[E, A](e: Either[E, A]): A        = e.fold(e => fail(s"$e"), identity)
  private val frame                                = get(Frame.screen("counting", 2, 2))
  private val grid                                 = get(Grid.over(frame, 2, 2))
  private def trial[K](key: K, offscreen: Boolean) =
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
    // The query without a match (q z) is not eligible (bead S0.7b).
    assertEquals(c.eligibleQueries, 2L)
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
    assertEquals(c.eligibleQueries, 1L)
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
    assertEquals(c.eligibleQueries, 2L)
    assertEquals(c.matched.eligiblePairs, 2L)
    assertEquals(c.controls.eligiblePairs, 4L)
  }

  test("a query without a match gets no control pairs and no score in either design") {
    // p x has encodings of other items but none of x: no match, so no controls.
    val ks = Vector(
      StudyKey("p", "a", "recall"),
      StudyKey("p", "x", "recall"),
      StudyKey("p", "a", "encode"),
      StudyKey("p", "b", "encode")
    )
    val (plan, work) = prepare(ks, scales = 2)
    val c            = get(work.counts)
    assertEquals(c.matched.eligiblePairs, 1L)
    assertEquals(c.controls.eligiblePairs, 1L)
    assertEquals(c.controls.focalWithPairs, 1L)
    // The control design leaves it out: it is not even an unmatched control key.
    assertEquals(c.controls.unmatchedFocal, 0L)
    assertEquals(c.eligibleQueries, 1L)
    assertEquals(c.pairRowsPerScale, 2L)
    // Both focal keys have a row in the matched reduction and the contrast;
    // only a is in the control reduction: (2 + 1) keys and 2 rows per scale.
    assertEquals((c.keysPerDesign, c.controlKeys), (2L, 1L))
    assertEquals((c.totalReductionKeys, c.totalContrastRows), (6L, 4L))
    assertEquals(c.cardinality.unmatched, Vector(ks(1)))
    val result = get(work.run)
    result.scales.foreach { scale =>
      assertEquals(scale.analyses.control.entries.map(_.key), Vector(ks(0)))
      assert(scale.analyses.control.entries.forall(_.result.isRight))
      // No score for the query without a match, in either design.
      assert(scale.analyses.matched.entries.forall(r => r.key == ks(0) || r.result.isLeft))
      val contrast = get(scale.contrast).rows.map(r => r.key -> r.difference.isRight).toMap
      assertEquals(contrast.get(ks(0)), Some(true))
      assert(!contrast.getOrElse(ks(1), false))
      assertEquals(scale.analyses.matchedSource.diagnostics.unmatchedLeft, Vector(ks(1)))
    }
    assertEquals(
      plan.preflight(Some(work.input)).findings,
      Vector(StudyFinding.UnmatchedFocal[StudyKey, Px](ks(1)))
    )
  }

  test("a query without a match before one with a match leaves its control scores unchanged") {
    // Controls are scored on the control design's own focal trials: an
    // unmatched query earlier in the input must not shift which maps they use.
    def keyed(withUnmatched: Boolean) =
      Option.when(withUnmatched)(StudyKey("p", "x", "recall")).toVector ++ Vector(
        StudyKey("p", "a", "recall"),
        StudyKey("p", "a", "encode"),
        StudyKey("p", "b", "encode")
      )
    def scores(keys: Vector[StudyKey], offscreen: Set[StudyKey]) =
      val input = StudyInput(Trials(keys.map(k => trial(k, offscreen(k)))))
      val plan  = get(
        StudyPlan.of(
          input.reference,
          StudyKey.layout(DefinitionId.studyLayout),
          grid,
          "recall",
          "encode",
          Weight.Duration,
          Vector(StudyEstimate.Binned()),
          FailurePolicy.RequireAll,
          StudyMethod.cosine[Px](DefinitionId.cosine),
          ()
        )
      )
      val result = get(get(plan.prepare(input)).run)
      result.scales.head.analyses.control.entries.map(r => r.key -> r.result).toMap
    // x's map fails (its fixation is off the frame), so reading it for a's
    // control pair would turn a's control score into a failure.
    val offscreen = Set(StudyKey("p", "x", "recall"))
    val alone     = scores(keyed(withUnmatched = false), offscreen)
    val beside    = scores(keyed(withUnmatched = true), offscreen)
    assert(alone.values.forall(_.isRight), alone.toString)
    assertEquals(beside, alone)
  }

  test("a query whose only reference is repeated has no match, so no controls") {
    val ks = Vector(
      StudyKey("p", "a", "recall"),
      StudyKey("p", "a", "encode"),
      StudyKey("p", "a", "encode"),
      StudyKey("p", "b", "encode")
    )
    val (_, work) = prepare(ks, scales = 1)
    val c         = get(work.counts)
    assertEquals((c.matched.eligiblePairs, c.controls.eligiblePairs), (0L, 0L))
    assertEquals(c.eligibleQueries, 0L)
  }

  test("matched cardinality remains available when only the control budget is exceeded") {
    val ks = Vector(
      StudyKey("p", "a", "recall"),
      StudyKey("p", "a", "encode"),
      StudyKey("p", "b", "encode"),
      StudyKey("p", "c", "encode")
    )
    val (plan, unrestricted) = prepare(ks, scales = 1)
    val limited = get(plan.prepare(unrestricted.input, get(PairScheduleBudget.of(4, 100L, 1))))
    assert(limited.counts.left.toOption.exists {
      case PlanError.Schedule(PairScheduleError.SelectedBudget(_, 2L, 1)) => true
      case _                                                              => false
    })
    val actual   = get(limited.matchedCardinality)
    val expected = get(unrestricted.matchedCardinality)
    assertEquals(actual.multiple, expected.multiple)
    assertEquals(actual.ambiguousReferences, expected.ambiguousReferences)
    assertEquals(actual.unmatched, expected.unmatched)
    assertEquals(actual.itemConflicts, expected.itemConflicts)
    assertEquals(get(get(limited.preview).matchedCardinality).multiple, expected.multiple)
  }

  test("matched ambiguity retains refusal precedence over an oversized control schedule") {
    def key(phase: String, label: String, item: String) =
      get(TrialKey.of("p", phase, label, TrialOccurrence.first, item))
    val focal = key("recall", "q", "a")
    val ks    = Vector(
      focal,
      key("encode", "a1", "a"),
      key("encode", "a2", "a"),
      key("encode", "b", "b"),
      key("encode", "c", "c"),
      key("encode", "d", "d")
    )
    val input = StudyInput(Trials(ks.map(k => trial(k, false))))
    val plan  = get(
      StudyPlan.configure(
        input.reference,
        TrialKey.layout(TrialKeyDefinitions.trialLayout),
        StudyGeometry.WholeFrame(grid),
        "recall",
        "encode",
        Weight.Duration,
        Vector(StudyScale.Native(StudyEstimate.Binned())),
        None,
        FailurePolicy.RequireAll,
        StudyMethod.cosine[Px](DefinitionId.cosine),
        (),
        StudyPairing.default
      )
    )
    val unlimited = get(plan.prepare(input))
    val limited   = get(plan.prepare(input, get(PairScheduleBudget.of(6, 100L, 2))))
    assert(limited.counts.left.toOption.exists {
      case PlanError.Schedule(PairScheduleError.SelectedBudget(_, 3L, 2)) => true
      case _                                                              => false
    })
    val expected = unlimited.work().left.toOption
    assert(expected.exists {
      case PlanError.MatchedCardinality(MatchedReferences.RequireOne, keys, _) =>
        keys == Vector(KeyDigest[TrialKey].digest(focal).render)
      case _ => false
    })
    assertEquals(limited.work().left.toOption, expected)
  }
