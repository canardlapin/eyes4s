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

import eyes4s.compare.ComparisonQuantum
import eyes4s.core.{Event, Scanpath, Weight}
import eyes4s.design.{FailurePolicy, KeyDigest, PairQuantum, Trial, Trials, WorkQuanta}
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px

class StudyQueryPairCountsSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val frame                             = get(Frame.screen("count-metadata", 2, 2))
  private val grid                              = get(Grid.over(frame, 2, 2))
  private val clock                             = ClockId("count-metadata")
  private val fixation                          = get(
    Event.Fixation.withoutDispersion(
      get(Interval.of(clock, Instant.micros(0), Instant.micros(1000))),
      Pt[Px](0.5, 0.5),
      1
    )
  )
  private val path = get(Scanpath.of(frame, clock, IArray(fixation)))

  private def prepared[K](keys: Vector[K], layout: StudyLayout[K], pairing: StudyPairing)(using
      KeyDigest[K]
  ) =
    val input = StudyInput(Trials(keys.map(key => Trial(key, (), path))))
    val plan  = get(
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
        pairing
      )
    )
    get(plan.prepare(input))

  private val a       = StudyKey("p", "a", "recall")
  private val b       = StudyKey("p", "b", "recall")
  private val missing = StudyKey("p", "missing", "recall")
  private val ar      = StudyKey("p", "a", "encode")
  private val br      = StudyKey("p", "b", "encode")
  private val keys    = Vector(
    a,
    b,
    missing,
    ar,
    br,
    StudyKey("p", "c", "encode"),
    StudyKey("q", "other", "excluded")
  )

  private def metadata[K](counts: StudyCounts[K]): Map[K, (Int, Option[K], Int)] =
    counts.queryPairs.view
      .mapValues(c => (c.matchedPairs, c.singleMatchedReference, c.controlPairs))
      .toMap

  test("completed cursor retains every focal key and exact matched/control operands") {
    val work   = prepared(keys, StudyKey.layout(DefinitionId.studyLayout), StudyPairing.default)
    val counts = get(
      Stepwise.complete(
        get(work.countWork),
        WorkQuanta(get(PairQuantum.of(1)), ComparisonQuantum.default)
      )
    )
    assertEquals(
      metadata(counts),
      Map(a -> (1, Some(ar), 2), b -> (1, Some(br), 2), missing -> (0, None, 0))
    )
    assertEquals(
      counts.queryPairs.values.map(_.matchedPairs.toLong).sum,
      counts.matched.eligiblePairs
    )
    assertEquals(
      counts.queryPairs.values.map(_.controlPairs.toLong).sum,
      counts.controls.eligiblePairs
    )
    assertEquals(
      counts.queryPairs.values.count(_.matchedPairs > 0).toLong,
      counts.eligibleQueries
    )
  }

  test("per-focal metadata is independent of page size and source order") {
    val work = prepared(keys, StudyKey.layout(DefinitionId.studyLayout), StudyPairing.default)
    def counted(quantum: Int) = get(
      Stepwise.complete(
        get(work.countWork),
        WorkQuanta(get(PairQuantum.of(quantum)), ComparisonQuantum.default)
      )
    )
    assertEquals(metadata(counted(1)), metadata(counted(1000)))
    val reversed =
      prepared(keys.reverse, StudyKey.layout(DefinitionId.studyLayout), StudyPairing.default)
    assertEquals(metadata(get(reversed.counts)), metadata(counted(1)))
  }

  test("multiple references retain cardinality without pretending one is unique") {
    def key(phase: String, trial: String, item: String) =
      get(TrialKey.of("p", phase, trial, TrialOccurrence.first, item))
    val query  = key("recall", "q", "a")
    val first  = key("encode", "e1", "a")
    val second = key("encode", "e2", "a")
    val other  = key("encode", "eb", "b")
    val work   = prepared(
      Vector(query, first, second, other),
      TrialKey.layout(TrialKeyDefinitions.trialLayout),
      StudyPairing.version1
    )
    val counts = get(
      Stepwise.complete(
        get(work.countWork),
        WorkQuanta(get(PairQuantum.of(1)), ComparisonQuantum.default)
      )
    )
    assertEquals(metadata(counts), Map(query -> (2, None, 1)))
    assertEquals(counts.cardinality.multiple, Vector(query -> Vector(first, second)))
  }

  test("ambiguous repeated focal keys keep an explicit zero entry; empty input stays empty") {
    val ambiguous =
      prepared(keys :+ a, StudyKey.layout(DefinitionId.studyLayout), StudyPairing.default)
    assertEquals(metadata(get(ambiguous.counts)).get(a), Some((0, None, 0)))
    val empty = prepared(
      Vector.empty[StudyKey],
      StudyKey.layout(DefinitionId.studyLayout),
      StudyPairing.default
    )
    assertEquals(get(empty.counts).queryPairs.size, 0)
  }
