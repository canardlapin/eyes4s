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

package consumer

import eyes4s.design.*
import eyes4s.kernel.*
import cats.kernel.Order

class PublicDesignSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val trials                            = Trials(
    Vector(Trial("a", "group", 2.0), Trial("b", "group", 4.0), Trial("c", "group", 8.0))
  )
  private val inputs = ContentHash.ofString("public-design")
  private val info   = EvaluationInfo("difference", EvaluationScale.Unitless)

  test(
    "reconstruction preserves directed and undirected orientations and refuses dropped rows"
  ) {
    val directed =
      evaluatePairs(pair(trials, trials, PairDesign.between[String, String].all), inputs, info)(
        (a, b) => Right(a + b): Either[String, Double]
      )
    val right         = directed.meanByRight(FailurePolicy.RequireAll)
    val restoredRight = get(
      Analysis.reconstructByRight(right.entries, right.diagnostics, right.provenance, directed)
    )
    assertEquals(restoredRight.rows, right.rows)
    assert(
      Analysis
        .reconstructByRight(
          right.entries.drop(1),
          right.diagnostics,
          right.provenance,
          directed
        )
        .isLeft
    )
    val undirected = evaluatePairs(
      pair(trials, Pairing.within[String].excludingSelf.canonicalUndirected),
      inputs,
      info
    )(SymmetricEvaluator[Double, String, Double]((a, b) => Right(math.abs(a - b))))
    val edges = undirected.meanEdges(FailurePolicy.RequireAll)
    assertEquals(edges.rows, Vector(() -> Right(4.0)))
    assertEquals(
      get(
        Analysis.reconstructEdges(
          edges.entries,
          edges.diagnostics,
          edges.provenance,
          undirected
        )
      ).rows,
      edges.rows
    )
    assert(
      Analysis
        .reconstructEdges(Vector.empty, edges.diagnostics, edges.provenance, undirected)
        .isLeft
    )
    val endpoints = undirected.meanByEndpoint(FailurePolicy.RequireAll)
    assertEquals(
      get(
        Analysis.reconstructByEndpoint(
          endpoints.entries,
          endpoints.diagnostics,
          endpoints.provenance,
          undirected
        )
      ).rows,
      endpoints.rows
    )
    assert(
      Analysis
        .reconstructByEndpoint(
          endpoints.entries.reverse,
          endpoints.diagnostics,
          endpoints.provenance,
          undirected
        )
        .isLeft
    )
  }

  test(
    "schedule inspection retains declared budgets duplicate evidence and source-space counts"
  ) {
    val budget   = get(PairScheduleBudget.of(10, 100, 100))
    val directed = get(
      DirectedPairSchedule.exhaustive(
        Vector("a", "b"),
        Vector("c"),
        Relation.all[String, String],
        budget
      )
    )
    assertEquals(directed.budget, budget)
    val within = get(
      WithinPairSchedule.directed(
        Vector("a", "a", "b"),
        Relation.all[String, String],
        SelfPolicy.Exclude,
        budget
      )
    )
    assertEquals(within.budget, budget)
    // Both ambiguous a occurrences are excluded; the sole usable b yields one candidate before self exclusion.
    assertEquals(within.candidatePairCount, 1L)
    assert(within.ambiguities.nonEmpty)
  }

  test("failed transformations keep metadata and values while identities hash structurally") {
    val (_, failures) = trials.traverseV(v => if v == 4 then Left("bad") else Right(v * 2))
    assertEquals(failures.map(f => (f.key, f.meta, f.value)), Vector(("b", "group", 4.0)))
    val frame    = get(Frame.screen("design-frame", 2, 2))
    def geometry = EvaluationGeometry.inFrame(frame)
    assertEquals(geometry.hashCode, geometry.hashCode)
    def spec = get(
      EvaluationSpec.of(
        "method",
        "1",
        Vector.empty,
        Vector("score"),
        geometry,
        EvaluationTime.OrderFree
      )
    )
    assertEquals(spec.hashCode, spec.hashCode); assert(Set(spec).contains(spec))
    assert(summon[Order[Seed]].lt(Seed(1), Seed(2)))
    assertEquals(get(PredictorId.of("predictor-X")).toString, "predictor-X")
    assert(PredictorId.of(" ").left.exists(_.message.contains("' '")))
    assert(SampleQuantum.of(-17).left.exists(_.message.contains("-17")))
    val underlying = PairScheduleError.InvalidCounts(-17, 19)
    assertEquals(EvaluationWorkError.Schedule(underlying).message, underlying.message)
    val mean = RepetitionMeanError.Incomplete("scales-X", 17, 3, 9).message
    Vector("scales-X", "17", "3", "9").foreach(v => assert(mean.contains(v)))
  }
