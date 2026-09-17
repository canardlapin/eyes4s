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

package eyes4s.codec

import eyes4s.compare.*
import eyes4s.design.*
import eyes4s.examples.MatchedControlFixtures
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

/** The frozen result-v1 payload pins the archived meaning of the pinned
  * study-v1 plan run on the pinned study-input-v1 input, independently of
  * the encoder, and a re-execution reproduces it bit for bit.
  */
class ResultV1Suite extends munit.FunSuite:
  private val OracleTolerance               = 1e-12
  private def get[E, A](e: Either[E, A]): A = e.fold(error => fail(s"$error"), identity)
  private val codec                         = StudyResultCodecs.cosine[Px]
  private def sameSimilarity(a: Similarity, b: Similarity): Boolean = a.value == b.value
  private def sameDifference(a: SignedDifference, b: SignedDifference): Boolean =
    a.value == b.value

  test(
    "frozen study-result v1 fixes the identity, densities, pair rows, denominators, provenance and contrast"
  ) {
    val result = get(codec.codec.parse(StudyResultFixtures.resultVersionOne))
    val plan   = get(StudyCodecs.cosine[Px].codec.parse(SavedStudyFixtures.versionOne))
    assertEquals(result.input.digest, "cebe7474ab5c2aec")
    assertEquals(result.input, plan.input)
    assertEquals(result.description, plan.description)
    assertEquals(result.scales.size, 1)
    val scale = result.scales.head
    assertEquals(scale.estimate, StudyEstimate.Binned())
    assertEquals(scale.excludedPhases, Vector.empty)
    assertEquals(scale.estimation.size, 12)
    assertEquals(scale.estimation.map(_._1), scale.estimation.map(_._1).sorted)
    scale.estimation.foreach { case (key, outcome) =>
      val mass = get(outcome.left.map(_.message))
      assertEquals(mass.grid, plan.grid, key)
      assertEquals(mass.provenance.steps.map(_.operation), Vector("normalise"))
    }
    val first = get(scale.estimation.head._2.left.map(_.message))
    assertEquals(
      first.values.toVector,
      Vector(0.11111111111111112, 0.22222222222222224, 0.22222222222222224, 0.4444444444444445)
    )
    assertEquals(first.provenance.inputs.render, "5dd8999b58bda362")

    val contrast = get(scale.contrast)
    assertEquals(scale.analyses.matchedSource.rows.size, 6)
    assertEquals(scale.analyses.controlSource.rows.size, 12)
    assert(scale.analyses.matchedSource.rows.forall(_.result.isRight))
    assert(scale.analyses.controlSource.rows.forall(_.result.isRight))
    assertEquals(
      get(scale.analyses.matchedSource.rows.head.result.left.map(_.message)).value,
      1.0
    )
    assertEquals(
      scale.analyses.matchedSource.rows.head.left -> scale.analyses.matchedSource.rows.head.right,
      StudyKey("s1", "a", "recall") -> StudyKey("s1", "a", "encode")
    )
    assertEquals(scale.analyses.matchedSource.diagnostics.eligiblePairCount, 6L)
    assertEquals(scale.analyses.matchedSource.diagnostics.selectedPairCount, 6)
    assertEquals(
      scale.analyses.matchedSource.diagnostics.pairSpace,
      PairSpace.BetweenDirected(
        "(participant == participant and stimulus == stimulus)",
        Selection.All
      )
    )
    assertEquals(scale.analyses.controlSource.diagnostics.eligiblePairCount, 12L)
    assertEquals(contrast.matched.entries.map(_.contributing), Vector.fill(6)(1))
    assertEquals(contrast.control.entries.map(_.contributing), Vector.fill(6)(2))
    assertEquals(contrast.matched.diagnostics.contributionCount, 6)
    assertEquals(contrast.control.diagnostics.contributionCount, 12)
    assertEquals(contrast.matched.diagnostics.reducedKeyCount, 6)
    assertEquals(contrast.matched.diagnostics.failedKeys, Vector.empty)
    assertEquals(contrast.matched.diagnostics.policy, FailurePolicy.RequireAll)
    assertEquals(contrast.matched.diagnostics.orientation, ReductionOrientation.ByLeft)
    assertEquals(
      contrast.matched.provenance.steps.map(_.operation),
      Vector(
        "pair",
        "evaluatePairs",
        "evaluationMethod",
        "evaluationParameters",
        "evaluationComponents",
        "evaluationDomain",
        "reducePairs"
      )
    )
    assertEquals(contrast.matched.provenance.inputs.render, "cebe7474ab5c2aec")
    assertEquals(
      scale.analyses.matchedSource.provenance,
      contrast.matched.provenance.copy(steps = contrast.matched.provenance.steps.dropRight(1))
    )
    assert(contrast.matched eq scale.analyses.matched)
    assert(contrast.control eq scale.analyses.control)
    assertEquals(contrast.matched.evaluation.name, "cosine")
    assertEquals(
      contrast.matched.evaluation.scale,
      EvaluationScale.Measure(MeasureScale.Bounded(0, 1))
    )
    val spec = get(contrast.matched.evaluation.specification.toRight("specification"))
    assertEquals(spec.method, "eyes4s.cosine")
    assertEquals(spec.components, Vector("value"))
    assertEquals(spec.geometry, EvaluationGeometry.onGrid(plan.grid))
    assertEquals(spec.time, EvaluationTime.OrderFree)

    assertEquals(contrast.rows.size, 6)
    assertEquals(get(contrast.rows.head.difference.left.map(_.message)).value, 0.28)
    contrast.rows.zip(MatchedControlFixtures.reductions).foreach { case (row, expected) =>
      assertEquals(s"${row.key.participant}/${row.key.stimulus}/${row.key.phase}", expected.id)
      assertEqualsDouble(
        get(row.difference.left.map(_.message)).value,
        expected.difference,
        OracleTolerance
      )
      assertEquals(row.matched.map(_.contributing), Some(1))
      assertEquals(row.control.map(_.contributing), Some(2))
    }
    assertEquals(
      get(codec.codec.encode(result)),
      get(io.circe.parser.parse(StudyResultFixtures.resultVersionOne))
    )
  }

  test("re-executing the pinned plan on the pinned input reproduces the archive bit for bit") {
    val input = get(StudyInputCodecs.study[Px].input.parse(StudyInputFixtures.inputVersionOne))
    val plan  = get(StudyCodecs.cosine[Px].codec.parse(SavedStudyFixtures.versionOne))
    val rerun = get(plan.run(input))
    val decoded = get(codec.codec.parse(StudyResultFixtures.resultVersionOne))
    assert(ResultEquivalence.same(rerun, decoded)(sameSimilarity, sameDifference))
    assertEquals(
      get(codec.codec.encode(rerun)),
      get(io.circe.parser.parse(StudyResultFixtures.resultVersionOne))
    )
    val bits = (r: StudyResult[StudyKey, Px, Similarity, SignedDifference]) =>
      get(r.scales.head.contrast).rows.map(row =>
        java.lang.Double.doubleToLongBits(get(row.difference.left.map(_.message)).value)
      )
    assertEquals(bits(rerun), bits(decoded))
  }
