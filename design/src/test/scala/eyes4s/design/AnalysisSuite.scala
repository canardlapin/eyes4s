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

package eyes4s.design

import eyes4s.compare.*
import eyes4s.kernel.ContentHash

import scala.compiletime.testing.typeCheckErrors

class AnalysisSuite extends munit.FunSuite:

  final case class Key(id: String) derives CanEqual
  final case class Meta(group: String) derives CanEqual

  private given KeyDigest[Key] = KeyDigest.derived[Key]

  private val info =
    EvaluationInfo("sum", EvaluationScale.Unitless)
  private val inputs =
    ContentHash.ofString("analysis-suite-inputs")

  private def trial(id: String, value: Double): Trial[Key, Meta, Double] =
    Trial(Key(id), Meta("group"), value)

  private def directedPairs: DirectedPaired[Key, Meta, Key, Meta, Double, Double] =
    val left  = Trials(Vector(trial("l1", 10.0), trial("l2", 20.0)))
    val right = Trials(Vector(trial("r1", 1.0), trial("r2", 2.0)))
    pair(left, right, Pairing.between[Key, Key].all)

  test("evaluatePairs retains both keys, typed failures, and realized counts") {
    val result = evaluatePairs(directedPairs, inputs, info) { (left, right) =>
      Either.cond(right == 1.0, left + right, "right operand rejected")
    }

    assertEquals(result.rows.size, 4)
    assertEquals(
      result.rows,
      Vector(
        PairScore(Key("l1"), Key("r1"), Right(11.0)),
        PairScore(Key("l1"), Key("r2"), Left("right operand rejected")),
        PairScore(Key("l2"), Key("r1"), Right(21.0)),
        PairScore(Key("l2"), Key("r2"), Left("right operand rejected"))
      )
    )
    assertEquals(result.diagnostics.eligiblePairCount, 4L)
    assertEquals(result.diagnostics.selectedPairCount, 4)
    assertEquals(result.provenance.inputs, inputs)
    assert(result.provenance.render.contains("evaluator=sum"))
    assert(result.provenance.render.contains("successful=2"))
    assert(result.provenance.render.contains("failed=2"))
  }

  test("meanByLeft and meanByRight name different orientations") {
    val evaluated =
      evaluatePairs(directedPairs, inputs, info)((left, right) => Right(left + right))

    val byLeft  = evaluated.meanByLeft(FailurePolicy.RequireAll)
    val byRight = evaluated.meanByRight(FailurePolicy.RequireAll)

    assertEquals(
      byLeft.rows,
      Vector(Key("l1") -> Right(11.5), Key("l2") -> Right(21.5))
    )
    assertEquals(
      byRight.rows,
      Vector(Key("r1") -> Right(16.0), Key("r2") -> Right(17.0))
    )
    assertEquals(byLeft.diagnostics.orientation, ReductionOrientation.ByLeft)
    assertEquals(byRight.diagnostics.orientation, ReductionOrientation.ByRight)
    assertEquals(byLeft.diagnostics.contributionCount, 4)
  }

  test("RequireAll never silently discards failed scores") {
    val evaluated = evaluatePairs(directedPairs, inputs, info) { (left, right) =>
      Either.cond(right == 1.0, left + right, "right operand rejected")
    }
    val reduced = evaluated.meanByLeft(FailurePolicy.RequireAll)

    assertEquals(reduced.diagnostics.successfulPairCount, 2)
    assertEquals(reduced.diagnostics.failedPairCount, 2)
    assertEquals(reduced.diagnostics.failedKeys, Vector(Key("l1"), Key("l2")))
    reduced.rows.foreach {
      case (
            key,
            Left(ReductionError.FailedScores(errorKey, successful, failed))
          ) =>
        assertEquals(errorKey, key)
        assertEquals(successful, 1)
        assertEquals(failed, 1)
      case other => fail(s"unexpected RequireAll result: $other")
    }
  }

  test("SuccessfulOnly states and enforces its minimum") {
    val evaluated = evaluatePairs(directedPairs, inputs, info) { (left, right) =>
      Either.cond(right == 1.0, left + right, "right operand rejected")
    }
    val atLeastOne = FailurePolicy.successfulOnly(1).toOption.get
    val atLeastTwo = FailurePolicy.successfulOnly(2).toOption.get

    assertEquals(
      evaluated.meanByLeft(atLeastOne).rows,
      Vector(Key("l1") -> Right(11.0), Key("l2") -> Right(21.0))
    )

    evaluated.meanByLeft(atLeastTwo).rows.foreach {
      case (
            _,
            Left(
              ReductionError.InsufficientSuccessful(
                _,
                required,
                successful,
                failed
              )
            )
          ) =>
        assertEquals(required, 2)
        assertEquals(successful, 1)
        assertEquals(failed, 1)
      case other => fail(s"unexpected SuccessfulOnly result: $other")
    }
  }

  test("SuccessfulOnly cannot represent a non-positive threshold") {
    assertEquals(
      FailurePolicy.successfulOnly(0),
      Left(ReductionPolicyError.NonPositiveMinimumSuccessful(0))
    )
    assert(FailurePolicy.successfulOnly(-2).isLeft)

    val errors = typeCheckErrors("""
      import eyes4s.design.*
      FailurePolicy.SuccessfulOnly(0)
    """)
    assert(errors.nonEmpty, "SuccessfulOnly accepted an unvalidated raw Int")
  }

  test("unmatched keys become explicit reduction rows") {
    val left =
      Trials(Vector(trial("matched", 10.0), trial("unmatched", 20.0)))
    val right     = Trials.one(Key("matched"), Meta("group"), 1.0)
    val design    = Pairing.matched[Key]
    val evaluated =
      evaluatePairs(pair(left, right, design), inputs, info)((a, b) => Right(a + b))

    val reduced = evaluated.meanByLeft(FailurePolicy.RequireAll)

    assertEquals(reduced.rows.head, Key("matched") -> Right(11.0))
    assertEquals(
      reduced.rows(1),
      Key("unmatched") -> Left(ReductionError.NoSelectedScores(Key("unmatched")))
    )
  }

  test("ambiguous keys become explicit reduction failures") {
    val left = Trials(
      Vector(
        Trial(Key("duplicate"), Meta("first"), 10.0),
        Trial(Key("duplicate"), Meta("second"), 20.0)
      )
    )
    val right     = Trials.one(Key("duplicate"), Meta("right"), 1.0)
    val evaluated =
      evaluatePairs(pair(left, right, Pairing.matched[Key]), inputs, info)((a, b) =>
        Right(a + b)
      )

    assertEquals(
      evaluated.meanByLeft(FailurePolicy.RequireAll).rows,
      Vector(
        Key("duplicate") ->
          Left(ReductionError.AmbiguousKey(Key("duplicate"), Vector(0, 1)))
      )
    )
  }

  test("canonical-undirected evaluation requires explicit symmetry evidence") {
    val trials = Trials(
      Vector(trial("a", 2.0), trial("b", 4.0), trial("c", 8.0))
    )
    val paired =
      pair(trials, Pairing.within[Key].excludingSelf.canonicalUndirected)
    val evaluator = SymmetricEvaluator[Double, String, Double] { (left, right) =>
      Right(math.abs(left - right))
    }

    val result = evaluatePairs(paired, inputs, info)(evaluator)
    assertEquals(result.rows.map(_.result), Vector(Right(2.0), Right(6.0), Right(4.0)))

    val errors = typeCheckErrors("""
      import eyes4s.design.*
      import eyes4s.kernel.ContentHash
      val trials = Trials(Vector(Trial(1, (), 1.0), Trial(2, (), 2.0)))
      val paired = pair(trials, Pairing.within[Int].canonicalUndirected)
      val info = EvaluationInfo("plain", EvaluationScale.Unitless)
      evaluatePairs(paired, ContentHash.empty, info) {
        (left: Double, right: Double) => Right(left + right): Either[String, Double]
      }
    """)
    assert(errors.nonEmpty, "an undirected analysis accepted an unmarked function")
  }

  test("meanEdges counts each canonical edge once") {
    val trials = Trials(
      Vector(trial("a", 2.0), trial("b", 4.0), trial("c", 8.0))
    )
    val paired =
      pair(trials, Pairing.within[Key].excludingSelf.canonicalUndirected)
    val evaluated = evaluatePairs(paired, inputs, info)(
      SymmetricEvaluator[Double, Nothing, Double]((left, right) =>
        Right(math.abs(left - right))
      )
    )

    val reduced = evaluated.meanEdges(FailurePolicy.RequireAll)

    assertEquals(reduced.rows, Vector(() -> Right(4.0)))
    assertEquals(reduced.diagnostics.orientation, ReductionOrientation.EdgesOnce)
    assertEquals(reduced.diagnostics.selectedPairCount, 3)
    assertEquals(reduced.diagnostics.contributionCount, 3)
  }

  test("meanEdges reports an empty canonical edge set as data") {
    val trials = Trials.one(Key("only"), Meta("group"), 2.0)
    val paired =
      pair(trials, Pairing.within[Key].excludingSelf.canonicalUndirected)
    val evaluated = evaluatePairs(paired, inputs, info)(
      SymmetricEvaluator[Double, Nothing, Double]((left, right) =>
        Right(math.abs(left - right))
      )
    )

    assertEquals(
      evaluated.meanEdges(FailurePolicy.RequireAll).rows,
      Vector(() -> Left(ReductionError.NoSelectedScores(())))
    )
  }

  test("meanByEndpoint explicitly mirrors each canonical edge") {
    val trials = Trials(
      Vector(trial("a", 2.0), trial("b", 4.0), trial("c", 8.0))
    )
    val paired =
      pair(trials, Pairing.within[Key].excludingSelf.canonicalUndirected)
    val evaluated = evaluatePairs(paired, inputs, info)(
      SymmetricEvaluator[Double, Nothing, Double]((left, right) =>
        Right(math.abs(left - right))
      )
    )

    val reduced = evaluated.meanByEndpoint(FailurePolicy.RequireAll)

    assertEquals(
      reduced.rows,
      Vector(
        Key("a") -> Right(4.0),
        Key("b") -> Right(3.0),
        Key("c") -> Right(5.0)
      )
    )
    assertEquals(
      reduced.diagnostics.orientation,
      ReductionOrientation.MirroredEndpoints
    )
    assertEquals(reduced.diagnostics.contributionCount, 6)
  }

  test("orientation-specific reductions are absent from the wrong result type") {
    val errors = typeCheckErrors("""
      import eyes4s.design.*
      val directed: DirectedPairwiseAnalysis[Int, Int, String, Double] = ???
      val undirected: UndirectedPairwiseAnalysis[Int, String, Double] = ???
      directed.meanEdges(FailurePolicy.RequireAll)
      undirected.meanByLeft(FailurePolicy.RequireAll)
    """)

    assert(errors.size >= 2)
  }

  test("ScoreMean instances preserve score types and MultiMatch fields") {
    assertEquals(
      ScoreMean[MeasureDistance]
        .mean(Vector(MeasureDistance(2.0), MeasureDistance(4.0)))
        .map(_.value),
      Right(3.0)
    )
    assertEquals(
      ScoreMean[Similarity]
        .mean(Vector(Similarity(0.2), Similarity(0.6)))
        .map(_.value),
      Right(0.4)
    )

    val first     = MultiMatchScore(0.1, 0.2, 0.3, 0.4, 0.5)
    val second    = MultiMatchScore(0.3, 0.4, 0.5, 0.6, 0.7)
    val averaged  = ScoreMean[MultiMatchScore].mean(Vector(first, second)).toOption.get
    val tolerance = 1e-12
    assertEqualsDouble(averaged.shape, 0.2, tolerance)
    assertEqualsDouble(averaged.direction, 0.3, tolerance)
    assertEqualsDouble(averaged.length, 0.4, tolerance)
    assertEqualsDouble(averaged.position, 0.5, tolerance)
    assertEqualsDouble(averaged.duration, 0.6, tolerance)
  }

  test("non-finite scores are reduction failures, not NaN results") {
    val evaluated =
      evaluatePairs(directedPairs, inputs, info)((_, _) => Right(Double.NaN))
    val reduced = evaluated.meanByLeft(FailurePolicy.RequireAll)

    reduced.rows.foreach {
      case (
            key,
            Left(
              ReductionError.MeanFailure(
                errorKey,
                ScoreMeanError.NonFiniteValue("value", 0, value)
              )
            )
          ) =>
        assertEquals(errorKey, key)
        assert(value.isNaN)
      case other => fail(s"unexpected non-finite reduction: $other")
    }
  }

  test("comparison overload records measure identity and scale") {
    val comparison = new SymmetricCompare[Double, Similarity]:
      val info = MeasureInfo(
        "absolute agreement",
        "one minus absolute difference",
        MeasureScale.Bounded(0.0, 1.0),
        None
      )

      def compare(left: Double, right: Double): Either[CompareError, Similarity] =
        Right(Similarity(1.0 - math.abs(left - right)))

    val trials = Trials(Vector(trial("a", 0.2), trial("b", 0.6)))
    val paired =
      pair(trials, Pairing.within[Key].excludingSelf.canonicalUndirected)
    val evaluated = evaluatePairs(paired, inputs, comparison)
    val tolerance = 1e-12

    assertEquals(evaluated.rows.size, 1)
    assertEqualsDouble(evaluated.rows.head.result.toOption.get.value, 0.6, tolerance)
    assert(evaluated.provenance.render.contains("evaluator=absolute agreement"))
    assert(evaluated.provenance.render.contains("scale=bounded [0.00, 1.00]"))
  }

  test("input identity and bottom-k policy participate in provenance") {
    val left   = Trials.one(Key("target"), Meta("group"), 10.0)
    val right  = Trials(Vector(trial("a", 1.0), trial("b", 2.0), trial("c", 3.0)))
    val design =
      Pairing
        .between[Key, Key]
        .bottomK(2, Seed(99L), SampleId("baseline"))
        .toOption
        .get
    val paired = pair(left, right, design)
    val first  =
      evaluatePairs(paired, ContentHash.ofString("input-a"), info)((a, b) => Right(a + b))
    val second =
      evaluatePairs(paired, ContentHash.ofString("input-b"), info)((a, b) => Right(a + b))

    assertNotEquals(first.provenance.digest, second.provenance.digest)
    assert(first.provenance.render.contains("selection=bottom-k"))
    assert(first.provenance.render.contains("seed=99"))
    assert(first.provenance.render.contains("sampleId=baseline"))
    assert(first.provenance.render.contains("cap=2"))
  }

end AnalysisSuite
