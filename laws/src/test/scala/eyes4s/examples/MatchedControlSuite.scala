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

package eyes4s.examples

import eyes4s.compare.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.laws.Tolerance

class MatchedControlSuite extends munit.FunSuite:
  import MatchedControlExample.*
  import MatchedControlFixtures.{fixations, pairs, reductions}

  // Four-cell dot products and two-value reductions need only rounding allowance.
  private val ReferenceTolerance = Tolerance(absolute = 1e-12, relative = 0.0)
  private val study = prepare(fixations).fold(error => fail(error.message), identity)

  private def close(actual: Double, expected: Double): Unit =
    assert(ReferenceTolerance.approxEquals(actual, expected), s"$actual != $expected")

  private def successful(analysis: Analysis[StudyKey, Similarity]): Map[String, Double] =
    analysis.rows.map { case (key, value) =>
      key.id -> value.fold(error => fail(error.message), _.value)
    }.toMap

  test("existing fixation summaries reach duration-weighted mass without a raw recording") {
    assertEquals(study.scanpaths.size, 12)
    assertEquals(study.scanpaths.rows.map(_.value.n).sum, 48)
    study.scanpaths.rows.foreach { trial =>
      assertEquals(trial.value.source, None)
      assertEquals(trial.value.sampleSupport, None)
      close(trial.value.dwellTotal.toSeconds, 0.9)
    }
    (study.sources.rows ++ study.references.rows).foreach { trial =>
      close(trial.value.sum, 1.0)
      val raw = fixations
        .filter(row =>
          row.participant == trial.key.participant && row.image == trial.key.image &&
            row.phase == trial.key.phase
        )
        .sortBy(_.ordinal)
      assertEquals(raw.size, 4)
      raw.zipWithIndex.foreach { case (row, cell) =>
        close(trial.value.unsafeAt(cell), row.durationMicros.toDouble / 900000.0)
      }
    }
  }

  test("selected pairs and reductions agree with rational enumeration and pinned eyesim") {
    Vector("matched" -> matchedDesign, "control" -> controlDesign).foreach {
      case (kind, design) =>
        val evaluated = evaluate(study, design)
        val expected  = pairs.filter(_.kind == kind)
        assertEquals(evaluated.rows.size, expected.size)
        assertEquals(evaluated.diagnostics.eligiblePairCount, expected.size.toLong)
        assertEquals(evaluated.diagnostics.selectedPairCount, expected.size)
        assert(evaluated.diagnostics.ambiguous.isEmpty)
        val actual = evaluated.rows.map(row => (row.left.id, row.right.id) -> row.result).toMap
        assertEquals(actual.keySet, expected.map(row => (row.source, row.reference)).toSet)
        expected.foreach { row =>
          close(
            actual((row.source, row.reference)).fold(e => fail(e.message), _.value),
            row.score
          )
        }
        val reduced = evaluated.meanByLeft(FailurePolicy.RequireAll)
        assertEquals(reduced.rows.size, 6)
        assertEquals(reduced.diagnostics.contributionCount, expected.size)
        assertEquals(reduced.diagnostics.failedPairCount, 0)
        assertEquals(reduced.diagnostics.orientation, ReductionOrientation.ByLeft)
        val scores = successful(reduced)
        reductions.foreach { row =>
          close(scores(row.id), if kind == "matched" then row.matched else row.control)
        }
    }
  }

  test("reordering source and reference trials preserves pair membership and keyed results") {
    val shuffled = study.copy(
      sources = Trials(study.sources.rows.reverse),
      references = Trials(study.references.rows.reverse)
    )
    Vector(matchedDesign, controlDesign).foreach { design =>
      val original  = evaluate(study, design)
      val reordered = evaluate(shuffled, design)
      assertEquals(
        reordered.rows.map(row => (row.left, row.right)).toSet,
        original.rows.map(row => (row.left, row.right)).toSet
      )
      val expected = successful(original.meanByLeft(FailurePolicy.RequireAll))
      successful(reordered.meanByLeft(FailurePolicy.RequireAll)).foreach { case (key, value) =>
        close(value, expected(key))
      }
    }
  }

  test("image-only matching is distinguishable from the intended participant-image relation") {
    val image          = Projection.named[StudyKey, String]("image")(_.image)
    val underspecified = Pairing.between[StudyKey, StudyKey].sameOn(image, image).all
    val incorrect      = evaluate(study, underspecified)
    assertEquals(incorrect.rows.size, 12)
    assert(incorrect.rows.exists(row => row.left.participant != row.right.participant))
    val scores = successful(incorrect.meanByLeft(FailurePolicy.RequireAll))
    assert(
      reductions.exists(row => !ReferenceTolerance.approxEquals(scores(row.id), row.matched))
    )
  }

  test("an unmatched focal trial survives as a named reduction failure") {
    val extra   = study.sources.rows(0).copy(key = StudyKey("s3", "a", "recall"))
    val altered = study.copy(sources = Trials(study.sources.rows :+ extra))
    Vector(matchedDesign, controlDesign).foreach { design =>
      val evaluated = evaluate(altered, design)
      assertEquals(evaluated.diagnostics.unmatchedLeft, Vector(extra.key))
      val reduced = evaluated.meanByLeft(FailurePolicy.RequireAll)
      assertEquals(reduced.rows.size, 7)
      assertEquals(
        reduced.rows.toMap.apply(extra.key),
        Left(ReductionError.NoSelectedScores(extra.key))
      )
    }
  }

  test("duplicate focal keys are excluded from evaluation and retained as ambiguity failures") {
    val duplicated = study.sources.rows(0)
    val altered    = study.copy(sources = Trials(study.sources.rows :+ duplicated))
    val evaluated  = evaluate(altered, matchedDesign)
    assertEquals(evaluated.rows.size, 5)
    assertEquals(
      evaluated.diagnostics.ambiguous,
      Vector(PairingAmbiguity.DuplicateLeft[StudyKey, StudyKey](duplicated.key, Vector(0, 6)))
    )
    val reduced = evaluated.meanByLeft(FailurePolicy.RequireAll)
    assertEquals(reduced.rows.size, 6)
    assertEquals(
      reduced.rows.toMap.apply(duplicated.key),
      Left(ReductionError.AmbiguousKey(duplicated.key, Vector(0, 6)))
    )
  }

  test("duplicate references are reported instead of selecting the first matching row") {
    val duplicated = study.references.rows(0)
    val altered    = study.copy(references = Trials(study.references.rows :+ duplicated))
    val evaluated  = evaluate(altered, matchedDesign)
    assertEquals(evaluated.rows.size, 5)
    assertEquals(
      evaluated.diagnostics.ambiguous,
      Vector(PairingAmbiguity.DuplicateRight[StudyKey, StudyKey](duplicated.key, Vector(0, 6)))
    )
    assertEquals(evaluated.diagnostics.unmatchedLeft, Vector(StudyKey("s1", "a", "recall")))
    assertEquals(evaluated.meanByLeft(FailurePolicy.RequireAll).rows.size, 6)
  }

  test(
    "incompatible reference geometry retains failed pairs and explicit reduction denominators"
  ) {
    val other    = prepare(fixations, "other-display").fold(e => fail(e.message), identity)
    val replaced = study.copy(references =
      Trials(
        study.references.rows.updated(0, other.references.rows(0))
      )
    )
    val evaluated = evaluate(replaced, controlDesign)
    assertEquals(evaluated.rows.size, 12)
    val failed = evaluated.rows.filter(_.result.isLeft)
    assertEquals(failed.size, 2)
    failed.foreach { row =>
      row.result match
        case Left(CompareError.Grids(error)) =>
          assert(error.message.contains("matched-control-display"))
          assert(error.message.contains("other-display"))
        case value => fail(s"expected named grid disagreement, got $value")
    }
    val strict = evaluated.meanByLeft(FailurePolicy.RequireAll)
    assertEquals(strict.rows.size, 6)
    assertEquals(strict.diagnostics.successfulPairCount, 10)
    assertEquals(strict.diagnostics.failedPairCount, 2)
    assertEquals(
      strict.diagnostics.failedKeys.toSet,
      Set(StudyKey("s1", "b", "recall"), StudyKey("s1", "c", "recall"))
    )
    val one        = FailurePolicy.successfulOnly(1).fold(e => fail(e.message), identity)
    val permissive = evaluated.meanByLeft(one)
    assertEquals(permissive.rows.size, 6)
    assertEquals(permissive.diagnostics.failedPairCount, 2)
    assertEquals(permissive.diagnostics.failedKeys, Vector.empty)
    val remaining = evaluated.rows
      .filter(_.left == StudyKey("s1", "b", "recall"))
      .flatMap(_.result.toOption)
    assertEquals(remaining.size, 1)
    close(successful(permissive)("s1/b/recall"), remaining(0).value)
  }

  test("constant Pearson is undefined despite the reference returning one") {
    val uniform = Surface
      .mass(study.grid, IArray(0.25, 0.25, 0.25, 0.25), study.sources.rows(0).value.provenance)
      .fold(e => fail(e.message), identity)
    assertEquals(
      Distribution.pearson[Px].compare(uniform, uniform),
      Left(CompareError.ConstantInput("Pearson correlation", CompareOperand.Both))
    )
  }

  test(
    "structured score means preserve components and their signed difference needs a new result type"
  ) {
    import MatchedControlFixtures.{
      structuredControls,
      structuredControlMean,
      structuredDifference
    }
    val scores = structuredControls.map { row =>
      MultiMatchScore
        .of(row.shape, row.direction, row.length, row.position, row.duration)
        .fold(e => fail(e.message), identity)
    }
    val mean = ScoreMean[MultiMatchScore].mean(scores).fold(e => fail(e.message), identity)
    close(mean.shape, structuredControlMean.shape)
    close(mean.direction, structuredControlMean.direction)
    close(mean.length, structuredControlMean.length)
    close(mean.position, structuredControlMean.position)
    close(mean.duration, structuredControlMean.duration)
    val delta = structuredDifference
    assert(
      MultiMatchScore
        .of(delta.shape, delta.direction, delta.length, delta.position, delta.duration)
        .isLeft
    )
  }

  test("production contrast agrees with every pinned rational target and denominator") {
    val result = analyse(study).fold(e => fail(e.message), identity)
    assertEquals(result.rows.map(_.key.id), reductions.map(_.id))
    result.rows.zip(reductions).foreach { case (actual, expected) =>
      close(actual.difference.fold(e => fail(e.message), _.value), expected.difference)
      assertEquals(actual.matched.map(_.contributing), Some(1))
      assertEquals(actual.control.map(_.contributing), Some(2))
    }
    assertEquals(result.matched.source.rows.size, 6)
    assertEquals(result.control.source.rows.size, 12)
    val reordered = study.copy(
      sources = Trials(study.sources.rows.reverse),
      references = Trials(study.references.rows.reverse)
    )
    val again = analyse(reordered).fold(e => fail(e.message), identity)
    assertEquals(again.rows.map(_.key), result.rows.map(_.key))
    again.rows.zip(result.rows).foreach { case (a, b) =>
      close(
        a.difference.fold(e => fail(e.message), _.value),
        b.difference.fold(e => fail(e.message), _.value)
      )
    }
  }

  test("production structured contrast agrees with the independent five-component target") {
    import MatchedControlFixtures.{structuredMatched, structuredControls, structuredDifference}
    def score(row: MatchedControlFixtures.Components): MultiMatchScore =
      MultiMatchScore
        .of(row.shape, row.direction, row.length, row.position, row.duration)
        .fold(e => fail(e.message), identity)
    val spec = EvaluationSpec
      .of(
        "analytic-component-fixture",
        "1",
        Vector.empty,
        Vector("shape", "direction", "length", "position", "duration"),
        EvaluationGeometry.Independent,
        EvaluationTime.OrderFree
      )
      .fold(e => fail(e.message), identity)
    def reduce(values: Vector[MultiMatchScore]): Analysis[String, MultiMatchScore] =
      val focal = Trials(Vector(Trial("trial", (), ())))
      val refs  = Trials(values.zipWithIndex.map { case (value, i) => Trial(i, (), value) })
      evaluatePairs(
        pair(focal, refs, Pairing.between[String, Int].all),
        ContentHash.empty,
        EvaluationInfo(
          "analytic component fixture",
          EvaluationScale.Measure(MeasureScale.Probability),
          Some(spec)
        )
      )((_, value) => Right(value)).meanByLeft(FailurePolicy.RequireAll)
    val result = contrast(
      reduce(Vector(score(structuredMatched))),
      reduce(structuredControls.map(score))
    ).fold(e => fail(e.message), identity)
    val delta = result.rows.head.difference.fold(e => fail(e.message), identity)
    close(delta.shape.value, structuredDifference.shape)
    close(delta.direction.value, structuredDifference.direction)
    close(delta.length.value, structuredDifference.length)
    close(delta.position.value, structuredDifference.position)
    close(delta.duration.value, structuredDifference.duration)
  }
