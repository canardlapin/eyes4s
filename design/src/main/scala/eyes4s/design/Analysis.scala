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
import eyes4s.kernel.*

/** The scientific scale of a generic pair evaluator. */
enum EvaluationScale derives CanEqual:
  case Measure(value: MeasureScale)
  case Count
  case Duration
  case Unitless

  def render: String = this match
    case Measure(value) => value.render
    case Count          => "count"
    case Duration       => "duration"
    case Unitless       => "unitless"

/** Typed metadata that makes a generic evaluator auditable. */
final case class EvaluationInfo(name: String, scale: EvaluationScale) derives CanEqual

object EvaluationInfo:
  def comparison[A, B, S](comparison: Compare[A, B, S]): EvaluationInfo =
    EvaluationInfo(comparison.info.name, EvaluationScale.Measure(comparison.scale))

/** Explicit evidence that a generic evaluator is symmetric.
  *
  * Canonical-undirected pair storage accepts this capability, not an ordinary
  * function. [[SymmetricCompare]] supplies the same evidence for comparisons.
  */
final class SymmetricEvaluator[A, E, S] private (
    f: (A, A) => Either[E, S]
):
  def evaluate(left: A, right: A): Either[E, S] = f(left, right)

object SymmetricEvaluator:
  def apply[A, E, S](
      f: (A, A) => Either[E, S]
  ): SymmetricEvaluator[A, E, S] =
    new SymmetricEvaluator(f)

/** Failures while computing a mean score. */
enum ScoreMeanError derives CanEqual:
  case EmptyValues(operand: String)
  case NonFiniteValue(component: String, index: Int, value: Double)
  case NonFiniteMean(component: String, value: Double)
  case InvalidComparisonValue(component: String, underlying: ComparisonValueError)

  def message: String = this match
    case EmptyValues(operand) =>
      s"$operand cannot compute a score mean from an empty collection."
    case NonFiniteValue(component, index, value) =>
      s"Score component $component at index $index is non-finite: $value."
    case NonFiniteMean(component, value) =>
      s"Score component $component produced a non-finite mean: $value."
    case InvalidComparisonValue(component, underlying) =>
      s"Score component $component produced an invalid comparison value: ${underlying.message}"

/** A score type whose non-empty arithmetic mean remains the same score type. */
trait ScoreMean[S]:
  def mean(values: Vector[S]): Either[ScoreMeanError, S]

object ScoreMean:
  def apply[S](using instance: ScoreMean[S]): ScoreMean[S] = instance

  given ScoreMean[Double] with
    def mean(values: Vector[Double]): Either[ScoreMeanError, Double] =
      finiteMean(values, "value")

  given ScoreMean[MeasureDistance] with
    def mean(values: Vector[MeasureDistance]): Either[ScoreMeanError, MeasureDistance] =
      finiteMean(values.map(_.value), "distance").flatMap { value =>
        MeasureDistance
          .of(value)
          .left
          .map(ScoreMeanError.InvalidComparisonValue("distance", _))
      }

  given ScoreMean[Similarity] with
    def mean(values: Vector[Similarity]): Either[ScoreMeanError, Similarity] =
      finiteMean(values.map(_.value), "similarity").flatMap { value =>
        Similarity
          .of(value)
          .left
          .map(ScoreMeanError.InvalidComparisonValue("similarity", _))
      }

  given ScoreMean[MultiMatchScore] with
    def mean(values: Vector[MultiMatchScore]): Either[ScoreMeanError, MultiMatchScore] =
      for
        shape     <- finiteMean(values.map(_.shape), "shape")
        direction <- finiteMean(values.map(_.direction), "direction")
        length    <- finiteMean(values.map(_.length), "length")
        position  <- finiteMean(values.map(_.position), "position")
        duration  <- finiteMean(values.map(_.duration), "duration")
        score     <- MultiMatchScore
          .of(shape, direction, length, position, duration)
          .left
          .map(ScoreMeanError.InvalidComparisonValue("MultiMatch", _))
      yield score

  /** A compensated mean of scaled terms.
    *
    * Scaling each term before summation avoids overflowing a finite mean when
    * several large, finite values are present.
    */
  private def finiteMean(
      values: Vector[Double],
      component: String
  ): Either[ScoreMeanError, Double] =
    if values.isEmpty then Left(ScoreMeanError.EmptyValues(component))
    else
      val divisor                         = values.size.toDouble
      var sum                             = 0.0
      var carry                           = 0.0
      var index                           = 0
      var failure: Option[ScoreMeanError] = None

      while index < values.size && failure.isEmpty do
        val value = values(index)
        if !value.isFinite then
          failure = Some(ScoreMeanError.NonFiniteValue(component, index, value))
        else
          val adjusted = value / divisor - carry
          val next     = sum + adjusted
          carry = (next - sum) - adjusted
          sum = next
        index += 1

      failure match
        case Some(error)           => Left(error)
        case None if !sum.isFinite =>
          Left(ScoreMeanError.NonFiniteMean(component, sum))
        case None => Right(sum)

end ScoreMean

/** Invalid raw reduction-policy operands. */
enum ReductionPolicyError derives CanEqual:
  case NonPositiveMinimumSuccessful(value: Int)

  def message: String = this match
    case NonPositiveMinimumSuccessful(value) =>
      s"SuccessfulOnly needs a positive minimumSuccessful value, got $value."

/** A positive minimum number of successful scores. */
opaque type MinimumSuccessful = Int

object MinimumSuccessful:
  def of(value: Int): Either[ReductionPolicyError, MinimumSuccessful] =
    if value > 0 then Right(value)
    else Left(ReductionPolicyError.NonPositiveMinimumSuccessful(value))

  extension (minimum: MinimumSuccessful) def value: Int = minimum

/** How score failures affect a reduction. */
enum FailurePolicy derives CanEqual:
  case RequireAll
  case SuccessfulOnly(minimumSuccessful: MinimumSuccessful)

  def render: String = this match
    case RequireAll              => "require-all"
    case SuccessfulOnly(minimum) =>
      s"successful-only(minimum=${minimum.value})"

object FailurePolicy:
  def successfulOnly(
      minimumSuccessful: Int
  ): Either[ReductionPolicyError, FailurePolicy] =
    MinimumSuccessful.of(minimumSuccessful).map(FailurePolicy.SuccessfulOnly.apply)

/** Which endpoint accounting convention produced a reduced analysis. */
enum ReductionOrientation derives CanEqual:
  case ByLeft
  case ByRight
  case EdgesOnce
  case MirroredEndpoints

/** A per-key reduction failure. */
enum ReductionError[K] derives CanEqual:
  case NoSelectedScores(key: K)
  case AmbiguousKey(key: K, sourceIndices: Vector[Int])
  case FailedScores(key: K, successful: Int, failed: Int)
  case InsufficientSuccessful(
      key: K,
      required: Int,
      successful: Int,
      failed: Int
  )
  case MeanFailure(key: K, underlying: ScoreMeanError)

  def message: String = this match
    case NoSelectedScores(key) =>
      s"Key $key has no selected scores to reduce."
    case AmbiguousKey(key, indices) =>
      s"Key $key is ambiguous at source indices ${indices.mkString("[", ", ", "]")}."
    case FailedScores(key, successful, failed) =>
      s"Key $key has $failed failed scores and $successful successful scores; RequireAll rejected it."
    case InsufficientSuccessful(key, required, successful, failed) =>
      s"Key $key needs $required successful scores, got $successful successful and $failed failed."
    case MeanFailure(key, underlying) =>
      s"Key $key could not be averaged: ${underlying.message}"

/** Realized pair- and reduction-level counts. */
final case class ReductionReport[K] private[design] (
    orientation: ReductionOrientation,
    policy: FailurePolicy,
    eligiblePairCount: Long,
    selectedPairCount: Int,
    successfulPairCount: Int,
    failedPairCount: Int,
    contributionCount: Int,
    reducedKeyCount: Int,
    failedKeys: Vector[K]
) derives CanEqual

/** A reduced, derived view of a primary [[PairwiseAnalysis]]. */
final case class Analysis[K, S] private[design] (
    rows: Vector[(K, Either[ReductionError[K], S])],
    diagnostics: ReductionReport[K],
    provenance: Provenance
) derives CanEqual

/** Evaluate every selected directed pair with the same total evaluator. */
def evaluatePairs[KL, ML, KR, MR, A, B, E, S](
    paired: DirectedPaired[KL, ML, KR, MR, A, B],
    inputs: ContentHash,
    info: EvaluationInfo
)(
    evaluator: (A, B) => Either[E, S]
): DirectedPairwiseAnalysis[KL, KR, E, S] =
  val rows = paired.pairs.map { case (left, right) =>
    PairScore(left.key, right.key, evaluator(left.value, right.value))
  }
  DirectedPairwiseAnalysis(
    rows,
    paired.diagnostics,
    EvaluationProvenance(inputs, info, paired.diagnostics, rows)
  )

/** Evaluate canonical-undirected pairs only with explicit symmetry evidence. */
def evaluatePairs[K, M, A, E, S](
    paired: UndirectedPaired[K, M, A],
    inputs: ContentHash,
    info: EvaluationInfo
)(
    evaluator: SymmetricEvaluator[A, E, S]
): UndirectedPairwiseAnalysis[K, E, S] =
  val rows = paired.pairs.map { case (left, right) =>
    PairScore(left.key, right.key, evaluator.evaluate(left.value, right.value))
  }
  UndirectedPairwiseAnalysis(
    rows,
    paired.diagnostics,
    EvaluationProvenance(inputs, info, paired.diagnostics, rows)
  )

/** Evaluate directed pairs through a comparison instance. */
def evaluatePairs[KL, ML, KR, MR, A, B, S](
    paired: DirectedPaired[KL, ML, KR, MR, A, B],
    inputs: ContentHash,
    comparison: Compare[A, B, S]
): DirectedPairwiseAnalysis[KL, KR, CompareError, S] =
  evaluatePairs(paired, inputs, EvaluationInfo.comparison(comparison))(comparison.compare)

/** Evaluate canonical-undirected pairs through a symmetric comparison. */
def evaluatePairs[K, M, A, S](
    paired: UndirectedPaired[K, M, A],
    inputs: ContentHash,
    comparison: SymmetricCompare[A, S]
): UndirectedPairwiseAnalysis[K, CompareError, S] =
  evaluatePairs(paired, inputs, EvaluationInfo.comparison(comparison))(
    SymmetricEvaluator(comparison.compare)
  )

extension [KL, KR, E, S](analysis: DirectedPairwiseAnalysis[KL, KR, E, S])

  def meanByLeft(
      policy: FailurePolicy
  )(using ScoreMean[S]): Analysis[KL, S] =
    Reduction.byLeft(analysis, policy)

  def meanByRight(
      policy: FailurePolicy
  )(using ScoreMean[S]): Analysis[KR, S] =
    Reduction.byRight(analysis, policy)

extension [K, E, S](analysis: UndirectedPairwiseAnalysis[K, E, S])

  def meanEdges(
      policy: FailurePolicy
  )(using ScoreMean[S]): Analysis[Unit, S] =
    Reduction.edges(analysis, policy)

  def meanByEndpoint(
      policy: FailurePolicy
  )(using ScoreMean[S]): Analysis[K, S] =
    Reduction.byEndpoint(analysis, policy)

private object EvaluationProvenance:

  def apply[KL, KR, E, S](
      inputs: ContentHash,
      info: EvaluationInfo,
      pairing: PairingReport[KL, KR],
      rows: Vector[PairScore[KL, KR, E, S]]
  ): Provenance =
    val successful = rows.count(_.result.isRight)
    val failed     = rows.size - successful

    Provenance(
      inputs,
      Vector(
        Provenance.Step("pair", pairingParams(pairing)),
        Provenance.Step(
          "evaluatePairs",
          Vector(
            "evaluator"  -> Provenance.Param.Text(info.name),
            "scale"      -> Provenance.Param.Text(info.scale.render),
            "successful" -> Provenance.Param.Num(successful.toDouble),
            "failed"     -> Provenance.Param.Num(failed.toDouble)
          )
        )
      )
    )

  private def pairingParams[KL, KR](
      pairing: PairingReport[KL, KR]
  ): Vector[(String, Provenance.Param)] =
    val counts = Vector(
      "relation"       -> Provenance.Param.Text(pairing.pairSpace.relation),
      "storage"        -> Provenance.Param.Text(pairing.storage.toString),
      "eligible"       -> Provenance.Param.Text(pairing.eligiblePairCount.toString),
      "selected"       -> Provenance.Param.Num(pairing.selectedPairCount.toDouble),
      "unmatchedLeft"  -> Provenance.Param.Num(pairing.unmatchedLeft.size.toDouble),
      "unmatchedRight" -> Provenance.Param.Num(pairing.unmatchedRight.size.toDouble),
      "ambiguous"      -> Provenance.Param.Num(pairing.ambiguous.size.toDouble)
    )

    pairing.pairSpace match
      case PairSpace.BetweenDirected(_, selection) =>
        counts ++ selectionParams(selection)
      case PairSpace.WithinDirected(_, self, selection) =>
        counts ++ Vector("self" -> Provenance.Param.Text(self.toString)) ++
          selectionParams(selection)
      case PairSpace.WithinUndirected(_, self) =>
        counts ++ Vector("self" -> Provenance.Param.Text(self.toString))

  private def selectionParams(
      selection: Selection
  ): Vector[(String, Provenance.Param)] =
    selection match
      case Selection.All =>
        Vector("selection" -> Provenance.Param.Text("all"))
      case Selection.BottomK(cap, seed, sampleId) =>
        Vector(
          "selection" -> Provenance.Param.Text("bottom-k"),
          "cap"       -> Provenance.Param.Num(cap.value.toDouble),
          "seed"      -> Provenance.Param.Text(seed.value.toString),
          "sampleId"  -> Provenance.Param.Text(sampleId.value)
        )

end EvaluationProvenance

private object Reduction:

  def byLeft[KL, KR, E, S](
      analysis: DirectedPairwiseAnalysis[KL, KR, E, S],
      policy: FailurePolicy
  )(using ScoreMean[S]): Analysis[KL, S] =
    val contributions = analysis.rows.map(row => row.left -> row.result)
    val ambiguous     = analysis.diagnostics.ambiguous.collect {
      case PairingAmbiguity.DuplicateLeft(key, indices) => key -> indices
    }
    reduce(
      analysis,
      contributions,
      analysis.diagnostics.unmatchedLeft,
      ambiguous,
      ReductionOrientation.ByLeft,
      policy
    )

  def byRight[KL, KR, E, S](
      analysis: DirectedPairwiseAnalysis[KL, KR, E, S],
      policy: FailurePolicy
  )(using ScoreMean[S]): Analysis[KR, S] =
    val contributions = analysis.rows.map(row => row.right -> row.result)
    val ambiguous     = analysis.diagnostics.ambiguous.collect {
      case PairingAmbiguity.DuplicateRight(key, indices) => key -> indices
    }
    reduce(
      analysis,
      contributions,
      analysis.diagnostics.unmatchedRight,
      ambiguous,
      ReductionOrientation.ByRight,
      policy
    )

  def edges[K, E, S](
      analysis: UndirectedPairwiseAnalysis[K, E, S],
      policy: FailurePolicy
  )(using ScoreMean[S]): Analysis[Unit, S] =
    reduce(
      analysis,
      analysis.rows.map(row => () -> row.result),
      Vector(()),
      Vector.empty,
      ReductionOrientation.EdgesOnce,
      policy
    )

  def byEndpoint[K, E, S](
      analysis: UndirectedPairwiseAnalysis[K, E, S],
      policy: FailurePolicy
  )(using ScoreMean[S]): Analysis[K, S] =
    val contributions =
      analysis.rows.flatMap(row => Vector(row.left -> row.result, row.right -> row.result))
    val ambiguous = analysis.diagnostics.ambiguous.flatMap {
      case PairingAmbiguity.DuplicateLeft(key, indices)  => Vector(key -> indices)
      case PairingAmbiguity.DuplicateRight(key, indices) => Vector(key -> indices)
    }
    val unmatched =
      distinct(analysis.diagnostics.unmatchedLeft ++ analysis.diagnostics.unmatchedRight)

    reduce(
      analysis,
      contributions,
      unmatched,
      ambiguous,
      ReductionOrientation.MirroredEndpoints,
      policy
    )

  private def reduce[K, KL, KR, E, S](
      analysis: PairwiseAnalysis[KL, KR, E, S],
      contributions: Vector[(K, Either[E, S])],
      unmatched: Vector[K],
      ambiguous: Vector[(K, Vector[Int])],
      orientation: ReductionOrientation,
      policy: FailurePolicy
  )(using mean: ScoreMean[S]): Analysis[K, S] =
    val keys = distinct(
      contributions.map(_._1) ++ unmatched ++ ambiguous.map(_._1)
    )

    val rows = keys.map { key =>
      val ambiguity = ambiguous.find(_._1 == key).map(_._2)
      val scores    = contributions.collect { case (`key`, result) =>
        result
      }
      key -> reduceOne(key, scores, ambiguity, policy)
    }

    val successfulPairs = analysis.rows.count(_.result.isRight)
    val failedPairs     = analysis.rows.size - successfulPairs
    val failedKeys      = rows.collect { case (key, Left(_)) => key }
    val report          = ReductionReport(
      orientation,
      policy,
      analysis.diagnostics.eligiblePairCount,
      analysis.diagnostics.selectedPairCount,
      successfulPairs,
      failedPairs,
      contributions.size,
      rows.size - failedKeys.size,
      failedKeys
    )
    val provenance = analysis.provenance.andThen(
      Provenance.Step(
        "reducePairs",
        Vector(
          "orientation"   -> Provenance.Param.Text(orientation.toString),
          "failurePolicy" -> Provenance.Param.Text(policy.render),
          "contributions" -> Provenance.Param.Num(contributions.size.toDouble),
          "reducedKeys"   -> Provenance.Param.Num(report.reducedKeyCount.toDouble),
          "failedKeys"    -> Provenance.Param.Num(report.failedKeys.size.toDouble)
        )
      )
    )

    Analysis(rows, report, provenance)

  private def reduceOne[K, E, S](
      key: K,
      scores: Vector[Either[E, S]],
      ambiguity: Option[Vector[Int]],
      policy: FailurePolicy
  )(using mean: ScoreMean[S]): Either[ReductionError[K], S] =
    ambiguity match
      case Some(indices)          => Left(ReductionError.AmbiguousKey(key, indices))
      case None if scores.isEmpty => Left(ReductionError.NoSelectedScores(key))
      case None                   =>
        val successful = scores.collect { case Right(value) => value }
        val failed     = scores.size - successful.size

        policy match
          case FailurePolicy.RequireAll if failed > 0 =>
            Left(ReductionError.FailedScores(key, successful.size, failed))
          case FailurePolicy.SuccessfulOnly(minimum) if successful.size < minimum.value =>
            Left(
              ReductionError.InsufficientSuccessful(
                key,
                minimum.value,
                successful.size,
                failed
              )
            )
          case _ =>
            mean.mean(successful).left.map(ReductionError.MeanFailure(key, _))

  private def distinct[K](values: Vector[K]): Vector[K] =
    values.foldLeft(Vector.empty[K]) { (found, value) =>
      if found.contains(value) then found else found :+ value
    }

end Reduction
