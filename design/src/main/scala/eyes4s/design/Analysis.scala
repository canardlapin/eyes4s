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

import cats.data.NonEmptyVector
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
final case class EvaluationInfo(
    name: String,
    scale: EvaluationScale,
    specification: Option[EvaluationSpec] = None
) derives CanEqual

object EvaluationInfo:
  def comparison[A, B, S](comparison: Compare[A, B, S]): EvaluationInfo =
    EvaluationInfo(comparison.info.name, EvaluationScale.Measure(comparison.scale))

  def comparison[A, B, S](
      comparison: Compare[A, B, S],
      specification: EvaluationSpec
  ): EvaluationInfo =
    EvaluationInfo(
      comparison.info.name,
      EvaluationScale.Measure(comparison.scale),
      Some(specification)
    )

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
final class Analysis[K, S] private[design] (
    val entries: Vector[ReductionRow[K, S]],
    val diagnostics: ReductionReport[K],
    val provenance: Provenance,
    val source: PairwiseAnalysis[?, ?, ?, S]
) derives CanEqual:
  def rows: Vector[(K, Either[ReductionError[K], S])] =
    entries.map(row => row.key -> row.result)
  def evaluation: EvaluationInfo = source.evaluation

/** Selected contributions and effective denominator for one focal key. */
final case class ReductionRow[K, S] private[design] (
    key: K,
    result: Either[ReductionError[K], S],
    successful: Int,
    failed: Int,
    contributing: Int
) derives CanEqual:
  def selected: Int = successful + failed

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
    EvaluationProvenance(inputs, info, paired.diagnostics, rows),
    info
  )

/** Maximum recording samples one bounded preprocessing or detection step may
  * feed to its machine. One sample is one unit; a machine whose per-sample
  * `step` is itself unbounded is not made bounded by this quantum.
  */
final class SampleQuantum private (val value: Int)
object SampleQuantum:
  val default: SampleQuantum                                 = new SampleQuantum(1 << 12)
  def of(value: Int): Either[WorkQuantaError, SampleQuantum] =
    if value <= 0 then Left(WorkQuantaError.InvalidSampleQuantum(value))
    else Right(new SampleQuantum(value))

enum WorkQuantaError derives CanEqual:
  case InvalidSampleQuantum(value: Int)
  def message: String = this match
    case InvalidSampleQuantum(value) => s"Sample quantum must be positive, got $value."

/** Page, comparison and sample quanta for one resumable step. `pairs` also
  * bounds the contributions or keys visited by one reduction or contrast step;
  * `samples` bounds one recording preprocessing or detection step and plays
  * no part in a study step.
  */
final case class WorkQuanta(
    pairs: PairQuantum,
    comparison: ComparisonQuantum,
    samples: SampleQuantum = SampleQuantum.default
)
object WorkQuanta:
  val default: WorkQuanta = WorkQuanta(PairQuantum.default, ComparisonQuantum.default)

/** Typed evidence of how one scheduled pair is evaluated.
  *
  * A [[PairEvaluation.Whole]] closure runs as one indivisible operation per
  * pair; the schedule is still paged, so cancellation is bounded at pair
  * granularity only. [[PairEvaluation.Bounded]] needs a [[BoundedCompare]],
  * whose cursor yields inside each pair; an arbitrary closure cannot be lifted
  * into it.
  */
sealed trait PairEvaluation[KL, KR, E, S]

object PairEvaluation:
  final class Whole[KL, KR, E, S](
      val evaluate: ScheduledPair[KL, KR] => Either[E, S]
  ) extends PairEvaluation[KL, KR, E, S]

  final class Bounded[KL, KR, A, B, E, S](
      val comparison: BoundedCompare[A, B, S],
      val budget: ComparisonBudget,
      val operands: ScheduledPair[KL, KR] => Either[E, (A, B)],
      val failure: (ScheduledPair[KL, KR], CompareError) => E
  ) extends PairEvaluation[KL, KR, E, S]

/** Refusals while advancing scheduled evaluation; every case names its operands. */
enum EvaluationWorkError derives CanEqual:
  case Schedule(underlying: PairScheduleError)
  case Comparison(underlying: ComparisonWorkError)

  def message: String = this match
    case Schedule(e)   => e.message
    case Comparison(e) => e.message

/** One step of scheduled evaluation. `workUnits` counts candidate visits for a
  * schedule page, one unit for beginning or wholly evaluating a pair, and
  * comparison units inside a bounded pair.
  */
enum EvaluationPage[KL, KR, E, S]:
  case More(workUnits: Int, next: EvaluationCursor[KL, KR, E, S])
  case Done(workUnits: Int, analysis: DirectedPairwiseAnalysis[KL, KR, E, S])

/** An immutable position inside scheduled evaluation. Scores accumulate in
  * schedule order, so the completed analysis does not depend on the quanta.
  *
  * Beginning a bounded pair whose comparison is already decided (for example
  * incompatible grids) costs one unit, and the next step records its outcome
  * as a zero-unit `More`. Drivers must treat a zero-unit step as progress, not
  * a stall, and must not assume `workUnits <= quanta.comparison`.
  */
final class EvaluationCursor[KL, KR, E, S] private[design] (
    private val evaluation: PairEvaluation[KL, KR, E, S],
    private val inputs: ContentHash,
    private val info: EvaluationInfo,
    private val schedule: Either[PairingReport[KL, KR], PairCursor[KL, KR]],
    private val pending: Vector[ScheduledPair[KL, KR]],
    private val offset: Int,
    private val current: Option[Comparing[KL, KR, E, S]],
    private val rows: Vector[PairScore[KL, KR, E, S]]
):
  /** Pairs whose outcome is already recorded. */
  def completedPairs: Int = rows.size

  def advance(
      quanta: WorkQuanta
  ): Either[EvaluationWorkError, EvaluationPage[KL, KR, E, S]] =
    current match
      case Some(comparing) =>
        Right(comparing.cursor.advance(quanta.comparison) match
          case ComparisonStep.More(units, next) =>
            EvaluationPage.More(units, copy(current = Some(comparing.continue(next))))
          case ComparisonStep.Done(units, result) =>
            EvaluationPage.More(
              units,
              copy(current = None, rows = rows :+ comparing.score(result))
            ))
      case None if offset < pending.size =>
        val pair = pending(offset)
        evaluation match
          case whole: PairEvaluation.Whole[KL, KR, E, S] =>
            Right(
              EvaluationPage.More(
                1,
                copy(
                  offset = offset + 1,
                  rows = rows :+ PairScore(pair.left, pair.right, whole.evaluate(pair))
                )
              )
            )
          case bounded: PairEvaluation.Bounded[KL, KR, ?, ?, E, S] =>
            begin(bounded, pair).map { started =>
              EvaluationPage.More(
                1,
                started match
                  case Left(error) =>
                    copy(
                      offset = offset + 1,
                      rows = rows :+ PairScore(pair.left, pair.right, Left(error))
                    )
                  case Right(comparing) => copy(offset = offset + 1, current = Some(comparing))
              )
            }
      case None =>
        schedule match
          case Right(cursor) =>
            cursor.advance(quanta.pairs) match
              case Left(error) => Left(EvaluationWorkError.Schedule(error))
              case Right(PairPage.More(pairs, units, next)) =>
                Right(
                  EvaluationPage
                    .More(units, copy(schedule = Right(next), pending = pairs, offset = 0))
                )
              case Right(PairPage.Done(pairs, units, report)) =>
                Right(
                  EvaluationPage
                    .More(units, copy(schedule = Left(report), pending = pairs, offset = 0))
                )
          case Left(report) =>
            Right(
              EvaluationPage.Done(
                0,
                DirectedPairwiseAnalysis(
                  rows,
                  report,
                  EvaluationProvenance(inputs, info, report, rows),
                  info
                )
              )
            )

  private def begin[A, B](
      bounded: PairEvaluation.Bounded[KL, KR, A, B, E, S],
      pair: ScheduledPair[KL, KR]
  ): Either[EvaluationWorkError, Either[E, Comparing[KL, KR, E, S]]] =
    bounded.operands(pair) match
      case Left(error)   => Right(Left(error))
      case Right((a, b)) =>
        ComparisonWork
          .start(bounded.comparison, a, b, bounded.budget)
          .left
          .map(EvaluationWorkError.Comparison.apply)
          .map(cursor => Right(Comparing(pair, cursor, bounded.failure)))

  private def copy(
      schedule: Either[PairingReport[KL, KR], PairCursor[KL, KR]] = schedule,
      pending: Vector[ScheduledPair[KL, KR]] = pending,
      offset: Int = offset,
      current: Option[Comparing[KL, KR, E, S]] = current,
      rows: Vector[PairScore[KL, KR, E, S]] = rows
  ): EvaluationCursor[KL, KR, E, S] =
    new EvaluationCursor(evaluation, inputs, info, schedule, pending, offset, current, rows)

/** A bounded comparison in progress for one scheduled pair. */
private final case class Comparing[KL, KR, E, S](
    pair: ScheduledPair[KL, KR],
    cursor: ComparisonCursor[S],
    failure: (ScheduledPair[KL, KR], CompareError) => E
):
  def continue(next: ComparisonCursor[S]): Comparing[KL, KR, E, S]    = copy(cursor = next)
  def score(result: Either[CompareError, S]): PairScore[KL, KR, E, S] =
    PairScore(pair.left, pair.right, result.left.map(failure(pair, _)))

object EvaluationWork:
  /** Begin resumable evaluation of every scheduled pair. No pair is visited here. */
  def start[KL, KR, E, S](
      schedule: DirectedPairSchedule[KL, KR],
      inputs: ContentHash,
      info: EvaluationInfo,
      evaluation: PairEvaluation[KL, KR, E, S]
  ): EvaluationCursor[KL, KR, E, S] =
    new EvaluationCursor(
      evaluation,
      inputs,
      info,
      Right(schedule.start),
      Vector.empty,
      0,
      None,
      Vector.empty
    )

  /** Drive a cursor to completion with fixed quanta. */
  def complete[KL, KR, E, S](
      cursor: EvaluationCursor[KL, KR, E, S],
      quanta: WorkQuanta
  ): Either[EvaluationWorkError, DirectedPairwiseAnalysis[KL, KR, E, S]] =
    @annotation.tailrec
    def loop(
        cursor: EvaluationCursor[KL, KR, E, S]
    ): Either[EvaluationWorkError, DirectedPairwiseAnalysis[KL, KR, E, S]] =
      cursor.advance(quanta) match
        case Left(error)                             => Left(error)
        case Right(EvaluationPage.More(_, next))     => loop(next)
        case Right(EvaluationPage.Done(_, analysis)) => Right(analysis)
    loop(cursor)

/** Evaluate bounded pair pages without retaining a second table of source pairs.
  * The evaluator runs whole per pair; completed scores remain fully
  * materialized for the existing reduction API.
  */
def evaluateScheduled[KL, KR, E, S](
    schedule: DirectedPairSchedule[KL, KR],
    inputs: ContentHash,
    info: EvaluationInfo,
    quantum: PairQuantum = PairQuantum.default
)(
    evaluator: ScheduledPair[KL, KR] => Either[E, S]
): Either[EvaluationWorkError, DirectedPairwiseAnalysis[KL, KR, E, S]] =
  EvaluationWork.complete(
    EvaluationWork.start(schedule, inputs, info, PairEvaluation.Whole(evaluator)),
    WorkQuanta(quantum, ComparisonQuantum.default)
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
    EvaluationProvenance(inputs, info, paired.diagnostics, rows),
    info
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
    ReductionWork.complete(meanByLeftWork(policy))

  def meanByRight(
      policy: FailurePolicy
  )(using ScoreMean[S]): Analysis[KR, S] =
    ReductionWork.complete(meanByRightWork(policy))

  /** The same reduction as [[meanByLeft]], in resumable steps. */
  def meanByLeftWork(
      policy: FailurePolicy
  )(using ScoreMean[S]): ReductionCursor[KL, S] =
    Reduction.byLeft(analysis, policy)

  /** The same reduction as [[meanByRight]], in resumable steps. */
  def meanByRightWork(
      policy: FailurePolicy
  )(using ScoreMean[S]): ReductionCursor[KR, S] =
    Reduction.byRight(analysis, policy)

extension [K, E, S](analysis: UndirectedPairwiseAnalysis[K, E, S])

  def meanEdges(
      policy: FailurePolicy
  )(using ScoreMean[S]): Analysis[Unit, S] =
    ReductionWork.complete(meanEdgesWork(policy))

  def meanByEndpoint(
      policy: FailurePolicy
  )(using ScoreMean[S]): Analysis[K, S] =
    ReductionWork.complete(meanByEndpointWork(policy))

  /** The same reduction as [[meanEdges]], in resumable steps. */
  def meanEdgesWork(
      policy: FailurePolicy
  )(using ScoreMean[S]): ReductionCursor[Unit, S] =
    Reduction.edges(analysis, policy)

  /** The same reduction as [[meanByEndpoint]], in resumable steps. */
  def meanByEndpointWork(
      policy: FailurePolicy
  )(using ScoreMean[S]): ReductionCursor[K, S] =
    Reduction.byEndpoint(analysis, policy)

/** One step of a keyed reduction. */
enum ReductionPage[K, S]:
  case More(workUnits: Int, next: ReductionCursor[K, S])
  case Done(workUnits: Int, analysis: Analysis[K, S])

/** An immutable position inside a keyed mean reduction.
  *
  * The first pass groups contributions, unmatched keys and ambiguities in
  * their original order, one unit per visit; the second reduces one key per
  * step, charging that key's contribution count. Keys are reduced in order of
  * first appearance, and each key's scores keep schedule order, so the result
  * does not depend on the quanta.
  *
  * A key step charges `max(1, scores.size)` units, so one step's `workUnits`
  * may exceed the quantum: the quantum bounds the keys and visits a step
  * starts, never the size of one key's group. Grouping keys by `K` relies on
  * `hashCode` being consistent with `equals`, as `contrast` already does.
  */
final class ReductionCursor[K, S] private[design] (
    private val source: PairwiseAnalysis[?, ?, ?, S],
    private val contributions: Vector[(K, Either[?, S])],
    private val unmatched: Vector[K],
    private val ambiguous: Vector[(K, Vector[Int])],
    private val orientation: ReductionOrientation,
    private val policy: FailurePolicy,
    private val position: Int,
    private val order: Vector[K],
    private val groups: Map[K, Vector[Either[?, S]]],
    private val ambiguities: Map[K, Vector[Int]],
    private val reduced: Int,
    private val entries: Vector[ReductionRow[K, S]]
)(using ScoreMean[S]):
  private def total: Int = contributions.size + unmatched.size + ambiguous.size

  /** Keys whose reduced row is already recorded. */
  def reducedKeys: Int = reduced

  def advance(quantum: PairQuantum): ReductionPage[K, S] =
    var work    = 0
    var pos     = position
    var keys    = order
    var grouped = groups
    var firsts  = ambiguities
    var done    = reduced
    var rows    = entries

    while work < quantum.value && pos < total do
      if pos < contributions.size then
        val (key, result) = contributions(pos)
        if !grouped.contains(key) then keys :+= key
        grouped = grouped.updated(key, grouped.getOrElse(key, Vector.empty) :+ result)
      else if pos < contributions.size + unmatched.size then
        val key = unmatched(pos - contributions.size)
        if !grouped.contains(key) then
          keys :+= key
          grouped = grouped.updated(key, Vector.empty)
      else
        val (key, indices) = ambiguous(pos - contributions.size - unmatched.size)
        if !grouped.contains(key) then
          keys :+= key
          grouped = grouped.updated(key, Vector.empty)
        if !firsts.contains(key) then firsts = firsts.updated(key, indices)
      pos += 1
      work += 1

    while work < quantum.value && pos == total && done < keys.size do
      val key        = keys(done)
      val scores     = grouped.getOrElse(key, Vector.empty)
      val result     = Reduction.reduceOne(key, scores, firsts.get(key), policy)
      val successful = scores.count(_.isRight)
      rows :+= ReductionRow(
        key,
        result,
        successful,
        scores.size - successful,
        if result.isRight then successful else 0
      )
      done += 1
      work += math.max(1, scores.size)

    if pos == total && done == keys.size then
      ReductionPage.Done(
        work,
        Reduction.finish(source, contributions.size, orientation, policy, rows)
      )
    else
      ReductionPage.More(
        work,
        new ReductionCursor(
          source,
          contributions,
          unmatched,
          ambiguous,
          orientation,
          policy,
          pos,
          keys,
          grouped,
          firsts,
          done,
          rows
        )
      )

object ReductionWork:
  /** Drive a reduction to completion with the default quantum. */
  def complete[K, S](cursor: ReductionCursor[K, S]): Analysis[K, S] =
    @annotation.tailrec
    def loop(cursor: ReductionCursor[K, S]): Analysis[K, S] =
      cursor.advance(PairQuantum.default) match
        case ReductionPage.More(_, next)     => loop(next)
        case ReductionPage.Done(_, analysis) => analysis
    loop(cursor)

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
      ) ++ info.specification.toVector.flatMap(_.steps)
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
  )(using ScoreMean[S]): ReductionCursor[KL, S] =
    val contributions = analysis.rows.map(row => row.left -> row.result)
    val ambiguous     = analysis.diagnostics.ambiguous.collect {
      case PairingAmbiguity.DuplicateLeft(key, indices) => key -> indices
    }
    start(
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
  )(using ScoreMean[S]): ReductionCursor[KR, S] =
    val contributions = analysis.rows.map(row => row.right -> row.result)
    val ambiguous     = analysis.diagnostics.ambiguous.collect {
      case PairingAmbiguity.DuplicateRight(key, indices) => key -> indices
    }
    start(
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
  )(using ScoreMean[S]): ReductionCursor[Unit, S] =
    start(
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
  )(using ScoreMean[S]): ReductionCursor[K, S] =
    val contributions =
      analysis.rows.flatMap(row => Vector(row.left -> row.result, row.right -> row.result))
    val ambiguous = analysis.diagnostics.ambiguous.flatMap {
      case PairingAmbiguity.DuplicateLeft(key, indices)  => Vector(key -> indices)
      case PairingAmbiguity.DuplicateRight(key, indices) => Vector(key -> indices)
    }
    val unmatched =
      distinct(analysis.diagnostics.unmatchedLeft ++ analysis.diagnostics.unmatchedRight)

    start(
      analysis,
      contributions,
      unmatched,
      ambiguous,
      ReductionOrientation.MirroredEndpoints,
      policy
    )

  private def start[K, KL, KR, E, S](
      analysis: PairwiseAnalysis[KL, KR, E, S],
      contributions: Vector[(K, Either[E, S])],
      unmatched: Vector[K],
      ambiguous: Vector[(K, Vector[Int])],
      orientation: ReductionOrientation,
      policy: FailurePolicy
  )(using ScoreMean[S]): ReductionCursor[K, S] =
    new ReductionCursor(
      analysis,
      contributions,
      unmatched,
      ambiguous,
      orientation,
      policy,
      0,
      Vector.empty,
      Map.empty,
      Map.empty,
      0,
      Vector.empty
    )

  def finish[K, S](
      analysis: PairwiseAnalysis[?, ?, ?, S],
      contributions: Int,
      orientation: ReductionOrientation,
      policy: FailurePolicy,
      entries: Vector[ReductionRow[K, S]]
  ): Analysis[K, S] =
    val successfulPairs = analysis.rows.count(_.result.isRight)
    val failedPairs     = analysis.rows.size - successfulPairs
    val failedKeys      = entries.collect { case row if row.result.isLeft => row.key }
    val report          = ReductionReport(
      orientation,
      policy,
      analysis.diagnostics.eligiblePairCount,
      analysis.diagnostics.selectedPairCount,
      successfulPairs,
      failedPairs,
      contributions,
      entries.size - failedKeys.size,
      failedKeys
    )
    new Analysis(entries, report, provenance(analysis, report), analysis)

  /** The reduction step appended to the source provenance; shared with reconstruction. */
  def provenance[K, S](
      analysis: PairwiseAnalysis[?, ?, ?, S],
      report: ReductionReport[K]
  ): Provenance =
    analysis.provenance.andThen(
      Provenance.Step(
        "reducePairs",
        Vector(
          "orientation"   -> Provenance.Param.Text(report.orientation.toString),
          "failurePolicy" -> Provenance.Param.Text(report.policy.render),
          "contributions" -> Provenance.Param.Num(report.contributionCount.toDouble),
          "reducedKeys"   -> Provenance.Param.Num(report.reducedKeyCount.toDouble),
          "failedKeys"    -> Provenance.Param.Num(report.failedKeys.size.toDouble)
        )
      )
    )

  def reduceOne[K, S](
      key: K,
      scores: Vector[Either[?, S]],
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

  def distinct[K](values: Vector[K]): Vector[K] =
    values.foldLeft(Vector.empty[K]) { (found, value) =>
      if found.contains(value) then found else found :+ value
    }

end Reduction

/** Refusals while rebuilding stored analyses; every case names its operands.
  *
  * Result constructors stay restricted. An archive supplies the parts it kept,
  * and these checks decide whether they describe one consistent computation:
  * counts agree, keys refer to rows that exist, denominators follow from the
  * outcomes, and the provenance is what the same derivation would produce.
  */
enum ReconstructionError[K] derives CanEqual:
  case PairCounts(eligible: Long, selected: Int)
  case RowCount(expected: Int, actual: Int)
  case Denominator(key: K, successful: Int, failed: Int, contributing: Int)
  case ResultKey(expected: K, found: K)
  case ResultCounts(key: K, result: ReductionError[K], successful: Int, failed: Int)
  case DuplicateEntry(key: K, indices: Vector[Int])
  case ReportCount(field: String, expected: Long, found: Long)
  case FailedKeys(expected: Vector[K], found: Vector[K])
  case ProvenanceConflict(stage: String, declared: Provenance, derived: Provenance)
  case ContrastDomain(expected: Vector[K], found: Vector[K])
  case ContrastRowShape(
      key: K,
      matched: Option[Either[ReductionError[K], Unit]],
      control: Option[Either[ReductionError[K], Unit]],
      difference: Either[ContrastRowError[K], Unit]
  )
  case ContrastOperand(key: K, operand: eyes4s.design.ContrastOperand)
  case Incompatible(issues: NonEmptyVector[ContrastCompatibilityError])
  case Orientation(expected: ReductionOrientation, found: ReductionOrientation)
  case KeyDomain(expected: Vector[K], found: Vector[K])
  case KeyDenominator(
      key: K,
      expectedSuccessful: Int,
      expectedFailed: Int,
      successful: Int,
      failed: Int
  )
  case OutcomeShape(
      key: K,
      expected: Option[ReductionError[K]],
      found: Option[ReductionError[K]]
  )

  def message: String = this match
    case PairCounts(eligible, selected) =>
      s"A pairing report cannot select $selected pairs out of $eligible eligible pairs."
    case RowCount(expected, actual) =>
      s"A pair analysis declares $expected selected pairs but stores $actual rows."
    case Denominator(key, successful, failed, contributing) =>
      s"Key $key has successful=$successful, failed=$failed and contributing=$contributing; " +
        "a reduced key contributes every successful score and a failed key none."
    case ResultKey(expected, found) =>
      s"Row $expected carries an outcome for key $found."
    case ResultCounts(key, result, successful, failed) =>
      s"Key $key stores successful=$successful and failed=$failed but its outcome says ${result.message}"
    case DuplicateEntry(key, indices) =>
      s"Key $key is reduced more than once, at entries $indices."
    case ReportCount(field, expected, found) =>
      s"Reduction report field $field is $found; the stored rows give $expected."
    case FailedKeys(expected, found) =>
      s"Reduction report names failed keys $found; the stored rows fail $expected."
    case ProvenanceConflict(stage, declared, derived) =>
      s"Stored $stage provenance ${declared.render} differs from the derived ${derived.render}."
    case ContrastDomain(expected, found) =>
      s"Contrast rows cover keys $found; the two analyses give $expected."
    case ContrastRowShape(key, matched, control, difference) =>
      s"Contrast key $key has matched=$matched, control=$control but stores difference $difference."
    case ContrastOperand(key, operand) =>
      s"Contrast key $key refers to a $operand row that the analysis does not contain."
    case Incompatible(issues)         => issues.toVector.map(_.message).mkString(" ")
    case Orientation(expected, found) =>
      s"A $expected reduction is stored with orientation $found."
    case KeyDomain(expected, found) =>
      s"Reduced keys $found differ from the keys the source pairs, unmatched keys and ambiguities give: $expected."
    case KeyDenominator(key, expectedSuccessful, expectedFailed, successful, failed) =>
      s"Key $key stores successful=$successful and failed=$failed; its source pairs give successful=$expectedSuccessful and failed=$expectedFailed."
    case OutcomeShape(key, expected, found) =>
      s"Key $key stores outcome ${found.map(_.message).getOrElse("a score")}; its source pairs and policy give ${expected.map(_.message).getOrElse("a score or a mean failure")}."

object ReductionRow:
  /** Checked reconstruction: the denominator follows from the outcome, and a
    * failed outcome's own counts agree with the row's.
    */
  def reconstruct[K, S](
      key: K,
      result: Either[ReductionError[K], S],
      successful: Int,
      failed: Int,
      contributing: Int
  ): Either[ReconstructionError[K], ReductionRow[K, S]] =
    val expected = if result.isRight then successful else 0
    if successful < 0 || failed < 0 || contributing != expected then
      Left(ReconstructionError.Denominator(key, successful, failed, contributing))
    else
      result match
        case Left(error) if errorKey(error) != key =>
          Left(ReconstructionError.ResultKey(key, errorKey(error)))
        case Left(error) if !consistent(error, successful, failed) =>
          Left(ReconstructionError.ResultCounts(key, error, successful, failed))
        case _ => Right(ReductionRow(key, result, successful, failed, contributing))

  private def errorKey[K](error: ReductionError[K]): K = error match
    case ReductionError.NoSelectedScores(k)                => k
    case ReductionError.AmbiguousKey(k, _)                 => k
    case ReductionError.FailedScores(k, _, _)              => k
    case ReductionError.InsufficientSuccessful(k, _, _, _) => k
    case ReductionError.MeanFailure(k, _)                  => k

  private def consistent[K](error: ReductionError[K], successful: Int, failed: Int): Boolean =
    error match
      case ReductionError.NoSelectedScores(_)                => successful == 0 && failed == 0
      case ReductionError.AmbiguousKey(_, _)                 => successful == 0 && failed == 0
      case ReductionError.FailedScores(_, s, f)              => s == successful && f == failed
      case ReductionError.InsufficientSuccessful(_, _, s, f) => s == successful && f == failed
      case ReductionError.MeanFailure(_, _)                  => true

object ReductionReport:
  /** Checked reconstruction of the realized counts alone; [[Analysis.reconstruct]]
    * cross-checks them against the stored rows.
    */
  def reconstruct[K](
      orientation: ReductionOrientation,
      policy: FailurePolicy,
      eligiblePairCount: Long,
      selectedPairCount: Int,
      successfulPairCount: Int,
      failedPairCount: Int,
      contributionCount: Int,
      reducedKeyCount: Int,
      failedKeys: Vector[K]
  ): Either[ReconstructionError[K], ReductionReport[K]] =
    if eligiblePairCount < 0L || selectedPairCount < 0 ||
      selectedPairCount.toLong > eligiblePairCount
    then Left(ReconstructionError.PairCounts(eligiblePairCount, selectedPairCount))
    else if successfulPairCount < 0 || failedPairCount < 0 ||
      successfulPairCount + failedPairCount != selectedPairCount
    then
      Left(
        ReconstructionError.ReportCount(
          "successfulPairCount + failedPairCount",
          selectedPairCount.toLong,
          successfulPairCount.toLong + failedPairCount.toLong
        )
      )
    else if contributionCount < 0 then
      Left(ReconstructionError.ReportCount("contributionCount", 0L, contributionCount.toLong))
    else if reducedKeyCount < 0 then
      Left(ReconstructionError.ReportCount("reducedKeyCount", 0L, reducedKeyCount.toLong))
    else
      Right(
        ReductionReport(
          orientation,
          policy,
          eligiblePairCount,
          selectedPairCount,
          successfulPairCount,
          failedPairCount,
          contributionCount,
          reducedKeyCount,
          failedKeys
        )
      )

object Analysis:
  /** Checked reconstruction of a by-left (by-focal) reduction from its stored
    * rows, report, provenance and typed directed source; see [[check]].
    */
  def reconstructByLeft[KL, KR, E, S](
      entries: Vector[ReductionRow[KL, S]],
      diagnostics: ReductionReport[KL],
      provenance: Provenance,
      source: DirectedPairwiseAnalysis[KL, KR, E, S]
  ): Either[ReconstructionError[KL], Analysis[KL, S]] =
    check(
      entries,
      diagnostics,
      provenance,
      source,
      ReductionOrientation.ByLeft,
      source.rows.map(row => row.left -> row.result),
      source.diagnostics.unmatchedLeft,
      source.diagnostics.ambiguous.collect {
        case PairingAmbiguity.DuplicateLeft(key, indices) =>
          key -> indices
      }
    )

  /** Checked reconstruction of a by-right reduction. */
  def reconstructByRight[KL, KR, E, S](
      entries: Vector[ReductionRow[KR, S]],
      diagnostics: ReductionReport[KR],
      provenance: Provenance,
      source: DirectedPairwiseAnalysis[KL, KR, E, S]
  ): Either[ReconstructionError[KR], Analysis[KR, S]] =
    check(
      entries,
      diagnostics,
      provenance,
      source,
      ReductionOrientation.ByRight,
      source.rows.map(row => row.right -> row.result),
      source.diagnostics.unmatchedRight,
      source.diagnostics.ambiguous.collect {
        case PairingAmbiguity.DuplicateRight(key, indices) =>
          key -> indices
      }
    )

  /** Checked reconstruction of an edges-once reduction of an undirected analysis. */
  def reconstructEdges[K, E, S](
      entries: Vector[ReductionRow[Unit, S]],
      diagnostics: ReductionReport[Unit],
      provenance: Provenance,
      source: UndirectedPairwiseAnalysis[K, E, S]
  ): Either[ReconstructionError[Unit], Analysis[Unit, S]] =
    check(
      entries,
      diagnostics,
      provenance,
      source,
      ReductionOrientation.EdgesOnce,
      source.rows.map(row => () -> row.result),
      Vector(()),
      Vector.empty
    )

  /** Checked reconstruction of a mirrored-endpoint reduction of an undirected analysis. */
  def reconstructByEndpoint[K, E, S](
      entries: Vector[ReductionRow[K, S]],
      diagnostics: ReductionReport[K],
      provenance: Provenance,
      source: UndirectedPairwiseAnalysis[K, E, S]
  ): Either[ReconstructionError[K], Analysis[K, S]] =
    check(
      entries,
      diagnostics,
      provenance,
      source,
      ReductionOrientation.MirroredEndpoints,
      source.rows.flatMap(row => Vector(row.left -> row.result, row.right -> row.result)),
      Reduction.distinct(source.diagnostics.unmatchedLeft ++ source.diagnostics.unmatchedRight),
      source.diagnostics.ambiguous.flatMap {
        case PairingAmbiguity.DuplicateLeft(key, indices)  => Vector(key -> indices)
        case PairingAmbiguity.DuplicateRight(key, indices) => Vector(key -> indices)
      }
    )

  /** The reduction is regrouped from the source exactly as the reduction
    * cursor groups it: keys in order of first appearance over contributions,
    * unmatched keys and ambiguities. The stored rows must cover exactly those
    * keys in that order; each row's successful and failed counts must be the
    * group's; each row's outcome must be the one the group and policy force
    * (ambiguity, no scores, a rejected failure count) or otherwise a score or
    * a mean failure, which alone needs arithmetic and is not recomputed. Every
    * report count is recomputed from the rows and the source, and the stored
    * provenance must equal the derivation.
    */
  private def check[K, S](
      entries: Vector[ReductionRow[K, S]],
      diagnostics: ReductionReport[K],
      provenance: Provenance,
      source: PairwiseAnalysis[?, ?, ?, S],
      orientation: ReductionOrientation,
      contributions: Vector[(K, Either[?, S])],
      unmatched: Vector[K],
      ambiguous: Vector[(K, Vector[Int])]
  ): Either[ReconstructionError[K], Analysis[K, S]] =
    var keys    = Vector.empty[K]
    var grouped = Map.empty[K, Vector[Either[?, S]]]
    var firsts  = Map.empty[K, Vector[Int]]
    contributions.foreach { case (key, result) =>
      if !grouped.contains(key) then keys :+= key
      grouped = grouped.updated(key, grouped.getOrElse(key, Vector.empty) :+ result)
    }
    unmatched.foreach { key =>
      if !grouped.contains(key) then
        keys :+= key
        grouped = grouped.updated(key, Vector.empty)
    }
    ambiguous.foreach { case (key, indices) =>
      if !grouped.contains(key) then
        keys :+= key
        grouped = grouped.updated(key, Vector.empty)
      if !firsts.contains(key) then firsts = firsts.updated(key, indices)
    }
    val successfulPairs = source.rows.count(_.result.isRight)
    val failedPairs     = source.rows.size - successfulPairs
    val failedKeys      = entries.collect { case row if row.result.isLeft => row.key }
    val counts          = Vector(
      (
        "eligiblePairCount",
        source.diagnostics.eligiblePairCount,
        diagnostics.eligiblePairCount
      ),
      (
        "selectedPairCount",
        source.diagnostics.selectedPairCount.toLong,
        diagnostics.selectedPairCount.toLong
      ),
      ("successfulPairCount", successfulPairs.toLong, diagnostics.successfulPairCount.toLong),
      ("failedPairCount", failedPairs.toLong, diagnostics.failedPairCount.toLong),
      ("contributionCount", contributions.size.toLong, diagnostics.contributionCount.toLong),
      (
        "reducedKeyCount",
        (entries.size - failedKeys.size).toLong,
        diagnostics.reducedKeyCount.toLong
      )
    )
    def forced(key: K, successful: Int, failed: Int): Option[ReductionError[K]] =
      firsts.get(key) match
        case Some(indices)                    => Some(ReductionError.AmbiguousKey(key, indices))
        case None if successful + failed == 0 => Some(ReductionError.NoSelectedScores(key))
        case None                             =>
          diagnostics.policy match
            case FailurePolicy.RequireAll if failed > 0 =>
              Some(ReductionError.FailedScores(key, successful, failed))
            case FailurePolicy.SuccessfulOnly(minimum) if successful < minimum.value =>
              Some(
                ReductionError.InsufficientSuccessful(key, minimum.value, successful, failed)
              )
            case _ => None
    def shape(found: Either[ReductionError[K], S]): Option[ReductionError[K]] = found match
      case Left(ReductionError.MeanFailure(_, _)) => None
      case Left(error)                            => Some(error)
      case Right(_)                               => None
    val rows = entries.collectFirst {
      case row
          if grouped
            .get(row.key)
            .exists(scores =>
              scores.count(_.isRight) != row.successful ||
                scores.size - scores.count(_.isRight) != row.failed
            ) =>
        val scores     = grouped(row.key)
        val successful = scores.count(_.isRight)
        ReconstructionError.KeyDenominator(
          row.key,
          successful,
          scores.size - successful,
          row.successful,
          row.failed
        )
      case row
          if grouped.contains(row.key) &&
            forced(row.key, row.successful, row.failed) != shape(row.result) =>
        ReconstructionError.OutcomeShape(
          row.key,
          forced(row.key, row.successful, row.failed),
          row.result.left.toOption
        )
    }
    if diagnostics.orientation != orientation then
      Left(ReconstructionError.Orientation(orientation, diagnostics.orientation))
    else if entries.map(_.key) != keys then
      Left(ReconstructionError.KeyDomain(keys, entries.map(_.key)))
    else
      rows.toLeft(()).flatMap { _ =>
        counts.collectFirst {
          case (field, expected, found) if expected != found =>
            ReconstructionError.ReportCount[K](field, expected, found)
        } match
          case Some(error) => Left(error)
          case None        =>
            val derived = Reduction.provenance(source, diagnostics)
            if diagnostics.failedKeys != failedKeys then
              Left(ReconstructionError.FailedKeys(failedKeys, diagnostics.failedKeys))
            else if derived != provenance then
              Left(ReconstructionError.ProvenanceConflict("reducePairs", provenance, derived))
            else Right(new Analysis(entries, diagnostics, provenance, source))
      }
