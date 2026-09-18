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

/** Prepared work binds the exact input, plan description, layout and method
  * identity to two reusable schedules. Preparation does no density estimation
  * or comparisons. Source indexing/identity checks are O(source rows), bounded
  * by the supplied budget; arbitrary custom projections must be pure/stable.
  *
  * Pair order follows the original input: focal-major, reference-minor. Keys
  * remain typed values; their display strings and digests never decide equality.
  * `run` and `work` share one [[StudyCursor]]: the ordinary runner drives it to
  * completion, so pure and resumable execution are one scientific path.
  */
final class PreparedStudy[K, U <: Unit2D, P, S, D] private[plan] (
    private val plan: StudyPlan[K, U, P, S, D],
    val input: StudyInput[K, U],
    val description: Vector[(String, Vector[Provenance.Param])],
    val focalIndices: Vector[Int],
    val referenceIndices: Vector[Int],
    val excludedPhases: Vector[K],
    val frameChecks: Vector[Either[StudyFailure[K], Unit]],
    val matched: DirectedPairSchedule[K, K],
    val controls: DirectedPairSchedule[K, K],
    val candidateVisitsAcrossScales: Long,
    val budget: PairScheduleBudget
):
  val inputReference: ArtifactRef[StudyInput[K, U]] = input.reference
  val layoutId: DefinitionId                        = plan.layout.id
  val methodId: DefinitionId                        = plan.method.id
  val estimates: Vector[StudyEstimate[U]]           = plan.estimates

  /** The plan grid every mass shares; its cell count is the declared work of
    * one bounded comparison, so a driver can bound a comparison stage.
    */
  val grid: Grid[U] = plan.grid

  /** Inspect the exact schedules and reduction choices without numerical work. */
  def preview: Either[PlanError, StudyPreview[K, U]] =
    checkUnchanged.map { _ =>
      new StudyPreview(
        inputReference,
        layoutId,
        methodId,
        description,
        focalIndices.map(i => input.trials.rows(i).key),
        referenceIndices.map(i => input.trials.rows(i).key),
        excludedPhases,
        matched,
        controls,
        plan.policy
      )
    }

  private def checkUnchanged: Either[PlanError, Unit] =
    Either.cond(
      plan.description == description,
      (),
      PlanError.ChangedPreparedPlan(methodId, layoutId)
    )

  /** How finely the method's comparisons can be interrupted; typed evidence, not a flag. */
  def capability: ExecutionCapability = plan.method.capability

  /** Execute this exact prepared plan, retaining its source order and evidence. */
  def run: Either[PlanError, StudyResult[K, U, S, D]] =
    execute(occupancy, Vector.empty)

  /** Resumable execution at whatever granularity the method supports.
    *
    * For a [[ExecutionCapability.BoundedComparison]] method every comparison
    * is checked against `budget` when a scale begins, before any trial of that
    * scale is estimated, because all masses share the plan grid. A synchronous
    * method ignores the budget: its comparisons are whole operations and the
    * cursor's capability says so.
    */
  def work(
      budget: ComparisonBudget = ComparisonBudget.default
  ): Either[PlanError, StudyCursor[K, U, S, D]] =
    work(budget, occupancy, Vector.empty)

  /** Resumable execution that refuses a method without bounded comparison
    * support, so an unsupported synchronous extension is diagnosed before any
    * work begins.
    */
  def boundedWork(
      budget: ComparisonBudget = ComparisonBudget.default
  ): Either[PlanError, StudyCursor[K, U, S, D]] =
    plan.method.execution match
      case MethodExecution.Bounded(_)     => work(budget)
      case MethodExecution.Synchronous(_) =>
        Left(PlanError.UnsupportedExecution(methodId, capability))

  private def occupancy(key: K, path: Scanpath[U]): Either[StudyFailure[K], PointMeasure[U]] =
    path.occupancy(plan.weight).left.map(StudyFailure.Occupancy(key, _))

  private[plan] def execute(
      occupancy: (K, Scanpath[U]) => Either[StudyFailure[K], PointMeasure[U]],
      context: Vector[(String, Provenance.Param)]
  ): Either[PlanError, StudyResult[K, U, S, D]] =
    work(ComparisonBudget.default, occupancy, context).flatMap(StudyWork.complete(_))

  private[plan] def work(
      budget: ComparisonBudget,
      occupancy: (K, Scanpath[U]) => Either[StudyFailure[K], PointMeasure[U]],
      context: Vector[(String, Provenance.Param)]
  ): Either[PlanError, StudyCursor[K, U, S, D]] =
    checkUnchanged.flatMap(_ => StudyWork.begin(plan, this, budget, occupancy, context))

object PreparedStudy:
  private[plan] def build[K, U <: Unit2D, P, S, D](
      plan: StudyPlan[K, U, P, S, D],
      input: StudyInput[K, U],
      budget: PairScheduleBudget
  ): Either[PlanError, PreparedStudy[K, U, P, S, D]] =
    for
      _ <- budget
        .checkCounts(input.trials.rows.size.toLong, 0L)
        .left
        .map(PlanError.Schedule.apply)
      phases          = input.trials.rows.map(t => plan.layout.phase(t.key))
      focal           = phases.indices.filter(i => phases(i) == plan.focalPhase).toVector
      reference       = phases.indices.filter(i => phases(i) == plan.referencePhase).toVector
      candidateVisits = BigInt(focal.size) * reference.size * 2 * plan.estimates.size
      _ <- Either.cond(
        candidateVisits <= budget.maxCandidatePairs,
        (),
        PlanError.StudyWorkBudget(
          focal.size,
          reference.size,
          plan.estimates.size,
          budget.maxCandidatePairs
        )
      )
      left          = focal.map(i => input.trials.rows(i).key)
      right         = reference.map(i => input.trials.rows(i).key)
      matchedDesign = Pairing
        .between[K, K]
        .sameOn(plan.layout.participant, plan.layout.participant)
        .sameOn(plan.layout.stimulus, plan.layout.stimulus)
        .all
      controlDesign = Pairing
        .between[K, K]
        .sameOn(plan.layout.participant, plan.layout.participant)
        .differentOn(plan.layout.stimulus, plan.layout.stimulus)
        .all
      matched <- DirectedPairSchedule
        .exhaustive(left, right, matchedDesign.relation, budget)
        .left
        .map(PlanError.Schedule.apply)
      controls <- DirectedPairSchedule
        .exhaustive(left, right, controlDesign.relation, budget)
        .left
        .map(PlanError.Schedule.apply)
    yield new PreparedStudy(
      plan,
      input,
      plan.description,
      focal,
      reference,
      phases.indices
        .filter(i => phases(i) != plan.focalPhase && phases(i) != plan.referencePhase)
        .map(i => input.trials.rows(i).key)
        .toVector,
      input.trials.rows.map(t =>
        Agreement
          .frames(plan.grid.frame, t.value.frame)
          .map(_ => ())
          .left
          .map(StudyFailure.Frame(t.key, _))
      ),
      matched,
      controls,
      candidateVisits.toLong,
      budget
    )

/** Which within-participant design a step worked on. */
enum StudyDesign derives CanEqual:
  case Matched, Control

/** The scientific stage a study step advanced. Indices address the plan's
  * estimate list and the input's trial rows.
  */
enum StudyStage derives CanEqual:
  case Estimating(scale: Int, trial: Int)
  case Comparing(scale: Int, design: StudyDesign)
  case Reducing(scale: Int, design: StudyDesign)
  case Contrasting(scale: Int)

  def scale: Int

/** One step of study execution. `workUnits` is the step's own count: one per
  * estimated trial, and the evaluation, reduction or contrast units otherwise.
  * `Done` carries the units of the step that completed the last scale.
  */
enum StudyStep[K, U <: Unit2D, S, D]:
  case More(stage: StudyStage, workUnits: Int, next: StudyCursor[K, U, S, D])
  case Done(workUnits: Int, result: StudyResult[K, U, S, D])

/** Estimation, evaluation, reduction and contrast of one plan, with the
  * plan's parameter type captured so the cursor need not name it.
  */
private[plan] final class StudyEngine[K, U <: Unit2D, S, D](
    val trials: Int,
    val estimates: Vector[StudyEstimate[U]],
    val matched: DirectedPairSchedule[K, K],
    val controls: DirectedPairSchedule[K, K],
    val excludedPhases: Vector[K],
    val capability: ExecutionCapability,
    val beginScale: Int => Either[PlanError, StudyScale[K, U, S, D]],
    val reduce: DirectedPairwiseAnalysis[K, K, StudyFailure[K], S] => ReductionCursor[K, S],
    val contrast: (
        Analysis[K, S],
        Analysis[K, S]
    ) => Either[ContrastError[K], ContrastCursor[K, S, D]],
    val finish: Vector[StudyScaleResult[K, U, S, D]] => StudyResult[K, U, S, D]
)

/** One scale's comparison instance and specification, created once per scale. */
private[plan] final class StudyScale[K, U <: Unit2D, S, D](
    val estimateTrial: Int => (K, Either[StudyFailure[K], Mass[U]]),
    val evaluate: (
        DirectedPairSchedule[K, K],
        Vector[(K, Either[StudyFailure[K], Mass[U]])]
    ) => EvaluationCursor[K, K, StudyFailure[K], S]
)

private[plan] sealed trait StudyPhase[K, S, D]
private[plan] object StudyPhase:
  final case class Estimate[K, S, D](trial: Int) extends StudyPhase[K, S, D]
  final case class CompareMatched[K, S, D](
      cursor: EvaluationCursor[K, K, StudyFailure[K], S]
  ) extends StudyPhase[K, S, D]
  final case class ReduceMatched[K, S, D](cursor: ReductionCursor[K, S])
      extends StudyPhase[K, S, D]
  final case class CompareControl[K, S, D](
      matched: Analysis[K, S],
      cursor: EvaluationCursor[K, K, StudyFailure[K], S]
  ) extends StudyPhase[K, S, D]
  final case class ReduceControl[K, S, D](
      matched: Analysis[K, S],
      cursor: ReductionCursor[K, S]
  ) extends StudyPhase[K, S, D]
  final case class Contrasting[K, S, D](cursor: ContrastCursor[K, S, D])
      extends StudyPhase[K, S, D]

/** An immutable position inside study execution.
  *
  * Per scale, in input order: one trial's estimation per step (a whole
  * operation), then matched evaluation, its by-focal reduction, control
  * evaluation, its reduction, and the keyed contrast, each in bounded steps.
  * Scales complete in plan order. Every intermediate value is retained in the
  * cursor, so re-advancing the same cursor is deterministic.
  */
final class StudyCursor[K, U <: Unit2D, S, D] private[plan] (
    private val engine: StudyEngine[K, U, S, D],
    private val scale: Int,
    private val current: StudyScale[K, U, S, D],
    private val masses: Vector[(K, Either[StudyFailure[K], Mass[U]])],
    private val phase: StudyPhase[K, S, D],
    private val completed: Vector[StudyScaleResult[K, U, S, D]]
):
  import StudyPhase.*

  /** The stage the next `advance` will work on. */
  def stage: StudyStage = phase match
    case Estimate(trial)      => StudyStage.Estimating(scale, trial)
    case CompareMatched(_)    => StudyStage.Comparing(scale, StudyDesign.Matched)
    case ReduceMatched(_)     => StudyStage.Reducing(scale, StudyDesign.Matched)
    case CompareControl(_, _) => StudyStage.Comparing(scale, StudyDesign.Control)
    case ReduceControl(_, _)  => StudyStage.Reducing(scale, StudyDesign.Control)
    case Contrasting(_)       => StudyStage.Contrasting(scale)

  def capability: ExecutionCapability = engine.capability

  /** The exact units of the reduction the next `advance` works on, known
    * once its scores are realised; `None` outside a reducing stage.
    */
  def reductionUnits: Option[Long] = phase match
    case ReduceMatched(cursor)    => Some(cursor.declaredUnits)
    case ReduceControl(_, cursor) => Some(cursor.declaredUnits)
    case _                        => None

  def advance(quanta: WorkQuanta): Either[PlanError, StudyStep[K, U, S, D]] =
    phase match
      case Estimate(trial) =>
        val next                       = masses :+ current.estimateTrial(trial)
        val after: StudyPhase[K, S, D] =
          if trial + 1 < engine.trials then Estimate(trial + 1)
          else CompareMatched(current.evaluate(engine.matched, next))
        Right(StudyStep.More(stage, 1, copy(masses = next, phase = after)))

      case CompareMatched(cursor) =>
        compare(quanta, cursor)(CompareMatched(_))(analysis =>
          Right(ReduceMatched(engine.reduce(analysis)))
        )

      case ReduceMatched(cursor) =>
        reduce(quanta, cursor)(ReduceMatched(_))(matched =>
          Right(CompareControl(matched, current.evaluate(engine.controls, masses)))
        )

      case CompareControl(matched, cursor) =>
        compare(quanta, cursor)(CompareControl(matched, _))(analysis =>
          Right(ReduceControl(matched, engine.reduce(analysis)))
        )

      case ReduceControl(matched, cursor) =>
        cursor.advance(quanta.pairs) match
          case ReductionPage.More(units, next) =>
            Right(StudyStep.More(stage, units, copy(phase = ReduceControl(matched, next))))
          case ReductionPage.Done(units, control) =>
            engine.contrast(matched, control) match
              case Left(error)     => finishScale(units, Left(error))
              case Right(contrast) =>
                Right(StudyStep.More(stage, units, copy(phase = Contrasting(contrast))))

      case Contrasting(cursor) =>
        cursor.advance(quanta.pairs) match
          case ContrastPage.More(units, next) =>
            Right(StudyStep.More(stage, units, copy(phase = Contrasting(next))))
          case ContrastPage.Done(units, contrast) => finishScale(units, Right(contrast))

  private def compare(
      quanta: WorkQuanta,
      cursor: EvaluationCursor[K, K, StudyFailure[K], S]
  )(
      continue: EvaluationCursor[K, K, StudyFailure[K], S] => StudyPhase[K, S, D]
  )(
      done: DirectedPairwiseAnalysis[K, K, StudyFailure[K], S] => Either[
        PlanError,
        StudyPhase[K, S, D]
      ]
  ): Either[PlanError, StudyStep[K, U, S, D]] =
    cursor.advance(quanta) match
      case Left(EvaluationWorkError.Schedule(error))   => Left(PlanError.Schedule(error))
      case Left(EvaluationWorkError.Comparison(error)) => Left(PlanError.ComparisonWork(error))
      case Right(EvaluationPage.More(units, next))     =>
        Right(StudyStep.More(stage, units, copy(phase = continue(next))))
      case Right(EvaluationPage.Done(units, analysis)) =>
        done(analysis).map(next => StudyStep.More(stage, units, copy(phase = next)))

  private def reduce(
      quanta: WorkQuanta,
      cursor: ReductionCursor[K, S]
  )(
      continue: ReductionCursor[K, S] => StudyPhase[K, S, D]
  )(
      done: Analysis[K, S] => Either[PlanError, StudyPhase[K, S, D]]
  ): Either[PlanError, StudyStep[K, U, S, D]] =
    cursor.advance(quanta.pairs) match
      case ReductionPage.More(units, next) =>
        Right(StudyStep.More(stage, units, copy(phase = continue(next))))
      case ReductionPage.Done(units, analysis) =>
        done(analysis).map(next => StudyStep.More(stage, units, copy(phase = next)))

  private def finishScale(
      units: Int,
      result: Either[ContrastError[K], Contrast[K, S, D]]
  ): Either[PlanError, StudyStep[K, U, S, D]] =
    val results = completed :+ new StudyScaleResult(
      engine.estimates(scale),
      masses,
      engine.excludedPhases,
      result
    )
    if scale + 1 < engine.estimates.size then
      StudyWork
        .beginScale(engine, scale + 1, results)
        .map(next => StudyStep.More(stage, units, next))
    else Right(StudyStep.Done(units, engine.finish(results)))

  private def copy(
      masses: Vector[(K, Either[StudyFailure[K], Mass[U]])] = masses,
      phase: StudyPhase[K, S, D]
  ): StudyCursor[K, U, S, D] =
    new StudyCursor(engine, scale, current, masses, phase, completed)

object StudyWork:

  /** Drive a study cursor to completion with fixed quanta. */
  def complete[K, U <: Unit2D, S, D](
      cursor: StudyCursor[K, U, S, D],
      quanta: WorkQuanta = WorkQuanta.default
  ): Either[PlanError, StudyResult[K, U, S, D]] =
    @annotation.tailrec
    def loop(cursor: StudyCursor[K, U, S, D]): Either[PlanError, StudyResult[K, U, S, D]] =
      cursor.advance(quanta) match
        case Left(error)                       => Left(error)
        case Right(StudyStep.More(_, _, next)) => loop(next)
        case Right(StudyStep.Done(_, result))  => Right(result)
    loop(cursor)

  private[plan] def begin[K, U <: Unit2D, P, S, D](
      plan: StudyPlan[K, U, P, S, D],
      work: PreparedStudy[K, U, P, S, D],
      budget: ComparisonBudget,
      occupancy: (K, Scanpath[U]) => Either[StudyFailure[K], PointMeasure[U]],
      context: Vector[(String, Provenance.Param)]
  ): Either[PlanError, StudyCursor[K, U, S, D]] =
    given Ordering[K]        = plan.layout.ordering
    given ScoreMean[S]       = plan.method.mean
    given Contrastable[S, D] = plan.method.difference

    def operands(
        masses: Vector[(K, Either[StudyFailure[K], Mass[U]])]
    )(pair: ScheduledPair[K, K]): Either[StudyFailure[K], (Mass[U], Mass[U])] =
      masses(work.focalIndices(pair.leftIndex))._2.flatMap(left =>
        masses(work.referenceIndices(pair.rightIndex))._2.map(right => (left, right))
      )

    def failure(pair: ScheduledPair[K, K], error: CompareError): StudyFailure[K] =
      StudyFailure.Comparison(pair.left, pair.right, error)

    val engine = new StudyEngine[K, U, S, D](
      work.input.trials.rows.size,
      plan.estimates,
      work.matched,
      work.controls,
      work.excludedPhases,
      plan.method.capability,
      scale =>
        val estimate = plan.estimates(scale)
        for
          specification <- plan.specification(estimate, context)
          scaled        <- plan.method.execution match
            case MethodExecution.Bounded(factory) =>
              val comparison = factory(plan.parameters)
              budget
                .check(comparison.info.name, plan.grid.size.toLong)
                .left
                .map(PlanError.ComparisonWork.apply)
                .map { _ =>
                  val info = EvaluationInfo.comparison(comparison, specification)
                  (
                      schedule: DirectedPairSchedule[K, K],
                      masses: Vector[
                        (K, Either[StudyFailure[K], Mass[U]])
                      ]
                  ) =>
                    EvaluationWork.start(
                      schedule,
                      work.input.hash,
                      info,
                      PairEvaluation.Bounded(comparison, budget, operands(masses), failure)
                    )
                }
            case MethodExecution.Synchronous(factory) =>
              val comparison = factory(plan.parameters)
              val info       = EvaluationInfo.comparison(comparison, specification)
              Right {
                (
                    schedule: DirectedPairSchedule[K, K],
                    masses: Vector[
                      (K, Either[StudyFailure[K], Mass[U]])
                    ]
                ) =>
                  EvaluationWork.start(
                    schedule,
                    work.input.hash,
                    info,
                    PairEvaluation.Whole[K, K, StudyFailure[K], S](pair =>
                      operands(masses)(pair).flatMap { case (left, right) =>
                        comparison.compare(left, right).left.map(failure(pair, _))
                      }
                    )
                  )
              }
        yield new StudyScale[K, U, S, D](
          trial => plan.estimateTrial(work, estimate, occupancy, trial),
          scaled
        )
      ,
      analysis => analysis.meanByLeftWork(plan.policy),
      (matched, control) => contrastWork(matched, control),
      scales => new StudyResult(plan.input, plan.description, scales)
    )
    beginScale(engine, 0, Vector.empty)

  private[plan] def beginScale[K, U <: Unit2D, S, D](
      engine: StudyEngine[K, U, S, D],
      scale: Int,
      completed: Vector[StudyScaleResult[K, U, S, D]]
  ): Either[PlanError, StudyCursor[K, U, S, D]] =
    engine.beginScale(scale).map { current =>
      val phase: StudyPhase[K, S, D] =
        if engine.trials > 0 then StudyPhase.Estimate(0)
        else StudyPhase.CompareMatched(current.evaluate(engine.matched, Vector.empty))
      new StudyCursor(engine, scale, current, Vector.empty, phase, completed)
    }
