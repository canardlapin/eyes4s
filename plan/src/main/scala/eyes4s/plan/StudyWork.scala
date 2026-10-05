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
  * or comparisons. Preparation includes source indexing, identity checks, canonical
  * sorting and digest rendering. It is outside the bounded cursor-step guarantee;
  * arbitrary custom projections, ordering and digest callbacks must be pure/stable.
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
    /** The input positions of the control design's focal trials: those with a
      * matched reference, in focal order (bead S0.7b). A control pair's left
      * index addresses this vector, a matched pair's [[focalIndices]].
      */
    private[plan] val controlFocalIndices: Vector[Int],
    val excludedPhases: Vector[K],
    val frameChecks: Vector[Either[StudyFailure[K], Unit]],
    val windowChecks: Vector[Either[StudyFailure[K], Unit]],
    val windowTallies: Vector[(K, Either[GeometryError, WindowTally])],
    val initialFixationTallies: Vector[(K, Either[GeometryError, InitialFixationTally])],
    private val keptPaths: Vector[Either[StudyFailure[K], Scanpath[U]]],
    val matched: DirectedPairSchedule[K, K],
    val controls: DirectedPairSchedule[K, K],
    val candidateVisitsAcrossScales: Long,
    val budget: PairScheduleBudget,
    private[plan] val countCardinality: CountCardinalityIndex[K],
    private[plan] val keysPerDesign: Long,
    private[plan] val controlKeys: Long
):
  private[plan] val countIdentity = new StudyCountIdentity

  val inputReference: ArtifactRef[StudyInput[K, U]] = input.reference
  val layoutId: DefinitionId                        = plan.layout.id
  val methodId: DefinitionId                        = plan.method.id
  val estimates: Vector[StudyEstimate[U]]           = plan.estimates

  /** The plan grid every mass shares; its cell count is the declared work of
    * one bounded comparison, so a driver can bound a comparison stage.
    */
  val grid: Grid[U] = plan.grid

  /** Where the plan's maps live and what it does with fixations outside. */
  val geometry: StudyGeometry[U] = plan.geometry

  /** Totals of [[windowTallies]]: records outside the window and the screen. */
  def windowSummary: WindowSummary = WindowSummary.of(windowTallies)

  /** Which fixations at the start of every trial the plan leaves out. */
  val initialFixations: InitialFixationPolicy[U] = plan.initialFixations

  /** Totals of [[initialFixationTallies]]: initial fixations dropped. */
  def initialFixationSummary: InitialFixationSummary =
    InitialFixationSummary.of(initialFixationTallies)

  /** One trial's fixations after the initial-fixation policy, or the failure
    * (another frame, or no fixation kept) that prevents mapping it.
    */
  private[plan] def keptPath(index: Int): Either[StudyFailure[K], Scanpath[U]] =
    keptPaths(index)

  /** How the plan pairs focal trials with matched and control references. */
  val pairing: StudyPairing = plan.pairing

  /** Whether every focal trial's matched reference is well defined under the
    * plan's pairing, computed once from this exact matched schedule. The same
    * value drives preflight, the refusal of [[work]] and [[preview]].
    */
  lazy val matchedCardinality: Either[PlanError, MatchedCardinality[K]] =
    StudyPairingWork.cardinality(
      plan.layout,
      plan.pairing,
      input.trials.rows.map(_.key),
      referenceIndices.map(i => input.trials.rows(i).key),
      matched
    )

  /** Why each focal trial without a matched reference has none, judged
    * against `inventory`, the trial inventory the input was admitted from
    * (bead S0.7b): by design, its reference not admitted, or not pairable.
    */
  def unmatchedReasons(inventory: InventoryLedger): Either[PlanError, UnmatchedReasons[K]] =
    matchedCardinality.map(c => unmatchedReasons(c.unmatched, Some(inventory)))

  private[plan] def unmatchedReasons(
      unmatched: Vector[K],
      inventory: Option[InventoryLedger]
  ): UnmatchedReasons[K] =
    inventory.fold(UnmatchedReasons.undetermined(unmatched))(
      UnmatchedReasons.of(plan.layout, plan.pairing, plan.referencePhase, unmatched, _)
    )

  /** Begin the exact-count traversal without performing any pair visits. */
  def countWork: Either[PlanError, CountCursor[K]] = CountCursor.of(plan, this)

  /** Synchronous convenience; effectful consumers execute [[countWork]] through
    * the shared runner for cancellation between pages. Its result includes matched
    * cardinality; matched-only callers do not traverse controls.
    */
  lazy val counts: Either[PlanError, StudyCounts[K]] =
    countWork.flatMap(cursor => Stepwise.complete(cursor, WorkQuanta.default))

  /** The refusal the pairing implies for this input, if any. A version-1
    * pairing on a layout without trial identity never refuses, so its matched
    * schedule is not traversed before execution.
    */
  private[plan] def pairingRefusal: Option[PlanError] =
    if !plan.pairing.canRefuse && plan.layout.trial.isEmpty then None
    else matchedCardinality.fold(Some(_), _.refusal(plan.layout))

  /** Inspect the exact schedules and reduction choices without numerical work. */
  def preview: Either[PlanError, StudyPreview[K, U]] = previewWith(None)

  /** Attach completed counts only to the prepared input and choices that produced them. */
  def preview(counts: StudyCounts[K]): Either[PlanError, StudyPreview[K, U]] =
    if counts.input != inputReference then
      Left(PlanError.ArtifactMismatch(counts.input.digest, inputReference.digest))
    else if counts.description != description then
      Left(PlanError.ChangedPreparedPlan(methodId, layoutId))
    else previewWith(Some(counts))

  private def previewWith(
      counts: Option[StudyCounts[K]]
  ): Either[PlanError, StudyPreview[K, U]] =
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
        plan.policy,
        windowTallies,
        plan.pairing,
        () => counts.fold(matchedCardinality)(c => Right(c.cardinality)),
        initialFixationTallies,
        counts
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

  /** Start from counts produced by this exact prepared object, without revisiting pairs.
    * Equivalent-looking descriptions or legacy input hashes are not ownership evidence.
    */
  def countedWork(
      counts: StudyCounts[K],
      budget: ComparisonBudget = ComparisonBudget.default
  ): Either[PlanError, StudyCursor[K, U, S, D]] =
    checkUnchanged
      .flatMap(_ =>
        Either.cond(
          counts.owner eq countIdentity,
          (),
          PlanError.ChangedPreparedPlan(methodId, layoutId)
        )
      )
      .flatMap(_ => counts.pairingRefusal.toLeft(()))
      .flatMap(_ => StudyWork.begin(plan, this, budget, occupancy, Vector.empty))

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
    checkUnchanged
      .flatMap(_ => pairingRefusal.toLeft(()))
      .flatMap(_ => StudyWork.begin(plan, this, budget, occupancy, context))

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
      left   = focal.map(i => input.trials.rows(i).key)
      right  = reference.map(i => input.trials.rows(i).key)
      frames = input.trials.rows.map(t =>
        Agreement
          .frames(plan.geometry.admission, t.value.frame)
          .map(_ => ())
          .left
          .map(StudyFailure.Frame(t.key, _))
      )
      kept = frames.zip(input.trials.rows).map { case (frame, t) =>
        frame.flatMap(_ => plan.initialFixationRule.kept(t.key, t.value))
      }
      relations    = StudyPairingWork.relations(plan.layout, plan.pairing, right)
      controlFocal = focal.filter(i => relations.matchable(input.trials.rows(i).key))
      matched <- DirectedPairSchedule
        .exhaustive(left, right, relations.matched, budget)
        .left
        .map(PlanError.Schedule.apply)
      controls <- DirectedPairSchedule
        // Controls only for queries with a match (bead S0.7b).
        .exhaustive(
          controlFocal.map(i => input.trials.rows(i).key),
          right,
          relations.controls,
          budget
        )
        .left
        .map(PlanError.Schedule.apply)
    yield new PreparedStudy(
      plan,
      input,
      plan.description,
      focal,
      reference,
      controlFocal,
      phases.indices
        .filter(i => phases(i) != plan.focalPhase && phases(i) != plan.referencePhase)
        .map(i => input.trials.rows(i).key)
        .toVector,
      frames,
      kept.zip(input.trials.rows).map { case (path, t) =>
        path.flatMap(StudyWindowing.check(plan.geometry, t.key, _))
      },
      plan.windowTallies(input),
      plan.initialFixationTallies(input),
      kept,
      matched,
      controls,
      candidateVisits.toLong,
      budget,
      StudyPairingWork
        .cardinalityBuilder(
          plan.layout,
          plan.pairing,
          input.trials.rows.map(_.key),
          right,
          left
        ),
      // Every focal key has a row in the matched reduction and the contrast;
      // only those with a match are in the control design (bead S0.7b).
      left.distinct.size.toLong,
      controlFocal.map(i => input.trials.rows(i).key).distinct.size.toLong
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
    val matched: FocalSchedule[K],
    val controls: FocalSchedule[K],
    val excludedPhases: Vector[K],
    val capability: ExecutionCapability,
    val beginScale: Int => Either[PlanError, StudyScaleWork[K, U, S, D]],
    val reduce: DirectedPairwiseAnalysis[K, K, StudyFailure[K], S] => ReductionCursor[K, S],
    val contrast: (
        Analysis[K, S],
        Analysis[K, S]
    ) => Either[ContrastError[K], ContrastCursor[K, S, D]],
    val finish: Vector[StudyScaleResult[K, U, S, D]] => StudyResult[K, U, S, D]
):
  /** A typed directed pair analysis whose failures are study failures. */
  type Source = DirectedPairwiseAnalysis[K, K, StudyFailure[K], S]

/** One scale's comparison instance and specification, created once per scale. */
private[plan] final class StudyScaleWork[K, U <: Unit2D, S, D](
    val estimateTrial: Int => (K, Either[StudyFailure[K], Mass[U]]),
    val evaluate: (
        FocalSchedule[K],
        Vector[(K, Either[StudyFailure[K], Mass[U]])]
    ) => EvaluationCursor[K, K, StudyFailure[K], S]
)

/** A pair schedule with the input positions of the focal trials its pairs'
  * left indices address. Each design has its own: the control design has
  * only the focal trials with a match (bead S0.7b).
  */
private[plan] final case class FocalSchedule[K](
    schedule: DirectedPairSchedule[K, K],
    focal: Vector[Int]
)

/** Phases carry the typed pair analyses forward, so the completed scale keeps
  * its sources with their key and failure types intact.
  */
private[plan] sealed trait StudyPhase[K, S, D]
private[plan] object StudyPhase:
  /** A typed directed pair analysis whose failures are study failures. */
  type Source[K, S] = DirectedPairwiseAnalysis[K, K, StudyFailure[K], S]

  /** Estimating the density of input trial `trial`, by index in input order. */
  final case class Estimate[K, S, D](trial: Int) extends StudyPhase[K, S, D]

  /** Evaluating the matched pair schedule. */
  final case class CompareMatched[K, S, D](
      cursor: EvaluationCursor[K, K, StudyFailure[K], S]
  ) extends StudyPhase[K, S, D]

  /** Reducing the evaluated matched pairs by focal key. */
  final case class ReduceMatched[K, S, D](source: Source[K, S], cursor: ReductionCursor[K, S])
      extends StudyPhase[K, S, D]

  /** Evaluating the control schedule, carrying the matched analysis forward. */
  final case class CompareControl[K, S, D](
      matchedSource: Source[K, S],
      matched: Analysis[K, S],
      cursor: EvaluationCursor[K, K, StudyFailure[K], S]
  ) extends StudyPhase[K, S, D]

  /** Reducing the evaluated control pairs by focal key. */
  final case class ReduceControl[K, S, D](
      matchedSource: Source[K, S],
      matched: Analysis[K, S],
      controlSource: Source[K, S],
      cursor: ReductionCursor[K, S]
  ) extends StudyPhase[K, S, D]

  /** Forming the keyed matched-minus-control contrast from both analyses. */
  final case class Contrasting[K, S, D](
      analyses: StudyAnalyses[K, S],
      cursor: ContrastCursor[K, S, D]
  ) extends StudyPhase[K, S, D]

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
    private val current: StudyScaleWork[K, U, S, D],
    private val masses: Vector[(K, Either[StudyFailure[K], Mass[U]])],
    private val phase: StudyPhase[K, S, D],
    private val completed: Vector[StudyScaleResult[K, U, S, D]]
):
  import StudyPhase.*

  /** The stage the next `advance` will work on. */
  def stage: StudyStage = phase match
    case Estimate(trial)           => StudyStage.Estimating(scale, trial)
    case CompareMatched(_)         => StudyStage.Comparing(scale, StudyDesign.Matched)
    case ReduceMatched(_, _)       => StudyStage.Reducing(scale, StudyDesign.Matched)
    case CompareControl(_, _, _)   => StudyStage.Comparing(scale, StudyDesign.Control)
    case ReduceControl(_, _, _, _) => StudyStage.Reducing(scale, StudyDesign.Control)
    case Contrasting(_, _)         => StudyStage.Contrasting(scale)

  def capability: ExecutionCapability = engine.capability

  /** Attempted maps over all scales, including failed estimations. */
  def completedMaps: Long = completed.iterator.map(_.estimation.size.toLong).sum + masses.size

  /** Decided pair rows over all scales, including failed comparisons. */
  def completedPairs: Long =
    val previous = completed.iterator.map { result =>
      result.analyses.matchedSource.rows.size.toLong + result.analyses.controlSource.rows.size
    }.sum
    val current = phase match
      case Estimate(_)                       => 0L
      case CompareMatched(cursor)            => cursor.completedPairs.toLong
      case ReduceMatched(source, _)          => source.rows.size.toLong
      case CompareControl(source, _, cursor) => source.rows.size.toLong + cursor.completedPairs
      case ReduceControl(matched, _, control, _) => matched.rows.size.toLong + control.rows.size
      case Contrasting(analyses, _)              =>
        analyses.matchedSource.rows.size.toLong + analyses.controlSource.rows.size
    previous + current

  /** Recorded reduction keys over every scientific scale and both designs. */
  def completedReductionKeys: Long =
    val previous = completed.iterator
      .map(result =>
        result.analyses.matched.entries.size.toLong + result.analyses.control.entries.size
      )
      .sum
    val current = phase match
      case Estimate(_) | CompareMatched(_)      => 0L
      case ReduceMatched(_, cursor)             => cursor.reducedKeys.toLong
      case CompareControl(_, matched, _)        => matched.entries.size.toLong
      case ReduceControl(_, matched, _, cursor) =>
        matched.entries.size.toLong + cursor.reducedKeys
      case Contrasting(analyses, _) =>
        analyses.matched.entries.size.toLong + analyses.control.entries.size
    previous + current

  /** Recorded contrast rows; a refused contrast contributes no invented rows. */
  def completedContrastRows: Long =
    completed.iterator.map(_.contrast.toOption.fold(0L)(_.rows.size.toLong)).sum +
      (phase match
        case Contrasting(_, cursor) => cursor.contrastedKeys.toLong
        case _                      => 0L)

  /** The exact units of the reduction the next `advance` works on, known
    * once its scores are realised; `None` outside a reducing stage.
    */
  def reductionUnits: Option[Long] = phase match
    case ReduceMatched(_, cursor)       => Some(cursor.declaredUnits)
    case ReduceControl(_, _, _, cursor) => Some(cursor.declaredUnits)
    case _                              => None

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
          Right(ReduceMatched(analysis, engine.reduce(analysis)))
        )

      case ReduceMatched(source, cursor) =>
        reduce(quanta, cursor)(ReduceMatched(source, _))(matched =>
          Right(CompareControl(source, matched, current.evaluate(engine.controls, masses)))
        )

      case CompareControl(matchedSource, matched, cursor) =>
        compare(quanta, cursor)(CompareControl(matchedSource, matched, _))(analysis =>
          Right(ReduceControl(matchedSource, matched, analysis, engine.reduce(analysis)))
        )

      case ReduceControl(matchedSource, matched, controlSource, cursor) =>
        cursor.advance(quanta.pairs) match
          case ReductionPage.More(units, next) =>
            Right(
              StudyStep.More(
                stage,
                units,
                copy(phase = ReduceControl(matchedSource, matched, controlSource, next))
              )
            )
          case ReductionPage.Done(units, control) =>
            val analyses = new StudyAnalyses(matchedSource, matched, controlSource, control)
            engine.contrast(matched, control) match
              case Left(error)     => finishScale(units, analyses, Left(error))
              case Right(contrast) =>
                Right(
                  StudyStep.More(stage, units, copy(phase = Contrasting(analyses, contrast)))
                )

      case Contrasting(analyses, cursor) =>
        cursor.advance(quanta.pairs) match
          case ContrastPage.More(units, next) =>
            Right(StudyStep.More(stage, units, copy(phase = Contrasting(analyses, next))))
          case ContrastPage.Done(units, contrast) =>
            finishScale(units, analyses, Right(contrast))

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
      analyses: StudyAnalyses[K, S],
      result: Either[ContrastError[K], Contrast[K, S, D]]
  ): Either[PlanError, StudyStep[K, U, S, D]] =
    val results = completed :+ new StudyScaleResult(
      engine.estimates(scale),
      masses,
      engine.excludedPhases,
      analyses,
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

/** Drives a study cursor to a completed result in bounded steps. Each step's quanta bound its
  * work, so a long study can be interleaved with other work or resumed from an immutable cursor.
  */
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

    // A pair's left index addresses its own schedule's focal trials.
    def operands(
        design: FocalSchedule[K],
        masses: Vector[(K, Either[StudyFailure[K], Mass[U]])]
    ): ScheduledPair[K, K] => Either[StudyFailure[K], (Mass[U], Mass[U])] =
      pair =>
        masses(design.focal(pair.leftIndex))._2.flatMap(left =>
          masses(work.referenceIndices(pair.rightIndex))._2.map(right => (left, right))
        )

    def failure(pair: ScheduledPair[K, K], error: CompareError): StudyFailure[K] =
      StudyFailure.Comparison(pair.left, pair.right, error)

    val engine = new StudyEngine[K, U, S, D](
      work.input.trials.rows.size,
      plan.estimates,
      FocalSchedule(work.matched, work.focalIndices),
      FocalSchedule(work.controls, work.controlFocalIndices),
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
                      design: FocalSchedule[K],
                      masses: Vector[
                        (K, Either[StudyFailure[K], Mass[U]])
                      ]
                  ) =>
                    EvaluationWork.start(
                      design.schedule,
                      work.input.hash,
                      info,
                      PairEvaluation
                        .Bounded(comparison, budget, operands(design, masses), failure)
                    )
                }
            case MethodExecution.Synchronous(factory) =>
              val comparison = factory(plan.parameters)
              val info       = EvaluationInfo.comparison(comparison, specification)
              Right {
                (
                    design: FocalSchedule[K],
                    masses: Vector[
                      (K, Either[StudyFailure[K], Mass[U]])
                    ]
                ) =>
                  EvaluationWork.start(
                    design.schedule,
                    work.input.hash,
                    info,
                    PairEvaluation.Whole[K, K, StudyFailure[K], S](pair =>
                      operands(design, masses)(pair).flatMap { case (left, right) =>
                        comparison.compare(left, right).left.map(failure(pair, _))
                      }
                    )
                  )
              }
        yield new StudyScaleWork[K, U, S, D](
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
