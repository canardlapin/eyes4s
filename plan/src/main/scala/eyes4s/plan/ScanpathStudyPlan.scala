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

/** Every comparison row keeps both trial keys, including method refusals. */
type ScanpathPairs[K] =
  DirectedPairwiseAnalysis[K, K, ScanpathComparisonError, ScanpathMethodOutput]

final class ScanpathMean[K] private[plan] (
    val key: K,
    val requested: Int,
    val successful: Int,
    val policy: FailurePolicy,
    val result: Either[ReductionError[K], Double]
):
  def contributing: Int = if result.isRight then successful else 0

final class ScanpathContrast[K] private[plan] (
    val key: K,
    val component: String,
    val matched: ScanpathMean[K],
    val control: ScanpathMean[K]
):
  def difference: Either[ReductionError[K], Double] =
    for
      left  <- matched.result
      right <- control.result
    yield left - right

final class ScanpathStudyResult[K, U <: Unit2D] private[plan] (
    val inputHash: ContentHash,
    val planHash: ContentHash,
    val matched: ScanpathPairs[K],
    val controls: ScanpathPairs[K],
    val contrasts: Vector[ScanpathContrast[K]],
    val excludedPhases: Vector[K]
):
  /** Pair failures retain both operands; component reductions keep the focal
    * key and the matched/control role. Strict overlap summary refusals are
    * reported while their complete query ledgers remain in the result.
    */
  def diagnostics: Vector[Diagnostic[K]] =
    val pairFailures = Vector("matched" -> matched, "control" -> controls).flatMap {
      (role, analysis) =>
        analysis.rows.flatMap { row =>
          row.result
            .flatMap(_.scores.left.map(ScanpathComparisonError.Fixations.apply))
            .left
            .toOption
            .map(error =>
              Diagnostic
                .of(error)
                .mapKeys[K](value => value)
                .within(Locus.Field(role), Locus.Pair(row.left, row.right))
            )
        }
    }
    val reductions = contrasts.flatMap { row =>
      Vector("matched" -> row.matched, "control" -> row.control).flatMap { (role, mean) =>
        mean.result.left.toOption.map(error =>
          Diagnostic
            .of(error)
            .within(Locus.Field(role), Locus.Field(row.component))
        )
      }
    }
    pairFailures ++ reductions

/** Complete raw scanpath study. Preparation and final reduction are whole
  * operations; comparison pages count whole pairs, with no intra-pair bound.
  */
final class ScanpathStudyPlan[K, U <: Unit2D] private (
    val layout: StudyLayout[K],
    val input: StudyInput[K, U],
    val frame: Frame[U],
    val focalPhase: String,
    val referencePhase: String,
    val method: ScanpathMethod[U],
    val pairing: StudyPairing,
    val controls: Selection,
    val failurePolicy: FailurePolicy,
    val budget: PairScheduleBudget,
    val evaluation: EvaluationInfo,
    val planHash: ContentHash,
    val unitSymbol: String
):
  def inputHash: ContentHash                                  = input.hash
  def inputRef: ArtifactRef[StudyInput[K, U]]                 = input.reference
  def description: Vector[(String, Vector[Provenance.Param])] =
    import Provenance.Param.*
    Vector(
      "input"  -> Vector(Text(inputHash.render)),
      "layout" -> Vector(Text(layout.id.name), Text(layout.id.version.toString)),
      "unit"   -> Vector(Text(unitSymbol)),
      "frame"  -> Vector(
        Text(frame.id.name),
        Num(frame.spec.xMin),
        Num(frame.spec.xMax),
        Num(frame.spec.yMin),
        Num(frame.spec.yMax),
        Text(frame.spec.yAxis.toString)
      ),
      "preparationAdmission" -> Vector(
        Text(ScanpathPreparationConvention.ConservativeRawPairsV1.toString)
      ),
      "focalPhase"     -> Vector(Text(focalPhase)),
      "referencePhase" -> Vector(Text(referencePhase)),
      "failurePolicy"  -> Vector(Text(failurePolicy.render)),
      "budget"         -> Vector(
        Text(budget.maxSourceRows.toString),
        Text(budget.maxCandidatePairs.toString),
        Text(budget.maxSelectedPairs.toString)
      ),
      "controlSelection" -> RepetitionViews.selection(controls)
    ) ++ pairing.description ++ method.parameters.map((name, values) =>
      s"method.$name" -> values
    ) ++
      Vector("method" -> Vector(Text(method.id.name), Text(method.id.version.toString)))
  def inspect: Either[DescriptorError, RecipeInspection] =
    ScanpathStudyDescriptions.inspect(this)
  def diff(other: ScanpathStudyPlan[K, U]): Vector[PlanChange] =
    PlanChange.between(description, other.description)
  def work: ScanpathStudyCursor[K, U]                   = ScanpathStudyCursor.start(this)
  def run: Either[PlanError, ScanpathStudyResult[K, U]] = work.complete()
  def preflight(
      available: Option[ArtifactRef[StudyInput[K, U]]]
  ): AnalysisReport[K, StudyInput[K, U]] =
    val findings = preparation.left.toOption.toVector.map { error =>
      AnalysisFinding.Refused(
        Diagnostic.of(error).mapKeys[K](value => value),
        Preflight.remedyFor(error)
      )
    }
    AnalysisReport.of(
      RecipeFamily.ScanpathComparison,
      description,
      inputRef,
      available,
      findings,
      Vector(UncheckedAspect.PairComparison, UncheckedAspect.FailurePolicyReduction)
    )

  private[plan] def preparation: Either[PlanError, PreparedScanpathStudy[K, U]] =
    given KeyDigest[K] = layout.digest
    val rows           = input.trials.rows
    val focal          = Trials(rows.filter(row => layout.phase(row.key) == focalPhase))
    val references     = Trials(rows.filter(row => layout.phase(row.key) == referencePhase))
    val relations      = StudyPairingWork.relations(layout, pairing, references.rows.map(_.key))
    for
      candidates <- budget
        .checkCounts(focal.rows.size.toLong, references.rows.size.toLong)
        .left
        .map(PlanError.Schedule.apply)
      _ <- Either.cond(
        candidates <= budget.maxSelectedPairs,
        (),
        PlanError.Schedule(
          PairScheduleError.SelectedBudget(
            ScanpathPreparationConvention.ConservativeRawPairsV1.toString,
            candidates,
            budget.maxSelectedPairs
          )
        )
      )
      matched = pair(
        focal,
        references,
        PairDesign.BetweenDirected(relations.matched, Selection.All)
      )
      cardinality = StudyPairingWork.cardinalityFromPairs(
        layout,
        pairing,
        rows.map(_.key),
        references.rows.map(_.key),
        matched.pairs.map((left, right) => left.key -> right.key),
        matched.diagnostics
      )
      _ <- cardinality.refusal(layout).toLeft(())
      controlFocal = Trials(focal.rows.filter(row => relations.matchable(row.key)))
      controlPairs = pair(
        controlFocal,
        references,
        PairDesign.BetweenDirected(relations.controls, controls)
      )
      _ <- Either.cond(
        matched.pairs.size <= budget.maxSelectedPairs,
        (),
        PlanError.Schedule(
          PairScheduleError.SelectedBudget(
            "matched",
            matched.pairs.size,
            budget.maxSelectedPairs
          )
        )
      )
      _ <- Either.cond(
        controlPairs.pairs.size <= budget.maxSelectedPairs,
        (),
        PlanError.Schedule(
          PairScheduleError
            .SelectedBudget("controls", controlPairs.pairs.size, budget.maxSelectedPairs)
        )
      )
    yield
      def compare(left: Scanpath[U], right: Scanpath[U]) =
        for
          _ <- Agreement
            .frames(frame, left.frame)
            .left
            .map(error => ScanpathComparisonError.Alignment(CompareError.Frames(error)))
          _ <- Agreement
            .frames(frame, right.frame)
            .left
            .map(error => ScanpathComparisonError.Alignment(CompareError.Frames(error)))
          result <- method.compare(left, right)
        yield result
      new PreparedScanpathStudy[K, U](
        PairedEvaluation.start(matched, inputHash, evaluation)(compare),
        PairedEvaluation.start(controlPairs, inputHash, evaluation)(compare),
        focal.rows.map(_.key),
        rows
          .filter(row =>
            layout.phase(row.key) != focalPhase &&
              layout.phase(row.key) != referencePhase
          )
          .map(_.key)
      )

  private[plan] def result(
      prepared: PreparedScanpathStudy[K, U],
      matched: ScanpathPairs[K],
      control: ScanpathPairs[K]
  ): ScanpathStudyResult[K, U] =
    def mean(
        key: K,
        name: String,
        pairs: ScanpathPairs[K]
    ): ScanpathMean[K] =
      val values = pairs.rows
        .filter(_.left == key)
        .map(
          _.result
            .flatMap(
              _.scores.left.map(ScanpathComparisonError.Fixations.apply)
            )
            .flatMap(scores =>
              scores
                .find(_._1 == name)
                .map(_._2)
                .toRight(ScanpathComparisonError.Parameter(s"missing component $name", 0))
            )
        )
      val good                                     = values.flatMap(_.toOption)
      val bad                                      = values.size - good.size
      val value: Either[ReductionError[K], Double] =
        val sourceIndices = input.trials.rows.zipWithIndex.collect {
          case (row, index) if layout.phase(row.key) == focalPhase && row.key == key => index
        }
        if sourceIndices.size > 1 then Left(ReductionError.AmbiguousKey(key, sourceIndices))
        else if values.isEmpty then Left(ReductionError.NoSelectedScores(key))
        else
          failurePolicy match
            case FailurePolicy.RequireAll if bad > 0 =>
              Left(ReductionError.FailedScores(key, good.size, bad))
            case FailurePolicy.SuccessfulOnly(minimum) if good.size < minimum.value =>
              Left(ReductionError.InsufficientSuccessful(key, minimum.value, good.size, bad))
            case _ => ScoreMean[Double].mean(good).left.map(ReductionError.MeanFailure(key, _))
      new ScanpathMean(key, values.size, good.size, failurePolicy, value)
    val contrasts = prepared.focal.distinct.flatMap { key =>
      method.componentNames.map { name =>
        new ScanpathContrast(key, name, mean(key, name, matched), mean(key, name, control))
      }
    }
    new ScanpathStudyResult(inputHash, planHash, matched, control, contrasts, prepared.excluded)

object ScanpathStudyPlan:
  def of[K, U <: Unit2D: UnitLabel](
      layout: StudyLayout[K],
      input: StudyInput[K, U],
      frame: Frame[U],
      focalPhase: String,
      referencePhase: String,
      method: ScanpathMethod[U],
      pairing: StudyPairing = StudyPairing.default,
      controls: Selection = Selection.All,
      failurePolicy: FailurePolicy = FailurePolicy.RequireAll,
      budget: PairScheduleBudget = PairScheduleBudget.default
  ): Either[PlanError, ScanpathStudyPlan[K, U]] =
    for
      _ <- Either.cond(
        focalPhase.trim.nonEmpty && referencePhase.trim.nonEmpty &&
          focalPhase != referencePhase,
        (),
        PlanError.InvalidPhases(focalPhase, referencePhase)
      )
      _ <- Either.cond(
        (pairing.matched match
          case MatchedReferences.SameOccurrence | MatchedReferences.Select(_) =>
            layout.occurrence.nonEmpty
          case _ => true
        ),
        (),
        PlanError.OccurrenceUnavailable(layout.id, pairing.matched)
      )
      specification <- EvaluationSpec
        .of(
          method.id.name,
          method.id.version.toString,
          method.parameters.flatMap((name, values) =>
            values.zipWithIndex.map((value, index) => s"$name.$index" -> value)
          ),
          method.componentNames,
          EvaluationGeometry.inFrame(frame),
          method match
            case ScanpathMethod.MultiMatch(_, _) => EvaluationTime.RelativeMicroseconds
            case ScanpathMethod.Overlap(spec)    => EvaluationTime.SharedClock(spec.clock)
            case ScanpathMethod.Transport(_)     => EvaluationTime.Ordered
        )
        .left
        .map(PlanError.Specification.apply)
    yield
      val evaluation = EvaluationInfo(
        method.info.name,
        EvaluationScale.Measure(method.info.scale),
        Some(specification)
      )
      val temporary = new ScanpathStudyPlan(
        layout,
        input,
        frame,
        focalPhase,
        referencePhase,
        method,
        pairing,
        controls,
        failurePolicy,
        budget,
        evaluation,
        input.hash,
        summon[UnitLabel[U]].symbol
      )
      val hash = ContentHash.combineAll(
        Vector(
          input.hash,
          Provenance
            .Step(
              "scanpath-study",
              temporary.description.flatMap((name, values) =>
                values.zipWithIndex.map((value, index) => s"$name.$index" -> value)
              )
            )
            .digest
        )
      )
      new ScanpathStudyPlan(
        layout,
        input,
        frame,
        focalPhase,
        referencePhase,
        method,
        pairing,
        controls,
        failurePolicy,
        budget,
        evaluation,
        hash,
        summon[UnitLabel[U]].symbol
      )

/** Reserve the raw focal/reference product against every pair ceiling before
  * allocating either pairing. Relation filtering does not relax this admission.
  */
enum ScanpathPreparationConvention derives CanEqual:
  case ConservativeRawPairsV1

enum ScanpathStudyStage derives CanEqual:
  case PreparingWholeStudy, ComparingMatchedPairs, ComparingControlPairs, ReducingWholeStudy

private[plan] final class PreparedScanpathStudy[K, U <: Unit2D](
    val matched: PairedEvaluation[K, K, ScanpathComparisonError, ScanpathMethodOutput],
    val controls: PairedEvaluation[K, K, ScanpathComparisonError, ScanpathMethodOutput],
    val focal: Vector[K],
    val excluded: Vector[K]
)

final class ScanpathStudyCursor[K, U <: Unit2D] private[plan] (
    private val plan: ScanpathStudyPlan[K, U],
    val stage: ScanpathStudyStage,
    private val prepared: Option[PreparedScanpathStudy[K, U]],
    private val current: Option[
      PairedEvaluation[K, K, ScanpathComparisonError, ScanpathMethodOutput]
    ],
    private val matched: Option[ScanpathPairs[K]],
    private val controls: Option[ScanpathPairs[K]]
):
  def totalPairs: Long = current.fold(0L)(_.totalPairs.toLong)
  def advance(quanta: WorkQuanta): Either[
    PlanError,
    WorkStep[ScanpathStudyStage, ScanpathStudyCursor[K, U], ScanpathStudyResult[K, U]]
  ] =
    stage match
      case ScanpathStudyStage.PreparingWholeStudy =>
        plan.preparation.map { ready =>
          WorkStep.More(
            stage,
            1,
            new ScanpathStudyCursor(
              plan,
              ScanpathStudyStage.ComparingMatchedPairs,
              Some(ready),
              Some(ready.matched),
              None,
              None
            )
          )
        }
      case ScanpathStudyStage.ComparingMatchedPairs |
          ScanpathStudyStage.ComparingControlPairs =>
        current match
          case Some(cursor) =>
            Right(cursor.advance(quanta.pairs) match
              case PairedPage.More(units, next) =>
                WorkStep.More(
                  stage,
                  units,
                  new ScanpathStudyCursor(plan, stage, prepared, Some(next), matched, controls)
                )
              case PairedPage.Done(units, result) =>
                if stage == ScanpathStudyStage.ComparingMatchedPairs then
                  WorkStep.More(
                    stage,
                    units,
                    new ScanpathStudyCursor(
                      plan,
                      ScanpathStudyStage.ComparingControlPairs,
                      prepared,
                      prepared.map(_.controls),
                      Some(result),
                      None
                    )
                  )
                else
                  WorkStep.More(
                    stage,
                    units,
                    new ScanpathStudyCursor(
                      plan,
                      ScanpathStudyStage.ReducingWholeStudy,
                      prepared,
                      None,
                      matched,
                      Some(result)
                    )
                  ))
          case None => Left(PlanError.ChangedPreparedPlan(plan.method.id, plan.layout.id))
      case ScanpathStudyStage.ReducingWholeStudy =>
        (prepared, matched, controls) match
          case (Some(ready), Some(left), Some(right)) =>
            Right(WorkStep.Done(1, plan.result(ready, left, right)))
          case _ => Left(PlanError.ChangedPreparedPlan(plan.method.id, plan.layout.id))
  def complete(
      quanta: WorkQuanta = WorkQuanta.default
  ): Either[PlanError, ScanpathStudyResult[K, U]] =
    @annotation.tailrec
    def loop(cursor: ScanpathStudyCursor[K, U]): Either[PlanError, ScanpathStudyResult[K, U]] =
      cursor.advance(quanta) match
        case Left(error)                      => Left(error)
        case Right(WorkStep.More(_, _, next)) => loop(next)
        case Right(WorkStep.Done(_, result))  => Right(result)
    loop(this)

object ScanpathStudyCursor:
  private[plan] def start[K, U <: Unit2D](
      plan: ScanpathStudyPlan[K, U]
  ): ScanpathStudyCursor[K, U] =
    new ScanpathStudyCursor(
      plan,
      ScanpathStudyStage.PreparingWholeStudy,
      None,
      None,
      None,
      None
    )
  given stepwise[K, U <: Unit2D]: Stepwise[ScanpathStudyCursor[
    K,
    U
  ], ScanpathStudyStage, PlanError, ScanpathStudyResult[K, U]] with
    def stage(cursor: ScanpathStudyCursor[K, U]): ScanpathStudyStage   = cursor.stage
    def advance(cursor: ScanpathStudyCursor[K, U], quanta: WorkQuanta) =
      cursor.advance(quanta)
