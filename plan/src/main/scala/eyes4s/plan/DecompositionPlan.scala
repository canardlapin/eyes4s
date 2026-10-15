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

import eyes4s.design.*
import eyes4s.kernel.*

/** Method-specific settings prevent an intercept on intercept-free NNLS. */
enum DecompositionSpec:
  case Ols(intercept: Intercept, rankTolerance: RelativeRankTolerance)
  case NonNegative(rankTolerance: RelativeRankTolerance, dualTolerance: RelativeDualTolerance)
  case Mixture(
      intercept: Intercept,
      rankTolerance: RelativeRankTolerance,
      dualTolerance: RelativeDualTolerance
  )

  def method: String = this match
    case Ols(_, _)         => "ols"
    case NonNegative(_, _) => "nnls"
    case Mixture(_, _, _)  => "mixture"
  def description: Vector[Provenance.Param] =
    import Provenance.Param.*
    this match
      case Ols(intercept, rank) =>
        Vector(Text("ols"), Text(intercept.toString), Num(rank.value))
      case NonNegative(rank, dual) => Vector(Text("nnls"), Num(rank.value), Num(dual.value))
      case Mixture(intercept, rank, dual) =>
        Vector(Text("mixture"), Text(intercept.toString), Num(rank.value), Num(dual.value))
  private[plan] def identity: ContentHash =
    val (text, numbers) = this match
      case Ols(intercept, rank)    => (Vector("ols", intercept.toString), IArray(rank.value))
      case NonNegative(rank, dual) => (Vector("nnls"), IArray(rank.value, dual.value))
      case Mixture(intercept, rank, dual) =>
        (Vector("mixture", intercept.toString), IArray(rank.value, dual.value))
    ContentHash.combineAll(text.map(ContentHash.ofString) :+ ContentHash.of(numbers))

enum DecompositionFit[U <: Unit2D]:
  case Ols(value: SurfaceOlsFit[U])
  case NonNegative(value: SurfaceNnlsFit[U])
  case Mixture(value: SurfaceMixtureFit[U])

final class DecompositionResult[U <: Unit2D] private[plan] (
    val inputHash: ContentHash,
    val planHash: ContentHash,
    val fit: DecompositionFit[U]
)

/** A complete checked response/predictor recipe. Solving is explicitly one
  * whole surface operation, including the existing solver's own iterations.
  */
final class DecompositionPlan[U <: Unit2D] private (
    val response: Mass[U],
    val predictors: PredictorSet[U],
    val spec: DecompositionSpec,
    val unitSymbol: String,
    val inputHash: ContentHash,
    val planHash: ContentHash
):
  def inputRef: ArtifactRef[(Mass[U], PredictorSet[U])]       = ArtifactRef.of(inputHash)
  def description: Vector[(String, Vector[Provenance.Param])] =
    DecompositionDescriptions.description(this)
  def inspect: Either[DescriptorError, RecipeInspection] =
    DecompositionDescriptions.inspect(this)
  def diff(other: DecompositionPlan[U]): Vector[PlanChange] =
    PlanChange.between(description, other.description)
  def preflight(
      available: Option[ArtifactRef[(Mass[U], PredictorSet[U])]]
  ): AnalysisReport[Nothing, (Mass[U], PredictorSet[U])] =
    AnalysisReport.of(
      RecipeFamily.Decomposition,
      description,
      inputRef,
      available,
      Vector.empty,
      Vector(UncheckedAspect.SurfaceSolving)
    )
  def work: DecompositionCursor[U]                            = new DecompositionCursor(this)
  def run: Either[DecompositionError, DecompositionResult[U]] = work.complete()
  private[plan] def solve: Either[DecompositionError, DecompositionResult[U]] =
    val fitted: Either[DecompositionError, DecompositionFit[U]] = spec match
      case DecompositionSpec.Ols(intercept, rank) =>
        Template
          .decompose(response, predictors, intercept, rank)
          .map(DecompositionFit.Ols.apply)
      case DecompositionSpec.NonNegative(rank, dual) =>
        Template
          .decomposeNonNegative(response, predictors, rank, dual)
          .map(DecompositionFit.NonNegative.apply)
      case DecompositionSpec.Mixture(intercept, rank, dual) =>
        Template
          .decomposeMixture(response, predictors, intercept, rank, dual)
          .map(DecompositionFit.Mixture.apply)
    fitted.map(new DecompositionResult(inputHash, planHash, _))

object DecompositionPlan:
  def of[U <: Unit2D: UnitLabel](
      response: Mass[U],
      predictors: PredictorSet[U],
      spec: DecompositionSpec
  ): Either[DecompositionError, DecompositionPlan[U]] =
    Agreement
      .grids(predictors.grid, response.grid)
      .left
      .map(DecompositionError.Geometry("response", _))
      .map { _ =>
        val grid     = response.grid
        val geometry = Vector(
          summon[UnitLabel[U]].symbol,
          grid.id.name,
          grid.frame.id.name,
          grid.frame.spec.yAxis.toString
        ).map(ContentHash.ofString) :+
          ContentHash.of(
            IArray(
              grid.frame.spec.xMin,
              grid.frame.spec.xMax,
              grid.frame.spec.yMin,
              grid.frame.spec.yMax,
              grid.nx.toDouble,
              grid.ny.toDouble
            )
          )
        val maps = Vector(
          ContentHash.of(response.values),
          response.provenance.digest
        ) ++ predictors.entries.flatMap { (id, mass) =>
          Vector(
            ContentHash.ofString(id.value),
            ContentHash.of(mass.values),
            mass.provenance.digest
          )
        }
        val input = ContentHash.combineAll(geometry ++ maps)
        val plan  = ContentHash.combineAll(
          Vector(
            input,
            spec.identity,
            ContentHash.ofString(LeastSquares.normConvention.toString)
          )
        )
        new DecompositionPlan(
          response,
          predictors,
          spec,
          summon[UnitLabel[U]].symbol,
          input,
          plan
        )
      }

enum DecompositionStage derives CanEqual:
  case SolvingWholeSurface

/** Cancellation can precede solving or follow completion; no intra-solver
  * cancellation, cell bound or iteration bound is promised by WorkQuanta.
  */
final class DecompositionCursor[U <: Unit2D] private[plan] (plan: DecompositionPlan[U]):
  def stage: DecompositionStage = DecompositionStage.SolvingWholeSurface
  def advance(
      @scala.annotation.unused quanta: WorkQuanta
  ): Either[DecompositionError, WorkStep[DecompositionStage, DecompositionCursor[
    U
  ], DecompositionResult[U]]] =
    plan.solve.map(WorkStep.Done(1, _))
  def complete(): Either[DecompositionError, DecompositionResult[U]] = plan.solve
object DecompositionCursor:
  given stepwise[U <: Unit2D]: Stepwise[DecompositionCursor[
    U
  ], DecompositionStage, DecompositionError, DecompositionResult[U]] with
    def stage(cursor: DecompositionCursor[U]): DecompositionStage = cursor.stage
    def advance(cursor: DecompositionCursor[U], quanta: WorkQuanta): Either[
      DecompositionError,
      WorkStep[DecompositionStage, DecompositionCursor[U], DecompositionResult[U]]
    ] = cursor.advance(quanta)
