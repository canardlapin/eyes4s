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

/** The source of the fit is explicit and serializable. */
enum TemplateFitRoute[X]:
  case Native[A]()                                 extends TemplateFitRoute[A]
  case ImportedFixed(receipt: TemplateImportedFit) extends TemplateFitRoute[Vector[Double]]

/** A training-only template fit and held-out evaluation, with an explicit
  * native or checked imported-fixed route and a complete source split.
  */
final class TemplateFitPlan[K, X] private (
    val split: TemplateSplit[K, X],
    val inputHash: ContentHash,
    val planHash: ContentHash,
    val route: TemplateFitRoute[X]
):
  def inputRef: ArtifactRef[Vector[TemplateObservation[K, X]]] = ArtifactRef.of(inputHash)
  def description: Vector[(String, Vector[Provenance.Param])]  =
    TemplateFitDescriptions.description(this)
  def inspect: Either[DescriptorError, RecipeInspection] = TemplateFitDescriptions.inspect(this)
  def diff(other: TemplateFitPlan[K, X]): Vector[PlanChange] =
    PlanChange.between(description, other.description)
  def preflight(
      available: Option[ArtifactRef[Vector[TemplateObservation[K, X]]]]
  ): AnalysisReport[K, Vector[TemplateObservation[K, X]]] =
    AnalysisReport.of(
      RecipeFamily.TemplateFit,
      description,
      inputRef,
      available,
      Vector.empty,
      Vector(UncheckedAspect.TemplateFitting, UncheckedAspect.TemplateEvaluation)
    )
  def work: TemplateFitCursor[K, X] =
    val work: TemplateValidationWork[K, X] = route match
      case TemplateFitRoute.Native()               => Template.crossValidationWork(split)
      case TemplateFitRoute.ImportedFixed(receipt) =>
        Template.importedCrossValidationWork(split, receipt)
    new TemplateFitCursor(this, work)
  private[plan] def rebuild(next: TemplateSplit[K, X])(using
      KeyDigest[K]
  ): Either[TemplateError, TemplateFitPlan[K, X]] = route match
    case TemplateFitRoute.Native()               => TemplateFitPlan.of(next)
    case TemplateFitRoute.ImportedFixed(receipt) => TemplateFitPlan.imported(next, receipt)
  def run: Either[TemplateError, TemplateFitResult[K, X]] = work.complete()

object TemplateFitPlan:
  private def create[K: KeyDigest, X](
      split: TemplateSplit[K, X],
      route: TemplateFitRoute[X]
  ): TemplateFitPlan[K, X] =
    val input = split.inputHash
    val fit   = route match
      case TemplateFitRoute.Native() =>
        ContentHash.combine(
          ContentHash.ofString("native"),
          ContentHash.ofString(LeastSquares.normConvention.toString)
        )
      case TemplateFitRoute.ImportedFixed(receipt) =>
        ContentHash.combine(ContentHash.ofString("imported-fixed"), receipt.identity)
    val plan = ContentHash.combineAll(
      Vector(
        input,
        ContentHash.ofString(split.design.method),
        fit
      ) ++ split.heldOutGroups.toVector.sorted.map(ContentHash.ofString)
    )
    new TemplateFitPlan(split, input, plan, route)
  def of[K: KeyDigest, X](
      split: TemplateSplit[K, X]
  ): Either[TemplateError, TemplateFitPlan[K, X]] =
    val native = split.design match
      case fixed: TemplateDesign.Fixed  => fixed.route == TemplateDesign.FixedRoute.Native
      case _: TemplateDesign.MeanMap[?] => true
    Either.cond(
      native,
      create(split, TemplateFitRoute.Native[X]()),
      TemplateError.Route(split.design.method, "native fit plan")
    )
  def imported[K: KeyDigest](
      split: TemplateSplit[K, Vector[Double]],
      receipt: TemplateImportedFit
  ): Either[TemplateError, TemplateFitPlan[K, Vector[Double]]] =
    receipt
      .bind(split.training)
      .map(_ => create(split, TemplateFitRoute.ImportedFixed(receipt)))

/** A completed validation bound to the complete source and fitting recipe. */
final class TemplateFitResult[K, X] private[plan] (
    val inputHash: ContentHash,
    val planHash: ContentHash,
    val validation: TemplateValidation[K, X]
)

/** The cursor delegates checked fitting and row pages to design. */
final class TemplateFitCursor[K, X] private[plan] (
    plan: TemplateFitPlan[K, X],
    work: TemplateValidationWork[K, X]
):
  def stage: TemplateValidationStage = work.stage
  def totalRows: Int                 = work.totalRows
  def processedRows: Int             = work.processedRows
  def advance(quanta: WorkQuanta): Either[
    TemplateError,
    WorkStep[TemplateValidationStage, TemplateFitCursor[K, X], TemplateFitResult[K, X]]
  ] =
    work.advance(quanta.pairs).map {
      case TemplateValidationStep.More(stage, units, next) =>
        WorkStep.More(stage, units, new TemplateFitCursor(plan, next))
      case TemplateValidationStep.Done(units, result) =>
        WorkStep.Done(units, new TemplateFitResult(plan.inputHash, plan.planHash, result))
    }
  def complete(
      quanta: WorkQuanta = WorkQuanta.default
  ): Either[TemplateError, TemplateFitResult[K, X]] =
    Stepwise.complete(this, quanta)

object TemplateFitCursor:
  given stepwise[K, X]: Stepwise[TemplateFitCursor[
    K,
    X
  ], TemplateValidationStage, TemplateError, TemplateFitResult[K, X]] with
    def stage(cursor: TemplateFitCursor[K, X]): TemplateValidationStage = cursor.stage
    def advance(cursor: TemplateFitCursor[K, X], quanta: WorkQuanta): Either[
      TemplateError,
      WorkStep[TemplateValidationStage, TemplateFitCursor[K, X], TemplateFitResult[K, X]]
    ] = cursor.advance(quanta)
