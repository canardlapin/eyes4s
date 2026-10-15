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

/** The indivisible native fit, then pages of whole held-out row predictions. */
enum TemplateValidationStage derives CanEqual:
  case FittingWholeTraining, EvaluatingWholeRows

enum TemplateValidationStep[K, X]:
  case More(stage: TemplateValidationStage, workUnits: Int, next: TemplateValidationWork[K, X])
  case Done(workUnits: Int, validation: TemplateValidation[K, X])

/** Native cross-validation as immutable work. Fitting is one whole operation;
  * PairQuantum bounds held-out row keys only, not training cells, feature
  * extraction or solver iterations. Cancellation may occur between advances.
  */
final class TemplateValidationWork[K, X] private[design] (
    split: TemplateSplit[K, X],
    evaluating: Option[(FittedTemplate[K, X], TemplateEvaluationWork[K, X])]
):
  def stage: TemplateValidationStage = evaluating match
    case None    => TemplateValidationStage.FittingWholeTraining
    case Some(_) => TemplateValidationStage.EvaluatingWholeRows
  def totalRows: Int     = split.heldOut.rows.size
  def processedRows: Int = evaluating.fold(0)(_._2.position)

  def advance(quantum: PairQuantum): Either[TemplateError, TemplateValidationStep[K, X]] =
    evaluating match
      case None =>
        for
          fitted <- Template.fit(split.training)
          work   <- fitted.evaluationWork(split.heldOut)
        yield TemplateValidationStep.More(
          TemplateValidationStage.FittingWholeTraining,
          1,
          new TemplateValidationWork(split, Some(fitted -> work))
        )
      case Some((fitted, work)) =>
        Right(work.advance(quantum) match
          case TemplateEvaluationStep.More(units, next) =>
            TemplateValidationStep.More(
              TemplateValidationStage.EvaluatingWholeRows,
              units,
              new TemplateValidationWork(split, Some(fitted -> next))
            )
          case TemplateEvaluationStep.Done(units, evaluation) =>
            TemplateValidationStep.Done(units, new TemplateValidation(fitted, evaluation)))

  def complete(
      quantum: PairQuantum = PairQuantum.default
  ): Either[TemplateError, TemplateValidation[K, X]] =
    @annotation.tailrec
    def loop(
        work: TemplateValidationWork[K, X]
    ): Either[TemplateError, TemplateValidation[K, X]] =
      work.advance(quantum) match
        case Left(error)                                    => Left(error)
        case Right(TemplateValidationStep.More(_, _, next)) => loop(next)
        case Right(TemplateValidationStep.Done(_, result))  => Right(result)
    loop(this)
