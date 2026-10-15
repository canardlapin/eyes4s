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

/** A page of held-out predictions. Failed predictions remain rows. */
enum TemplateEvaluationStep[K, X]:
  case More(workUnits: Int, next: TemplateEvaluationWork[K, X])
  case Done(workUnits: Int, evaluation: TemplateEvaluation[K])

/** Pure evaluation of one already-fitted template, preserving the held-out
  * input order and both identities. A work unit visits one held-out key;
  * feature extraction and prediction for that key are whole operations.
  * Cancellation may land between pages, never inside a feature extraction.
  */
final class TemplateEvaluationWork[K, X] private[design] (
    fitted: FittedTemplate[K, X],
    heldOut: TemplateHeldOut[K, X],
    val position: Int,
    completed: Vector[TemplatePrediction[K]]
):
  def total: Int = heldOut.rows.size

  def advance(quantum: PairQuantum): TemplateEvaluationStep[K, X] =
    val count = math.min(quantum.value, total - position)
    val until = position + count
    val rows  = heldOut.rows.slice(position, until).map(fitted.prediction)
    val next  = completed ++ rows
    if until == total then
      TemplateEvaluationStep.Done(
        count,
        new TemplateEvaluation(fitted.training.hash, heldOut.hash, next)
      )
    else
      TemplateEvaluationStep.More(
        count,
        new TemplateEvaluationWork(fitted, heldOut, until, next)
      )

  def complete(quantum: PairQuantum = PairQuantum.default): TemplateEvaluation[K] =
    @annotation.tailrec
    def loop(work: TemplateEvaluationWork[K, X]): TemplateEvaluation[K] =
      work.advance(quantum) match
        case TemplateEvaluationStep.More(_, next)   => loop(next)
        case TemplateEvaluationStep.Done(_, result) => result
    loop(this)
