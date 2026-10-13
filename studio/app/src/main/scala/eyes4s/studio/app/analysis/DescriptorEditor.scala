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

package eyes4s.studio.app.analysis

import eyes4s.codec.FormCodecs
import eyes4s.plan.{FormValues, FormView}
import eyes4s.studio.app.AppModel
import eyes4s.studio.core.backend.{RecipeEditRequest, RecipeFormResult}
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.{RecipeChange, Perspective}

/** A request generation belongs to an exact recipe target and raw input. */
final case class RecipeRead(generation: Long, request: RecipeEditRequest) derives CanEqual

/** Incomplete input is local; accepted edits make one undoable document command.
  * A late answer cannot overwrite another edit, revision, family or dataset.
  */
final case class DescriptorEditor(
    target: Option[RecipeEditRequest] = None,
    generation: Long = 0L,
    pending: Boolean = false,
    form: Option[FormView] = None,
    values: Option[FormValues] = None,
    result: Option[RecipeFormResult] = None,
    error: Option[String] = None
) derives CanEqual:
  def sync(model: AppModel): (DescriptorEditor, Option[RecipeRead]) =
    val next = Option
      .when(model.perspective == Perspective.Analysis)(AnalysisSelection.selected(model))
      .flatten
      .map(c => RecipeEditRequest(c.id, c.dataset, c.recipe, compareTo = Some(c.seedRecipe)))
    if next == target then (this, None)
    else
      val serial = generation + 1L
      val state  = DescriptorEditor(target = next, generation = serial, pending = next.nonEmpty)
      (state, next.map(RecipeRead(serial, _)))

  def edit(raw: FormValues): (DescriptorEditor, Option[RecipeRead]) = target match
    case None          => (this, None)
    case Some(request) =>
      FormCodecs.values.encode(raw) match
        case Left(problem) =>
          (copy(values = Some(raw), result = None, error = Some(problem.message)), None)
        case Right(json) =>
          val serial = generation + 1L
          (
            copy(
              generation = serial,
              values = Some(raw),
              pending = true,
              result = None,
              error = None
            ),
            Some(RecipeRead(serial, request.copy(values = Some(json))))
          )

  def read(read: RecipeRead, answer: Either[String, RecipeFormResult]): DescriptorEditor =
    if read.generation != generation || !target.contains(read.request.copy(values = None)) then
      this
    else
      answer.flatMap(r =>
        r.decoded.left.map(_.message).map((view, raw) => (r, view, raw))
      ) match
        case Left(problem) => copy(pending = false, result = None, error = Some(problem))
        case Right((served, view, raw)) =>
          copy(
            pending = false,
            form = Some(view),
            values = Some(raw),
            result = Some(served),
            error = None
          )

  def command(model: AppModel): Option[Command] =
    for
      request <- target
      context <- AnalysisSelection.selected(model)
      if !pending && request == RecipeEditRequest(
        context.id,
        context.dataset,
        context.recipe,
        compareTo = Some(context.seedRecipe)
      )
      if model.document.draft.forall(_.id == context.id)
      accepted <- result.flatMap(_.accepted)
      changes = RecipeChange.between(context.recipe, accepted)
      if changes.nonEmpty
    yield
      if model.document.draft.isEmpty then Command.StartDraft(context.id, None, changes)
      else Command.ChangeRecipes(changes)
