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
import eyes4s.plan.{FormValues, FormView, DefinitionId}
import eyes4s.studio.app.{AppModel, Intent, StoryModels}
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.*

class DescriptorEditorSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val model                             = StoryModels.t2Analysis
  private val initial                           = DescriptorEditor().sync(model)
  private val request                           = initial._2.get
  private val raw                               = FormValues.empty
  private def answer(recipe: Recipe)            = RecipeFormResult(
    get(FormCodecs.view.encode(get(FormView.of(DefinitionId.study, Vector.empty)))),
    get(FormCodecs.values.encode(raw)),
    Some(recipe),
    Vector.empty,
    None,
    Vector.empty,
    Vector.empty,
    Vector.empty
  )
  test("an older validation answer cannot overwrite more recent raw input") {
    val loaded  = initial._1.read(request, Right(answer(request.request.recipe)))
    val (a, ar) = loaded.edit(raw)
    val (b, br) = a.edit(raw)
    assertEquals(b.read(ar.get, Right(answer(request.request.recipe))), b)
    assert(!b.read(br.get, Right(answer(request.request.recipe))).pending)
    assertEquals(b.command(model), None)
  }
  test("switching away invalidates a pending answer") {
    val other = AppModel.update(model, Intent.SwitchPerspective(Perspective.Explore))._1
    val reset = initial._1.sync(other)._1
    assertEquals(reset.target, None)
    assertEquals(reset.read(request, Right(answer(request.request.recipe))), reset)
  }
  test(
    "accepted settings make one undoable draft edit, without changing saved revisions or runs"
  ) {
    val recipe  = request.request.recipe.copy(grid = get(GridSize.of(32, 24)))
    val loaded  = initial._1.read(request, Right(answer(recipe)))
    val command = loaded.command(model).get
    val next    = AppModel.update(model, Intent.Dispatch(command))._1
    assertEquals(next.document.draftRecipe, Some(recipe))
    assertEquals(next.document.analyses, model.document.analyses)
    assertEquals(next.document.runs, model.document.runs)
    assertEquals(loaded.command(next), None)
  }
