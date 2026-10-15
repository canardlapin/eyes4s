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

package eyes4s.studio.core.backend

import eyes4s.codec.{CodecError, FormCodecs}
import eyes4s.plan.{FormValues, FormView}
import eyes4s.studio.core.document.Recipe
import io.circe.{Codec, Json}

/** The complete editing target, independent of a concurrently registered draft.
  * `values` uses eyes4s.form-values; the backend validates its envelope and fields.
  */
final case class RecipeEditRequest(
    revision: AnalysisRevision,
    dataset: DatasetRevision,
    recipe: Recipe,
    values: Option[Json] = None,
    compareTo: Option[Recipe] = None
) derives CanEqual,
      Codec.AsObject

/** Scientific sentence tokens retain the library's field and semantic role. */
final case class RecipeToken(text: String, field: Option[String], role: Option[String])
    derives CanEqual,
      Codec.AsObject
final case class RecipeProblem(field: String, message: String) derives CanEqual, Codec.AsObject
final case class RecipeScaleWarning(scale: Int, kind: String, actual: Double, limit: Double)
    derives CanEqual,
      Codec.AsObject
final case class RecipeCell(x: Double, y: Double) derives CanEqual, Codec.AsObject

/** Descriptors and raw input use the library's own versioned wire contracts.
  * Invalid input is retained with its field problems; only `accepted` can be applied.
  */
final case class RecipeFormResult(
    form: Json,
    values: Json,
    accepted: Option[Recipe],
    sentence: Vector[RecipeToken],
    cellDegrees: Option[RecipeCell],
    warnings: Vector[RecipeScaleWarning],
    problems: Vector[RecipeProblem],
    conventions: Vector[String],
    differences: Vector[String] = Vector.empty,
    differenceProblem: Option[String] = None
) derives CanEqual,
      Codec.AsObject:
  def decoded: Either[CodecError, (FormView, FormValues)] =
    for
      view <- FormCodecs.view.decode(form)
      raw  <- FormCodecs.values.decode(values)
    yield (view, raw)
