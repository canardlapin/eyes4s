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

package eyes4s.examples

import cats.syntax.all.*
import eyes4s.codec.*
import eyes4s.design.*
import eyes4s.io.*
import eyes4s.plan.*

/** Exact public-API example used by docs/TEMPLATE_FITTING.md and the JVM CLI.
  * Fixed features, no learned preprocessing. Every operation returns a typed error.
  * One recipe codec saves both routes: the native fit and the historical R `lm` import.
  */
object TemplateFitGuide:
  type Error = TemplateError | CodecError | PlanError | TemplateCsvError
  final case class Prepared(savedRecipe: String, trainingCsv: String)
  def codec: Either[PlanError, VersionedCodec[TemplateSplit[String, Vector[Double]]]] = for
    recipe <- DefinitionId.of("eyes4s.template-recipe", 1)
    key    <- DefinitionId.of("example.trial-key", 1)
  yield TemplateRecipeCodec.of[String, Vector[Double]](recipe, VersionedCodec.string(key))

  def saveNative(split: TemplateSplit[String, Vector[Double]]): Either[Error, String] = for
    persistence <- codec
    json        <- persistence.encode(split)
  yield json.spaces2

  def evaluateNative(savedRecipe: String): Either[Error, TemplateEvaluation[String]] = for
    persistence <- codec
    split       <- persistence.parse(savedRecipe)
    validated   <- Template.crossValidate(split)
  yield validated.evaluation

  /** The worked split: natively fitted by default, or fitted by R's `lm`
    * from a training-only export when `route` is `ImportedLm`.
    */
  def input(
      heldOutResponse: Double = 7.0,
      route: TemplateDesign.FixedRoute = TemplateDesign.FixedRoute.Native
  ): Either[TemplateError, TemplateSplit[String, Vector[Double]]] =
    for
      basis <- TemplateBasis.of(
        "fixed-template-features/1",
        Vector("template-a", "template-b"),
        "score"
      )
      design = route match
        case TemplateDesign.FixedRoute.Native     => TemplateDesign.fixed(basis)
        case TemplateDesign.FixedRoute.ImportedLm => TemplateDesign.importedLm(basis)
      rows <- Vector(
        ("a", "train-a", Vector(1.0, 0.0), 1.0),
        ("b", "train-a", Vector(0.0, 1.0), 2.0),
        ("c", "train-b", Vector(1.0, 1.0), 3.0),
        ("d", "train-b", Vector(2.0, 1.0), 4.0),
        ("e", "test", Vector(3.0, 2.0), heldOutResponse)
      ).traverse { (k, f, x, y) => TemplateObservation.of(k, f, x, y) }
      split <- TemplateSplit.of(design, rows, Set("test"))
    yield split

  /** The worked split for the R `lm` route. */
  def historicalInput(
      heldOutResponse: Double = 7.0
  ): Either[TemplateError, TemplateSplit[String, Vector[Double]]] =
    input(heldOutResponse, TemplateDesign.FixedRoute.ImportedLm)

  def prepare(split: TemplateSplit[String, Vector[Double]]): Either[Error, Prepared] = for
    persistence <- codec
    saved       <- persistence.encode(split)
  yield Prepared(saved.spaces2, TemplateFitCsv.training(split.training))

  def evaluate(
      savedRecipe: String,
      coefficientsCsv: String
  ): Either[Error, TemplateEvaluation[String]] = for
    persistence <- codec
    split       <- persistence.parse(savedRecipe)
    model       <- TemplateFitCsv.importFit(split.training, coefficientsCsv)
    result      <- model.evaluate(split.heldOut)
  yield result

  def message(error: Error): String = error match
    case e: TemplateError    => e.message
    case e: TemplateCsvError => e.message
    case e: CodecError       => e.message
    case e: PlanError        => e.message
