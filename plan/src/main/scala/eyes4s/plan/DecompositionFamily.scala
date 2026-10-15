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

object DecompositionViews:
  private[plan] given Labelled[Intercept] with
    def token(value: Intercept): String = value.toString
    def label(value: Intercept): String = value.toString
  private val intercept = RecipeViews.choice(
    "intercept",
    "Include an OLS intercept or a uniform mixture background",
    Intercept.values.toVector
  )
  private val rank = RecipeViews.number(
    "rankTolerance",
    "Relative pivot threshold after unit-norm column scaling",
    Quantity.Dimensionless,
    NumberShape.Real,
    NumericBounds.openUnit
  )
  private val dual = RecipeViews.number(
    "dualTolerance",
    "Relative active-set dual/KKT tolerance",
    Quantity.Dimensionless,
    NumberShape.Real,
    NumericBounds.openUnit
  )
  val spec: FieldView = RecipeViews.view(
    "spec",
    "Surface solver, intercept/background and explicit numerical tolerances",
    FieldKind.Variant(
      Vector(
        VariantCase("ols", "Unconstrained ordinary least squares", Vector(intercept, rank)),
        VariantCase(
          "nnls",
          "Intercept-free nonnegative least squares; coefficients need not sum to one",
          Vector(rank, dual)
        ),
        VariantCase(
          "mixture",
          "Simplex-constrained predictor mixture, optionally including uniform background",
          Vector(intercept, rank, dual)
        )
      )
    )
  )

final case class DecompositionRecipe(spec: DecompositionSpec):
  def plan[U <: Unit2D: UnitLabel](
      template: DecompositionPlan[U]
  ): Either[DecompositionError, DecompositionPlan[U]] =
    DecompositionPlan.of(template.response, template.predictors, spec)

final class DecompositionForm:
  import RawParts.*
  import DecompositionViews.given
  private def rank(raw: RawValue) = real("spec", raw, "rankTolerance").flatMap(value =>
    RelativeRankTolerance.of(value).left.map(RecipeParameterError.LeastSquares.apply)
  )
  private def dual(raw: RawValue) = real("spec", raw, "dualTolerance").flatMap(value =>
    RelativeDualTolerance.of(value).left.map(RecipeParameterError.LeastSquares.apply)
  )
  private def intercept(raw: RawValue) =
    part("spec", raw, "intercept").flatMap(choose("spec", _, Intercept.values.toVector))
  val spec: StructuredField[RecipeParameterError, DecompositionSpec] =
    StructuredField.of[RecipeParameterError, DecompositionSpec](DecompositionViews.spec)(
      raw =>
        token("spec", raw).flatMap {
          case "ols" =>
            for i <- intercept(raw); r <- rank(raw) yield DecompositionSpec.Ols(i, r)
          case "nnls" =>
            for r <- rank(raw); d <- dual(raw) yield DecompositionSpec.NonNegative(r, d)
          case "mixture" =>
            for i <- intercept(raw); r <- rank(raw); d <- dual(raw)
            yield DecompositionSpec.Mixture(i, r, d)
          case other => Left(unknown("spec", other, Vector("ols", "nnls", "mixture")))
        },
      {
        case DecompositionSpec.Ols(i, r) =>
          variant("ols", "intercept" -> choice(i), "rankTolerance" -> number(r.value))
        case DecompositionSpec.NonNegative(r, d) =>
          variant(
            "nnls",
            "rankTolerance" -> number(r.value),
            "dualTolerance" -> number(d.value)
          )
        case DecompositionSpec.Mixture(i, r, d) =>
          variant(
            "mixture",
            "intercept"     -> choice(i),
            "rankTolerance" -> number(r.value),
            "dualTolerance" -> number(d.value)
          )
      },
      _.message
    )
  val fields: Vector[FormField[RecipeParameterError, ?]] = Vector(spec)
  def views: Vector[FieldView]                           = fields.map(_.view)
  def parse(values: FormValues): Either[FieldError[RecipeParameterError], DecompositionRecipe] =
    spec.parse(values.get(spec.view.id)).map(DecompositionRecipe.apply)
  def validate(field: FieldId, raw: RawValue): Either[FieldError[RecipeParameterError], Unit] =
    if field == spec.view.id then spec.parse(raw).map(_ => ())
    else Left(FieldError.UnknownField(field))
  def values[U <: Unit2D](plan: DecompositionPlan[U]): FormValues =
    FormValues.of(spec.view.id -> spec.raw(plan.spec))

object DecompositionDescriptions:
  def description[U <: Unit2D](
      plan: DecompositionPlan[U]
  ): Vector[(String, Vector[Provenance.Param])] =
    import Provenance.Param.*
    val grid = plan.response.grid
    Vector(
      "decomposition.input" -> Vector(Text(plan.inputHash.render)),
      "normConvention"      -> Vector(Text(LeastSquares.normConvention.toString)),
      "grid"                -> Vector(
        Text(plan.unitSymbol),
        Text(grid.id.name),
        Text(grid.frame.id.name),
        Num(grid.nx.toDouble),
        Num(grid.ny.toDouble)
      ),
      "predictors" -> plan.predictors.entries.map((id, _) => Text(id.value)),
      "spec"       -> plan.spec.description
    )
  def inspect[U <: Unit2D](
      plan: DecompositionPlan[U]
  ): Either[DescriptorError, RecipeInspection] =
    val views = Vector(
      RecipeViews.reference(
        "normConvention",
        "Shared fdlibm Euclidean norms in QR and active-set admission"
      ),
      RecipeViews.reference(
        "decomposition.input",
        "Complete response/predictor cells, their provenance and geometry"
      ),
      RecipeViews
        .reference("grid", "Static spatial unit, nominal frame/grid and cell dimensions"),
      RecipeViews.reference("predictors", "Ordered unique predictor identities"),
      DecompositionViews.spec
    )
    val metadata = views.map(view => view.id.value -> ParameterInfo.literal(view)).toMap
    import cats.syntax.all.*
    description(plan)
      .traverse { (id, values) =>
        metadata
          .get(id)
          .toRight(DescriptorError.UnexplainedFields(Vector(id)))
          .map(new InspectedParameter(_, values))
      }
      .map(
        new RecipeInspection(
          _,
          Vector(
            "OLS coefficients may be signed; NNLS coefficients are nonnegative scale factors, not mixture weights",
            "Mixture weights and optional uniform background sum to one",
            "Residuals are observed minus fitted; descriptive cellwise diagnostics make no independence or inference claim",
            "The entire surface solve is one operation with no intra-solver cancellation guarantee"
          ),
          ExecutionCapability.SynchronousWholeOperation
        )
      )
