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

import cats.syntax.all.*
import eyes4s.design.*
import eyes4s.kernel.*

/** Editable held-out split selection. Every source row and predeclared
  * design is supplied by context; selecting folds reruns checked exclusion.
  */
final case class TemplateFitRecipe(heldOutGroups: Vector[String]):
  def plan[K: KeyDigest, X](
      template: TemplateFitPlan[K, X]
  ): Either[TemplateError, TemplateFitPlan[K, X]] =
    for
      _ <- Either.cond(
        heldOutGroups.nonEmpty && heldOutGroups.distinct.size == heldOutGroups.size,
        (),
        TemplateError.SplitGroups(heldOutGroups)
      )
      split <- TemplateSplit.of(template.split.design, template.split.rows, heldOutGroups.toSet)
      plan  <- template.rebuild(split)
    yield plan

final class TemplateFitForm[K, X](template: TemplateFitPlan[K, X]):
  import RawParts.*
  private val options = template.split.rows.map(_.splitGroup).distinct.sorted
  val heldOutGroups: StructuredField[RecipeParameterError, Vector[String]] =
    val view = RecipeViews.view(
      "heldOutGroups",
      "Unique split groups held out; their matching groups are excluded from training",
      FieldKind.Repeated(
        RecipeViews.view(
          "group",
          "A split group from the supplied observations",
          FieldKind.Choice(ChoiceSource.Fixed(options.map(group => ChoiceOption(group, group))))
        ),
        1,
        Some(options.size)
      )
    )
    StructuredField.of[RecipeParameterError, Vector[String]](view)(
      {
        case RawValue.Items(values) => values.traverse(token("heldOutGroups", _))
        case _                      => Left(missing("heldOutGroups", "items"))
      },
      values => RawValue.Items(values.map(RawValue.Choice.apply)),
      _.message
    )
  val fields: Vector[FormField[RecipeParameterError, ?]] = Vector(heldOutGroups)
  def views: Vector[FieldView]                           = fields.map(_.view)
  def parse(values: FormValues): Either[FieldError[RecipeParameterError], TemplateFitRecipe] =
    heldOutGroups.parse(values.get(heldOutGroups.view.id)).map(TemplateFitRecipe.apply)
  def validate(field: FieldId, raw: RawValue): Either[FieldError[RecipeParameterError], Unit] =
    if field == heldOutGroups.view.id then heldOutGroups.parse(raw).map(_ => ())
    else Left(FieldError.UnknownField(field))
  def values: FormValues = FormValues.of(
    heldOutGroups.view.id -> heldOutGroups.raw(template.split.heldOutGroups.toVector.sorted)
  )

object TemplateFitDescriptions:
  def description[K, X](
      plan: TemplateFitPlan[K, X]
  ): Vector[(String, Vector[Provenance.Param])] =
    import Provenance.Param.*
    val split  = plan.split
    val design = split.design match
      case fixed: TemplateDesign.Fixed =>
        Vector[Provenance.Param](
          Text("fixed"),
          Text(fixed.basis.id),
          Text(fixed.basis.responseUnit)
        )
      case mean: TemplateDesign.MeanMap[?] =>
        Vector[Provenance.Param](
          Text("mean-map"),
          Text(mean.splitUnit),
          Text(mean.responseUnit)
        )
    val (route, receipt) = plan.route match
      case TemplateFitRoute.Native() =>
        (Vector[Provenance.Param](Text("native")), Vector[Provenance.Param](Text("none")))
      case TemplateFitRoute.ImportedFixed(value) =>
        (
          Vector[Provenance.Param](Text("imported-fixed")),
          Vector[Provenance.Param](
            Text(value.trainingHash),
            Text(value.backend),
            Num(value.rank.toDouble),
            Num(value.observations.toDouble)
          ) ++ value.coefficients.map(Num.apply)
        )

    val norm = plan.route match
      case TemplateFitRoute.Native()         => LeastSquares.normConvention.toString
      case TemplateFitRoute.ImportedFixed(_) => "external-receipt"
    Vector(
      "normConvention" -> Vector(Text(norm)),
      "template.input" -> Vector(Text(plan.inputHash.render)),
      "design"         -> design,
      "fittingRoute"   -> route,
      "receipt"        -> receipt,
      "method"         -> Vector(Text(split.design.method)),
      "features"       -> split.design.featureNames.map(Text.apply),
      "responseUnit"   -> Vector(Text(split.design.responseUnit)),
      "heldOutGroups"  -> split.heldOutGroups.toVector.sorted.map(Text.apply),
      "training"       -> Vector(
        Text(split.training.hash.render),
        Num(split.training.rows.size.toDouble)
      ),
      "heldOut" -> Vector(
        Text(split.heldOut.hash.render),
        Num(split.heldOut.rows.size.toDouble)
      ),
      "excluded" -> Vector(Num(split.excluded.size.toDouble))
    )
  def inspect[K, X](plan: TemplateFitPlan[K, X]): Either[DescriptorError, RecipeInspection] =
    val form  = new TemplateFitForm(plan)
    val views = Vector(
      RecipeViews.reference(
        "normConvention",
        "Shared native fdlibm norm arithmetic, or the recorded external fitting backend"
      ),
      RecipeViews.reference(
        "design",
        "Predeclared fixed-feature basis or mean-map split and response units"
      ),
      RecipeViews.reference(
        "fittingRoute",
        "Native fitting or checking an explicitly imported fixed-feature receipt"
      ),
      RecipeViews.reference(
        "receipt",
        "Imported training identity, backend, rank, observations and coefficients; absent for native fitting"
      ),

      RecipeViews
        .reference("template.input", "Complete source rows, including match-group exclusions"),
      RecipeViews
        .reference("method", "Versioned native solver or training-mean template method"),
      RecipeViews.reference(
        "features",
        "Ordered predeclared design columns or training-mean cosine feature"
      ),
      RecipeViews.reference("responseUnit", "Scientific response unit"),
      form.heldOutGroups.view,
      RecipeViews.reference("training", "Training-only identity and admitted row count"),
      RecipeViews.reference("heldOut", "Held-out identity and row count"),
      RecipeViews.reference(
        "excluded",
        "Rows excluded because their match groups occur in evaluation"
      )
    )
    val metadata = views.map(view => view.id.value -> ParameterInfo.literal(view)).toMap
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
            "Fitting uses only admitted training rows; held-out match groups are excluded",
            "Native fitting is one whole operation; evaluation pages whole held-out row keys",
            "Failed predictions remain rows and prevent silently reducing MSE denominators",
            "Imported fixed-feature receipts retain their external backend and source binding; checking a receipt does not run the external fit"
          ),
          ExecutionCapability.SynchronousWholeOperation
        )
      )
