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
import eyes4s.compare.MapSimilarityMethod
import eyes4s.design.*
import eyes4s.kernel.{Provenance, Unit2D}

/** Choice labels and field views of the repetition family (CR4 S3). */
object RepetitionViews:
  import RecipeViews as V
  import FieldKind.*

  given Labelled[RepetitionRule] with
    def token(value: RepetitionRule): String = value.toString
    def label(value: RepetitionRule): String = value match
      case RepetitionRule.SameParticipant      => "The same participant"
      case RepetitionRule.DifferentParticipant => "Another participant"
      case RepetitionRule.SameStimulus         => "The same stimulus"
      case RepetitionRule.DifferentStimulus    => "Another stimulus"
      case RepetitionRule.SameOccasion         => "The same occasion"
      case RepetitionRule.DifferentOccasion    => "Another occasion"

  given Labelled[MapSimilarityMethod] with
    def token(value: MapSimilarityMethod): String = value.token
    def label(value: MapSimilarityMethod): String = value.token

  val method: FieldView = V.choice(
    "method",
    "The map similarity method every pair is scored with",
    MapSimilarityMethod.values
  )

  private def rules(id: String, meaning: String): FieldView =
    V.view(
      id,
      meaning,
      Repeated(
        V.choice("rule", "One rule of the conjunction", RepetitionRule.values.toVector),
        1,
        Some(3)
      )
    )

  val matched: FieldView =
    rules(
      "matched",
      "The rules a matched pair meets: a conjunction on participant, stimulus and occasion"
    )
  val controls: FieldView =
    rules("controls", "The rules a control pair meets; disjoint from the matched rules")

  val controlSelection: FieldView = V.view(
    "controlSelection",
    "Every eligible control pair, or a keyed bottom-k sample of them per focal trial",
    Variant(
      Vector(
        VariantCase("all", "Every eligible control pair", Vector.empty),
        VariantCase(
          "bottomK",
          "At most k control pairs per focal trial, by keyed hash",
          Vector(
            V.atLeastOne("cap", "Control pairs kept per focal trial", Counted.Occurrences),
            V.number(
              "seed",
              "The sample's 64-bit seed",
              Quantity.Dimensionless,
              NumberShape.Int64
            ),
            V.text("sample", "The sample's identity")
          )
        )
      )
    )
  )

  /** The description of a selection: `all`, or the cap, seed and sample. */
  def selection(value: Selection): Vector[Provenance.Param] =
    import Provenance.Param.*
    value match
      case Selection.All                        => Vector(Text("all"))
      case Selection.BottomK(cap, seed, sample) =>
        Vector(
          Text("bottomK"),
          Num(cap.value.toDouble),
          Text(seed.value.toString),
          Text(sample.value)
        )

/** The typed values of a repetition plan's own form fields. */
final case class RepetitionRecipe(
    method: MapSimilarityMethod,
    matched: Vector[RepetitionRule],
    controls: Vector[RepetitionRule],
    selection: Selection,
    policy: FailurePolicy
):
  /** The repetition plan these values describe over `template`'s layout, grid
    * and supplied maps, or the plan's own whole-recipe refusal (relations that
    * conflict or overlap).
    */
  def plan[K, U <: Unit2D: eyes4s.kernel.UnitLabel](
      template: RepetitionPlan[K, U]
  ): Either[RepetitionPlanError, RepetitionPlan[K, U]] =
    RepetitionRelations
      .of(matched, controls)
      .flatMap(relations =>
        RepetitionPlan.of(
          template.layout,
          relations,
          method,
          selection,
          policy,
          template.grid,
          template.trials
        )
      )

/** The form of a repetition plan's own fields. The layout, grid and supplied
  * maps come from outside the form; the matched and control rules are checked
  * together by the plan ([[RepetitionRecipe.plan]]).
  */
final class RepetitionForm:
  import RawParts.*
  import RepetitionViews.given
  private type E = RecipeParameterError
  private def err(e: E): String = e.message

  val method: StructuredField[E, MapSimilarityMethod] =
    StructuredField.of[E, MapSimilarityMethod](RepetitionViews.method)(
      raw => choose("method", raw, MapSimilarityMethod.values),
      choice(_),
      err
    )

  private def rules(view: FieldView): StructuredField[E, Vector[RepetitionRule]] =
    val id = view.id.value
    StructuredField.of[E, Vector[RepetitionRule]](view)(
      {
        case RawValue.Items(items) =>
          items.traverse(choose(id, _, RepetitionRule.values.toVector))
        case _ => Left(missing(id, "items"))
      },
      rs => RawValue.Items(rs.map(choice(_))),
      err
    )

  val matched: StructuredField[E, Vector[RepetitionRule]]  = rules(RepetitionViews.matched)
  val controls: StructuredField[E, Vector[RepetitionRule]] = rules(RepetitionViews.controls)

  val controlSelection: StructuredField[E, Selection] =
    StructuredField.of[E, Selection](RepetitionViews.controlSelection)(
      raw =>
        token("controlSelection", raw).flatMap {
          case "all"     => Right(Selection.All)
          case "bottomK" =>
            for
              cap    <- int("controlSelection", raw, "cap")
              limit  <- PairLimit.of(cap).left.map(RecipeParameterError.Pairing.apply)
              seed   <- long("controlSelection", raw, "seed")
              sample <- tokenPart("controlSelection", raw, "sample")
            yield Selection.BottomK(limit, Seed(seed), SampleId(sample))
          case t => Left(unknown("controlSelection", t, Vector("all", "bottomK")))
        },
      {
        case Selection.All                        => variant("all")
        case Selection.BottomK(cap, seed, sample) =>
          variant(
            "bottomK",
            "cap"    -> number(cap.value),
            "seed"   -> number(seed.value),
            "sample" -> RawValue.Text(sample.value)
          )
      },
      err
    )

  val failurePolicy: StructuredField[E, FailurePolicy] =
    StructuredField.of[E, FailurePolicy](RecipeViews.failurePolicy)(
      raw =>
        optional(raw).fold(Right(FailurePolicy.RequireAll))(n =>
          intOf("failurePolicy", n).flatMap(
            FailurePolicy.successfulOnly(_).left.map(RecipeParameterError.Reduction.apply)
          )
        ),
      {
        case FailurePolicy.RequireAll        => RawValue.Absent
        case FailurePolicy.SuccessfulOnly(m) => number(m.value)
      },
      err
    )

  val fields: Vector[FormField[E, ?]] =
    Vector(method, matched, controls, controlSelection, failurePolicy)
  def views: Vector[FieldView] = fields.map(_.view)

  def validate(field: FieldId, raw: RawValue): Either[FieldError[E], Unit] =
    fields
      .find(_.view.id == field)
      .toRight(FieldError.UnknownField(field))
      .flatMap(_.parse(raw).map(_ => ()))

  def parse(values: FormValues): Either[FormParse.Refusals, RepetitionRecipe] =
    val m = method.parse(values.get(method.view.id))
    val a = matched.parse(values.get(matched.view.id))
    val c = controls.parse(values.get(controls.view.id))
    val s = controlSelection.parse(values.get(controlSelection.view.id))
    val p = failurePolicy.parse(values.get(failurePolicy.view.id))
    FormParse.errors(m, a, c, s, p).toLeft(()).flatMap { _ =>
      (for m1 <- m; a1 <- a; c1 <- c; s1 <- s; p1 <- p
      yield RepetitionRecipe(m1, a1, c1, s1, p1)).left
        .map(cats.data.NonEmptyVector.one)
    }

  def values[K, U <: Unit2D](plan: RepetitionPlan[K, U]): FormValues =
    FormValues.of(
      method.view.id           -> method.raw(plan.method),
      matched.view.id          -> matched.raw(plan.relations.matched),
      controls.view.id         -> controls.raw(plan.relations.controls),
      controlSelection.view.id -> controlSelection.raw(plan.controls),
      failurePolicy.view.id    -> failurePolicy.raw(plan.policy)
    )
