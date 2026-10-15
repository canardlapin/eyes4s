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
import eyes4s.kernel.Provenance

/** Epochs describe observed anchors and bins for another analysis. They are a
  * component, not a recipe family: no result archive or independent scheduler
  * is implied by their descriptor (bead bd-01M3DH0RZP5ZV2K70PD0ZVS7PK).
  */
object EpochComponent:
  import RecipeViews as V
  import FieldKind.*

  val component: AnalysisComponent = AnalysisComponent.Epochs

  private given Labelled[FinalBin] with
    def token(value: FinalBin): String = value.toString
    def label(value: FinalBin): String = value match
      case FinalBin.RequireExactDivision => "Require complete bins"
      case FinalBin.IncludeShortFinal    => "Include a shorter final bin"
      case FinalBin.ExcludeAndReport     => "Exclude and report the final remainder"

  private val fields = Vector(
    V.reference("markerSchema", "The exact versioned codec of the typed mark key"),
    V.reference("anchor.kind", "The observed mark key selected from the trial timeline"),
    V.view(
      "anchor.occurrence",
      "Which occurrence of the observed mark anchors the trial",
      Variant(
        Vector(
          VariantCase("RequireUnique", "Exactly one matching mark", Vector.empty),
          VariantCase("First", "The first matching mark", Vector.empty),
          VariantCase("Last", "The last matching mark", Vector.empty),
          VariantCase(
            "Nth",
            "A matching mark by zero-based occurrence",
            Vector(
              V.number(
                "index",
                "Zero-based index in stable observed timeline order",
                Quantity.Count(Counted.Occurrences),
                NumberShape.Int64,
                NumericBounds.nonNegative
              )
            )
          )
        )
      )
    ),
    V.view(
      "window",
      "Half-open offsets around the observed anchor; an empty window is legal",
      Group(
        Vector(
          V.signedMicros("from", "Inclusive offset from the observed anchor"),
          V.signedMicros("until", "Exclusive offset from the observed anchor")
        ),
        GroupRule.Independent
      )
    ),
    V.number(
      "binWidth",
      "Positive bin width in exact integer microseconds",
      Quantity.Duration,
      NumberShape.Int64,
      NumericBounds.atLeastOne
    ),
    V.choice(
      "finalBin",
      "How a window remainder smaller than the bin width is handled",
      FinalBin.values.toVector
    )
  )

  /** Descriptive values preserve signed 64-bit microseconds and occurrence
    * indices as text rather than rounding them through binary64.
    */
  def description[K](
      plan: EpochPlan[K],
      markerSchema: DefinitionId
  ): Vector[(String, Vector[Provenance.Param])] =
    import Provenance.Param.Text
    val occurrence = plan.anchor.occurrence match
      case Occurrence.Nth(index) => Vector(Text("Nth"), Text(index.toLong.toString))
      case value                 => Vector(Text(value.toString))
    Vector(
      "markerSchema" -> Vector(Text(markerSchema.name), Text(markerSchema.version.toString)),
      "anchor.kind"  -> Vector(Text(plan.anchor.kind.toString)),
      "anchor.occurrence" -> occurrence,
      "window"            -> Vector(
        Text(plan.window.from.toMicros.toString),
        Text(plan.window.until.toMicros.toString)
      ),
      "binWidth" -> Vector(Text(plan.binWidth.toMicros.toString)),
      "finalBin" -> Vector(Text(plan.finalBin.toString))
    )

  def inspect[K](
      plan: EpochPlan[K],
      markerSchema: DefinitionId
  ): Either[DescriptorError, RecipeInspection] =
    val values     = description(plan, markerSchema)
    val fieldIds   = fields.map(_.id.value)
    val byId       = fields.map(view => view.id.value -> view).toMap
    val valueIds   = values.map(_._1)
    val mismatched =
      (valueIds.filterNot(byId.contains) ++ fieldIds.filterNot(valueIds.contains)).distinct
    if byId.size != fields.size || valueIds.distinct.size != valueIds.size then
      Left(DescriptorError.DuplicateFields(fields.map(_.id.value) ++ valueIds))
    else if mismatched.nonEmpty then Left(DescriptorError.UnexplainedFields(mismatched))
    else
      values
        .traverse { (id, parameters) =>
          byId
            .get(id)
            .toRight(DescriptorError.UnexplainedFields(Vector(id)))
            .map(view => new InspectedParameter(ParameterInfo.literal(view), parameters))
        }
        .map(parameters =>
          new RecipeInspection(
            parameters,
            Vector(
              "ObservedTimeline is required; planned timestamps cannot anchor an epoch.",
              "Equal-time marks retain source order; Nth is zero-based.",
              "Window offsets are half-open and nondecreasing; zero width is permitted.",
              "The caller supplies the allocation budget when resolving bins."
            ),
            ExecutionCapability.SynchronousWholeOperation
          )
        )
