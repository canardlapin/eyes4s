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

import eyes4s.compare.MapSimilarityMethod
import eyes4s.design.*
import eyes4s.kernel.Provenance

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
            V.text("seed", "The sample's 64-bit seed"),
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
