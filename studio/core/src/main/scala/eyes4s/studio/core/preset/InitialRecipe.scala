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

package eyes4s.studio.core.preset

import cats.syntax.all.*
import eyes4s.plan.DefinitionId
import eyes4s.studio.core.document.*

/** Named initial configuration for the first analysis. Geometry and units
  * come from the admitted dataset; the offered preset declares the phases.
  * These are editable analysis defaults, not a computed result.
  */
object InitialRecipe:
  val GridColumns: Int             = 64
  val GridRows: Int                = 48
  val ScaleDegrees: Vector[Double] = Vector(0.5, 1.0, 2.0, 4.0)

  def of(
      dataset: DatasetRevisionSpec,
      preset: Preset
  ): Either[DocumentError, (Recipe, StudioFields)] =
    for
      selected <- RecipePresets
        .of(preset)
        .toRight(DocumentError.PresetWithoutDefaults(preset, dataset.id))
      grid   <- GridSize.of(GridColumns, GridRows)
      scales <- ScaleDegrees.traverse(Sigma.of).flatMap(ScaleSet.of)
      image = dataset.geometry.image
      window <- AnalysisWindow.of(
        image.left.toDouble,
        image.top.toDouble,
        image.left.toDouble + image.width,
        image.top.toDouble + image.height
      )
      name <- RevisionName.of(
        s"${selected.phases.reference.label} → ${selected.phases.focal.label}"
      )
      recipe = selected.applyTo(
        Recipe(
          None,
          selected.layout,
          MethodSpec(DefinitionRef.fromCore(DefinitionId.cosine), Vector.empty),
          selected.phases,
          WeightChoice.Duration,
          FailureChoice.RequireAll,
          grid,
          Some(window),
          Some(OffWindowChoice.Exclude),
          scales,
          Some(dataset.geometry.pixelsPerDegree),
          MatchedChoice.RequireOne,
          ControlChoice.SameSelection,
          UnmatchedChoice.ReportNoMatch,
          InitialFixationChoice.KeepAll
        )
      )
    yield (recipe, StudioFields(preset, name, ""))
