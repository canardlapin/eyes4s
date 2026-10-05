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

import eyes4s.plan.{
  PlanError,
  RecipeFamily,
  StudyPhases,
  TrialKeyDefinitions,
  UnmatchedFocalPolicy
}
import eyes4s.studio.core.backend.Phase
import eyes4s.studio.core.document.{
  DefinitionRef,
  PhasePair,
  Preset,
  Recipe,
  RecipeChange,
  RecipeField,
  UnmatchedChoice
}

/** One studio recipe preset (ticket S7.1; Analysis.dc.html, recipe): the
  * defaults and wording of an analysis over the one eyes4s fixation-study
  * engine. A preset fills the library's own recipe: the match layout (eyes4s
  * `TrialKeyDefinitions.trialLayout`, which pairs on participant and item, as
  * every studio dataset is keyed by its trial inventory), the focal (query)
  * and reference phases eyes4s `StudyPhases` accepts and, where its design
  * needs one, the eyes4s `UnmatchedFocalPolicy`. Every other recipe field is
  * the analyst's.
  *
  * A preset decides nothing about a query's outcome: whether a query without
  * a match has none by design is eyes4s's `UnmatchedKind`, which the studio
  * only words.
  */
final class RecipePreset private[preset] (
    val preset: Preset,
    val phases: PhasePair,
    val unmatched: Option[UnmatchedFocalPolicy]
):
  /** The eyes4s recipe family every preset fills. */
  def family: RecipeFamily = RecipeFamily.FixationStudy

  /** The match layout: trials keyed by participant, phase, trial and
    * occurrence, paired on their item.
    */
  def layout: DefinitionRef = DefinitionRef.fromCore(TrialKeyDefinitions.trialLayout)

  /** The phases as eyes4s checks them: non-blank and different. */
  def corePhases: Either[PlanError, StudyPhases] =
    StudyPhases.of(phases.focal.label, phases.reference.label)

  /** The recipe fields the preset sets, in field order. */
  def declared: Vector[RecipeField] =
    Vector(RecipeField.Layout, RecipeField.Phases) ++
      unmatched.map(_ => RecipeField.UnmatchedFocal)

  /** `recipe` with the preset's declared fields; every other field kept. */
  def applyTo(recipe: Recipe): Recipe =
    val set = recipe.copy(layout = layout, phases = phases)
    unmatched.fold(set)(u => set.copy(unmatched = UnmatchedChoice.of(u)))

  /** Whether `recipe` already holds every declared field. */
  def holds(recipe: Recipe): Boolean = applyTo(recipe) == recipe

  /** The plan.diff of choosing this preset for `recipe`: one change per
    * declared field that differs, in field order.
    */
  def changes(recipe: Recipe): Vector[RecipeChange] =
    RecipeChange.between(recipe, applyTo(recipe))

/** The presets the Analysis perspective offers, in the board's order. */
object RecipePresets:
  val encodingRetrieval: RecipePreset = new RecipePreset(
    Preset.EncodingRetrieval,
    PhasePair(Phase.Retrieval, Phase.Encoding),
    None
  )

  val perceptionImagery: RecipePreset = new RecipePreset(
    Preset.PerceptionImagery,
    PhasePair(Phase("Imagery"), Phase("Perception")),
    None
  )

  /** Old probes are compared with their study trial; lures and novel probes
    * have none, so a query without a match is reported, never a reason to
    * refuse the study.
    */
  val recognition: RecipePreset = new RecipePreset(
    Preset.Recognition,
    PhasePair(Phase("Recognition"), Phase("Study")),
    Some(UnmatchedFocalPolicy.ReportNoMatch)
  )

  val all: Vector[RecipePreset] = Vector(encodingRetrieval, perceptionImagery, recognition)

  /** The fields any preset declares: what a change must touch before the
    * preset a recipe holds is resolved again.
    */
  val declaredFields: Set[RecipeField] = all.flatMap(_.declared).toSet

  /** The preset of `preset`; `Custom` has none. */
  def of(preset: Preset): Option[RecipePreset] = all.find(_.preset == preset)

  /** The preset whose conventions `recipe` holds: `current` while it still
    * holds them, else the first preset that does, else `Custom` (a recipe
    * that holds no preset's conventions).
    */
  def resolve(current: Preset, recipe: Recipe): Preset =
    of(current)
      .filter(_.holds(recipe))
      .orElse(all.find(_.holds(recipe)))
      .fold(Preset.Custom)(_.preset)
