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

import eyes4s.plan.{PlanError, RecipeFamily, StudyPhases, UnmatchedFocalPolicy}
import eyes4s.studio.core.assets.DisplayKind
import eyes4s.studio.core.backend.{
  Eligibility,
  Phase,
  StudioDiagnostic,
  TrialDisposition,
  TrialKey
}
import eyes4s.studio.core.document.{
  PhasePair,
  Preset,
  Recipe,
  RecipeChange,
  RecipeField,
  UnmatchedChoice
}

/** How a preset reads a query that eyes4s leaves without a matched reference
  * (`study-finding.unmatched-focal`).
  */
enum UnmatchedReading derives CanEqual:
  /** Every such query is a finding of the data: "No match". */
  case NoMatch

  /** A query whose participant has no reference-phase trial of its item in
    * the trial inventory has none by design ("No corresponding study trial
    * (by design)": lures and novel probes); one whose reference the
    * inventory declares but admission dropped is still "No match".
    */
  case ByDesignWhenUndeclared

/** Whether the trial inventory declares a reference-phase trial of a query's
  * participant and item: the design as declared, before admission.
  */
enum InventoryReference derives CanEqual:
  case Declared, Undeclared

/** The category of one resolved query under a preset: the board's count
  * strip. Only an eligible query has a matched reference.
  */
enum QueryCategory derives CanEqual:
  case Eligible(matched: TrialKey)
  case NoMatch(diagnostic: StudioDiagnostic)
  case NotAdmitted(disposition: TrialDisposition)

  /** No corresponding reference trial, by the preset's design. */
  case ByDesign

/** Why a resolved query has no category under a preset. Each case names the
  * query and the trials that disagree.
  */
enum PresetError derives CanEqual:
  /** The backend called `query` eligible without naming its match. */
  case EligibleWithoutMatch(preset: Preset, query: TrialKey)

  /** The backend matched `query` to `matched`, but the inventory declares no
    * reference-phase trial of its item: a match the design does not have.
    */
  case UndeclaredMatch(preset: Preset, query: TrialKey, matched: TrialKey)

  def message: String = this match
    case EligibleWithoutMatch(p, q) =>
      s"Query ${q.label} (${q.phase.label}) is eligible under the $p preset but names no " +
        "matched reference."
    case UndeclaredMatch(p, q, m) =>
      s"Query ${q.label} (${q.phase.label}) is matched to ${m.label} (${m.phase.label}) under " +
        s"the $p preset, but the trial inventory declares no ${m.phase.label} trial of its " +
        "item for that participant."

/** One studio recipe preset (ticket S7.1; Analysis.dc.html, recipe): the
  * defaults and wording of an analysis over the one eyes4s fixation-study
  * engine. A preset fills the library's own recipe: it declares the focal
  * (query) and reference phases eyes4s `StudyPhases` accepts and, where its
  * design needs one, the eyes4s `UnmatchedFocalPolicy`. Every other recipe
  * field is the analyst's.
  *
  * Queries are paired with references of the same participant and item: the
  * layout's stimulus projection, the only key eyes4s pairs on. The display
  * kinds are the preset's convention for what each phase showed, not a
  * reading of the data; the inventory states each trial's own kind.
  */
final class RecipePreset private[preset] (
    val preset: Preset,
    val phases: PhasePair,
    val unmatched: Option[UnmatchedFocalPolicy],
    val queryDisplay: DisplayKind,
    val referenceDisplay: DisplayKind,
    val reading: UnmatchedReading
):
  /** The eyes4s recipe family every preset fills. */
  def family: RecipeFamily = RecipeFamily.FixationStudy

  /** The phases as eyes4s checks them: non-blank and different. */
  def corePhases: Either[PlanError, StudyPhases] =
    StudyPhases.of(phases.focal.label, phases.reference.label)

  /** The recipe fields the preset sets, in field order. */
  def declared: Vector[RecipeField] =
    Vector(RecipeField.Phases) ++ unmatched.map(_ => RecipeField.UnmatchedFocal)

  /** `recipe` with the preset's declared fields; every other field kept. */
  def applyTo(recipe: Recipe): Recipe =
    val phased = recipe.copy(phases = phases)
    unmatched.fold(phased)(u => phased.copy(unmatched = UnmatchedChoice.of(u)))

  /** Whether `recipe` already holds every declared field. */
  def holds(recipe: Recipe): Boolean = applyTo(recipe) == recipe

  /** The plan.diff of choosing this preset for `recipe`: one change per
    * declared field that differs, in field order.
    */
  def changes(recipe: Recipe): Vector[RecipeChange] =
    RecipeChange.between(recipe, applyTo(recipe))

  /** The category of a resolved query under this preset. A query is by
    * design only when eyes4s left it unmatched, the preset reads such a
    * query as by design, and the inventory declares no reference for it; a
    * match the inventory does not declare is refused, never shown.
    */
  def categorize(
      query: TrialKey,
      eligibility: Eligibility,
      matched: Option[TrialKey],
      reference: InventoryReference
  ): Either[PresetError, QueryCategory] = eligibility match
    case Eligibility.QueryNotAdmitted(d) => Right(QueryCategory.NotAdmitted(d))
    case Eligibility.NoMatch(diagnostic) =>
      (reading, reference) match
        case (UnmatchedReading.ByDesignWhenUndeclared, InventoryReference.Undeclared) =>
          Right(QueryCategory.ByDesign)
        case _ => Right(QueryCategory.NoMatch(diagnostic))
    case Eligibility.Eligible =>
      (matched, reference) match
        case (None, _) => Left(PresetError.EligibleWithoutMatch(preset, query))
        case (Some(m), InventoryReference.Undeclared) =>
          Left(PresetError.UndeclaredMatch(preset, query, m))
        case (Some(m), InventoryReference.Declared) => Right(QueryCategory.Eligible(m))

/** The presets the Analysis perspective offers, in the board's order. */
object RecipePresets:
  val encodingRetrieval: RecipePreset = new RecipePreset(
    Preset.EncodingRetrieval,
    PhasePair(Phase.Retrieval, Phase.Encoding),
    None,
    DisplayKind.BlankWithFixationCross,
    DisplayKind.Image,
    UnmatchedReading.NoMatch
  )

  val perceptionImagery: RecipePreset = new RecipePreset(
    Preset.PerceptionImagery,
    PhasePair(Phase("Imagery"), Phase("Perception")),
    None,
    DisplayKind.Blank,
    DisplayKind.Image,
    UnmatchedReading.NoMatch
  )

  /** Old probes are compared with their study trial; lures and novel probes
    * have none by design, so a query without a match is reported, never a
    * reason to refuse the study.
    */
  val recognition: RecipePreset = new RecipePreset(
    Preset.Recognition,
    PhasePair(Phase("Recognition"), Phase("Study")),
    Some(UnmatchedFocalPolicy.ReportNoMatch),
    DisplayKind.Image,
    DisplayKind.Image,
    UnmatchedReading.ByDesignWhenUndeclared
  )

  val all: Vector[RecipePreset] = Vector(encodingRetrieval, perceptionImagery, recognition)

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
