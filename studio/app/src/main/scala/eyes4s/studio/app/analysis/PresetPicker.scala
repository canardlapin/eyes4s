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

package eyes4s.studio.app.analysis

import eyes4s.studio.app.Intent
import eyes4s.plan.UnmatchedKind
import eyes4s.studio.app.text.{PresetText, PresetTextId, UnmatchedText}
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.{DraftContext, Preset, Recipe, StudioDocument}
import eyes4s.studio.core.freshness.FreshnessText
import eyes4s.studio.core.preset.{InitialRecipe, RecipePreset, RecipePresets}

/** One preset of the picker: its title and line in the board's words,
  * whether the recipe holds it, and the plan.diff choosing it would make.
  */
final case class PresetOptionVM(
    preset: Preset,
    title: String,
    detail: String,
    selected: Boolean,
    changes: String,
    choose: Option[Intent],
    accessible: String
) derives CanEqual

/** The recipe pane's preset picker (ticket S7.1; Analysis.dc.html, recipe):
  * the three presets, the one the edited recipe holds, and a note when it
  * holds none.
  */
final case class PresetPickerVM(
    heading: String,
    options: Vector[PresetOptionVM],
    note: Option[String]
) derives CanEqual

/** The preset picker over the document (ticket S7.1). The edited recipe is
  * the draft's, else the latest revision's. Choosing a preset sets only its
  * declared fields ([[RecipePreset.changes]]), in one undoable command, so the
  * draft's plan.diff is exactly those fields. Save & run records the preset
  * the saved recipe holds. Pure.
  */
object PresetPicker:

  /** The presets offered, in the board's order, each with its line. */
  val offered: Vector[(RecipePreset, PresetTextId)] = Vector(
    RecipePresets.encodingRetrieval -> PresetTextId.DetailEncodingRetrieval,
    RecipePresets.perceptionImagery -> PresetTextId.DetailPerceptionImagery,
    RecipePresets.recognition       -> PresetTextId.DetailRecognition
  )

  /** The revision the draft starts from (or would) and the recipe edited. */
  def edited(document: StudioDocument): Option[(DraftContext, Recipe)] =
    document.draftContext
      .map(c => (c, c.recipe))
      .orElse(document.latestAnalysis.map(a => (DraftContext.saved(a.id, a), a.recipe)))

  /** The preset the edited recipe holds, if there is a recipe. */
  def selected(document: StudioDocument): Option[Preset] =
    edited(document).map((base, recipe) => RecipePresets.resolve(base.studio.preset, recipe))

  /** The one document command that chooses `preset`, an edit undone in one
    * step: a draft of the latest revision with the preset's changes when
    * there is no draft, else those changes to the draft together. None when
    * the recipe already holds it, or there is no revision, or it is `Custom`
    * (a recipe becomes custom by editing its fields, not by a choice).
    */
  def command(document: StudioDocument, preset: Preset): Option[Command] =
    (edited(document), RecipePresets.of(preset)) match
      case (Some((base, recipe)), Some(p)) =>
        val changes = p.changes(recipe)
        Option.when(changes.nonEmpty)(
          if document.draft.isEmpty then Command.StartDraft(base.id, None, changes)
          else Command.ChangeRecipes(changes)
        )
      case (None, Some(_)) if document.analyses.isEmpty && document.draft.isEmpty =>
        document.latestAdmitted.flatMap(dataset =>
          InitialRecipe
            .of(dataset, preset)
            .toOption
            .map((recipe, studio) => Command.StartAnalysis(dataset.id, recipe, studio))
        )
      case _ => None

  def vm(document: StudioDocument): PresetPickerVM =
    import PresetTextId.*
    val target  = edited(document)
    val current = selected(document)
    val options = offered.map { (p, line) =>
      val title  = PresetText(Title, p.phases.reference.label, p.phases.focal.label)
      val detail = line match
        case DetailRecognition =>
          PresetText(
            line,
            UnmatchedText(UnmatchedKind.NoReferenceInDesign, p.phases.reference.label)
          )
        case _ => PresetText(line, p.phases.focal.label, p.phases.reference.label)
      val changes = target.map((_, recipe) => p.changes(recipe))
      val diff    = changes.fold(PresetText(NoAnalysis)) { cs =>
        if cs.isEmpty then PresetText(Unchanged)
        else PresetText(Changes, FreshnessText.describe(cs))
      }
      PresetOptionVM(
        preset = p.preset,
        title = title,
        detail = detail,
        selected = current.contains(p.preset),
        changes = diff,
        choose = command(document, p.preset).map(_ => Intent.ChoosePreset(p.preset)),
        accessible = PresetText(OptionAccessible, title, detail, diff)
      )
    }
    val note =
      if target.isEmpty then Some(PresetText(NoAnalysis))
      else Option.when(current.contains(Preset.Custom))(PresetText(CustomNote))
    PresetPickerVM(PresetText(Heading), options, note)
