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

package eyes4s.studio.core.command

import eyes4s.codec.CanonicalDigest
import eyes4s.plan.AdmissionDecision as CoreAdmissionDecision
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.*
import eyes4s.studio.core.document.DocumentGen.right
import eyes4s.studio.core.fixture.StoryMoments

/** A named sample of every command case and journal entry, pinned in
  * [[CommandPins]]. Built from the story moments.
  */
object CommandSamples:
  import Command.*
  import StoryMoments.{r2, r3, rev4, run5, run7, run8}

  private val t1      = DocumentSamples.t1
  private val t2      = DocumentSamples.t2
  private val pending = t1.dataset(r3).get
  private val draft   = t2.draft.get
  private val rev4Rec = t2.analysis(rev4).get.recipe
  private val spec    = t2.reporting.head
  private val figure1 = t2.figures.head
  private val panelA  = right(PanelLetter.of("A"))

  /** r3's content digest in t1, as VerifyDataset records it. */
  val verified: CanonicalDigest[DatasetRevisionSpec] =
    DatasetRevisionSpec.contentDigest(pending).toOption.get

  /** A lab column passed through as a number attribute. */
  val pupil: DeclaredAttributes = right(
    DeclaredAttributes.of(
      Vector(AttributeBinding(right(ColumnName.of("Pupil")), AttributeKindChoice.Number))
    )
  )

  private val rule = CorrectionRule(CorrectionTarget.AllTrials, CoordinateCorrection.FlipY)

  val commands: Vector[(String, Command)] = Vector(
    "ImportSources" -> ImportSources(
      Some(r2),
      pending.sources,
      pending.mapping,
      pending.units,
      pending.geometry,
      DeclaredAttributes.empty,
      None,
      None
    ),
    "ImportSources.attributes" -> ImportSources(
      Some(r2),
      pending.sources,
      pending.mapping,
      pending.units,
      pending.geometry,
      pupil,
      None,
      None
    ),
    "ImportSources.admission" -> ImportSources(
      Some(r3),
      pending.sources,
      pending.mapping,
      pending.units,
      pending.geometry,
      DeclaredAttributes.empty,
      Some(AdmissionChoice(OffScreenChoice.QuarantineTrial, Vector(rule))),
      None
    ),
    "ReviseDataset" -> ReviseDataset(
      r3,
      pending.mapping,
      DeclaredUnits(Some(TimeUnit.Seconds)),
      pending.geometry,
      pupil,
      None
    ),
    "RestoreDataset"       -> RestoreDataset(pending),
    "DiscardDataset"       -> DiscardDataset(r3),
    "SetMapping"           -> SetMapping(r3, pending.mapping),
    "SetUnits"             -> SetUnits(r3, DeclaredUnits(Some(TimeUnit.Seconds))),
    "SetGeometry"          -> SetGeometry(r3, pending.geometry),
    "SetOffScreenPolicy"   -> SetOffScreenPolicy(r3, OffScreenChoice.QuarantineTrial),
    "AddCorrection"        -> AddCorrection(r3, 0, rule),
    "RemoveCorrection"     -> RemoveCorrection(r3, 0),
    "VerifyDataset"        -> VerifyDataset(r3),
    "WithdrawVerification" -> WithdrawVerification(r3),
    "ResumeVerification"   -> ResumeVerification(r3, verified),
    "Admit"                -> Admit(
      r3,
      verified,
      Some(CoreAdmissionDecision.ReviewExclusions),
      CoreBinding.unbound,
      CoreBinding.unbound
    ),
    "StartDraft"   -> StartDraft(rev4, None, draft.changes),
    "RestoreDraft" -> RestoreDraft(draft),
    "ChangeRecipe" -> ChangeRecipe(RecipeChange.Grid(rev4Rec.grid, right(GridSize.of(32, 24)))),
    "ChangeRecipes" -> ChangeRecipes(
      Vector(
        RecipeChange.Grid(rev4Rec.grid, right(GridSize.of(32, 24))),
        RecipeChange.Weighting(rev4Rec.weighting, WeightChoice.Uniform)
      )
    ),
    "RebaseDraft"       -> RebaseDraft(r2),
    "DiscardDraft"      -> DiscardDraft,
    "SaveAndRun"        -> SaveAndRun(None),
    "SaveAndRun.studio" -> SaveAndRun(
      Some(StudioFields(Preset.Custom, right(RevisionName.of("σ 8° added")), "rerun"))
    ),
    "RecordRunOutcome" -> RecordRunOutcome(
      run8,
      RunLifecycle.Cancelled(Some(StageKind.Comparing)),
      CoreBinding.unbound
    ),
    "CancelRun" -> CancelRun(run8),
    "BindPlan"  -> BindPlan(
      rev4,
      CanonicalDigest.parse[StudyPlanArtifact]("0123456789abcdef" * 4).toOption.get,
      right(SemanticIdentity.of("00112233445566ff"))
    ),
    "PutReporting"    -> PutReporting(spec),
    "RemoveReporting" -> RemoveReporting(spec.id),
    "CreateFigure"    -> CreateFigure(run7, spec.id, figure1.panels.take(1)),
    "RestoreFigure"   -> RestoreFigure(figure1),
    "DeleteFigure"    -> DeleteFigure(figure1.id),
    "BindFigure"      -> BindFigure(figure1.id, run5, spec.id),
    "SetPanelScale"   -> SetPanelScale(figure1.id, panelA, PanelScale.At(right(Sigma.of(2.0)))),
    "SetPanelSelection" -> SetPanelSelection(figure1.id, panelA, PanelSelection.AllQueries),
    "AddPanel"          -> AddPanel(
      figure1.id,
      5,
      figure1.panels(3).copy(letter = right(PanelLetter.of("F")))
    ),
    "RemovePanel"    -> RemovePanel(figure1.id, right(PanelLetter.of("E"))),
    "RetitlePanel"   -> RetitlePanel(figure1.id, panelA, "Encoding gaze · P17"),
    "SetPerspective" -> SetPerspective(Perspective.Compare),
    "SetTheme"       -> SetTheme(Theme.Dark),
    "SetStage"       -> SetStage(StageAppearance.Mid),
    "SetMapOpacity"  -> SetMapOpacity(right(MapOpacity.of(0.4))),
    "SetUnderlay"    -> SetUnderlay(true),
    "ShowRun"        -> ShowRun(Some(run7)),
    "ShowRun.none"   -> ShowRun(None),
    "SaveLayout" -> SaveLayout(Perspective.Figures, Some(LayoutBlob("""{"root":"figures"}"""))),
    "SaveLayout.clear" -> SaveLayout(Perspective.Data, None)
  )

  val entries: Vector[(String, JournalEntry)] = Vector(
    "entry.Apply"    -> JournalEntry.Apply(SetTheme(Theme.Dark)),
    "entry.Undo"     -> JournalEntry.Undo,
    "entry.Redo"     -> JournalEntry.Redo,
    "entry.UndoView" -> JournalEntry.UndoView,
    "entry.RedoView" -> JournalEntry.RedoView
  )

  /** A short session on t2: a figure edit, a view change, undo and redo,
    * undoing the view change, then Save & run.
    */
  val session: Vector[JournalEntry] = Vector(
    JournalEntry.Apply(SetPanelSelection(figure1.id, panelA, PanelSelection.AllQueries)),
    JournalEntry.Apply(SetTheme(Theme.Dark)),
    JournalEntry.Undo,
    JournalEntry.Redo,
    JournalEntry.UndoView,
    JournalEntry.Apply(SaveAndRun(None))
  )
