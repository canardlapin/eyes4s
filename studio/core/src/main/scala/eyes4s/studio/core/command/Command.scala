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

import eyes4s.studio.core.backend.{AnalysisRevision, DatasetRevision, RunId}
import eyes4s.studio.core.document.*
import io.circe.Codec

/** The four kinds of change (DESIGN_SPEC section 8), tagged the same
  * everywhere. Only `ViewOnly` commands leave the science untouched.
  */
enum ChangeKind derives CanEqual, Codec.AsObject:
  case DatasetReadmit, AnalysisRerun, ReportingNoRerun, ViewOnly

  def label: String = this match
    case DatasetReadmit   => "Dataset · re-admit"
    case AnalysisRerun    => "Analysis · rerun"
    case ReportingNoRerun => "Reporting · no rerun"
    case ViewOnly         => "View only"

/** A document edit (ticket S2.2): data, serialised in the autosave journal
  * ([[CommandJournal]]) and by scripting (S3.6). Commands name their targets
  * and carry the new value; the reducer ([[Reducer]]) checks them against the
  * document and computes each one's inverse from the state it replaces.
  *
  * Hover and selection are not commands: they are app state (S1.0, S3.3).
  * Nor is a backend job handle (session state, [[JobHandle]]).
  *
  * The `Restore*` cases are the exact inverses of the removals (they put a
  * captured value back at its id); a script may use them, and they are
  * validated like every other command.
  */
enum Command derives CanEqual, Codec.AsObject:

  // --- Dataset · re-admit --------------------------------------------------

  /** A new pending dataset revision with the next id, from freshly imported
    * sources; a re-import names its `parent` and inherits its admission
    * choices.
    */
  case ImportSources(
      parent: Option[DatasetRevision],
      sources: Sources,
      mapping: ColumnMapping,
      units: DeclaredUnits,
      geometry: Geometry
  )

  /** Put a discarded pending dataset revision back. */
  case RestoreDataset(dataset: DatasetRevisionSpec)

  /** Drop a pending dataset revision nothing refers to. */
  case DiscardDataset(dataset: DatasetRevision)

  case SetMapping(dataset: DatasetRevision, mapping: ColumnMapping)
  case SetUnits(dataset: DatasetRevision, units: DeclaredUnits)
  case SetGeometry(dataset: DatasetRevision, geometry: Geometry)
  case SetOffScreenPolicy(dataset: DatasetRevision, policy: OffScreenChoice)

  /** Insert a correction rule at `index` (0 to the rule count). */
  case AddCorrection(dataset: DatasetRevision, index: Int, rule: CorrectionRule)
  case RemoveCorrection(dataset: DatasetRevision, index: Int)

  /** Ask the backend to admit a pending revision for verification (story
    * moment t1); the document is unchanged until [[Admit]].
    */
  case VerifyDataset(dataset: DatasetRevision)

  /** Record the admission decision: the pending revision becomes admitted,
    * bound to the ledger and inventory the backend produced. A history
    * barrier.
    */
  case Admit(
      dataset: DatasetRevision,
      ledger: CoreBinding[AdmissionLedgerArtifact],
      inventory: CoreBinding[TrialInventoryArtifact]
  )

  // --- Analysis · rerun ----------------------------------------------------

  /** A draft of `base` with the next analysis id. */
  case StartDraft(
      base: AnalysisRevision,
      dataset: Option[DatasetRevision],
      changes: Vector[RecipeChange]
  )

  /** Put a discarded draft back exactly. */
  case RestoreDraft(draft: Draft)

  /** Change one recipe field of the draft, creating a draft of the latest
    * revision if there is none; `before` must be what the draft's recipe
    * holds. A change that returns every field and the dataset to the base
    * removes the draft.
    */
  case ChangeRecipe(change: RecipeChange)

  /** Configure the draft on `dataset` (its base's dataset clears the
    * rebase), creating a draft of the latest revision if there is none.
    */
  case RebaseDraft(dataset: DatasetRevision)

  case DiscardDraft

  /** Save the draft as an analysis revision and start a run of it on its
    * dataset (effect [[Effect.RequestRun]]). A history barrier.
    */
  case SaveAndRun(studio: Option[StudioFields])

  /** A backend fact: a running run completed, failed or was cancelled. Not
    * part of the undo history.
    */
  case RecordRunOutcome(
      run: RunId,
      state: RunLifecycle,
      archive: CoreBinding[ResultArchiveArtifact]
  )

  // --- Reporting · no rerun ------------------------------------------------

  /** Create a reporting spec, or replace the one with the same id. */
  case PutReporting(spec: ReportingSpec)

  /** Remove a reporting spec no figure binds. */
  case RemoveReporting(reporting: ReportingId)

  /** A figure with the next number, bound to one run and one spec. */
  case CreateFigure(run: RunId, reporting: ReportingId, panels: Vector[PanelSpec])

  /** Put a deleted figure back exactly. */
  case RestoreFigure(figure: FigureSpec)

  case DeleteFigure(figure: FigureId)

  /** Bind (or rebind) a figure to one run and one reporting spec. Keeping a
    * stale figure is not a command: it stays bound and freshness is derived
    * (S2.7).
    */
  case BindFigure(figure: FigureId, run: RunId, reporting: ReportingId)

  case SetPanelScale(figure: FigureId, panel: PanelLetter, scale: PanelScale)
  case SetPanelSelection(figure: FigureId, panel: PanelLetter, selection: PanelSelection)

  // --- View only -----------------------------------------------------------

  case SetPerspective(perspective: Perspective)
  case SetTheme(theme: Theme)
  case SetStage(stage: StageAppearance)
  case SetMapOpacity(opacity: MapOpacity)
  case SetUnderlay(shown: Boolean)

  /** The run Compare and the banners show (`None` for none). */
  case ShowRun(run: Option[RunId])

  /** Save (or, with `None`, clear) a perspective's docking layout. */
  case SaveLayout(perspective: Perspective, layout: Option[LayoutBlob])

  /** The case name, for messages. */
  def name: String = productPrefix

  def kind: ChangeKind = this match
    case _: (ImportSources | RestoreDataset | DiscardDataset | SetMapping | SetUnits |
          SetGeometry | SetOffScreenPolicy | AddCorrection | RemoveCorrection | VerifyDataset |
          Admit) =>
      ChangeKind.DatasetReadmit
    case _: (StartDraft | RestoreDraft | ChangeRecipe | RebaseDraft | SaveAndRun |
          RecordRunOutcome) =>
      ChangeKind.AnalysisRerun
    case DiscardDraft => ChangeKind.AnalysisRerun
    case _: (PutReporting | RemoveReporting | CreateFigure | RestoreFigure | DeleteFigure |
          BindFigure | SetPanelScale | SetPanelSelection) =>
      ChangeKind.ReportingNoRerun
    case _: (SetPerspective | SetTheme | SetStage | SetMapOpacity | SetUnderlay | ShowRun |
          SaveLayout) =>
      ChangeKind.ViewOnly

/** What the application must do after a command: data, performed by the
  * shell or a service, never by the reducer (DESIGN_SPEC section 13).
  */
enum Effect derives CanEqual:
  /** Start `run` of `analysis` on `dataset` (S3.1). */
  case RequestRun(run: RunId, analysis: AnalysisRevision, dataset: DatasetRevision)

  /** Admit a pending dataset revision for verification. */
  case RequestAdmission(dataset: DatasetRevision)

  /** The document changed; schedule a save (S2.4a/b). */
  case Persist

/** Why the science history cannot be undone past a point. */
enum HistoryBarrier derives CanEqual:
  /** Save & run made `analysis` and started `run`; undo never deletes a run. */
  case RunRequested(analysis: AnalysisRevision, run: RunId)

  /** `dataset` was admitted; runs may already stand on it. */
  case DatasetAdmitted(dataset: DatasetRevision)

  def message: String = this match
    case RunRequested(analysis, run) =>
      s"Saving ${analysis.label} started ${run.label}; earlier edits can no longer be undone."
    case DatasetAdmitted(dataset) =>
      s"Dataset ${dataset.label} was admitted; earlier edits can no longer be undone."

/** How a command enters the history. */
enum Recording derives CanEqual:
  /** Undoable by applying `inverse`. */
  case Reversible(inverse: Command)

  /** Irreversible: undo stops here. */
  case Barrier(barrier: HistoryBarrier)

  /** Not an edit to undo: a backend fact or a request with no document
    * change. The stacks are left as they are.
    */
  case Unrecorded
