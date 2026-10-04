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
import eyes4s.studio.core.backend.{AnalysisRevision, DatasetRevision, JobId, RunId}
import eyes4s.studio.core.document.*
import eyes4s.studio.core.document.AdmissionDecision.coreDecision
import eyes4s.studio.core.document.DigestJson.given
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
    * sources. `attributes` are the fixation columns without a role, declared
    * so they pass through admission (UI-H); a journal line written before
    * S5.2 has none. `admission` is the new revision's off-screen policy and
    * correction rules; without one, a re-import inherits its `parent`'s and
    * a first import takes the default. A re-admit that changes the policy or
    * the corrections of an admitted revision is therefore one command, and
    * one undo (S5.5). A journal line written before S5.5 has none.
    * `inventory` maps the trials source (S5.4); a journal line written before
    * S5.4 has none.
    */
  case ImportSources(
      parent: Option[DatasetRevision],
      sources: Sources,
      mapping: ColumnMapping,
      units: DeclaredUnits,
      geometry: Geometry,
      attributes: DeclaredAttributes,
      admission: Option[AdmissionChoice],
      inventory: Option[InventoryMapping]
  )

  /** Revise a pending revision's column mapping, declared units, geometry,
    * attributes and inventory mapping in one step (the import wizard's
    * commit, S5.2, S5.4): one undo restores them all.
    */
  case ReviseDataset(
      dataset: DatasetRevision,
      mapping: ColumnMapping,
      units: DeclaredUnits,
      geometry: Geometry,
      attributes: DeclaredAttributes,
      inventory: Option[InventoryMapping]
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

  /** Send a pending revision to the backend for verification (story moment
    * t1): the revision records its content digest as `Verifying`, and the
    * effect [[Effect.RequestAdmission]] carries the same digest. While it
    * is verifying, the revision's content cannot be edited; withdraw the
    * verification first (undo does).
    */
  case VerifyDataset(dataset: DatasetRevision)

  /** Return a verifying revision to `Pending`, so it can be edited again. */
  case WithdrawVerification(dataset: DatasetRevision)

  /** Put a withdrawn verification of `content` back exactly, without a new
    * request (the backend was already asked).
    */
  case ResumeVerification(
      dataset: DatasetRevision,
      content: CanonicalDigest[DatasetRevisionSpec]
  )

  /** Record the admission decision for the content that was verified: the
    * verifying revision becomes admitted, bound to the ledger and inventory
    * the backend produced. `verified` is the digest the backend was sent; it
    * must be the recorded one and the revision's current content. `policy`
    * is the eyes4s `AdmissionDecision` the revision is admitted under,
    * recorded in [[AdmissionDecision.Admitted]] (S5.6); a journal line
    * written before S5.6 has none. A history barrier.
    */
  case Admit(
      dataset: DatasetRevision,
      verified: CanonicalDigest[DatasetRevisionSpec],
      policy: Option[CoreAdmissionDecision],
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
    * dataset (effect [[Effect.RequestRun]]). A history barrier. Without
    * `studio`, the revision keeps its base's name and description and takes
    * the preset its recipe holds (`RecipePresets.resolve`).
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

  /** Ask the backend to cancel a running run's job (effect
    * [[Effect.CancelJob]]); the outcome arrives as [[RecordRunOutcome]].
    */
  case CancelRun(run: RunId)

  /** A backend fact: preparation bound `revision` to its eyes4s plan, by CR3
    * digest, and its input, by semantic identity ("input digest · plan
    * rev N", DESIGN_SPEC section 9). A plan is bound once; an input already
    * recorded must be the same one. Not part of the undo history.
    */
  case BindPlan(
      revision: AnalysisRevision,
      plan: CanonicalDigest[StudyPlanArtifact],
      input: SemanticIdentity
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

  /** Insert `panel` at `index` (0 to the panel count). */
  case AddPanel(figure: FigureId, index: Int, panel: PanelSpec)

  /** Remove a panel; a figure keeps at least one. */
  case RemovePanel(figure: FigureId, panel: PanelLetter)

  case RetitlePanel(figure: FigureId, panel: PanelLetter, title: String)

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
    case _: (ImportSources | ReviseDataset | RestoreDataset | DiscardDataset | SetMapping |
          SetUnits | SetGeometry | SetOffScreenPolicy | AddCorrection | RemoveCorrection |
          VerifyDataset | WithdrawVerification | ResumeVerification | Admit) =>
      ChangeKind.DatasetReadmit
    case _: (StartDraft | RestoreDraft | ChangeRecipe | RebaseDraft | SaveAndRun |
          RecordRunOutcome | CancelRun | BindPlan) =>
      ChangeKind.AnalysisRerun
    case DiscardDraft => ChangeKind.AnalysisRerun
    case _: (PutReporting | RemoveReporting | CreateFigure | RestoreFigure | DeleteFigure |
          BindFigure | SetPanelScale | SetPanelSelection | AddPanel | RemovePanel |
          RetitlePanel) =>
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

  /** Verify `dataset` for admission; `content` is the digest a later
    * [[Command.Admit]] must carry.
    */
  case RequestAdmission(dataset: DatasetRevision, content: CanonicalDigest[DatasetRevisionSpec])

  /** Cancel the backend job running `run`. */
  case CancelJob(run: RunId, job: JobId)

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
