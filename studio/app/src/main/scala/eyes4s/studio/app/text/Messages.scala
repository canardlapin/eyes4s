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

package eyes4s.studio.app.text

/** Every user-visible string of the shell, by id (DESIGN_SPEC section 13:
  * strings are owned by studio-app). A template names its arguments by
  * position, `{0}`, `{1}`, …; [[Messages.render]] fills them.
  *
  * Pane view-models add their own ids with their screens.
  */
enum MessageId derives CanEqual:
  // --- App bar and window --------------------------------------------------
  case AppName, UntitledProject, WindowTitle
  case PerspectiveData, PerspectiveExplore, PerspectiveAnalysis, PerspectiveCompare
  case PerspectiveFigures

  // --- Jobs chip and status job slot ----------------------------------------
  case JobsIdle, JobsIdleAccessible, JobTitle, JobQueued, JobCancelling
  case JobPairs, JobCount, JobCountingTotal, JobAtMostTotal, JobRunningAccessible
  case JobFailed, JobFailedWith, JobCancelled, JobCancelledAt, JobReady
  case StageEstimating, StageComparing, StageReducing, StageContrasting
  case DiagnosticsOne, DiagnosticsMany
  case Cancel

  // --- Context strip ---------------------------------------------------------
  case Back, Forward
  case CrumbNewProject, CrumbDataset, CrumbDatasetDraft, CrumbAnalyses, CrumbDraftRevision
  case CrumbRevision, CrumbSummary, CrumbFigures, CrumbPanel, CrumbQuery, CrumbPair
  case CrumbPairWithDesign, CrumbMap, CrumbReduction, CrumbFixation, CrumbRecord
  case LineageEncodingRetrieval, LineageRecognition, LineageCustom
  case SectionSources, SectionColumnMapping, SectionTrialMetadata, SectionAdmission
  case SectionGeometry
  case DesignMatched, DesignControl

  // --- Freshness -------------------------------------------------------------
  case BadgeShown, BadgeAnalysisPart, BadgeDataPart, BadgePartSeparator, BadgeEmpty
  case BadgeDataset, BadgeShowing, BadgeShowingState, BadgeNewer, BadgeNewerPercent
  case ToneCurrent, ToneStale, ToneRunning, ToneFailed, ToneCancelled, ToneNoRun
  case DatasetPending, DatasetVerifying, NoRunOnDataset, PendingNote
  case DraftChip, ChangesOne, ChangesMany, DraftReady, DraftUnchecked, BlockersOne
  case BlockersMany

  // --- Banner -----------------------------------------------------------------
  case BannerShowing, BannerShowingBrief, BannerShowingStale, BannerFigureShows
  case BannerDraftNotRun, BannerFiguresNeverFollow, BannerNewerRunning
  case BannerNewerRunningChanges, BannerNewerCompleted, BannerNoRun, BannerStaleSuperseded
  case BannerStaleDataset, ChangeAdds, ChangeMovesData, ClauseSeparator
  case ReviewInAnalysis, DiscardDraft, ShowRun, ShowRunWhenFinished, OpenAnalysis

  // --- Status bar ---------------------------------------------------------------
  case SelectedLabel, NoSelection, SelectedMore, Saved, NotSaved
  case PathSeparator, PathDetail, PathPair, PathFixation, PathRecord, PathMap
  case PathReduction, PathSummary, PathSummaryGroup, PathGroupCell, PathPanel
  case PathPanelUnscaled, AllScales, ScaleNumber
  case HintDataFirstRun, HintDataVerify, HintDataPending, HintExplore, HintAnalysis
  case HintCompareSummary, HintCompareQuery, HintFigures

  // --- Commands --------------------------------------------------------------------
  case CommandUndo, CommandRedo, CommandUndoView, CommandRedoView, CommandNextPane
  case CommandMaximize, CommandCancelRun, CommandShowRun, CommandReviewDraft
  case CommandDiscardDraft, CommandImport
  case CommandRenameProject, CommandRevealProject, CommandProjectInfo
  case CommandResetPerspective, MenuView, WindowEdited

  // --- Notices ---------------------------------------------------------------------
  case NoticeUnavailable, NoticeBlocked, NoticeLayoutsReset, Dismiss
  case CommandPreviousPane, CommandNextTab, CommandPreviousTab
  case NoticeSaveFailed

  // --- Shell chrome (S1.6–S1.11) ------------------------------------------------------
  case MenuFile, MenuEdit, MenuGo, MenuRun, MenuWindow, CommandBack, CommandForward
  case PerspectiveAccessible, ProjectAccessible, CrumbCurrentAccessible, CrumbOpensAccessible
  case DraftChipAccessible, ConfirmDiscardDraft, KeepDraft, TabShowTable, TabShowPlot

/** A message catalogue: one template per id. */
trait Catalogue:
  def template(id: MessageId): String

object Catalogue:

  /** The reference English catalogue, in the boards' wording. */
  val english: Catalogue = id =>
    import MessageId.*
    id match
      case AppName             => "Eyes Studio"
      case UntitledProject     => "Untitled project"
      case WindowTitle         => "{0}.eyes"
      case PerspectiveData     => "Data"
      case PerspectiveExplore  => "Explore"
      case PerspectiveAnalysis => "Analysis"
      case PerspectiveCompare  => "Compare"
      case PerspectiveFigures  => "Figures"

      case JobsIdle             => "No jobs"
      case JobsIdleAccessible   => "Jobs: none running"
      case JobTitle             => "Run {0}"
      case JobQueued            => "Queued"
      case JobCancelling        => "Cancelling…"
      case JobPairs             => "{0} / {1} pairs"
      case JobCount             => "{0} / {1}"
      case JobCountingTotal     => "counting…"
      case JobAtMostTotal       => "≤ {0}"
      case JobRunningAccessible => "Run {0} {1}, {2} of {3} pairs"
      case JobFailed            => "Run {0} failed"
      case JobFailedWith        => "Run {0} failed · {1}"
      case JobReady             => "Run {0} ready — Show"
      case JobCancelled         => "Run {0} cancelled"
      case JobCancelledAt       => "Run {0} cancelled · {1}"
      case StageEstimating      => "Estimating"
      case StageComparing       => "Comparing"
      case StageReducing        => "Reducing"
      case StageContrasting     => "Contrasting"
      case DiagnosticsOne       => "{0} diagnostic"
      case DiagnosticsMany      => "{0} diagnostics"
      case Cancel               => "Cancel"

      case Back                     => "Back ({0})"
      case Forward                  => "Forward ({0})"
      case CrumbNewProject          => "New project"
      case CrumbDataset             => "Dataset {0}"
      case CrumbDatasetDraft        => "Dataset {0} (draft)"
      case CrumbAnalyses            => "Analyses"
      case CrumbDraftRevision       => "Draft {0}"
      case CrumbRevision            => "Analysis {0}"
      case CrumbSummary             => "Summary · {0}"
      case CrumbFigures             => "Figures"
      case CrumbPanel               => "Panel {0}"
      case CrumbQuery               => "{0} · {1}"
      case CrumbPair                => "pair {0} × {1}"
      case CrumbPairWithDesign      => "pair {0} × {1} · {2}"
      case CrumbMap                 => "map {0} · {1}"
      case CrumbReduction           => "{0} · {1} reduction · {2}"
      case CrumbFixation            => "{0} · fixation {1}"
      case CrumbRecord              => "{0} record {1}"
      case LineageEncodingRetrieval => "Reinstatement · Enc→Ret"
      case LineageRecognition       => "Recognition"
      case LineageCustom            => "Custom analysis"
      case SectionSources           => "Sources"
      case SectionColumnMapping     => "Column mapping"
      case SectionTrialMetadata     => "Trial metadata"
      case SectionAdmission         => "Admission"
      case SectionGeometry          => "Geometry"
      case DesignMatched            => "matched"
      case DesignControl            => "control"

      case BadgeShown         => "Analysis {0} · {1} · data {2} · {3}"
      case BadgeAnalysisPart  => "Analysis {0}"
      case BadgeDataPart      => "data {0}"
      case BadgePartSeparator => " · "
      case BadgeEmpty         => "No dataset · no analysis"
      case BadgeDataset       => "Dataset {0} · {1} · {2}"
      case BadgeShowing       => "Showing analysis {0} · {1} · data {2}"
      case BadgeShowingState  => "Showing analysis {0} · {1} · data {2} · {3}"
      case BadgeNewer         => "{0} · {1} running"
      case BadgeNewerPercent  => "{0} · {1} running · {2}"
      case ToneCurrent        => "current"
      case ToneStale          => "stale"
      case ToneRunning        => "running"
      case ToneFailed         => "failed"
      case ToneCancelled      => "cancelled"
      case ToneNoRun          => "no run yet"
      case DatasetPending     => "draft"
      case DatasetVerifying   => "verifying"
      case NoRunOnDataset     => "no run on {0} yet"
      case PendingNote        => "Run {0} ({1}) used {2} · becomes stale when {3} is admitted"
      case DraftChip          => "Draft {0} · {1} · {2}"
      case ChangesOne         => "{0} change"
      case ChangesMany        => "{0} changes"
      case DraftReady         => "ready"
      case DraftUnchecked     => "not checked"
      case BlockersOne        => "{0} blocker"
      case BlockersMany       => "{0} blockers"

      case BannerShowing            => "Showing {0} (analysis {1})."
      case BannerShowingBrief       => "Showing {0} ({1})."
      case BannerShowingStale       => "Showing {0} (analysis {1}, data {2})."
      case BannerFigureShows        => "{0} shows {1} (analysis {2})."
      case BannerDraftNotRun        => "Draft {0} {1} and has not been run."
      case BannerFiguresNeverFollow => "Figures never follow a draft."
      case BannerNewerRunning       =>
        "Run {0} ({1}) is running — results will not replace this view until you choose Show."
      case BannerNewerRunningChanges =>
        "Run {0} ({1}, {2}) is running — results will not replace this view until you " +
          "choose Show."
      case BannerNewerCompleted  => "Run {0} ({1}) has finished — choose Show to see it."
      case BannerNoRun           => "Compare · No run yet"
      case BannerStaleSuperseded => "Stale · {0} belongs to {1}"
      case BannerStaleDataset    => "Stale · {0} used data {1}; {2} is admitted"
      case ChangeAdds            => "adds {0}"
      case ChangeMovesData       => "moves to data {0}"
      case ClauseSeparator       => "; "
      case ReviewInAnalysis      => "Review in Analysis"
      case DiscardDraft          => "Discard draft"
      case ShowRun               => "Show run {0}"
      case ShowRunWhenFinished   => "Show run {0} · when finished"
      case OpenAnalysis          => "Open Analysis"

      case SelectedLabel      => "Selected:"
      case NoSelection        => "No selection"
      case SelectedMore       => "{0} (+{1} more)"
      case Saved              => "Saved {0}"
      case NotSaved           => "Not saved"
      case PathSeparator      => " › "
      case PathDetail         => "{0} · {1}"
      case PathPair           => "{0} › {1} × {2} ({3}) · {4}"
      case PathFixation       => "fixation {0}"
      case PathRecord         => "{0} record {1}"
      case PathMap            => "{0} map"
      case PathReduction      => "{0} {1} reduction"
      case PathSummary        => "{0} · {1} · {2}"
      case PathSummaryGroup   => "{0} · {1} › {2} · {3}"
      case PathGroupCell      => "{0} · {1} · {2}"
      case PathPanel          => "{0} › Panel {1} ({2} · {3})"
      case PathPanelUnscaled  => "{0} › Panel {1} ({2})"
      case AllScales          => "all scales"
      case ScaleNumber        => "scale {0}"
      case HintDataFirstRun   => "Drop files anywhere in this window"
      case HintDataVerify     => "Mapping and geometry changes make a new dataset revision"
      case HintDataPending    => "Mapping and geometry changes apply when {0} is admitted"
      case HintExplore        => "View state only · no analysis changed"
      case HintAnalysis       => "Edits change the draft only · runs are never relabelled"
      case HintCompareSummary => "Enter on a participant opens its queries · F6 next pane"
      case HintCompareQuery   => "Prev / Next steps controls · F6 next pane"
      case HintFigures        => "Arrow keys move between panels · Enter opens the binding"

      case CommandUndo         => "Undo"
      case CommandRedo         => "Redo"
      case CommandUndoView     => "Undo view change"
      case CommandRedoView     => "Redo view change"
      case CommandNextPane     => "Next pane"
      case CommandMaximize     => "Maximize the focused group"
      case CommandCancelRun    => "Cancel run"
      case CommandShowRun      => "Show the finished run"
      case CommandReviewDraft  => "Review draft in Analysis"
      case CommandDiscardDraft => "Discard draft"
      case CommandImport       => "Import sources…"

      case CommandRenameProject    => "Rename…"
      case CommandRevealProject    => "Reveal in Finder"
      case CommandProjectInfo      => "Project info"
      case CommandResetPerspective => "Reset perspective"
      case MenuView                => "View"
      case WindowEdited            => "{0} — Edited"

      case NoticeUnavailable  => "{0} is not available now."
      case NoticeBlocked      => "{0}: {1}"
      case NoticeLayoutsReset =>
        "The saved layout of {0} could not be read ({1}); it shows the default layout."
      case Dismiss             => "Dismiss"
      case CommandPreviousPane => "Previous pane"
      case CommandNextTab      => "Next tab"
      case CommandPreviousTab  => "Previous tab"
      case NoticeSaveFailed    => "The project could not be saved: {0}"

      case CommandBack            => "Back"
      case CommandForward         => "Forward"
      case MenuFile               => "File"
      case MenuEdit               => "Edit"
      case MenuGo                 => "Go"
      case MenuRun                => "Run"
      case MenuWindow             => "Window"
      case PerspectiveAccessible  => "{0} ({1})"
      case ProjectAccessible      => "Project {0}"
      case CrumbCurrentAccessible => "{0}, current location"
      case CrumbOpensAccessible   => "{0}, opens in {1}"
      case DraftChipAccessible    => "{0}; review in Analysis"
      case ConfirmDiscardDraft    => "Discard draft {0} and its {1}? Runs are not affected."
      case KeepDraft              => "Keep draft"
      case TabShowTable           => "Show table"
      case TabShowPlot            => "Show plot"

/** Renders catalogue templates. Total: an argument a template does not name
  * is ignored, and a placeholder with no argument is left as written, so a
  * catalogue mistake shows on screen rather than failing.
  */
final case class Messages(catalogue: Catalogue):

  def apply(id: MessageId, args: String*): String =
    Messages.fill(catalogue.template(id), args.toVector)

object Messages:
  val english: Messages = Messages(Catalogue.english)

  /** Each `{i}` of `template`: its start, end (exclusive) and index. */
  private def slots(template: String): Vector[(Int, Int, Int)] =
    val found = Vector.newBuilder[(Int, Int, Int)]
    var i     = 0
    while i < template.length do
      if template.charAt(i) == '{' then
        val close  = template.indexOf('}', i + 1)
        val digits = if close > i + 1 then template.substring(i + 1, close) else ""
        if digits.nonEmpty && digits.length <= 3 && digits.forall(_.isDigit) then
          found += ((i, close + 1, digits.toInt))
          i = close + 1
        else i += 1
      else i += 1
    found.result()

  /** `template` with each `{i}` replaced by `args(i)`, when there is one. */
  def fill(template: String, args: Vector[String]): String =
    val out  = new StringBuilder
    val last = slots(template).foldLeft(0) { case (from, (start, end, index)) =>
      out.append(template.substring(from, start))
      out.append(args.lift(index).getOrElse(template.substring(start, end)))
      end
    }
    out.append(template.substring(last)).toString

  /** The indices of the placeholders a template names. */
  def placeholders(template: String): Set[Int] = slots(template).map(_._3).toSet
